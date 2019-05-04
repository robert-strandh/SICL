(in-package #:cleavir-hir-transformations)

;;; FIXME: move to cleavir-ir
(deftype call-instruction ()
  '(or cleavir-ir:funcall-instruction
    cleavir-ir:multiple-value-call-instruction))

;;; Return a list of (e1 . e2), e1 and e2 being enter instructions. Each cons indicates that e1 may call e2.
;;; If a function can escape, it could be called by anything. The graph doesn't include this fact.
;;; ORIGINS is the result of compute-origins below.
(defun generate-call-graph (initial-instruction origins)
  (let (graph)
    (cleavir-ir:map-instructions-with-owner
     (lambda (instruction owner)
       (when (typep instruction 'call-instruction)
         (loop for o in (gethash instruction origins)
               unless (eq o :unknown)
                 do (pushnew (cons owner (cleavir-ir:code o)) graph :test #'equal))))
     initial-instruction)
    graph))

;;; Run some analysis on the simple functions in the given initial-instruction.
;;; It's top-down - if a simple function contains other simple functions, only the
;;; simple function highest up will be analyzed.
;;; So e.g. if L1 includes L2 and L3, and L2 includes L4, and other than L1 they're all simple,
;;; analyze-simples of L1 will call analyzer on only L2 and L3.
;;; FIXME: May analyze an insruction more than once if there are multiple encloses of one ENTER.
;;; But this function isn't used at the moment anyway.
(defun analyze-simples (analyzer initial-instruction)
  (let ((dag (build-function-dag initial-instruction))
        (location-owners (compute-location-owners initial-instruction))
        (destinies (compute-destinies initial-instruction)))
    (let ((trappers (discern-trappers dag destinies))
          (sharing (discern-sharing dag location-owners)))
      (labels ((aux (node)
                 (let ((enter (enter-instruction node)))
                   (if (and (null (cdr (gethash enter sharing))) ; toplevel
                            (gethash enter trappers)) ; trapper
                       (funcall analyzer enter)
                       (mapc #'aux (children node))))))
          (aux dag))))
  (values))

;;; Return a hash from ENTERs (representing functions) to booleans.
;;; If the boolean is T, the function doesn't have any sub-functions that escape.
;;; (It may, itself, escape, but that's irrelevant here.)
;;; Obvious future refinement: If a function escapes, but not to outside a function,
;;; that function can still be a trapper.
(defun discern-trappers (function-dag destinies)
  (let ((result (make-hash-table :test #'eq)))
    (labels ((aux (node)
               ;; FIXME: recurses into children multiple times. Efficiency concern
               (let* ((enclose (enclose-instruction node))
                      (enter (cleavir-ir:code enclose))
                      ;; if there no children, recursion stops here.
                      (child-non-escapes (mapcar #'aux (children node))))
                 (setf (gethash enter result)
                       (every #'identity child-non-escapes)) ; no child escapes
                 ;; Now return whether WE don't escape, for our caller's sake.
                 (not (member :escape (gethash enclose destinies))))))
      ;; We have to do the recursion funny since the initial instruction
      ;; has no enclose/cannot escape.
      (setf (gethash (initial-instruction function-dag) result)
            (every #'aux (children function-dag))))
    result))

(defun data (instruction)
  (append (cleavir-ir:inputs instruction)
	  (cleavir-ir:outputs instruction)))

;;; Given an initial instruction and this stuff v, computes a hash table
;;; with each ENTER in the graph as a key (representing functions), and conses as values.
;;; The car of the cons is a list of lexical locations owned by the function
;;; that are referred to in other functions, and the cdr is a list of locations
;;; referred to by the function that are owned by other functions.
(defun discern-sharing (function-dag location-owners)
  (let ((result (make-hash-table :test #'eq)))
    (labels ((init-enter (enter)
               ;; Note we may initialize some enters more than once if there are
               ;; multiple encloses of the same ENTER.
               (setf (gethash enter result) (cons nil nil)))
             (mark-binder (enter location)
               (pushnew location (car (gethash enter result))))
             (mark-user (enter location)
               (pushnew location (cdr (gethash enter result))))
             (mark-up (this owner location)
               ;; THIS is an enter instruction node, OWNER is LOCATION's owner (which is not THIS).
               ;; Mark THIS and intermediate functions as closing over location.
               (loop with nodes = (gethash this (dag-nodes function-dag))
                     until (null nodes)
                     do (let ((node (pop nodes)))
                          (unless (typep node 'function-dag) ; no ENCLOSE, and we're on top: do nothing
                            (let ((enter (enter-instruction node)))
                              (unless (eq owner enter) ; also do nothing
                                ;; mark the function as a user,
                                (mark-user enter location)
                                ;; and continue up.
                                (setf nodes (append nodes (parents node))))))))))
      #+(or)(declare (inline init-enter mark-binder mark-user))
      (cleavir-ir:map-instructions-with-owner
       (lambda (instruction instruction-owner)
         (when (typep instruction 'cleavir-ir:enter-instruction)
           (init-enter instruction))
         (loop for datum in (data instruction)
               do (when (typep datum 'cleavir-ir:lexical-location)
                    (let ((location-owner (gethash datum location-owners)))
                      (unless (eq instruction-owner location-owner)
                        (mark-binder location-owner datum)
                        (mark-up instruction-owner location-owner datum))))))
       (initial-instruction function-dag)))
    result))

;;; An incremental version of compute destinies. Only compute the
;;; destinies from a given enclose-instruction.
(defun find-enclose-destinies (enclose-instruction)
  (let ((destinies '())
        (worklist (cleavir-ir:outputs enclose-instruction)))
    (loop (when (null worklist)
            (return destinies))
          (let ((work (pop worklist)))
            ;; note that we could hit the same work multiple times, so we use pushnew liberally.
            (loop for next in (cleavir-ir:using-instructions work)
                  do (typecase next
                       ;; here is where we could allow other instructions, etc.
                       (cleavir-ir:assignment-instruction
                        (push (first (cleavir-ir:outputs next)) worklist))
                       (call-instruction
                        (if (eq work (first (cleavir-ir:inputs next)))
                            ;; callee
                            (pushnew next destinies :test #'eq)
                            ;; arguments
                            (pushnew :escape destinies :test #'eq)))
                       (t ; treat as unknown
                        (pushnew :escape destinies :test #'eq))))))))

(defun destiny-find-encloses (call-instruction)
  (let ((worklist (list (first (cleavir-ir:inputs call-instruction))))
        (encloses '()))
    (loop (when (null worklist)
            (return encloses))
          (let ((work (pop worklist)))
            ;; note that we could hit the same work multiple times, so we use pushnew liberally.
            (loop for next in (cleavir-ir:defining-instructions work)
                  do (typecase next
                       ;; here is where we could allow other instructions, etc.
                       (cleavir-ir:assignment-instruction
                        (push (first (cleavir-ir:inputs next)) worklist))
                       (cleavir-ir:enclose-instruction
                        (pushnew next encloses))))))))

;;; Compute a hash table from enclose instructions to "destinies".
;;; A destiny is a list. Elements of the list are either call instructions or :escape.
;;; Each list has no duplicates.
(defun compute-destinies (initial-instruction)
  (let ((destinies (make-hash-table :test #'eq)))
    (cleavir-ir:map-instructions-arbitrary-order
     (lambda (i)
       (when (typep i 'cleavir-ir:enclose-instruction)
         (setf (gethash i destinies)
               (find-enclose-destinies i))))
     initial-instruction)
    destinies))

;;; Compute a hash table from call instructions to "origins".
;;; An origin is a list. Elements of the list are either enclose instructions,
;;; fdefinition instructions, or :unknown. Each list has no duplicates.
(defun compute-origins (initial-instruction)
  (let ((origins (make-hash-table :test #'eq)))
    (cleavir-ir:map-instructions-arbitrary-order
     (lambda (i)
       (when (typep i 'call-instruction)
         (let ((origin nil) (worklist (list (first (cleavir-ir:inputs i)))))
           (setf (gethash i origins)
                 (loop
                   (if (null worklist)
                       (return origin)
                       (let ((work (pop worklist)))
                         (loop for next in (cleavir-ir:defining-instructions work)
                               do (typecase next
                                    ;; here is where we could allow other instructions, etc.
                                    (cleavir-ir:assignment-instruction
                                     (push (first (cleavir-ir:inputs next)) worklist))
                                    (cleavir-ir:enclose-instruction
                                     (pushnew next origin :test #'eq))
                                    (cleavir-ir:fdefinition-instruction
                                     (pushnew next origin :test #'eq))
                                    (t ; treat as unknown
                                     (pushnew :unknown origin :test #'eq)))))))))))
     initial-instruction)
    origins))
