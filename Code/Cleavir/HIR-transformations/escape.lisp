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
