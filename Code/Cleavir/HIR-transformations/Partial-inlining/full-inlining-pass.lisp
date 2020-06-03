(in-package #:cleavir-partial-inlining)

;;; FIXME: move? finesse?
(defun all-parameters-required-p (enter)
  (every (lambda (param) (typep param 'cleavir-ir:lexical-location))
         (cleavir-ir:lambda-list enter)))
(defun lambda-list-too-hairy-p (lambda-list)
  (or (member '&rest lambda-list)
      (member '&key lambda-list)))

;;; Check that a call is valid so that we can avoid inlining invalid calls,
;;; and the inliner can safely assume calls are valid.
;;; That is, for something like ((lambda ()) x), we want a full call so that
;;; the client's runtime can do its normal error signaling.
;;; FIXME: We should probably signal a warning. If we do, make sure it's not
;;; redundant with CST-to-AST's.
(defun call-valid-p (enter call)
  (let ((lambda-list (cleavir-ir:lambda-list enter))
        (nargs (1- (length (cleavir-ir:inputs call))))) ; one input for the function
    (assert (not (lambda-list-too-hairy-p lambda-list)))
    ;; KLUDGE: This is basically another lambda list parser.
    (let ((required-count 0)
          (optional-count 0)
          (state :required))
      (dolist (item lambda-list)
        (cond ((eq item '&optional)
               (setq state :optional))
              (t
               (case state
                 (:required (incf required-count))
                 (:optional (incf optional-count))))))
      (<= required-count nargs (+ optional-count required-count)))))

;;; get one potential inline that can be done, or else NIL.
;;; An inline here is a list (enter call uniquep), where uniquep expresses whether the function
;;; is not used for anything but this call.
(defun one-potential-inline (dag)
  (labels ((maybe-return-inline (node)
             (let* ((enter (cleavir-hir-transformations:enter-instruction node))
                    (enclose (cleavir-hir-transformations:enclose-instruction node))
                    (fun (first (cleavir-ir:outputs enclose)))
                    result)
               (copy-propagate-1 fun)
               (when (all-parameters-required-p enter)
                 ;; Now we just need to pick off any recursive uses, direct or indirect.
                 (loop for user in (cleavir-ir:using-instructions fun)
                       ;; Make sure all users are actually call instructions that only
                       ;; use FUN in call position.
                       unless (and (typep user 'cleavir-ir:funcall-instruction)
                                   (eq (first (cleavir-ir:inputs user)) fun)
                                   (not (member fun (rest (cleavir-ir:inputs user)))))
                         return nil
                       when (parent-node-p node (instruction-owner user)) ; recursive
                         return nil
                       ;; For now, since M-V-C doesn't track inlining
                       ;; information, only check explicit inline
                       ;; declarations for local functions.
                       unless (or (typep user 'cleavir-ir:multiple-value-call-instruction)
                                  (eq (cleavir-ir:inline-declaration user) 'inline))
                         return nil
                       unless (call-valid-p enter user)
                         return nil
                       ;; We're all good, but keep looking through for escapes and recursivity.
                       do (setf result (list enter user node))
                       finally (return-from one-potential-inline result)))))
           (parent-node-p (parent enter)
             ;; parent is a node (i.e. enclose), enter is an enter instruction
             ;; we return T iff the enter is enclosed by a node that has parent
             ;; as an ancestor.
             (let ((todo (gethash enter (cleavir-hir-transformations:dag-nodes dag))))
               (loop until (null todo)
                     do (let ((test (pop todo)))
                          (cond ((eq test dag) (return nil)) ; recursed all the way up
                                ((eq test parent) (return t))
                                (t (setf todo
                                         (append todo
                                                 (cleavir-hir-transformations:parents test)))))))))
           (depth-first-search (node)
             (maybe-return-inline node)
             ;; It didn't return, so keep going.
             (mapc #'depth-first-search (cleavir-hir-transformations:children node))))
    ;; We don't call maybe-return-inline on the toplevel function itself, since it obviously can't
    ;; be inlined, and doesn't have an enclose-instruction, etc.
    (mapc #'depth-first-search (cleavir-hir-transformations:children dag))
    ;; No dice.
    nil))

;;; Returns whether the location needs an explicit cell.
(defun explicit-cell-p (location)
  (let ((owner (gethash location *location-ownerships*)))
    (and (not (cleavir-hir-transformations:read-only-location-p location))
         (dolist (user (cleavir-ir:using-instructions location))
           (unless (eq (gethash user *instruction-ownerships*) owner)
             (return t))))))

;;; Introduce cells for those bound locations which need them.
(defun convert-binding-instructions (binding-assignments)
  (dolist (binding-assignment binding-assignments)
    (let ((location (first (cleavir-ir:outputs binding-assignment))))
      (when (and location (explicit-cell-p location))
        (let ((create (make-instance 'cleavir-ir:create-cell-instruction
                        :origin (cleavir-ir:origin binding-assignment)
                        :policy (cleavir-ir:policy binding-assignment)
                        :dynamic-environment (cleavir-ir:dynamic-environment binding-assignment))))
          (dolist (user (cleavir-ir:using-instructions location))
            (cleavir-hir-transformations:replace-inputs location location user))
          (dolist (definer (cleavir-ir:defining-instructions location))
            (cleavir-hir-transformations:replace-outputs location location definer))
          (cleavir-ir:insert-instruction-after create binding-assignment)
          (setf (cleavir-ir:outputs create) (list location)))))))

(defun full-inlining-pass (initial-instruction)
  ;; Need to remove all useless instructions first for incremental
  ;; r-u-i to catch everything.
  (cleavir-remove-useless-instructions:remove-useless-instructions initial-instruction)
  (loop with *instruction-ownerships*
          = (cleavir-hir-transformations:compute-instruction-owners initial-instruction)
        with *location-ownerships*
          = (cleavir-hir-transformations:compute-location-owners initial-instruction)
        with *function-dag* = (cleavir-hir-transformations:build-function-dag initial-instruction)
        with *binding-assignments* = '()
        for inline = (one-potential-inline *function-dag*)
        until (null inline)
        do (destructuring-bind (enter call node) inline
             ;; Find all instructions that could potentially be deleted after inlining.
             (let ((function-defs (cleavir-ir:defining-instructions (first (cleavir-ir:inputs call)))))
               (inline-function initial-instruction call enter (make-hash-table :test #'eq))
               (dolist (deleted
                        (cleavir-remove-useless-instructions:remove-useless-instructions-from function-defs))
                 (typecase deleted
                   (cleavir-ir:enclose-instruction
                    (cleavir-hir-transformations:remove-enclose-from-function-dag
                     *function-dag*
                     deleted))))))
        finally
           (cleavir-remove-useless-instructions:remove-useless-instructions initial-instruction)
           ;; We must reinitialize data here, because some data may
           ;; still have instruction references from instructions in
           ;; the unreachable portion of the graph. It is important
           ;; because read-only analysis depends on accurate data
           ;; information.
           (cleavir-ir:reinitialize-data initial-instruction)
           (convert-binding-instructions *binding-assignments*)))

(defun do-inlining (initial-instruction)
  ;; Do this first to pick off stuff that doesn't need full copying
  ;; inlining.
  (interpolable-function-analyze initial-instruction)
  (full-inlining-pass initial-instruction))
