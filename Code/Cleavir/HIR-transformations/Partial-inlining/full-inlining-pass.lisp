(in-package #:cleavir-partial-inlining)

;; FIXME: move? finesse?
(defun all-parameters-required-p (enter)
  (every (lambda (param) (typep param 'cleavir-ir:lexical-location))
         (cleavir-ir:lambda-list enter)))

;; get one potential inline that can be done, or else NIL.
;; An inline here is a list (enter call uniquep), where uniquep expresses whether the function
;; is not used for anything but this call.
(defun one-potential-inline (initial-instruction)
  (let ((dag (cleavir-hir-transformations:build-function-dag initial-instruction))
        (destinies (cleavir-hir-transformations:compute-destinies initial-instruction)))
    (let ((trappers (cleavir-hir-transformations:discern-trappers dag destinies)))
      (labels ((maybe-return-inline (node)
                 (let ((enter (cleavir-hir-transformations:enter-instruction node)))
                   (when (and (all-parameters-required-p enter)
                              (gethash enter trappers))
                     ;; function's environment does not escape.
                     ;; Now we just need to pick off any recursive uses, direct or indirect.
                     (loop with enclose = (cleavir-hir-transformations:enclose-instruction node)
                           with result
                           with enclose-destinies = (gethash enclose destinies)
                           with unique-p = (unique-p enter enclose-destinies)
                           for caller in enclose-destinies
                           for call-owner = (gethash caller *instruction-ownerships*)
                           when (eq caller :escape)
                             return nil
                           when (parent-node-p node call-owner) ; recursive
                             return nil
                           ;; We're all good, but keep looking through for escapes and recursivity.
                           do (setf result (list enter caller unique-p))
                           finally (return-from one-potential-inline result)))))
               (unique-p (enter enclose-destinies)
                 ;; Return T iff the ENTER is only used for one call, which allows most copying
                 ;; during the inline process to be skipped.
                 (and (= (length enclose-destinies) 1)
                      (= (length (gethash enter (cleavir-hir-transformations:dag-nodes dag))) 1)))
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
        nil))))

(defun do-inlining (initial-instruction)
  (loop with *instruction-ownerships* = (cleavir-hir-transformations:compute-instruction-owners initial-instruction)
        for *location-ownerships* = (cleavir-hir-transformations:compute-location-owners initial-instruction)
        for inline = (one-potential-inline initial-instruction)
        until (null inline)
        do (destructuring-bind (enter call uniquep) inline
             (inline-function initial-instruction call enter (make-hash-table :test #'eq) :uniquep uniquep))
           (cleavir-ir:set-predecessors initial-instruction)
           (cleavir-remove-useless-instructions:remove-useless-instructions initial-instruction)))
