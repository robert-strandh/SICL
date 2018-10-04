(in-package #:cleavir-partial-inlining)

(defun instruction-and-all-users (instruction)
  (loop with all = (list instruction)
        with worklist = (cleavir-ir:outputs instruction)
        do (if (null worklist)
               (return all)
               (let ((work (pop worklist)))
                 (loop for next in (cleavir-ir:using-instructions work)
                       do (pushnew next all :test #'eq)
                          (etypecase next
                            (cleavir-ir:assignment-instruction
                             (push (first (cleavir-ir:outputs next)) worklist))
                            ;; FIXME: cleavir-ir:call-instruction
                            ((or cleavir-ir:funcall-instruction
                                 cleavir-ir:multiple-value-call-instruction))))))))

(defun delete-enclose (enclose-instruction)
  (mapc #'cleavir-ir:delete-instruction (instruction-and-all-users enclose-instruction)))

;; FIXME: move? finesse?
(defun all-parameters-required-p (enter)
  (every (lambda (param) (typep param 'cleavir-ir:lexical-location))
         (cleavir-ir:lambda-list enter)))

;; get one potential inline that can be done, or else NIL.
;; An inline here is a list (enter call enclose enclose-unique-p enter-unique-p)
;; enclose-unique-p is true iff the enclose is only used for this call (and thus can be deleted)
;; enter-unique-p is true iff the enter instruction is the CODE of only this enclose.
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
                           with enclose-unique-p = (= (length enclose-destinies) 1)
                           with enter-unique-p
                           = (= (length (gethash enter (cleavir-hir-transformations:dag-nodes dag))) 1)
                           for caller in enclose-destinies
                           for call-owner = (gethash caller *instruction-ownerships*)
                           when (eq caller :escape)
                             return nil
                           when (parent-node-p node call-owner) ; recursive
                             return nil
                           ;; We're all good, but keep looking through for escapes and recursivity.
                           do (setf result (list enter caller enclose enclose-unique-p enter-unique-p))
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
        nil))))

(defun do-inlining (initial-instruction)
  (loop with *instruction-ownerships* = (cleavir-hir-transformations:compute-instruction-owners initial-instruction)
        with *location-ownerships* = (cleavir-hir-transformations:compute-location-owners initial-instruction)
        for inline = (one-potential-inline initial-instruction)
        until (null inline)
        do (destructuring-bind (enter call enclose enclose-unique-p enter-unique-p) inline
             (inline-function initial-instruction call enter (make-hash-table :test #'eq) :uniquep uniquep)
             (when enclose-unique-p
               ;; enclose was only used once, and we've just eliminated that - delete it.
               (delete-enclose enclose)))
           (cleavir-ir:set-predecessors initial-instruction)
           (cleavir-ir:reinitialize-data initial-instruction)))
           (cleavir-remove-useless-instructions:remove-useless-instructions initial-instruction)))
