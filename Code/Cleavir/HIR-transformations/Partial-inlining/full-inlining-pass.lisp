(in-package #:cleavir-partial-inlining)

(defun delete-function-nonrecursive (enter)
  (cleavir-ir:map-local-instructions
   (lambda (instruction)
     (setf (cleavir-ir:inputs instruction) nil
           (cleavir-ir:outputs instruction) nil))
   enter))

(defun delete-function (enter dag)
  (cleavir-ir:map-local-instructions
   (lambda (instruction)
     ;; Due to the extra consistency methods, this will ensure the DATA
     ;; still stop linking back to this instruction.
     (setf (cleavir-ir:inputs instruction) nil
           ;; note: may work weirdly with enter instructions, due to the lambda-list consistency method.
           (cleavir-ir:outputs instruction) nil)
     (when (typep instruction 'cleavir-ir:enclose-instruction)
       ;; We need to delete the ENCLOSE's node.
       ;; (If the CODE has no other ENCLOSE, DELETE-NODE will then
       ;;  trigger DELETE-FUNCTION recursively.)
       (let* ((inner-enter (cleavir-ir:code instruction))
              (nodes (gethash inner-enter (cleavir-hir-transformations:dag-nodes dag)))
              (node (find instruction nodes :key #'cleavir-hir-transformations:enclose-instruction)))
         (if node
             (delete-node node dag)
             (error "BUG: Inconsistency in function DAG: no node for enclose ~a" instruction)))))
   enter))

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

(defun enter-unique-p (enter dag)
  (= (length (gethash enter (cleavir-hir-transformations:dag-nodes dag))) 1))

(defun delete-node (node dag)
  (let* ((node-enter (cleavir-hir-transformations:enter-instruction node))
         (nodes-table (cleavir-hir-transformations:dag-nodes dag))
         (enter-nodes (gethash node-enter nodes-table))
         (new-enter-nodes (remove node enter-nodes)))
    ;; Take our node out of the table.
    (cond ((null new-enter-nodes) ; function completely deleted
           (remhash node-enter nodes-table)
           (delete-function node-enter dag))
          (t (setf (gethash node-enter nodes-table) new-enter-nodes)))
    ;; Iterate over parents, removing the parenthood relationship.
    (loop for parent in (cleavir-hir-transformations:parents node)
          for parent-children = (cleavir-hir-transformations:children parent)
          for new-parent-children = (remove node parent-children)
          do (setf (cleavir-hir-transformations:children parent) new-parent-children))
    ;; Iterate over children, removing the parenthood relationship.
    (loop for child in (cleavir-hir-transformations:children node)
          for child-parents = (cleavir-hir-transformations:parents child)
          for new-child-parents = (remove node child-parents)
          when (null new-child-parents) ; we're the only parent - child is now disconnected
            do (delete-node child dag) ; destroy
          else do (setf (cleavir-hir-transformations:parents child) new-child-parents))))

;; FIXME: move? finesse?
(defun all-parameters-required-p (enter)
  (every (lambda (param) (typep param 'cleavir-ir:lexical-location))
         (cleavir-ir:lambda-list enter)))

;; get one potential inline that can be done, or else NIL.
;; An inline here is a list (enter call uniquep), where uniquep expresses whether the function
;; is not used for anything but this call.
(defun one-potential-inline (initial-instruction dag)
  (let ((destinies (cleavir-hir-transformations:compute-destinies initial-instruction)))
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
                           with enter-unique-p = (enter-unique-p enter dag)
                           for caller in enclose-destinies
                           for call-owner = (gethash caller *instruction-ownerships*)
                           when (eq caller :escape)
                             return nil
                           when (parent-node-p node call-owner) ; recursive
                             return nil
                           ;; We're all good, but keep looking through for escapes and recursivity.
                           do (setf result (list enter caller node enclose-unique-p enter-unique-p))
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
  (loop for *instruction-ownerships*
          = (cleavir-hir-transformations:compute-instruction-owners initial-instruction)
        for *location-ownerships*
          = (cleavir-hir-transformations:compute-location-owners initial-instruction)
        for dag = (cleavir-hir-transformations:build-function-dag initial-instruction)
        for inline = (one-potential-inline initial-instruction dag)
        until (null inline)
        do (destructuring-bind (enter call enclose enclose-unique-p enter-unique-p) inline
             (declare (ignore enclose-unique-p enter-unique-p))
             (inline-function initial-instruction call enter (make-hash-table :test #'eq)))
           (cleavir-ir:set-predecessors initial-instruction)
           (cleavir-ir:reinitialize-data initial-instruction)
           (cleavir-remove-useless-instructions:remove-useless-instructions initial-instruction)))
