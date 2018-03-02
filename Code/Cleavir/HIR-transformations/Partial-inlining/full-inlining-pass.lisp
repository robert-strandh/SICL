(in-package #:cleavir-partial-inlining)

;; FIXME: move? finesse?
(defun all-parameters-required-p (enter)
  (every (lambda (param) (typep param 'cleavir-ir:lexical-location))
         (cleavir-ir:lambda-list enter)))

(defun potential-inlines (initial-instruction)
  (let ((tree (cleavir-hir-transformations:build-function-tree initial-instruction))
        (location-owners (cleavir-hir-transformations:compute-location-owners initial-instruction))
        (instruction-owners (cleavir-hir-transformations:compute-instruction-owners initial-instruction))
        (destinies (cleavir-hir-transformations:compute-destinies initial-instruction)))
    (let ((trappers (cleavir-hir-transformations:discern-trappers tree destinies))
          (sharing (cleavir-hir-transformations:discern-sharing tree location-owners)))
      (labels ((inlines (node)
                 (let ((enter (cleavir-hir-transformations:enter-instruction node)))
                   (when (and (all-parameters-required-p enter)
                              (null (cdr (gethash enter sharing)))
                              (gethash enter trappers))
                     ;; function is simple.
                     ;; Now we just need to pick off any recursive uses, direct or indirect.
                     (loop with enclose = (cleavir-hir-transformations:enclose-instruction node)
                           for caller in (gethash enclose destinies)
                           when (eq caller :escape)
                             return nil
                           when (parent-node-p node (gethash (gethash caller instruction-owners)
                                                             (cleavir-hir-transformations:tree-nodes tree)))
                             ;; recursive
                             return nil
                           collect (cons enter caller)))))
               (all-inlines (node)
                 (append (inlines node) (mapcan #'all-inlines (cleavir-hir-transformations:children node))))
               (parent-node-p (parent child)
                 (if (eq child tree) ; we've recursed all the way up with no luck.
                     nil
                     (or (eq parent child)
                         (parent-node-p parent (cleavir-hir-transformations:parent child))))))
        (mapcan #'all-inlines (cleavir-hir-transformations:children tree))))))

(defun do-inlining (initial-instruction)
  ;; nreverse because we have to inline inner functions first, because
  ;; inline will copy them and lose the original instructions.
  ;; For example, consider (flet ((foo (x) (flet ((bar (x) (print x))) (bar x)))) (foo x))
  ;; If we inline foo first, the call in it will be copied, and won't be in our
  ;; list of potential inlines, so bar won't be inlined.
  ;; FIXME: Probably should be a bit nicer about how we do that than just nreverse
  (loop for (enter . call) in (nreverse (potential-inlines initial-instruction))
        do (inline-function initial-instruction call enter (make-hash-table :test #'eq))))
