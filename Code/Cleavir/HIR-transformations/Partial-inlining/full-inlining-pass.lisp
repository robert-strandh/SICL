(in-package #:cleavir-partial-inlining)

;; FIXME: move? finesse?
(defun all-parameters-required-p (enter)
  (every (lambda (param) (typep param 'cleavir-ir:lexical-location))
         (cleavir-ir:lambda-list enter)))

#+(or)
(defun potential-inlines (initial-instruction)
  (let ((dag (cleavir-hir-transformations:build-function-dag initial-instruction))
        (location-owners (cleavir-hir-transformations:compute-location-owners initial-instruction))
        (instruction-owners (cleavir-hir-transformations:compute-instruction-owners initial-instruction))
        (destinies (cleavir-hir-transformations:compute-destinies initial-instruction)))
    (let ((trappers (cleavir-hir-transformations:discern-trappers dag destinies))
          (sharing (cleavir-hir-transformations:discern-sharing dag location-owners)))
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
                           when (parent-node-p node caller)
                             ;; recursive
                             return nil
                           collect (cons enter caller)))))
               (all-inlines (node)
                 (append (inlines node) (mapcan #'all-inlines (cleavir-hir-transformations:children node))))
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
                                                     (cleavir-hir-transformations:parents test))))))))))
        (reduce #'append
                (mapcar #'all-inlines (cleavir-hir-transformations:children dag)))))))

;; get one potential inline that can be done.
(defun one-potential-inline (initial-instruction)
  (let ((dag (cleavir-hir-transformations:build-function-dag initial-instruction))
        (location-owners (cleavir-hir-transformations:compute-location-owners initial-instruction))
        (instruction-owners (cleavir-hir-transformations:compute-instruction-owners initial-instruction))
        (destinies (cleavir-hir-transformations:compute-destinies initial-instruction)))
    (let ((trappers (cleavir-hir-transformations:discern-trappers dag destinies))
          (sharing (cleavir-hir-transformations:discern-sharing dag location-owners)))
      (labels ((maybe-return-inline (node)
                 (let ((enter (cleavir-hir-transformations:enter-instruction node)))
                   (when (and (all-parameters-required-p enter)
                              (null (cdr (gethash enter sharing)))
                              (gethash enter trappers))
                     ;; function is simple.
                     ;; Now we just need to pick off any recursive uses, direct or indirect.
                     (loop with enclose = (cleavir-hir-transformations:enclose-instruction node)
                           with result
                           for caller in (gethash enclose destinies)
                           when (eq caller :escape)
                             return nil
                           when (parent-node-p node caller)
                             ;; recursive
                             return nil
                           ;; we're all good, but keep looking through for escapes
                           do (setf result (cons enter caller))
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
                                                     (cleavir-hir-transformations:parents test))))))))))
        (maphash (lambda (enter nodes)
                   (unless (eq enter initial-instruction) ; toplevel isn't going to be inlined
                     (mapc #'maybe-return-inline nodes)))
                 (cleavir-hir-transformations:dag-nodes dag))))))

(defun do-inlining (initial-instruction)
  (loop for inline = (one-potential-inline initial-instruction)
        until (null inline)
        do (let ((enter (car inline)) (call (cdr inline)))
             (inline-function initial-instruction call enter (make-hash-table :test #'eq)))
           (cleavir-remove-useless-instructions:remove-useless-instructions initial-instruction))
  #+(or)
  (loop for inlines = (potential-inlines initial-instruction)
        for n from 0
        until (null inlines)
        do (format t "Inline ~d~%" n)
           (finish-output)
           (cleavir-ir-graphviz:draw-flowchart
            initial-instruction (format nil "/tmp/inline~d.dot" n))
           (loop for (enter . call) in inlines
                 do (inline-function initial-instruction call enter (make-hash-table :test #'eq)))
           (format t "RUI~%")
           (cleavir-remove-useless-instructions:remove-useless-instructions initial-instruction)))
