(cl:in-package #:cleavir-ast)

(defparameter *map-ast-depth-first-preorder-meter*
  (make-instance 'cleavir-meter:size-meter
    :name "MAP-AST-DEPTH-FIRST-SEARCH-PREORDER"))

(defun map-ast-depth-first-preorder (function ast)
  (cleavir-meter:with-meter (m *map-ast-depth-first-preorder-meter*)
    (let ((visited-p (make-hash-table :test #'eq))
          (nodes-to-process '()))
      (flet ((register-if-unvisited (node)
               (unless (gethash node visited-p)
                 (cleavir-meter:increment-size m)
                 (setf (gethash node visited-p) t)
                 (push node nodes-to-process))))
        (register-if-unvisited ast)
        (loop until (null nodes-to-process)
              do (let ((node (pop nodes-to-process)))
                   (funcall function node)
                   (loop for child in (children node)
                         do (register-if-unvisited child))))))))
