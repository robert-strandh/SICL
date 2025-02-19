(cl:in-package #:sicl-hir-visualizer)

(defun map-instructions-arbitrary-order (function first-instruction)
  (let ((visited (make-hash-table :test #'eq)))
    (labels ((aux (instruction)
               (unless (gethash instruction visited)
                 (setf (gethash instruction visited) t)
                 (funcall function instruction)
                 (mapcar #'aux (ir:successors instruction))
                 (when (typep instruction 'ir:enclose-instruction)
                   (aux (ir:parse-arguments-instruction instruction))))))
      (aux first-instruction))))

               
