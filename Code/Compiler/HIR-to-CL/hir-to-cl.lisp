(cl:in-package #:sicl-hir-to-cl)

(defun hir-to-cl (initial-instruction)
  (let ((enter-instructions (sort-functions initial-instruction))
        (context (make-instance 'context)))
    (loop for enter-instruction in (butlast enter-instructions)
          do (setf (gethash enter-instruction (function-names context))
                   (gensym "function")))
    (translate-enter-instruction initial-instruction context)))
