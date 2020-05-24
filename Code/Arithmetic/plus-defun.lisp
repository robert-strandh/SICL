(cl:in-package #:sicl-arithmetic)

(defun + (&rest arguments)
  (cond ((null arguments) 0)
        ((null (rest arguments))
         (let ((argument (first arguments)))
           (if (numberp argument)
               argument
               (error 'argument-to-plus-must-be-a-number))))
        (t
         (loop with result = (first arguments)
               for argument in (rest arguments)
               do (setf result (binary-plus result argument))
               finally (return result)))))
