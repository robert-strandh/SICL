(cl:in-package #:sicl-character)

(define-compiler-macro char< (&whole form &rest arguments)
  (cond ((not (and (cleavir-code-utilities:proper-list-p arguments)
               (>= (length arguments) 1)))
         form)
        ((= (length arguments) 1)
         `(characterp ,(car arguments)))
        (t (let* ((vars (loop for argument in arguments collect (gensym))))
             `(let ,(loop for var in vars
                          for arg in arguments
                          collect `(,var ,arg))
                (and ,@(loop for (var1 var2) on vars
                             repeat (1- (length vars))
                             collect `(binary-char< ,var1 ,var2))))))))
