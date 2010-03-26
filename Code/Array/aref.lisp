(defun my-aref (array &rest subscripts)
  (let ((dimensions (array-dimensions array)))
    (assert (= (length dimensions)
               (length subscripts)))
    (loop for subscript in subscripts
          for dimension in dimensions
          do (assert (< -1 subscript dimension)))
    (row-major-aref array
                    (+ (loop for subscript in subscripts
                             for remaining-dimensions on (cdr dimensions)
                             sum (* subscript (reduce #'* remaining-dimensions)))
                       (car (last subscripts))))))

(define-compiler-macro my-aref (array &rest subscripts)
  (let* ((length (length subscripts))
         (subscript-vars (loop repeat length collect (gensym)))
         (dimension-vars (loop repeat length collect (gensym)))
         (remaining-dimension-vars (loop repeat (1- length) collect (gensym))))
    `(let ((array ,array))
       (assert (= ,length (length (array-dimensions array))))
       (let (,@(mapcar #'list subscript-vars subscripts))
         (let (,@(loop for var in dimension-vars
                       for dimension from 0
                       collect `(,var (array-dimension array ,dimension))))
           ,@(loop for svar in subscript-vars
                   for dvar in dimension-vars
                   collect `(assert (< -1 ,svar ,dvar)))
           (let (,@(loop for rdvar in remaining-dimension-vars
                         for rdvars on (cdr dimension-vars)
                         collect `(,rdvar (* ,@rdvars))))
             (row-major-aref array
                             (+ ,@(loop for svar in subscript-vars
                                        for rdvar in remaining-dimension-vars
                                        collect `(* ,svar ,rdvar))
                                ,(car (last subscript-vars))))))))))