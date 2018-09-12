(cl:in-package #:sicl-clos)

(define-method-combination standard ()
  ((around-methods (:around))
   (before-methods (:before))
   (primary-methods () :required t)
   (after-methods (:after)))
  (flet ((call-methods (methods)
           (loop for method in methods
                 collect `(call-method ,method))))
    (let ((all-but-around-form
            (if (and (null before-methods)
                     (null after-methods)
                     (null (rest primary-methods)))
                `(call-method ,(first primary-methods))
                `(multiple-value-prog1
                     (progn ,@(call-methods before-methods)
                            (call-method ,(first primary-methods)
                                         ,(rest primary-methods)))
                   ,@(call-methods (reverse after-methods))))))
      (if (null around-methods)
          all-but-around-form
          `(call-method ,(first around-methods)
                        (,@(rest around-methods)
                         (make-method ,all-but-around-form)))))))
