(cl:in-package #:sicl-new-boot)

(defmethod trucler:get-setf-expansion
    (client (environment clostrum-basic:run-time-environment) place)
  (if (atom place)
      (let ((new (gensym)))
        (values '() '() (list new) `(setq ,place ,new) place))
      (let* ((operator (first place))
             (arguments (rest place))
             (expander (clostrum:setf-expander client environment operator)))
        (if (null expander)
            (let ((temporaries
                    (loop repeat (length arguments) collect (gensym)))
                  (new (gensym)))
              (values temporaries
                      arguments
                      (list new)
                      `(funcall #'(setf ,operator ,new ,@temporaries))
                      `(,operator ,@temporaries)))
            (funcall expander place)))))
