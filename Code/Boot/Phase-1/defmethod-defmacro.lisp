(cl:in-package #:sicl-clos)

;;; This is a simplified version of the macro DEFMETHOD that is good
;;; enough to use for macro expansion during bootstrappping.
(defmacro defmethod
    (&environment environment function-name &rest rest)
  (multiple-value-bind
        (qualifiers required remaining specializers declarations documentation forms)
      (parse-defmethod rest)
    (let ((generic-function-var (gensym))
          (method-var (gensym))
          (lambda-list (append required remaining)))
      (let ((method-lambda
              (make-method-lambda-default
               nil
               nil
               `(lambda ,lambda-list
                  ,@declarations
                  (block ,(if (consp function-name)
                              (second function-name)
                              function-name)
                    ,@forms))
               nil)))
        `(ensure-method
          ',function-name
          :method-class 'standard-method
          :lambda-list ',lambda-list
          :qualifiers ',qualifiers
          :specializers (list ,@(canonicalize-specializers specializers))
          :documentation ',documentation
          :function ,method-lambda)))))

