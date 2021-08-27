(cl:in-package #:sicl-clos)

(defmacro defmethod
    (&environment environment function-name &rest rest)
  (multiple-value-bind
        (qualifiers required remaining specializers declarations documentation forms)
      (parse-defmethod rest)
    (let ((generic-function-var (gensym))
          (method-var (gensym))
          (lambda-list (append required remaining)))
      (multiple-value-bind
            (generic-function-class-name method-class-name)
          (generic-function-class-names function-name environment)
        (let ((method-lambda
                (make-method-lambda
                 (class-prototype (find-class generic-function-class-name))
                 (class-prototype (find-class method-class-name))
                 `(lambda ,lambda-list
                    ,@declarations
                    (block ,(if (consp function-name)
                                (second function-name)
                                function-name)
                      ,@forms))
                 environment)))
          `(ensure-method
            ',function-name
            :method-class ',method-class-name
            :lambda-list ',lambda-list
            :qualifiers ',qualifiers
            :specializers (list ,@(canonicalize-specializers specializers))
            :documentation ',documentation
            :function ,method-lambda))))))
