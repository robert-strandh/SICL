(cl:in-package #:sicl-setf)

(define-setf-expander the (value-type place &environment env)
  (multiple-value-bind (temps forms store-vars storing-form accessing-form)
      (get-setf-expansion place env)
    (values temps
            forms
            store-vars
            `(multiple-value-bind ,store-vars
                 (the ,value-type (values ,store-vars))
               ,storing-form)
            `(the ,value-type ,accessing-form))))
