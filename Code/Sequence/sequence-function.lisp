(cl:in-package #:sicl-sequence)

(defclass sequence-function (fast-generic-function)
  ()
  (:metaclass sicl-clos:funcallable-standard-class))

(defmethod no-applicable-method
    ((sequence-function sequence-function) &rest arguments)
  (maybe-signal-sequence-type-error sequence-function arguments)
  (call-next-method))

(defmethod no-primary-method
    ((sequence-function sequence-function) &rest arguments)
  (maybe-signal-sequence-type-error sequence-function arguments)
  (call-next-method))

(defun maybe-signal-sequence-type-error (sequence-function arguments)
  (dolist (sealed-domain (sealed-domains sequence-function))
    (loop for specializer in (sealable-metaobjects:domain-specializers sealed-domain)
          for argument in arguments
          when (subtypep specializer 'sequence)
            unless (typep argument 'sequence)
              do (error 'type-error
                        :expected-type 'sequence
                        :datum argument))))
