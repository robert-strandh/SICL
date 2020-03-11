(cl:in-package #:sicl-sequence)

(defclass sequence-function (fast-generic-function)
  ()
  (:metaclass funcallable-standard-class))

(defmethod no-applicable-method
    ((sequence-function sequence-function) &rest arguments)
  (maybe-signal-sequence-type-error sequence-function arguments)
  (call-next-method))

(defmethod no-primary-method
    ((sequence-function sequence-function) &rest arguments)
  (maybe-signal-sequence-type-error sequence-function arguments))

(defun maybe-signal-sequence-type-error (sequence-function arguments)
  (dolist (sealed-domain (sealed-domains sequence-function))
    (loop for specializer in sealed-domain
          for argument in arguments
          when (eq specializer (find-class 'sequence))
            unless (typep argument 'sequence)
              do (error 'type-error
                        :expected-type 'sequence
                        :datum argument))))
