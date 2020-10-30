(cl:in-package #:sicl-clos)

(defmethod class-default-initargs ((class forward-referenced-class))
  (error 'attempt-to-access-default-initargs-of-forward-referenced-class
         :offending-class class))

(defmethod class-precedence-list ((class forward-referenced-class))
  (error 'attempt-to-access-precedence-list-of-forward-referenced-class
         :offending-class class))

(defmethod class-slots ((class forward-referenced-class))
  (error 'attempt-to-access-effective-slots-of-forward-referenced-class))
