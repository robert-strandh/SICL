(cl:in-package #:sicl-clos)

(defmethod class-prototype :before ((class class))
  (unless (class-finalized-p class)
    (error 'attempt-to-access-prototype-of-unfinalized-class
           :offending-class class)))
