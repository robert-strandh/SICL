(cl:in-package #:sicl-clos)

;;; The AMOP says that CLASS-DIRECT-SLOTS should return the empty list
;;; for a built-in class.
(defmethod class-direct-slots ((class built-in-class))
  (declare (ignore class))
  '())

;;; The AMOP says that CLASS-SLOTS should return the empty list for a
;;; built-in class.
(defmethod class-slots ((class built-in-class))
  (declare (ignore class))
  '())

;;; The AMOP says that CLASS-DEFAULT-INITARGS should return the empty
;;; list for a built-in class.
(defmethod class-default-initargs ((class built-in-class))
  (declare (ignore class))
  '())
