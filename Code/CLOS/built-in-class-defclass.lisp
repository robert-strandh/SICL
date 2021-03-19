(cl:in-package #:sicl-clos)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class BUILT-IN-CLASS.
;;;
;;; The AMOP says that the readers CLASS-DIRECT-DEFAULT-INITARGS,
;;; CLASS-DIRECT-SLOTS, CLASS-DEFAULT-INITARGS, and CLASS-SLOTS should
;;; return the empty list for a built-in class.

(defclass built-in-class (real-class)
  ((%direct-slots
    :initform '()
    :reader class-direct-slots)
   (%finalized-p
    :initform t
    :reader class-finalized-p)))

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
