(cl:in-package #:sicl-arithmetic)

;;; During bootstrapping, we represent a double float as a standard
;;; object with a slot containing an integer representing the bits of
;;; the representation of the float.

(defclass double-float (float standard-object)
  ())

;; (defclass double-float (float standard-object)
;;   ())

(defun double-float-p (object)
  (typep object 'double-float))
