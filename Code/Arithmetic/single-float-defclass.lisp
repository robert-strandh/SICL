(cl:in-package #:sicl-arithmetic)

;;; During bootstrapping, we represent a single float as a standard
;;; object with a slot containing an integer representing the bits of
;;; the representation of the float.

(defclass single-float (float standard-object)
  ())

;; (defclass single-float (float)
;;   ()
;;   (:metaclass built-in-class))

(defun single-float-p (object)
  (typep object 'single-float))
