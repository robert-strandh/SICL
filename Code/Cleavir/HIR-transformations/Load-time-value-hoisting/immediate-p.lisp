(cl:in-package #:cleavir-load-time-value-hoisting)

;;; An object is an immediate if the compiler can construct it cheaply
;;; whenever needed.  The notion of immediate objects is necessarily
;;; implementation dependent.  Systems can - and probably should - further
;;; specialize IMMEDIATE-P to include or exclude certain types of objects.

(defmethod immediate-p (object system)
  (declare (ignore object system))
  nil)

(defmethod immediate-p ((input cleavir-ir:constant-input) system)
  (immediate-p (cleavir-ir:value input)))

(defmethod immediate-p ((input cleavir-ir:immediate-input) system)
  t)

(defmethod immediate-p ((fixnum fixnum) system)
  t)

(defmethod immedate-p ((character character) system)
  t)
