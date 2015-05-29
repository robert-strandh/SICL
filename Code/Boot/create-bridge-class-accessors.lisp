(cl:in-package #:sicl-boot)

(defun create-bridge-class-accessors (boot)
  (let ((c (c2 boot))
	(r (r3 boot)))
  (ld "../CLOS/method-function-defgeneric.lisp" c r)))
