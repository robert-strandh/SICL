(cl:in-package #:sicl-boot)

(defun create-bridge-classes (boot)
  (let ((c (c1 boot))
	(r (r2 boot)))
    (ld "../CLOS/t-defclass.lisp" c r)
    (ld "../CLOS/standard-object-defclass.lisp" c r)
    (ld "../CLOS/metaobject-defclass.lisp" c r)))
