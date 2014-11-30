(cl:in-package #:sicl-extrinsic-hir-compiler)

(defun rp (filename)
  (asdf:system-relative-pathname :sicl-extrinsic-hir-compiler filename))

(load (rp "../../Environment/lambda.lisp"))
(load (rp "../../Environment/multiple-value-bind.lisp"))
(load (rp "../../Environment/setf.lisp"))
