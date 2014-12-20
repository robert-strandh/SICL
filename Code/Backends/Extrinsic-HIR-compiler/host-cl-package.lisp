(cl:in-package #:common-lisp-user)

(defpackage #:host-common-lisp
  (:nicknames #:host-cl)
  (:export . #.(loop for symbol being each external-symbol in '#:common-lisp
		     collect (symbol-name symbol))))
