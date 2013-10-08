(cl:in-package #:sicl-conditions)

;;; FIXME: we need some more specific types here.
(declaim (ftype (function (t &rest list) nil)
		error))

(declaim (ftype (function (&optional (or null condition)) nil)
		abort))

