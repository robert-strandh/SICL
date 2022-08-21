(cl:in-package #:sicl-type)

;;; FIXME: try defining some more specific types
(declaim (ftype (function (t t &optional t) (member t nil))
                typep))

(declaim (ftype (function (t t) t)
                coerce))

;;; FIXME: the optional parameter should be of type ENVIRONMENT.
(declaim (ftype (function (t t &optional t) (member t nil))
                subtypep))
