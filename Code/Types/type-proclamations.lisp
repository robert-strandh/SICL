(cl:in-package #:sicl-type)

;;; FIXME: try defining some more specific types
(proclaim '(ftype (function (t t &optional t) (member t nil))
            typep))

(proclaim '(ftype (function (t t) t)
            coerce))
