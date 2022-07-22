(cl:in-package #:sicl-clos)

(proclaim '(ftype (function (list)
                   ;; FIXME: be more specific!
                   (values t t t t t t t))
            parse-defmethod))

(proclaim '(ftype (function (list) list)
            canonicalize-specializers))
