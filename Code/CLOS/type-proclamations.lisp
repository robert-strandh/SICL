(cl:in-package #:sicl-clos)

(declaim (ftype (function (list)
                          ;; FIXME: be more specific!
                          (values t t t t t t t))
                parse-defmethod))

(declaim (ftype (function (list) list)
                canonicalize-specializers))
