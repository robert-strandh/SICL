(cl:in-package #:sicl-clos)

(declaim (ftype (function (list)
                          ;; FIXME: be more specific!
                          (values t t t t t t t))
                parse-defmethod))

(declaim (ftype (function (list) list)
                canonicalize-specializers))

(declaim (ftype (function ((or symbol class)
                           &key &allow-other-keys))
                make-instance))

(declaim (ftype (function (t &rest t) t)
                no-applicable-method))

(declaim (ftype (function (class) t)
                finalize-inheritance))
