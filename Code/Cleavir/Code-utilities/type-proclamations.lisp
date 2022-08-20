(cl:in-package #:cleavir-code-utilities)

(declaim (ftype (function (list) list)
                canonicalize-ordinary-lambda-list
                canonicalize-specialized-lambda-list
                canonicalize-destructuring-lambda-list
                canonicalize-generic-function-lambda-list
                canonicalize-specialized-lambda-list
                canonicalize-define-method-combination-arguments-lambda-list
                canonicalize-define-modify-macro-lambda-list))

(declaim (ftype (function (list) list)
                extract-required))

(declaim (ftype (function (t) (member t nil))
                proper-list-p))

(declaim (ftype (function (list) (values list (or string null) list))
                separate-function-body))

(declaim (ftype (function (list) (values list list))
                separate-ordinary-body))

(declaim (ftype (function (list) list)
                lambda-list-variables))

(declaim (ftype (function (list) list)
                extract-named-group))
