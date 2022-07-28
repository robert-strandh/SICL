(cl:in-package #:cleavir-code-utilities)

(declaim (ftype (function (list) list)
                canonicalize-ordinary-lambda-list
                canonicalize-specialized-lambda-list
                canonicalize-destructuring-lambda-list
                canonicalize-generic-function-lambda-list
                canonicalize-specialized-lambda-list
                canonicalize-define-method-combination-arguments-lambda-list
                canonicalize-define-modify-macro-lambda-list))
