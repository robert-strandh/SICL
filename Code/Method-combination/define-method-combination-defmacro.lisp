(cl:in-package #:sicl-method-combination)

(defmacro define-method-combination (&environment env name &rest more-arguments)
  (define-method-combination-expander env name more-arguments))
