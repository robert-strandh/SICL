(cl:in-package #:sicl-method-combination)

(defmacro define-method-combination (name &rest more-arguments)
  (define-method-combination-expander name more-arguments))
