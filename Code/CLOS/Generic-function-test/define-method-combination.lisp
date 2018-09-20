(cl:in-package #:sicl-clos)

(defmacro define-method-combination (name &rest more-arguments)
  (sicl-method-combination::define-method-combination-expander *env* name more-arguments))
