(cl:in-package #:sicl-conditions)

(defmacro define-condition (name (&rest supertypes) direct-slots &rest options)
  (define-condition-expander name supertypes direct-slots options))
