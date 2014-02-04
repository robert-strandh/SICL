(in-package #:sicl-clos)

(defmacro define-built-in-class (name superclasses slots)
  (if (eq name t)
      nil
      `(defclass ,name ,superclasses ,slots)))
