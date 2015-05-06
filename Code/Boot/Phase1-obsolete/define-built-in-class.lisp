(in-package #:sicl-boot-phase1)

(defmacro define-built-in-class (name superclasses slots)
  (if (eq name t)
      nil
      `(defclass ,name ,superclasses ,slots)))
