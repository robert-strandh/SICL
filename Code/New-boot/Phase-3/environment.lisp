(cl:in-package #:sicl-new-boot-phase-3)

(defclass environment (sicl-new-boot:environment)
  ())

(defmethod sicl-genv:typep
    (object (type-specifier (eql 'function)) (environment environment))
  (typep object 'function))

(defmethod sicl-genv:typep
    (object (type-specifier (eql 'class)) (environment environment))
  (not (symbolp object)))
