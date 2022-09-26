(cl:in-package #:sicl-cst-to-ast-s)

(defclass node ()
  ((%source :initarg :source :reader source)))

(defclass name-mixin ()
  ((%name :initarg :name :reader name)))

(defclass default-value-form-mixin ()
  ((%default-value-form
    :initarg :default-value-form
    :reader default-value-form)))

(defclass supplied-p-variable-mixin ()
  ((%supplied-p-variable-name
    :initarg :supplied-p-variable-name
    :reader supplied-p-variable-name)))

(defclass required-parameter (node name-mixin)
  ())

(defclass optional-parameter
    (node
     name-mixin
     default-value-form-mixin
     supplied-p-variable-name-mixin)
  ())

(defclass rest-parameter (node name-mixin)
  ())
