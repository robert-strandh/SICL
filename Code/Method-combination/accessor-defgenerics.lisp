(cl:in-package #:sicl-method-combination)

(defgeneric name (method-combination-template))

(defgeneric variant-signature-determiner (method-combination-template))

(defgeneric variants (method-combination-template))

(defgeneric (setf variants) (new-variants method-combination-template))

(defgeneric effective-method-form-function (method-combination-template))
