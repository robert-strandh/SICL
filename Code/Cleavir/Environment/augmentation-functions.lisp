(cl:in-package #:cleavir-environment)

(defgeneric add-lexical-variable (environment symbol &optional identity))

(defgeneric add-special-variable (environment symbol))

(defgeneric add-local-symbol-macro (environment symbol expansion))

(defgeneric add-local-function (environment function-name &optional identity))

(defgeneric add-local-macro (environment symbol expander))

(defgeneric add-block (environment symbol &optional identity))

(defgeneric add-tag (environment symbol &optional identity))

(defgeneric add-variable-type (environment symbol type))

(defgeneric add-function-type (environment function-name type))

(defgeneric add-variable-ignore (environment symbol ignore))

(defgeneric add-function-ignore (environment function-name ignore))

(defgeneric add-variable-dynamic-extent (environment symbol))

(defgeneric add-function-dynamic-extent (environment function-name))

(defgeneric add-optimize (environment optimize policy))

(defgeneric add-inline (environment function-name inline))

(defgeneric add-inline-expansion (environment function-name expansion))
