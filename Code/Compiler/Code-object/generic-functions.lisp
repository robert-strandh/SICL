(cl:in-package #:sicl-compiler)

(defgeneric instructions (code-object))

(defgeneric frame-maps (code-object))

(defgeneric callee-saves-register-maps (code-object))

(defgeneric callee-saves-stack-maps (code-object))
