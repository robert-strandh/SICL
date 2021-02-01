(cl:in-package #:sicl-compiler)

(defgeneric instructions (code-object))

(defgeneric frame-maps (code-object))

(defgeneric callee-saves-register-maps (code-object))

(defgeneric callee-saves-stack-maps (code-object))

(defgeneric constants (code-object))

(defgeneric (setf constants) (constants code-object))

(defgeneric function-names (code-object))

(defgeneric hir (code-object))
