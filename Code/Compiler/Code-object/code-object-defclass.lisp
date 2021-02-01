(cl:in-package #:sicl-compiler)

(defclass code-object ()
  ((%instructions :initarg :instructions :reader instructions)
   (%frame-maps :initarg :frame-maps :reader frame-maps)
   (%callee-saves-register-maps
    :initarg :callee-saves-register-maps
    :reader callee-saves-register-maps)
   (%callee-saves-stack-maps
    :initarg :callee-saves-stack-maps
    :reader callee-saves-stack-maps)
   (%constants :initform '() :initarg :constants :accessor constants)
   (%function-names :initarg :function-names :reader function-names)
   (%hir :initarg :hir :reader hir)))
