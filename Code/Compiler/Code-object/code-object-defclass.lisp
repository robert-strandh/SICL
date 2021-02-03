(cl:in-package #:sicl-compiler)

(defgeneric instructions (code-object))

(defgeneric (setf instructions) (instructions code-object))

(defgeneric frame-maps (code-object))

(defgeneric (setf frame-maps) (frame-maps code-object))

(defgeneric callee-saves-register-maps (code-object))

(defgeneric (setf callee-saves-register-maps) (maps code-object))

(defgeneric callee-saves-stack-maps (code-object))

(defgeneric (setf callee-saves-stack-maps) (maps code-object))

(defgeneric constants (code-object))

(defgeneric (setf constants) (constants code-object))

(defgeneric call-sites (code-object))

(defgeneric (setf call-sites) (call-sites code-object))

(defgeneric function-names (code-object))

(defgeneric (setf function-names) (function-names code-object))

(defgeneric ast (code-object))

(defgeneric (setf ast) (ast code-object))

(defgeneric ir (code-object))

(defgeneric (setf ir) (ir code-object))

(defgeneric hir-thunks (code-object))

(defgeneric (setf hir-thunks) (hir-thunks code-object))

(defclass code-object ()
  ((%instructions :initform '() :accessor instructions)
   (%frame-maps :initform '() :accessor frame-maps)
   (%callee-saves-register-maps
    :initform '()
    :accessor callee-saves-register-maps)
   (%callee-saves-stack-maps
    :initform '()
    :accessor callee-saves-stack-maps)
   (%constants :initform '() :initarg :constants :accessor constants)
   (%call-sites :initform '() :accessor call-sites)
   (%function-names :initform '() :accessor function-names)
   (%ast :initarg :ast :accessor ast)
   (%ir :initarg :ir :accessor ir)
   (%hir-thunks :initform nil :accessor hir-thunks)))
