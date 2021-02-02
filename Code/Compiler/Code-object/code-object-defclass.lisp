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

(defgeneric function-names (code-object))

(defgeneric (setf function-names) (function-names code-object))

(defgeneric ir (code-object))

(defgeneric (setf ir) (ir code-object))

(defclass code-object ()
  ((%instructions :initarg :instructions :accessor instructions)
   (%frame-maps :initarg :frame-maps :accessor frame-maps)
   (%callee-saves-register-maps
    :initarg :callee-saves-register-maps
    :accessor callee-saves-register-maps)
   (%callee-saves-stack-maps
    :initarg :callee-saves-stack-maps
    :accessor callee-saves-stack-maps)
   (%constants :initform '() :initarg :constants :accessor constants)
   (%function-names :initarg :function-names :accessor function-names)
   (%ir :initarg :ir :accessor ir)))
