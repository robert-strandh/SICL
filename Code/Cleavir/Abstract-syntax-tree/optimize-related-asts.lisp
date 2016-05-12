(cl:in-package #:cleavir-ast)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class OPTIMIZE-AST.

(defclass optimize-ast (ast)
  ((%child-ast :initarg :child-ast :reader child-ast)
   (%value :initarg :value :reader value)))

(cleavir-io:define-save-info optimize-ast
  (:child-ast child-ast)
  (:value value))

(defmethod children ((ast optimize-ast))
  (list (child-ast ast)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class SPEED-AST.

(defclass speed-ast (optimize-ast)
  ())

(defun make-speed-ast (child-ast value &key origin)
  (make-instance 'speed-ast
    :origin origin
    :child-ast child-ast
    :values value))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class DEBUG-AST.

(defclass debug-ast (optimize-ast)
  ())

(defun make-debug-ast (child-ast value &key origin)
  (make-instance 'debug-ast
    :origin origin
    :child-ast child-ast
    :values value))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class SPACE-AST.

(defclass space-ast (optimize-ast)
  ())

(defun make-space-ast (child-ast value &key origin)
  (make-instance 'space-ast
    :origin origin
    :child-ast child-ast
    :values value))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class SAFETY-AST.

(defclass safety-ast (optimize-ast)
  ())

(defun make-safety-ast (child-ast value &key origin)
  (make-instance 'safety-ast
    :origin origin
    :child-ast child-ast
    :values value))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class COMPILATION-SPEED-AST.

(defclass compilation-speed-ast (optimize-ast)
  ())

(defun make-compilation-speed-ast (child-ast value &key origin)
  (make-instance 'compilation-speed-ast
    :origin origin
    :child-ast child-ast
    :values value))
