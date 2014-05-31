(in-package #:cleavir-ast)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class SHORT-FLOAT-ADD-AST.
;;;
;;; This AST is used for adding two values of type SHORT-FLOAT.
;;;
;;; It can be used by an implementation that supports the SHORT-FLOAT
;;; data type.  
;;;
;;; Both inputs must be of type SHORT-FLOAT, so in safe code this
;;; restriction has to be checked before this AST is evaluated. 

(defclass short-float-add-ast (ast)
  ((%arg1-ast :initarg :arg1-ast :reader arg1-ast)
   (%arg2-ast :initarg :arg2-ast :reader arg2-ast)))

(defun make-short-float-add-ast (arg1-ast arg2-ast)
  (make-instance 'short-float-add-ast
    :arg1-ast arg1-ast
    :arg2-ast arg2-ast))

(cleavir-io:define-save-info short-float-add-ast
  (:arg1-ast arg1-ast)
  (:arg2-ast arg2-ast))

(defmethod children ((ast short-float-add-ast))
  (list (arg1-ast ast) (arg2-ast ast)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class SHORT-FLOAT-SUB-AST.
;;;
;;; This AST is used for subtracting two values of type SHORT-FLOAT.
;;;
;;; It can be used by an implementation that supports the SHORT-FLOAT
;;; data type.  
;;;
;;; Both inputs must be of type SHORT-FLOAT, so in safe code this
;;; restriction has to be checked before this AST is evaluated. 

(defclass short-float-sub-ast (ast)
  ((%arg1-ast :initarg :arg1-ast :reader arg1-ast)
   (%arg2-ast :initarg :arg2-ast :reader arg2-ast)))

(defun make-short-float-sub-ast (arg1-ast arg2-ast)
  (make-instance 'short-float-sub-ast
    :arg1-ast arg1-ast
    :arg2-ast arg2-ast))

(cleavir-io:define-save-info short-float-sub-ast
  (:arg1-ast arg1-ast)
  (:arg2-ast arg2-ast))

(defmethod children ((ast short-float-sub-ast))
  (list (arg1-ast ast) (arg2-ast ast)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class SHORT-FLOAT-MUL-AST.
;;;
;;; This AST is used for multiplying two values of type SHORT-FLOAT.
;;;
;;; It can be used by an implementation that supports the SHORT-FLOAT
;;; data type.  
;;;
;;; Both inputs must be of type SHORT-FLOAT, so in safe code this
;;; restriction has to be checked before this AST is evaluated. 

(defclass short-float-mul-ast (ast)
  ((%arg1-ast :initarg :arg1-ast :reader arg1-ast)
   (%arg2-ast :initarg :arg2-ast :reader arg2-ast)))

(defun make-short-float-mul-ast (arg1-ast arg2-ast)
  (make-instance 'short-float-mul-ast
    :arg1-ast arg1-ast
    :arg2-ast arg2-ast))

(cleavir-io:define-save-info short-float-mul-ast
  (:arg1-ast arg1-ast)
  (:arg2-ast arg2-ast))

(defmethod children ((ast short-float-mul-ast))
  (list (arg1-ast ast) (arg2-ast ast)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class SINGLE-FLOAT-ADD-AST.
;;;
;;; This AST is used for adding two values of type SINGLE-FLOAT.
;;;
;;; It can be used by an implementation that supports the SINGLE-FLOAT
;;; data type.
;;;
;;; Both inputs must be of type SINGLE-FLOAT, so in safe code this
;;; restriction has to be checked before this AST is evaluated. 

(defclass single-float-add-ast (ast)
  ((%arg1-ast :initarg :arg1-ast :reader arg1-ast)
   (%arg2-ast :initarg :arg2-ast :reader arg2-ast)))

(defun make-single-float-add-ast (arg1-ast arg2-ast)
  (make-instance 'single-float-add-ast
    :arg1-ast arg1-ast
    :arg2-ast arg2-ast))

(cleavir-io:define-save-info single-float-add-ast
  (:arg1-ast arg1-ast)
  (:arg2-ast arg2-ast))

(defmethod children ((ast single-float-add-ast))
  (list (arg1-ast ast) (arg2-ast ast)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class SINGLE-FLOAT-SUB-AST.
;;;
;;; This AST is used for subtracting two values of type SINGLE-FLOAT.
;;;
;;; It can be used by an implementation that supports the SINGLE-FLOAT
;;; data type.  
;;;
;;; Both inputs must be of type SINGLE-FLOAT, so in safe code this
;;; restriction has to be checked before this AST is evaluated. 

(defclass single-float-sub-ast (ast)
  ((%arg1-ast :initarg :arg1-ast :reader arg1-ast)
   (%arg2-ast :initarg :arg2-ast :reader arg2-ast)))

(defun make-single-float-sub-ast (arg1-ast arg2-ast)
  (make-instance 'single-float-sub-ast
    :arg1-ast arg1-ast
    :arg2-ast arg2-ast))

(cleavir-io:define-save-info single-float-sub-ast
  (:arg1-ast arg1-ast)
  (:arg2-ast arg2-ast))

(defmethod children ((ast single-float-sub-ast))
  (list (arg1-ast ast) (arg2-ast ast)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class SINGLE-FLOAT-MUL-AST.
;;;
;;; This AST is used for multiplying two values of type SINGLE-FLOAT.
;;;
;;; It can be used by an implementation that supports the SINGLE-FLOAT
;;; data type.  
;;;
;;; Both inputs must be of type SINGLE-FLOAT, so in safe code this
;;; restriction has to be checked before this AST is evaluated. 

(defclass single-float-mul-ast (ast)
  ((%arg1-ast :initarg :arg1-ast :reader arg1-ast)
   (%arg2-ast :initarg :arg2-ast :reader arg2-ast)))

(defun make-single-float-mul-ast (arg1-ast arg2-ast)
  (make-instance 'single-float-mul-ast
    :arg1-ast arg1-ast
    :arg2-ast arg2-ast))

(cleavir-io:define-save-info single-float-mul-ast
  (:arg1-ast arg1-ast)
  (:arg2-ast arg2-ast))

(defmethod children ((ast single-float-mul-ast))
  (list (arg1-ast ast) (arg2-ast ast)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class DOUBLE-FLOAT-ADD-AST.
;;;
;;; This AST is used for adding two values of type DOUBLE-FLOAT.
;;;
;;; It can be used by an implementation that supports the DOUBLE-FLOAT
;;; data type.  
;;;
;;; Both inputs must be of type DOUBLE-FLOAT, so in safe code this
;;; restriction has to be checked before this AST is evaluated. 

(defclass double-float-add-ast (ast)
  ((%arg1-ast :initarg :arg1-ast :reader arg1-ast)
   (%arg2-ast :initarg :arg2-ast :reader arg2-ast)))

(defun make-double-float-add-ast (arg1-ast arg2-ast)
  (make-instance 'double-float-add-ast
    :arg1-ast arg1-ast
    :arg2-ast arg2-ast))

(cleavir-io:define-save-info double-float-add-ast
  (:arg1-ast arg1-ast)
  (:arg2-ast arg2-ast))

(defmethod children ((ast double-float-add-ast))
  (list (arg1-ast ast) (arg2-ast ast)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class DOUBLE-FLOAT-SUB-AST.
;;;
;;; This AST is used for subtracting two values of type DOUBLE-FLOAT.
;;;
;;; It can be used by an implementation that supports the DOUBLE-FLOAT
;;; data type.  
;;;
;;; Both inputs must be of type DOUBLE-FLOAT, so in safe code this
;;; restriction has to be checked before this AST is evaluated. 

(defclass double-float-sub-ast (ast)
  ((%arg1-ast :initarg :arg1-ast :reader arg1-ast)
   (%arg2-ast :initarg :arg2-ast :reader arg2-ast)))

(defun make-double-float-sub-ast (arg1-ast arg2-ast)
  (make-instance 'double-float-sub-ast
    :arg1-ast arg1-ast
    :arg2-ast arg2-ast))

(cleavir-io:define-save-info double-float-sub-ast
  (:arg1-ast arg1-ast)
  (:arg2-ast arg2-ast))

(defmethod children ((ast double-float-sub-ast))
  (list (arg1-ast ast) (arg2-ast ast)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class LONG-FLOAT-ADD-AST.
;;;
;;; This AST is used for adding two values of type LONG-FLOAT.
;;;
;;; It can be used by an implementation that supports the LONG-FLOAT
;;; data type.  
;;;
;;; Both inputs must be of type LONG-FLOAT, so in safe code this
;;; restriction has to be checked before this AST is evaluated. 

(defclass long-float-add-ast (ast)
  ((%arg1-ast :initarg :arg1-ast :reader arg1-ast)
   (%arg2-ast :initarg :arg2-ast :reader arg2-ast)))

(defun make-long-float-add-ast (arg1-ast arg2-ast)
  (make-instance 'long-float-add-ast
    :arg1-ast arg1-ast
    :arg2-ast arg2-ast))

(cleavir-io:define-save-info long-float-add-ast
  (:arg1-ast arg1-ast)
  (:arg2-ast arg2-ast))

(defmethod children ((ast long-float-add-ast))
  (list (arg1-ast ast) (arg2-ast ast)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class LONG-FLOAT-SUB-AST.
;;;
;;; This AST is used for subtracting two values of type LONG-FLOAT.
;;;
;;; It can be used by an implementation that supports the LONG-FLOAT
;;; data type.  
;;;
;;; Both inputs must be of type LONG-FLOAT, so in safe code this
;;; restriction has to be checked before this AST is evaluated. 

(defclass long-float-sub-ast (ast)
  ((%arg1-ast :initarg :arg1-ast :reader arg1-ast)
   (%arg2-ast :initarg :arg2-ast :reader arg2-ast)))

(defun make-long-float-sub-ast (arg1-ast arg2-ast)
  (make-instance 'long-float-sub-ast
    :arg1-ast arg1-ast
    :arg2-ast arg2-ast))

(cleavir-io:define-save-info long-float-sub-ast
  (:arg1-ast arg1-ast)
  (:arg2-ast arg2-ast))

(defmethod children ((ast long-float-sub-ast))
  (list (arg1-ast ast) (arg2-ast ast)))
