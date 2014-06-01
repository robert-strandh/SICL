(in-package #:cleavir-ast)

(defmacro make-float-ast (name)
  `(progn 
     (defclass ,name (ast)
       ((%arg1-ast :initarg :arg1-ast :reader arg1-ast)
	(%arg2-ast :initarg :arg2-ast :reader arg2-ast)))

     (cleavir-io:define-save-info ,name
       (:arg1-ast arg1-ast)
       (:arg2-ast arg2-ast))

     (defmethod children ((ast ,name))
       (list (arg1-ast ast) (arg2-ast ast)))))

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

(make-float-ast short-float-add-ast)

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
;;; Class SHORT-FLOAT-DIV-AST.
;;;
;;; This AST is used for dividing two values of type SHORT-FLOAT.
;;;
;;; It can be used by an implementation that supports the SHORT-FLOAT
;;; data type.  
;;;
;;; Both inputs must be of type SHORT-FLOAT, so in safe code this
;;; restriction has to be checked before this AST is evaluated. 

(defclass short-float-div-ast (ast)
  ((%arg1-ast :initarg :arg1-ast :reader arg1-ast)
   (%arg2-ast :initarg :arg2-ast :reader arg2-ast)))

(defun make-short-float-div-ast (arg1-ast arg2-ast)
  (make-instance 'short-float-div-ast
    :arg1-ast arg1-ast
    :arg2-ast arg2-ast))

(cleavir-io:define-save-info short-float-div-ast
  (:arg1-ast arg1-ast)
  (:arg2-ast arg2-ast))

(defmethod children ((ast short-float-div-ast))
  (list (arg1-ast ast) (arg2-ast ast)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class SHORT-FLOAT-LESS-AST.
;;;
;;; This class can be used to implement a binary LESS-THAN function.
;;; It requires both its arguments to be of type SHORT-FLOAT.  It can
;;; only occur as the TEST-AST of an IF-AST.  If this AST occurs in a
;;; position where a value is required, an error is signaled.
;;;
;;; It can be used by an implementation that supports the SHORT-FLOAT
;;; data type.

(defclass short-float-less-ast (ast)
  ((%arg1-ast :initarg :arg1-ast :reader arg1-ast)
   (%arg2-ast :initarg :arg2-ast :reader arg2-ast)))

(defun make-short-float-less-ast (arg1-ast arg2-ast)
  (make-instance 'short-float-less-ast
    :arg1-ast arg1-ast
    :arg2-ast arg2-ast))

(cleavir-io:define-save-info short-float-less-ast
  (:arg1-ast arg1-ast)
  (:arg2-ast arg2-ast))

(defmethod children ((ast short-float-less-ast))
  (list (arg1-ast ast) (arg2-ast ast)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class SHORT-FLOAT-NOT-GREATER-AST.
;;;
;;; This class can be used to implement a binary <= function.  It
;;; requires both its arguments to be of type SHORT-FLOAT.  It can
;;; only occur as the TEST-AST of an IF-AST.  If this AST occurs in a
;;; position where a value is required, an error is signaled.
;;;
;;; It can be used by an implementation that supports the SHORT-FLOAT
;;; data type.

(defclass short-float-not-greater-ast (ast)
  ((%arg1-ast :initarg :arg1-ast :reader arg1-ast)
   (%arg2-ast :initarg :arg2-ast :reader arg2-ast)))

(defun make-short-float-not-greater-ast (arg1-ast arg2-ast)
  (make-instance 'short-float-not-greater-ast
    :arg1-ast arg1-ast
    :arg2-ast arg2-ast))

(cleavir-io:define-save-info short-float-not-greater-ast
  (:arg1-ast arg1-ast)
  (:arg2-ast arg2-ast))

(defmethod children ((ast short-float-not-greater-ast))
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
;;; Class SINGLE-FLOAT-DIV-AST.
;;;
;;; This AST is used for dividing two values of type SINGLE-FLOAT.
;;;
;;; It can be used by an implementation that supports the SINGLE-FLOAT
;;; data type.  
;;;
;;; Both inputs must be of type SINGLE-FLOAT, so in safe code this
;;; restriction has to be checked before this AST is evaluated. 

(defclass single-float-div-ast (ast)
  ((%arg1-ast :initarg :arg1-ast :reader arg1-ast)
   (%arg2-ast :initarg :arg2-ast :reader arg2-ast)))

(defun make-single-float-div-ast (arg1-ast arg2-ast)
  (make-instance 'single-float-div-ast
    :arg1-ast arg1-ast
    :arg2-ast arg2-ast))

(cleavir-io:define-save-info single-float-div-ast
  (:arg1-ast arg1-ast)
  (:arg2-ast arg2-ast))

(defmethod children ((ast single-float-div-ast))
  (list (arg1-ast ast) (arg2-ast ast)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class SINGLE-FLOAT-LESS-AST.
;;;
;;; This class can be used to implement a binary LESS-THAN function.
;;; It requires both its arguments to be of type SINGLE-FLOAT.  It can
;;; only occur as the TEST-AST of an IF-AST.  If this AST occurs in a
;;; position where a value is required, an error is signaled.
;;;
;;; It can be used by an implementation that supports the SINGLE-FLOAT
;;; data type.

(defclass single-float-less-ast (ast)
  ((%arg1-ast :initarg :arg1-ast :reader arg1-ast)
   (%arg2-ast :initarg :arg2-ast :reader arg2-ast)))

(defun make-single-float-less-ast (arg1-ast arg2-ast)
  (make-instance 'single-float-less-ast
    :arg1-ast arg1-ast
    :arg2-ast arg2-ast))

(cleavir-io:define-save-info single-float-less-ast
  (:arg1-ast arg1-ast)
  (:arg2-ast arg2-ast))

(defmethod children ((ast single-float-less-ast))
  (list (arg1-ast ast) (arg2-ast ast)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class SINGLE-FLOAT-NOT-GREATER-AST.
;;;
;;; This class can be used to implement a binary <= function.  It
;;; requires both its arguments to be of type SINGLE-FLOAT.  It can
;;; only occur as the TEST-AST of an IF-AST.  If this AST occurs in a
;;; position where a value is required, an error is signaled.
;;;
;;; It can be used by an implementation that supports the SINGLE-FLOAT
;;; data type.

(defclass single-float-not-greater-ast (ast)
  ((%arg1-ast :initarg :arg1-ast :reader arg1-ast)
   (%arg2-ast :initarg :arg2-ast :reader arg2-ast)))

(defun make-single-float-not-greater-ast (arg1-ast arg2-ast)
  (make-instance 'single-float-not-greater-ast
    :arg1-ast arg1-ast
    :arg2-ast arg2-ast))

(cleavir-io:define-save-info single-float-not-greater-ast
  (:arg1-ast arg1-ast)
  (:arg2-ast arg2-ast))

(defmethod children ((ast single-float-not-greater-ast))
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
;;; Class DOUBLE-FLOAT-MUL-AST.
;;;
;;; This AST is used for multiplying two values of type DOUBLE-FLOAT.
;;;
;;; It can be used by an implementation that supports the DOUBLE-FLOAT
;;; data type.  
;;;
;;; Both inputs must be of type DOUBLE-FLOAT, so in safe code this
;;; restriction has to be checked before this AST is evaluated. 

(defclass double-float-mul-ast (ast)
  ((%arg1-ast :initarg :arg1-ast :reader arg1-ast)
   (%arg2-ast :initarg :arg2-ast :reader arg2-ast)))

(defun make-double-float-mul-ast (arg1-ast arg2-ast)
  (make-instance 'double-float-mul-ast
    :arg1-ast arg1-ast
    :arg2-ast arg2-ast))

(cleavir-io:define-save-info double-float-mul-ast
  (:arg1-ast arg1-ast)
  (:arg2-ast arg2-ast))

(defmethod children ((ast double-float-mul-ast))
  (list (arg1-ast ast) (arg2-ast ast)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class DOUBLE-FLOAT-DIV-AST.
;;;
;;; This AST is used for dividing two values of type DOUBLE-FLOAT.
;;;
;;; It can be used by an implementation that supports the DOUBLE-FLOAT
;;; data type.  
;;;
;;; Both inputs must be of type DOUBLE-FLOAT, so in safe code this
;;; restriction has to be checked before this AST is evaluated. 

(defclass double-float-div-ast (ast)
  ((%arg1-ast :initarg :arg1-ast :reader arg1-ast)
   (%arg2-ast :initarg :arg2-ast :reader arg2-ast)))

(defun make-double-float-div-ast (arg1-ast arg2-ast)
  (make-instance 'double-float-div-ast
    :arg1-ast arg1-ast
    :arg2-ast arg2-ast))

(cleavir-io:define-save-info double-float-div-ast
  (:arg1-ast arg1-ast)
  (:arg2-ast arg2-ast))

(defmethod children ((ast double-float-div-ast))
  (list (arg1-ast ast) (arg2-ast ast)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class DOUBLE-FLOAT-LESS-AST.
;;;
;;; This class can be used to implement a binary LESS-THAN function.
;;; It requires both its arguments to be of type DOUBLE-FLOAT.  It can
;;; only occur as the TEST-AST of an IF-AST.  If this AST occurs in a
;;; position where a value is required, an error is signaled.
;;;
;;; It can be used by an implementation that supports the DOUBLE-FLOAT
;;; data type.

(defclass double-float-less-ast (ast)
  ((%arg1-ast :initarg :arg1-ast :reader arg1-ast)
   (%arg2-ast :initarg :arg2-ast :reader arg2-ast)))

(defun make-double-float-less-ast (arg1-ast arg2-ast)
  (make-instance 'double-float-less-ast
    :arg1-ast arg1-ast
    :arg2-ast arg2-ast))

(cleavir-io:define-save-info double-float-less-ast
  (:arg1-ast arg1-ast)
  (:arg2-ast arg2-ast))

(defmethod children ((ast double-float-less-ast))
  (list (arg1-ast ast) (arg2-ast ast)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class DOUBLE-FLOAT-NOT-GREATER-AST.
;;;
;;; This class can be used to implement a binary <= function.  It
;;; requires both its arguments to be of type DOUBLE-FLOAT.  It can
;;; only occur as the TEST-AST of an IF-AST.  If this AST occurs in a
;;; position where a value is required, an error is signaled.
;;;
;;; It can be used by an implementation that supports the DOUBLE-FLOAT
;;; data type.

(defclass double-float-not-greater-ast (ast)
  ((%arg1-ast :initarg :arg1-ast :reader arg1-ast)
   (%arg2-ast :initarg :arg2-ast :reader arg2-ast)))

(defun make-double-float-not-greater-ast (arg1-ast arg2-ast)
  (make-instance 'double-float-not-greater-ast
    :arg1-ast arg1-ast
    :arg2-ast arg2-ast))

(cleavir-io:define-save-info double-float-not-greater-ast
  (:arg1-ast arg1-ast)
  (:arg2-ast arg2-ast))

(defmethod children ((ast double-float-not-greater-ast))
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class LONG-FLOAT-MUL-AST.
;;;
;;; This AST is used for multiplying two values of type LONG-FLOAT.
;;;
;;; It can be used by an implementation that supports the LONG-FLOAT
;;; data type.  
;;;
;;; Both inputs must be of type LONG-FLOAT, so in safe code this
;;; restriction has to be checked before this AST is evaluated. 

(defclass long-float-mul-ast (ast)
  ((%arg1-ast :initarg :arg1-ast :reader arg1-ast)
   (%arg2-ast :initarg :arg2-ast :reader arg2-ast)))

(defun make-long-float-mul-ast (arg1-ast arg2-ast)
  (make-instance 'long-float-mul-ast
    :arg1-ast arg1-ast
    :arg2-ast arg2-ast))

(cleavir-io:define-save-info long-float-mul-ast
  (:arg1-ast arg1-ast)
  (:arg2-ast arg2-ast))

(defmethod children ((ast long-float-mul-ast))
  (list (arg1-ast ast) (arg2-ast ast)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class LONG-FLOAT-DIV-AST.
;;;
;;; This AST is used for dividing two values of type LONG-FLOAT.
;;;
;;; It can be used by an implementation that supports the LONG-FLOAT
;;; data type.  
;;;
;;; Both inputs must be of type LONG-FLOAT, so in safe code this
;;; restriction has to be checked before this AST is evaluated. 

(defclass long-float-div-ast (ast)
  ((%arg1-ast :initarg :arg1-ast :reader arg1-ast)
   (%arg2-ast :initarg :arg2-ast :reader arg2-ast)))

(defun make-long-float-div-ast (arg1-ast arg2-ast)
  (make-instance 'long-float-div-ast
    :arg1-ast arg1-ast
    :arg2-ast arg2-ast))

(cleavir-io:define-save-info long-float-div-ast
  (:arg1-ast arg1-ast)
  (:arg2-ast arg2-ast))

(defmethod children ((ast long-float-div-ast))
  (list (arg1-ast ast) (arg2-ast ast)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class LONG-FLOAT-LESS-AST.
;;;
;;; This class can be used to implement a binary LESS-THAN function.
;;; It requires both its arguments to be of type LONG-FLOAT.  It can
;;; only occur as the TEST-AST of an IF-AST.  If this AST occurs in a
;;; position where a value is required, an error is signaled.
;;;
;;; It can be used by an implementation that supports the LONG-FLOAT
;;; data type.

(defclass long-float-less-ast (ast)
  ((%arg1-ast :initarg :arg1-ast :reader arg1-ast)
   (%arg2-ast :initarg :arg2-ast :reader arg2-ast)))

(defun make-long-float-less-ast (arg1-ast arg2-ast)
  (make-instance 'long-float-less-ast
    :arg1-ast arg1-ast
    :arg2-ast arg2-ast))

(cleavir-io:define-save-info long-float-less-ast
  (:arg1-ast arg1-ast)
  (:arg2-ast arg2-ast))

(defmethod children ((ast long-float-less-ast))
  (list (arg1-ast ast) (arg2-ast ast)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class LONG-FLOAT-NOT-GREATER-AST.
;;;
;;; This class can be used to implement a binary <= function.  It
;;; requires both its arguments to be of type LONG-FLOAT.  It can
;;; only occur as the TEST-AST of an IF-AST.  If this AST occurs in a
;;; position where a value is required, an error is signaled.
;;;
;;; It can be used by an implementation that supports the LONG-FLOAT
;;; data type.

(defclass long-float-not-greater-ast (ast)
  ((%arg1-ast :initarg :arg1-ast :reader arg1-ast)
   (%arg2-ast :initarg :arg2-ast :reader arg2-ast)))

(defun make-long-float-not-greater-ast (arg1-ast arg2-ast)
  (make-instance 'long-float-not-greater-ast
    :arg1-ast arg1-ast
    :arg2-ast arg2-ast))

(cleavir-io:define-save-info long-float-not-greater-ast
  (:arg1-ast arg1-ast)
  (:arg2-ast arg2-ast))

(defmethod children ((ast long-float-not-greater-ast))
  (list (arg1-ast ast) (arg2-ast ast)))
