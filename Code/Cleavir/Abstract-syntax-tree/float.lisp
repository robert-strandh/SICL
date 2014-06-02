(in-package #:cleavir-ast)

(defmacro define-one-arg-float-ast (name)
  `(progn 
     (defclass ,name (ast)
       ((%arg-ast :initarg :arg-ast :reader arg-ast)))

     (cleavir-io:define-save-info ,name
       (:arg-ast arg-ast))

     (defmethod children ((ast ,name))
       (list (arg-ast ast)))))

(defmacro define-two-arg-float-ast (name)
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

(define-two-arg-float-ast short-float-add-ast)

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

(define-two-arg-float-ast short-float-sub-ast)

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

(define-two-arg-float-ast short-float-mul-ast)

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

(define-two-arg-float-ast short-float-div-ast)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class SHORT-FLOAT-LESS-AST.
;;;
;;; This class can be used to implement a binary < function.  It
;;; requires both its arguments to be of type SHORT-FLOAT.  It can
;;; only occur as the TEST-AST of an IF-AST.  If this AST occurs in a
;;; position where a value is required, an error is signaled.
;;;
;;; It can be used by an implementation that supports the SHORT-FLOAT
;;; data type.

(define-two-arg-float-ast short-float-less-ast)

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

(define-two-arg-float-ast short-float-not-greater-ast)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class SHORT-FLOAT-GREATER-AST.
;;;
;;; This class can be used to implement a binary > function.  It
;;; requires both its arguments to be of type SHORT-FLOAT.  It can
;;; only occur as the TEST-AST of an IF-AST.  If this AST occurs in a
;;; position where a value is required, an error is signaled.
;;;
;;; It can be used by an implementation that supports the SHORT-FLOAT
;;; data type.

(define-two-arg-float-ast short-float-greater-ast)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class SHORT-FLOAT-NOT-LESS-AST.
;;;
;;; This class can be used to implement a binary >= function.  It
;;; requires both its arguments to be of type SHORT-FLOAT.  It can
;;; only occur as the TEST-AST of an IF-AST.  If this AST occurs in a
;;; position where a value is required, an error is signaled.
;;;
;;; It can be used by an implementation that supports the SHORT-FLOAT
;;; data type.

(define-two-arg-float-ast short-float-not-less-ast)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class SHORT-FLOAT-EQUAL-AST.
;;;
;;; This class can be used to implement a binary = function.  It
;;; requires both its arguments to be of type SHORT-FLOAT.  It can
;;; only occur as the TEST-AST of an IF-AST.  If this AST occurs in a
;;; position where a value is required, an error is signaled.
;;;
;;; It can be used by an implementation that supports the SHORT-FLOAT
;;; data type.

(define-two-arg-float-ast short-float-equal-ast)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class SHORT-FLOAT-SIN-AST.
;;;
;;; This AST is used for computing the sine of a value of type
;;; SHORT-FLOAT.
;;;
;;; It can be used by an implementation that supports the SHORT-FLOAT
;;; data type.  
;;;
;;; The input must be of type SHORT-FLOAT, so in safe code this
;;; restriction has to be checked before this AST is evaluated.

(define-one-arg-float-ast short-float-sin-ast)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class SHORT-FLOAT-COS-AST.
;;;
;;; This AST is used for computing the cosine of a value of type
;;; SHORT-FLOAT.
;;;
;;; It can be used by an implementation that supports the SHORT-FLOAT
;;; data type.  
;;;
;;; The input must be of type SHORT-FLOAT, so in safe code this
;;; restriction has to be checked before this AST is evaluated.

(define-one-arg-float-ast short-float-cos-ast)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class SINGLE-FLOAT-ADD-AST.
;;;
;;; This AST is used for adding two values of type SINGLE-FLOAT.
;;;
;;; Both inputs must be of type SINGLE-FLOAT, so in safe code this
;;; restriction has to be checked before this AST is evaluated. 

(define-two-arg-float-ast single-float-add-ast)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class SINGLE-FLOAT-SUB-AST.
;;;
;;; This AST is used for subtracting two values of type SINGLE-FLOAT.
;;;
;;; Both inputs must be of type SINGLE-FLOAT, so in safe code this
;;; restriction has to be checked before this AST is evaluated. 

(define-two-arg-float-ast single-float-sub-ast)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class SINGLE-FLOAT-MUL-AST.
;;;
;;; This AST is used for multiplying two values of type SINGLE-FLOAT.
;;;
;;; Both inputs must be of type SINGLE-FLOAT, so in safe code this
;;; restriction has to be checked before this AST is evaluated. 

(define-two-arg-float-ast single-float-mul-ast)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class SINGLE-FLOAT-DIV-AST.
;;;
;;; This AST is used for dividing two values of type SINGLE-FLOAT.
;;;
;;; Both inputs must be of type SINGLE-FLOAT, so in safe code this
;;; restriction has to be checked before this AST is evaluated. 

(define-two-arg-float-ast single-float-div-ast)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class SINGLE-FLOAT-LESS-AST.
;;;
;;; This class can be used to implement a binary < function.  It
;;; requires both its arguments to be of type SINGLE-FLOAT.  It can
;;; only occur as the TEST-AST of an IF-AST.  If this AST occurs in a
;;; position where a value is required, an error is signaled.

(define-two-arg-float-ast single-float-less-ast)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class SINGLE-FLOAT-NOT-GREATER-AST.
;;;
;;; This class can be used to implement a binary <= function.  It
;;; requires both its arguments to be of type SINGLE-FLOAT.  It can
;;; only occur as the TEST-AST of an IF-AST.  If this AST occurs in a
;;; position where a value is required, an error is signaled.

(define-two-arg-float-ast single-float-not-greater-ast)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class SINGLE-FLOAT-GREATER-AST.
;;;
;;; This class can be used to implement a binary > function.  It
;;; requires both its arguments to be of type SINGLE-FLOAT.  It can
;;; only occur as the TEST-AST of an IF-AST.  If this AST occurs in a
;;; position where a value is required, an error is signaled.

(define-two-arg-float-ast single-float-greater-ast)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class SINGLE-FLOAT-NOT-LESS-AST.
;;;
;;; This class can be used to implement a binary >= function.  It
;;; requires both its arguments to be of type SINGLE-FLOAT.  It can
;;; only occur as the TEST-AST of an IF-AST.  If this AST occurs in a
;;; position where a value is required, an error is signaled.

(define-two-arg-float-ast single-float-not-less-ast)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class SINGLE-FLOAT-EQUAL-AST.
;;;
;;; This class can be used to implement a binary = function.  It
;;; requires both its arguments to be of type SINGLE-FLOAT.  It can
;;; only occur as the TEST-AST of an IF-AST.  If this AST occurs in a
;;; position where a value is required, an error is signaled.
;;;
;;; It can be used by an implementation that supports the SINGLE-FLOAT
;;; data type.

(define-two-arg-float-ast single-float-equal-ast)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class SINGLE-FLOAT-SIN-AST.
;;;
;;; This AST is used for computing the sine of a value of type
;;; SINGLE-FLOAT.
;;;
;;; It can be used by an implementation that supports the SINGLE-FLOAT
;;; data type.  
;;;
;;; The input must be of type SINGLE-FLOAT, so in safe code this
;;; restriction has to be checked before this AST is evaluated.

(define-one-arg-float-ast single-float-sin-ast)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class SINGLE-FLOAT-COS-AST.
;;;
;;; This AST is used for computing the cosine of a value of type
;;; SINGLE-FLOAT.
;;;
;;; It can be used by an implementation that supports the SINGLE-FLOAT
;;; data type.  
;;;
;;; The input must be of type SINGLE-FLOAT, so in safe code this
;;; restriction has to be checked before this AST is evaluated.

(define-one-arg-float-ast single-float-cos-ast)

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

(define-two-arg-float-ast double-float-add-ast)

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

(define-two-arg-float-ast double-float-sub-ast)

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

(define-two-arg-float-ast double-float-mul-ast)

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

(define-two-arg-float-ast double-float-div-ast)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class DOUBLE-FLOAT-LESS-AST.
;;;
;;; This class can be used to implement a binary < function.  It
;;; requires both its arguments to be of type DOUBLE-FLOAT.  It can
;;; only occur as the TEST-AST of an IF-AST.  If this AST occurs in a
;;; position where a value is required, an error is signaled.
;;;
;;; It can be used by an implementation that supports the DOUBLE-FLOAT
;;; data type.

(define-two-arg-float-ast double-float-less-ast)

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

(define-two-arg-float-ast double-float-not-greater-ast)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class DOUBLE-FLOAT-GREATER-AST.
;;;
;;; This class can be used to implement a binary > function.  It
;;; requires both its arguments to be of type DOUBLE-FLOAT.  It can
;;; only occur as the TEST-AST of an IF-AST.  If this AST occurs in a
;;; position where a value is required, an error is signaled.
;;;
;;; It can be used by an implementation that supports the DOUBLE-FLOAT
;;; data type.

(define-two-arg-float-ast double-float-greater-ast)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class DOUBLE-FLOAT-NOT-LESS-AST.
;;;
;;; This class can be used to implement a binary >= function.  It
;;; requires both its arguments to be of type DOUBLE-FLOAT.  It can
;;; only occur as the TEST-AST of an IF-AST.  If this AST occurs in a
;;; position where a value is required, an error is signaled.
;;;
;;; It can be used by an implementation that supports the DOUBLE-FLOAT
;;; data type.

(define-two-arg-float-ast double-float-not-less-ast)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class DOUBLE-FLOAT-EQUAL-AST.
;;;
;;; This class can be used to implement a binary = function.  It
;;; requires both its arguments to be of type DOUBLE-FLOAT.  It can
;;; only occur as the TEST-AST of an IF-AST.  If this AST occurs in a
;;; position where a value is required, an error is signaled.
;;;
;;; It can be used by an implementation that supports the DOUBLE-FLOAT
;;; data type.

(define-two-arg-float-ast double-float-equal-ast)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class DOUBLE-FLOAT-SIN-AST.
;;;
;;; This AST is used for computing the sine of a value of type
;;; DOUBLE-FLOAT.
;;;
;;; It can be used by an implementation that supports the DOUBLE-FLOAT
;;; data type.  
;;;
;;; The input must be of type DOUBLE-FLOAT, so in safe code this
;;; restriction has to be checked before this AST is evaluated.

(define-one-arg-float-ast double-float-sin-ast)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class DOUBLE-FLOAT-COS-AST.
;;;
;;; This AST is used for computing the cosine of a value of type
;;; DOUBLE-FLOAT.
;;;
;;; It can be used by an implementation that supports the DOUBLE-FLOAT
;;; data type.  
;;;
;;; The input must be of type DOUBLE-FLOAT, so in safe code this
;;; restriction has to be checked before this AST is evaluated.

(define-one-arg-float-ast double-float-cos-ast)

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

(define-two-arg-float-ast long-float-add-ast)

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

(define-two-arg-float-ast long-float-sub-ast)

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

(define-two-arg-float-ast long-float-mul-ast)

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

(define-two-arg-float-ast long-float-div-ast)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class LONG-FLOAT-LESS-AST.
;;;
;;; This class can be used to implement a binary < function.  It
;;; requires both its arguments to be of type LONG-FLOAT.  It can only
;;; occur as the TEST-AST of an IF-AST.  If this AST occurs in a
;;; position where a value is required, an error is signaled.
;;;
;;; It can be used by an implementation that supports the LONG-FLOAT
;;; data type.

(define-two-arg-float-ast long-float-less-ast)

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

(define-two-arg-float-ast long-float-not-greater-ast)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class LONG-FLOAT-GREATER-AST.
;;;
;;; This class can be used to implement a binary > function.  It
;;; requires both its arguments to be of type LONG-FLOAT.  It can
;;; only occur as the TEST-AST of an IF-AST.  If this AST occurs in a
;;; position where a value is required, an error is signaled.
;;;
;;; It can be used by an implementation that supports the LONG-FLOAT
;;; data type.

(define-two-arg-float-ast long-float-greater-ast)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class LONG-FLOAT-NOT-LESS-AST.
;;;
;;; This class can be used to implement a binary >= function.  It
;;; requires both its arguments to be of type LONG-FLOAT.  It can
;;; only occur as the TEST-AST of an IF-AST.  If this AST occurs in a
;;; position where a value is required, an error is signaled.
;;;
;;; It can be used by an implementation that supports the LONG-FLOAT
;;; data type.

(define-two-arg-float-ast long-float-not-less-ast)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class LONG-FLOAT-EQUAL-AST.
;;;
;;; This class can be used to implement a binary = function.  It
;;; requires both its arguments to be of type LONG-FLOAT.  It can
;;; only occur as the TEST-AST of an IF-AST.  If this AST occurs in a
;;; position where a value is required, an error is signaled.
;;;
;;; It can be used by an implementation that supports the LONG-FLOAT
;;; data type.

(define-two-arg-float-ast long-float-equal-ast)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class LONG-FLOAT-SIN-AST.
;;;
;;; This AST is used for computing the sine of a value of type
;;; LONG-FLOAT.
;;;
;;; It can be used by an implementation that supports the LONG-FLOAT
;;; data type.  
;;;
;;; The input must be of type LONG-FLOAT, so in safe code this
;;; restriction has to be checked before this AST is evaluated.

(define-one-arg-float-ast long-float-sin-ast)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class LONG-FLOAT-COS-AST.
;;;
;;; This AST is used for computing the cosine of a value of type
;;; LONG-FLOAT.
;;;
;;; It can be used by an implementation that supports the LONG-FLOAT
;;; data type.  
;;;
;;; The input must be of type LONG-FLOAT, so in safe code this
;;; restriction has to be checked before this AST is evaluated.

(define-one-arg-float-ast long-float-cos-ast)
