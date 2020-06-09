(cl:in-package #:cleavir-ast)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class SINGLE-FLOAT-P-AST.
;;;
;;; This AST can be used by implementations that represent single
;;; floats as immediate objects with tags.  It can only occur as the
;;; test of an IF-AST.

(defclass single-float-p-ast (ast boolean-ast-mixin)
  ((%object-ast :initarg :object-ast :reader object-ast)))

(cleavir-io:define-save-info single-float-p-ast
  (:object-ast object-ast))

(defmethod children ((ast single-float-p-ast))
  (list (object-ast ast)))

(defmacro define-simple-one-arg-float-ast (name)
  `(progn 
     (defclass ,name (ast one-value-ast-mixin)
       ((%subtype :initarg :subtype :reader subtype)
        (%arg-ast :initarg :arg-ast :reader arg-ast)))

     (cleavir-io:define-save-info ,name
       (:subtype subtype)
       (:arg-ast arg-ast))

     (defmethod children ((ast ,name))
       (list (arg-ast ast)))))

(defmacro define-simple-two-arg-float-ast (name)
  `(progn 
     (defclass ,name (ast one-value-ast-mixin)
       ((%subtype :initarg :subtype :reader subtype)
        (%arg1-ast :initarg :arg1-ast :reader arg1-ast)
        (%arg2-ast :initarg :arg2-ast :reader arg2-ast)))

     (cleavir-io:define-save-info ,name
       (:subtype subtype)
       (:arg1-ast arg1-ast)
       (:arg2-ast arg2-ast))

     (defmethod children ((ast ,name))
       (list (arg1-ast ast) (arg2-ast ast)))))

(defmacro define-simple-float-comparison-ast (name)
  `(progn 
     (defclass ,name (ast boolean-ast-mixin)
       ((%subtype :initarg :subtype :reader subtype)
        (%arg1-ast :initarg :arg1-ast :reader arg1-ast)
        (%arg2-ast :initarg :arg2-ast :reader arg2-ast)))

     (cleavir-io:define-save-info ,name
       (:subtype subtype)
       (:arg1-ast arg1-ast)
       (:arg2-ast arg2-ast))

     (defmethod children ((ast ,name))
       (list (arg1-ast ast) (arg2-ast ast)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class FLOAT-ADD-AST.
;;;
;;; This AST is used for adding two values of type FLOAT.
;;;
;;; Both inputs must be of the same subtype of FLOAT as indicated by
;;; the SUBTYPE slot.  In safe code, the types of the argument must be
;;; explicitly tested before this AST is evaluated.

(define-simple-two-arg-float-ast float-add-ast)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class FLOAT-SUB-AST.
;;;
;;; This AST is used for subtracting two values of type FLOAT.
;;;
;;; Both inputs must be of the same subtype of FLOAT as indicated by
;;; the SUBTYPE slot.  In safe code, the types of the argument must be
;;; explicitly tested before this AST is evaluated.

(define-simple-two-arg-float-ast float-sub-ast)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class FLOAT-MUL-AST.
;;;
;;; This AST is used for multiplying two values of type FLOAT.
;;;
;;; Both inputs must be of the same subtype of FLOAT as indicated by
;;; the SUBTYPE slot.  In safe code, the types of the argument must be
;;; explicitly tested before this AST is evaluated.

(define-simple-two-arg-float-ast float-mul-ast)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class FLOAT-DIV-AST.
;;;
;;; This AST is used for dividing two values of type FLOAT.
;;;
;;; Both inputs must be of the same subtype of FLOAT as indicated by
;;; the SUBTYPE slot.  In safe code, the types of the argument must be
;;; explicitly tested before this AST is evaluated.

(define-simple-two-arg-float-ast float-div-ast)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class FLOAT-LESS-AST.
;;;
;;; This class can be used to implement a binary < function.  It can
;;; only occur as the TEST-AST of an IF-AST.  If this AST occurs in a
;;; position where a value is required, an error is signaled.
;;;
;;; Both inputs must be of the same subtype of FLOAT as indicated by
;;; the SUBTYPE slot.  In safe code, the types of the argument must be
;;; explicitly tested before this AST is evaluated.

(define-simple-float-comparison-ast float-less-ast)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class FLOAT-NOT-GREATER-AST.
;;;
;;; This class can be used to implement a binary <= function.  It can
;;; only occur as the TEST-AST of an IF-AST.  If this AST occurs in a
;;; position where a value is required, an error is signaled.
;;;
;;; Both inputs must be of the same subtype of FLOAT as indicated by
;;; the SUBTYPE slot.  In safe code, the types of the argument must be
;;; explicitly tested before this AST is evaluated.

(define-simple-float-comparison-ast float-not-greater-ast)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class FLOAT-GREATER-AST.
;;;
;;; This class can be used to implement a binary > function.  It can
;;; only occur as the TEST-AST of an IF-AST.  If this AST occurs in a
;;; position where a value is required, an error is signaled.
;;;
;;; Both inputs must be of the same subtype of FLOAT as indicated by
;;; the SUBTYPE slot.  In safe code, the types of the argument must be
;;; explicitly tested before this AST is evaluated.

(define-simple-float-comparison-ast float-greater-ast)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class FLOAT-NOT-LESS-AST.
;;;
;;; This class can be used to implement a binary >= function.  It can
;;; only occur as the TEST-AST of an IF-AST.  If this AST occurs in a
;;; position where a value is required, an error is signaled.
;;;
;;; Both inputs must be of the same subtype of FLOAT as indicated by
;;; the SUBTYPE slot.  In safe code, the types of the argument must be
;;; explicitly tested before this AST is evaluated.

(define-simple-float-comparison-ast float-not-less-ast)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class FLOAT-EQUAL-AST.
;;;
;;; This class can be used to implement a binary = function.  It can
;;; only occur as the TEST-AST of an IF-AST.  If this AST occurs in a
;;; position where a value is required, an error is signaled.
;;;
;;; Both inputs must be of the same subtype of FLOAT as indicated by
;;; the SUBTYPE slot.  In safe code, the types of the argument must be
;;; explicitly tested before this AST is evaluated.

(define-simple-float-comparison-ast float-equal-ast)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class FLOAT-SIN-AST.
;;;
;;; This AST is used for computing the sine of a floating-point value.
;;;
;;; The input must be of the type indicated by the SUBTYPE slot, so in
;;; safe code this restriction has to be checked before this AST is
;;; evaluated.

(define-simple-one-arg-float-ast float-sin-ast)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class FLOAT-COS-AST.
;;;
;;; This AST is used for computing the cosine of a floating-point
;;; value.
;;;
;;; The input must be of the type indicated by the SUBTYPE slot, so in
;;; safe code this restriction has to be checked before this AST is
;;; evaluated.

(define-simple-one-arg-float-ast float-cos-ast)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class FLOAT-SQRT-AST.
;;;
;;; This AST is used for computing the square root of a floating-point
;;; value.
;;;
;;; The input must be of the type indicated by the SUBTYPE slot, so in
;;; safe code this restriction has to be checked before this AST is
;;; evaluated.

(define-simple-one-arg-float-ast float-sqrt-ast)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class COERCE-AST.
;;;
;;; This AST can be used to convert a number of one type into another
;;; type. Both types are compile-time constants.

(defclass coerce-ast (ast one-value-ast-mixin)
  ((%from-type :initarg :from :reader from-type)
   (%to-type :initarg :to :reader to-type)
   (%arg-ast :initarg :arg-ast :reader arg-ast)))

(cleavir-io:define-save-info coerce-ast
  (:from from-type)
  (:to to-type)
  (:arg-ast arg-ast))

(defmethod children ((ast coerce-ast))
  (list (arg-ast ast)))
