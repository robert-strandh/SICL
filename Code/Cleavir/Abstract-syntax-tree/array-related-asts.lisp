(cl:in-package #:cleavir-ast)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class AREF-AST.
;;;
;;; This AST is the base class for all AST classes used to read an
;;; element from an array, whether unspecialized or specialized.  The
;;; INDEX-AST is an AST that evaluates to a row-major index into the
;;; array that is the value of ARRAY-AST.

(defclass aref-ast (ast one-value-ast-mixin)
  ((%array-ast :initarg :array-ast :reader array-ast)
   (%index-ast :initarg :index-ast :reader index-ast)))

(cleavir-io:define-save-info aref-ast
  (:array-ast array-ast)
  (:index-ast index-ast))

(defmethod children ((ast aref-ast))
  (list (array-ast ast) (index-ast ast)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class ASET-AST.
;;;
;;; This AST is the base class for all AST classes used to write an
;;; element to an array, whether unspecialized or specialized.  The
;;; INDEX-AST is an AST that evaluates to a row-major index into the
;;; array that is the value of ARRAY-AST.  The ELEMENT-AST evaluates
;;; to the element to be written, and that element must be of a type
;;; that is acceptable to store in the array, according to how the
;;; array is specialized.

(defclass aset-ast (ast no-value-ast-mixin)
  ((%array-ast :initarg :array-ast :reader array-ast)
   (%index-ast :initarg :index-ast :reader index-ast)
   (%element-ast :initarg :element-ast :reader element-ast)))

(cleavir-io:define-save-info aset-ast
  (:array-ast array-ast)
  (:index-ast index-ast)
  (:element-ast element-ast))

(defmethod children ((ast aset-ast))
  (list (array-ast ast) (index-ast ast) (element-ast ast)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Macro DEFINE-ARRAY-ASTS.
;;;

(defmacro define-array-asts (simple-reader-class-name
			     simple-reader-class-constructor-name
			     simple-writer-class-name
			     simple-writer-class-constructor-name
			     non-simple-reader-class-name
			     non-simple-reader-class-constructor-name
			     non-simple-writer-class-name
			     non-simple-writer-class-constructor-name)
  `(progn
     (defclass ,simple-reader-class-name (aref-ast)
       ())

     (defun ,simple-reader-class-constructor-name
	 (array-ast index-ast &key origin (policy *policy*))
       (make-instance ',simple-reader-class-name
	 :origin origin :policy policy
	 :array-ast array-ast
	 :index-ast index-ast))

     (defclass ,simple-writer-class-name (aset-ast)
       ())

     (defun ,simple-writer-class-constructor-name
	 (array-ast index-ast element-ast &key origin (policy *policy*))
       (make-instance ',simple-writer-class-name
	 :origin origin :policy policy
	 :array-ast array-ast
	 :index-ast index-ast
	 :element-ast element-ast))

     (defclass ,non-simple-reader-class-name (aref-ast)
       ())

     (defun ,non-simple-reader-class-constructor-name
	 (array-ast index-ast &key origin (policy *policy*))
       (make-instance ',non-simple-reader-class-name
	 :origin origin :policy policy
	 :array-ast array-ast
	 :index-ast index-ast))

     (defclass ,non-simple-writer-class-name (aset-ast)
       ())

     (defun ,non-simple-writer-class-constructor-name
	 (array-ast index-ast element-ast &key origin (policy *policy*))
       (make-instance ',non-simple-writer-class-name
	 :origin origin :policy policy
	 :array-ast array-ast
	 :index-ast index-ast
	 :element-ast element-ast))))

(define-array-asts
  simple-t-aref-ast
  make-simple-t-aref-ast
  simple-t-aset-ast
  make-simple-t-aset-ast
  non-simple-t-aref-ast
  make-non-simple-t-aref-ast
  non-simple-t-aset-ast
  make-non-simple-t-aset-ast)

(define-array-asts
  simple-short-float-aref-ast
  make-simple-short-float-aref-ast
  simple-short-float-aset-ast
  make-simple-short-float-aset-ast
  non-simple-short-float-aref-ast
  make-non-simple-short-float-aref-ast
  non-simple-short-float-aset-ast
  make-non-simple-short-float-aset-ast)

(define-array-asts
  simple-single-float-aref-ast
  make-simple-single-float-aref-ast
  simple-single-float-aset-ast
  make-simple-single-float-aset-ast
  non-simple-single-float-aref-ast
  make-non-simple-single-float-aref-ast
  non-simple-single-float-aset-ast
  make-non-simple-single-float-aset-ast)

(define-array-asts
  simple-double-float-aref-ast
  make-simple-double-float-aref-ast
  simple-double-float-aset-ast
  make-simple-double-float-aset-ast
  non-simple-double-float-aref-ast
  make-non-simple-double-float-aref-ast
  non-simple-double-float-aset-ast
  make-non-simple-double-float-aset-ast)

(define-array-asts
  simple-long-float-aref-ast
  make-simple-long-float-aref-ast
  simple-long-float-aset-ast
  make-simple-long-float-aset-ast
  non-simple-long-float-aref-ast
  make-non-simple-long-float-aref-ast
  non-simple-long-float-aset-ast
  make-non-simple-long-float-aset-ast)

(define-array-asts
  simple-bit-aref-ast
  make-simple-bit-aref-ast
  simple-bit-aset-ast
  make-simple-bit-aset-ast
  non-simple-bit-aref-ast
  make-non-simple-bit-aref-ast
  non-simple-bit-aset-ast
  make-non-simple-bit-aset-ast)

(define-array-asts
  simple-unsigned-byte-8-aref-ast
  make-simple-unsigned-byte-8-aref-ast
  simple-unsigned-byte-8-aset-ast
  make-simple-unsigned-byte-8-aset-ast
  non-simple-unsigned-byte-8-aref-ast
  make-non-simple-unsigned-byte-8-aref-ast
  non-simple-unsigned-byte-8-aset-ast
  make-non-simple-unsigned-byte-8-aset-ast)

(define-array-asts
  simple-unsigned-byte-16-aref-ast
  make-simple-unsigned-byte-16-aref-ast
  simple-unsigned-byte-16-aset-ast
  make-simple-unsigned-byte-16-aset-ast
  non-simple-unsigned-byte-16-aref-ast
  make-non-simple-unsigned-byte-16-aref-ast
  non-simple-unsigned-byte-16-aset-ast
  make-non-simple-unsigned-byte-16-aset-ast)
