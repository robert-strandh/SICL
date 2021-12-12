(cl:in-package #:cleavir-ast)

;;;; We define the abstract syntax trees (ASTs) that represent not
;;;; only Common Lisp code, but also the low-level operations that we
;;;; use to implement the Common Lisp operators that can not be
;;;; portably implemented using other Common Lisp operators.
;;;; 
;;;; The AST is a very close representation of the source code, except
;;;; that the environment is no longer present, so that there are no
;;;; longer any different namespaces for functions and variables.  And
;;;; of course, operations such as MACROLET are not present because
;;;; they only alter the environment.  
;;;;
;;;; The AST form is the preferred representation for some operations;
;;;; in particular for PROCEDURE INTEGRATION (sometimes called
;;;; INLINING).

(defgeneric children (ast))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class AST.  The base class for all AST classes.

(defclass ast ()
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Mixin classes.

;;; This class is used as a superclass for ASTs that produce Boolean
;;; results, so are mainly used as the TEST-AST of an IF-AST.
(defclass boolean-ast-mixin () ())

;;; This class is used as a superclass for ASTs that produce no value
;;; and that must be compiled in a context where no value is required.
(defclass no-value-ast-mixin () ())

;;; This class is used as a superclass for ASTs that produce a single
;;; value that is typically not just a Boolean value.
(defclass one-value-ast-mixin () ())

;;; This class is used as a superclass for ASTs that produce two
;;; values, and that must be compiled in a context where two values
;;; are required.
(defclass two-values-ast-mixin () ())

;;; This class is used as a superclass for ASTs that have no side
;;; effect.
(defclass side-effect-free-ast-mixin () ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Predicate to test whether an AST is side-effect free.
;;;
;;; For instances of SIDE-EFFECT-FREE-AST-MIXIN, this predicate always
;;; returns true.  For others, it has a default method that returns
;;; false.  Implementations may add a method on some ASTs such as
;;; CALL-AST that return true only if a particular call is side-effect
;;; free.

(defgeneric side-effect-free-p (ast))

(defmethod side-effect-free-p (ast)
  (declare (ignore ast))
  nil)

(defmethod side-effect-free-p ((ast side-effect-free-ast-mixin))
  (declare (ignorable ast))
  t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; This function is a trampoline to MAKE-INSTANCE.  The purpose is to
;;; make it easier to trace AST creation or to set breakpoints.

(defgeneric make-ast (class &rest initargs))

;;; We want to make sure that MAKE-INSTANCE is not used directly when
;;; an instance of an AST class is created.

;;; The way we do it is that we define a special variable *MAKING-AST*
;;; with a default value of NIL, i.e. false.
(defparameter *making-ast* nil)

;;; Then we define a :BEFORE method on INITIALIZE-INSTANCE that
;;; signals an error when *MAKING-AST* is false.  Thus, calling
;;; MAKE-INSTANCE to create an instance of an AST class will result in
;;; an error being signaled.
(defmethod initialize-instance :before ((object ast) &rest initargs)
  (declare (ignore object initargs))
  (unless *making-ast*
    (error "To create an AST instance, use MAKE-AST.")))

;;; We make sure that this primary method on MAKE-AST specialized to
;;; the class CLASS is always called when an AST instance is created.
;;; That way, we make it easier to define auxiliary methods EQL
;;; specialized to a particular AST class.
(defmethod make-ast ((class class) &rest initargs)
  (let ((*making-ast* t))
    (apply #'make-instance class initargs)))

;;; To make sure the primary method on MAKE-AST specialized to the
;;; class CLASS is always called when an AST instance is created, we
;;; define another primary method, specialized to SYMBOL that
;;; calls MAKE-AST recursively with the class metaobject.
(defmethod make-ast ((class-or-name cl:symbol) &rest initargs)
  (let ((class (find-class class-or-name)))
    (apply #'make-ast class initargs)))
