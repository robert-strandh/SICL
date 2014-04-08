(in-package #:cleavir-ast)

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
  ((%children :initform '() :initarg :children :accessor children)))

(cleavir-io:define-save-info ast (:children children))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; AST classes for standard common lisp features. 
;;;
;;; There is mostly a different type of AST for each Common Lisp
;;; special operator, but there are some exceptions.  Here are the
;;; Common Lisp special operators: BLOCK, CATCH, EVAL-WHEN, FLET,
;;; FUNCTION, GO, IF, LABELS, LET, LET*, LOAD-TIME-VALUE, LOCALLY,
;;; MACROLET, MULTIPLE-VALUE-CALL, MULTIPLE-VALUE-PROG1, PROGN, PROGV,
;;; QUOTE, RETURN-FROM, SETQ, SYMBOL-MACROLET, TAGBODY, THE, THROW,
;;; UNWIND-PROTECT.
;;;
;;; Some of these only influence the environment and do not need a
;;; representation as ASTs.  These are: LOCALLY, MACROLET, and
;;; SYMBOL-MACROLET.
;;;
;;; The LET special form is compiled into a function call of a LAMBDA
;;; expression.  LET* is compiled as nested LETs.  FLET and LABELS are
;;; like LET except that the symbols the bind are in the function
;;; namespace, but the distinciton between namespeces no longer exists
;;; in the AST.
;;; 
;;; A LAMBDA expression, either inside (FUNCTION (LAMBDA ...)) or when
;;; it is the CAR of a compound form, compiles into a FUNCTION-AST.
;;; The FUNCTION special form does not otherwise require an AST
;;; because the other form of the FUNCTION special form is just a
;;; conversion between namespaces and again, namespaces are no longer
;;; present in the AST.
;;;
;;; Some special operators are implemented as macros which is allowed
;;; by the HyperSpec.  These are CATCH, THROW, UNWIND-PROTECT,
;;; MULTIPLE-VALUE-PROG1, MULTIPLE-VALUE-CALL, and PROGV.
;;;
;;; We also define ASTs that do not correspond to any Common Lisp
;;; special operators, because we simplify later code generation that
;;; way.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class IMMEDIATE-AST. 
;;;
;;; This class represents constants that can be represented as
;;; immediate values in compiled code.  Since the restrictions on
;;; immediate values depend on the backend, this AST is introduced in
;;; a backend-specific transformation that converts certain constants
;;; to immediates.

(defclass immediate-ast (ast)
  ((%value :initarg :value :reader value)))

(defun make-immediate-ast (value)
  (make-instance 'immediate-ast :value value))

(cleavir-io:define-save-info immediate-ast (:value value))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class CONSTANT-AST. 
;;;
;;; This class represents Lisp constants in source code.  
;;;
;;; If the constant that was found was wrapped in QUOTE, then the
;;; QUOTE is not part of the value here, because it was stripped off.
;;;
;;; If the constant that was found was a constant variable, then the
;;; value here represents the value of that constant variable at
;;; compile time.

(defclass constant-ast (ast)
  ((%value :initarg :value :reader value)))

(defun make-constant-ast (value)
  (make-instance 'constant-ast :value value))

(cleavir-io:define-save-info constant-ast (:value value))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class GLOBAL-AST. 
;;; 
;;; A GLOBAL-AST represents a reference to a global FUNCTION, i.e., a
;;; name that is known to be associated with a function in the global
;;; environment.  Such a reference contains the name of the function
;;; and the TYPE of the function as it was declared in the context
;;; where the AST was created.

(defclass global-ast (ast)
  ((%name :initarg :name :reader name)
   (%function-type :initform t :initarg :function-type :accessor function-type)
   (%children :initform '() :allocation :class)))

(defun make-global-ast (name)
  (make-instance 'global-ast :name name))

(cleavir-io:define-save-info global-ast
  (:name name) (:function-type function-type))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class SPECIAL-AST.
;;; 
;;; A SPECIAL-AST represents a reference to a special variable.  Such
;;; a reference contains the name of the variable.

(defclass special-ast (ast)
  ((%name :initarg :name :reader name)
   (%children :initform '() :allocation :class)))

(defun make-special-ast (name)
  (make-instance 'special-ast :name name))

(cleavir-io:define-save-info special-ast (:name name))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class LEXICAL-AST.
;;; 
;;; A LEXICAL-AST represents a reference to a lexical variable.  Such
;;; a reference contains the name of the variable, but it is used only
;;; for debugging perposes and for the purpose of error reporting.

(defclass lexical-ast (ast)
  ((%name :initarg :name :reader name)))

(defun make-lexical-ast (name)
  (make-instance 'lexical-ast :name name))

(cleavir-io:define-save-info lexical-ast (:name name))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class CALL-AST. 
;;;
;;; A CALL-AST represents a function call.  

(defclass call-ast (ast)
  ())

(defun make-call-ast (callee-ast argument-asts)
  (make-instance 'call-ast
    :children (cons callee-ast argument-asts)))

(defmethod callee-ast ((ast call-ast))
  (first (children ast)))

(defmethod argument-asts ((ast call-ast))
  (rest (children ast)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class FUNCTION-AST.
;;;
;;; A function AST represents an explicit lambda expression, but also
;;; implicit lambda expressions such as the ones found in FLET and
;;; LABELS.
;;;
;;; The lambda list is not a normal lambda list.  It has the following
;;; form: 
;;; ([r1 .. rl [&optional o1 ..om] [&key k1 .. kn &allow-other-keys]]]) 
;;;
;;; where: 
;;;
;;;   - Each ri is a LEXICAL-AST. 
;;;
;;;   - Each oi is a list of two LEXICAL-ASTs.  The second of the 
;;;     two conceptually contains a Boolean value indicating whether
;;;     the first one contains a value supplied by the caller.  
;;;
;;;   - Each ki is a list of a symbol and two LEXICAL-ASTs.  The
;;;     symbol is the keyword-name that a caller must supply in order
;;;     to pass the corresponding argument.  The second of the two
;;;     LEXICAL-ASTs conceptually contains a Boolean value indicating
;;;     whether the first LEXICAL-AST contains a value supplied by the
;;;     caller.
;;;
;;; The LEXICAL-ASTs in the lambda list are potentially unrelated to
;;; the variables that were given in the original lambda expression,
;;; and they are LEXICAL-ASTs independently of whether the
;;; corresponding variable that was given in the original lambda
;;; expression is a lexical variable or a special variable.
;;;
;;; The body of the FUNCTION-AST must contain code that tests the
;;; second of the two LEXICAL-ASTs and initializes variables if
;;; needed.  The if the second LEXICAL-AST in any oi contains FALSE,
;;; then the code in the body is not allowed to test the second
;;; LEXICAL-ASTs of any of the ki because they may not be set
;;; correctly (conceptually, they all have the value FALSE then).

(defclass function-ast (ast)
  ((%lambda-list :initarg :lambda-list :reader lambda-list)))

(defun make-function-ast (body-ast lambda-list)
  (make-instance 'function-ast
    :children (list body-ast)
    :lambda-list lambda-list))

(defmethod body-ast ((ast function-ast))
  (first (children ast)))

(cleavir-io:define-save-info function-ast (:lambda-list lambda-list))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class PROGN-AST.

(defclass progn-ast (ast)
  ())

(defun make-progn-ast (form-asts)
  (make-instance 'progn-ast
    :children form-asts))

(defmethod form-asts ((ast progn-ast))
  (children ast))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class BLOCK-AST.

(defclass block-ast (ast)
  ())

(defun make-block-ast (body-ast)
  (make-instance 'block-ast
    :children (list body-ast)))
  
(defmethod body-ast ((ast block-ast))
  (first (children ast)))

(defmethod (setf body-ast) (new-body (ast block-ast))
  (setf (first (children ast)) new-body))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class RETURN-FROM-AST.

(defclass return-from-ast (ast)
  ())

(defun make-return-from-ast (block-ast form-ast)
  (make-instance 'return-from-ast
    :children (list block-ast form-ast)))
  
(defmethod block-ast ((ast return-from-ast))
  (first (children ast)))

(defmethod form-ast ((ast return-from-ast))
  (second (children ast)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class SETQ-AST.

(defclass setq-ast (ast)
  ())

(defun make-setq-ast (lhs-ast value-ast)
  (make-instance 'setq-ast
    :children (list lhs-ast value-ast)))

(defmethod lhs-ast ((ast setq-ast))
  (first (children ast)))

(defmethod value-ast ((ast setq-ast))
  (second (children ast)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class TAG-AST.

(defclass tag-ast (ast)
  ((%name :initarg :name :reader name)))

(defun make-tag-ast (name)
  (make-instance 'tag-ast
    :name name))

(cleavir-io:define-save-info tag-ast (:name name))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class TAGBODY-AST.

(defclass tagbody-ast (ast)
  ())

(defun make-tagbody-ast (items)
  (make-instance 'tagbody-ast
    :children items))

(defmethod items ((ast tagbody-ast))
  (children ast))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class GO-AST.

(defclass go-ast (ast)
  ())

(defun make-go-ast (tag-ast)
  (make-instance 'go-ast
    :children  (list tag-ast)))

(defmethod tag-ast ((ast go-ast))
  (first (children ast)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class THE-AST.

(defclass the-ast (ast)
  ((%value-type :initarg :value-type :reader value-type)))

(defun make-the-ast (form-ast &rest types)
  (make-instance 'the-ast
    :children (list* form-ast types)
    :value-type (mapcar #'value types)))

(defmethod form-ast ((ast the-ast))
  (first (children ast)))

(cleavir-io:define-save-info the-ast (:value-type value-type))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class TYPEQ-AST.
;;;
;;; This AST can be thought of as a translation to an AST of a
;;; hypothetical special form (TYPEQ <form> <type-specifier>) which is
;;; like the function TYPEP, except that the type specifier is not
;;; evaluated.  
;;;
;;; Like the hypothetical special form, this AST has two children.
;;; The first child is an AST corresponding to <form> and the second
;;; child is a CONSTANT-AST containing <type-specifier> as a constant.
;;; In addition, this AST has a slot that contains <type-specifier>.
;;; The reason for this duplication of information is that the child
;;; containing the CONSTANT-AST might later be replaced by a reference
;;; to some location where the constant can be found, but we still
;;; need the type information for the purpose of type inference. 
;;;
;;; Like a call to the function TYPEP, the value of this AST is a
;;; generalized Boolean that is TRUE if and only if <form> is of type
;;; <type-specifier>.
;;;
;;; Implementations that interpret the special form (THE <type>
;;; <form>) as an error if <form> is not of type <type> might generate
;;; a TYPEQ-AST contained in an IF-AST instead of a THE-AST, and to
;;; have the ELSE branch of the IF-AST call ERROR.
;;;
;;; The TYPEQ-AST can also be used as a target for the standard macro
;;; CHECK-TYPE.  An implementation might for instance expand
;;; CHECK-TYPE to a form containing an implementation-specific special
;;; operator; e.g, (UNLESS (TYPEQ <form> <type-spec>) (CERROR ...))
;;; and then translate the implementation-specific special operator
;;; TYPEQ into a TYPEQ-AST.
;;;
;;; The TYPEQ-AST generates instructions that are used in the static
;;; type inference phase.  If static type inference can determine the
;;; value of the TYPEQ-AST, then no runtime test is required.  If not,
;;; then a call to TYPEP is generated instead. 

(defclass typeq-ast (ast)
  ((%type-specifier :initarg :type-specifier :reader type-specifier)))

(defun make-typeq-ast (form-ast type-specifier-ast)
  (make-instance 'typeq-ast
    :children (list form-ast type-specifier-ast)))

(defmethod form-ast ((ast typeq-ast))
  (first (children ast)))

(defmethod type-specifier-ast ((ast typeq-ast))
  (second (children ast)))

(defmethod initialize-instance :after ((ast typeq-ast) &key &allow-other-keys)
  (reinitialize-instance
   ast
   :type-specifier (value (type-specifier-ast ast))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class LOAD-TIME-VALUE-AST.
;;;
;;; This AST corresponds directly to the LOAD-TIME-VALUE special
;;; operator.  It has a single child and it produces a single value.
;;;
;;; The optional argument READ-ONLY-P is not a child of the AST
;;; because it can only be a Boolean which is not evaluated, so we
;;; know at AST creation time whether it is true or false. 

(defclass load-time-value-ast (ast)
  ((%read-only-p :initarg :read-only-p :reader read-only-p)))

(defun make-load-time-value-ast (form-ast &optional read-only-p)
  (make-instance 'load-time-value-ast
    :children  (list form-ast)
    :read-only-p read-only-p))

(defmethod form-ast ((ast load-time-value-ast))
  (first (children ast)))

;;; Even though READ-ONLY-P is not a child of the AST, it needs to be
;;; saved when the AST is saved. 
(cleavir-io:define-save-info load-time-value-ast (:read-only-p read-only-p))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class IF-AST.
;;;
;;; This AST corresponds directly to the IF special operator.  It
;;; produces as many values as the AST in the THEN-AST or ELSE-AST
;;; produces, according to the value of the TEST AST.

(defclass if-ast (ast)
  ())

(defun make-if-ast (test-ast then-ast else-ast)
  (make-instance 'if-ast
    :children (list test-ast then-ast else-ast)))

(defmethod test-ast ((ast if-ast))
  (first (children ast)))

(defmethod then-ast ((ast if-ast))
  (second (children ast)))

(defmethod else-ast ((ast if-ast))
  (third (children ast)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class CAR-AST.
;;;
;;; This AST can be used to implement the function CAR.  However, it
;;; does not correspond exactly to the function CAR, because the value
;;; of the single child must be a CONS cell. 

(defclass car-ast (ast)
  ())

(defun make-car-ast (cons-ast)
  (make-instance 'car-ast
    :children (list cons-ast)))

(defmethod cons-ast ((ast car-ast))
  (first (children ast)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class CDR-AST.
;;;
;;; This AST can be used to implement the function CDR.  However, it
;;; does not correspond exactly to the function CDR, because the value
;;; of the single child must be a CONS cell. 

(defclass cdr-ast (ast)
  ())

(defun make-cdr-ast (cons-ast)
  (make-instance 'cdr-ast
    :children (list cons-ast)))

(defmethod cons-ast ((ast cdr-ast))
  (first (children ast)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class RPLACA-AST.
;;;
;;; This AST can be used to implement the function RPLACA and the
;;; function (SETF CAR) in implementations where it is a function.
;;; This AST differs from the function RPLACA in that it does not
;;; generate any value.  

(defclass rplaca-ast (ast)
  ())

(defun make-rplaca-ast (cons-ast object-ast)
  (make-instance 'rplaca-ast
    :children (list cons-ast object-ast)))

(defmethod cons-ast ((ast rplaca-ast))
  (first (children ast)))

(defmethod object-ast ((ast rplaca-ast))
  (second (children ast)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class RPLACD-AST.
;;;
;;; This AST can be used to implement the function RPLACD and the
;;; function (SETF CDR) in implementations where it is a function.
;;; This AST differs from the function RPLACD in that it does not
;;; generate any value.  

(defclass rplacd-ast (ast)
  ())

(defun make-rplacd-ast (cons-ast object-ast)
  (make-instance 'rplacd-ast
    :children (list cons-ast object-ast)))

(defmethod cons-ast ((ast rplacd-ast))
  (first (children ast)))

(defmethod object-ast ((ast rplacd-ast))
  (second (children ast)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class FIXNUM-ADD-AST.
;;;
;;; This AST can be used to implement a binary addition function.  It
;;; requires both its arguments to be of type FIXNUM.  It generates
;;; two values that we may call RESULT (a FIXNUM) and OVERFLOWP (a
;;; Boolean).  OVERFLOWP indicates whether the operation resulted in
;;; an overflow.  When it is false, then there was no overflow, and
;;; RESULT is the sum of the two arguments.  When OVERFLOWP is true,
;;; the operation resulted in an overflow.  Then, if RESULT is
;;; negative, then a BIGNUM with the value 2^n + RESULT should be
;;; created, where n is the number of bits in a word.  If RESULT is
;;; non-negative, then a BIGNUM with the value RESULT - 2^n should be
;;; created.

(defclass fixnum-add-ast (ast)
  ())

(defun make-fixnum-add-ast (arg1-ast arg2-ast)
  (make-instance 'fixnum-add-ast
    :children (list arg1-ast arg2-ast)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class FIXNUM-SUB-AST.
;;;
;;; This AST can be used to implement a binary subtraction function.
;;; It requires both its arguments to be of type FIXNUM.  It generates
;;; two values that we may call RESULT (a FIXNUM) and OVERFLOWP (a
;;; Boolean).  OVERFLOWP indicates whether the operation resulted in
;;; an overflow.  When it is false, then there was no overflow, and
;;; RESULT is the sum of the two arguments.  When OVERFLOWP is true,
;;; the operation resulted in an overflow.  Then, if RESULT is
;;; negative, then a BIGNUM with the value 2^n + RESULT should be
;;; created, where n is the number of bits in a word.  If RESULT is
;;; non-negative, then a BIGNUM with the value RESULT - 2^n should be
;;; created.

(defclass fixnum-sub-ast (ast)
  ())

(defun make-fixnum-sub-ast (arg1-ast arg2-ast)
  (make-instance 'fixnum-sub-ast
    :children (list arg1-ast arg2-ast)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class SI-READ-AST.
;;;
;;; This AST can be used to read a slot from a standard instance.  It
;;; has two children, an AST that must have a standard instance as its
;;; value, and an AST that must have a fixnum as its value and that
;;; indicates a slot number (starting from 0).  This AST generates a
;;; single value, namely the contents of the slot with the number given.

(defclass si-read-ast (ast)
  ())

(defun make-si-read-ast (si-ast slot-number-ast)
  (make-instance 'si-read-ast
    :children (list si-ast slot-number-ast)))

(defmethod si-ast ((ast si-read-ast))
  (first (children ast)))

(defmethod slot-number-ast ((ast si-read-ast))
  (second (children ast)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class SI-WRITE-AST.
;;;
;;; This AST can be used to write a slot in a standard instance.  It
;;; has three children, an AST that must have a standard instance as
;;; its value, an AST that must have a fixnum as its value and that
;;; indicates a slot number (starting from 0), and an AST that
;;; generates the new value to store in the slot.  This AST generates
;;; no values. 

(defclass si-write-ast (ast)
  ())

(defun make-si-write-ast (si-ast slot-number-ast value-ast)
  (make-instance 'si-write-ast
    :children (list si-ast slot-number-ast value-ast)))

(defmethod si-ast ((ast si-write-ast))
  (first (children ast)))

(defmethod slot-number-ast ((ast si-write-ast))
  (second (children ast)))

(defmethod value-ast ((ast si-write-ast))
  (third (children ast)))
