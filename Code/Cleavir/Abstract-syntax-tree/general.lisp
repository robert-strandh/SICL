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

(defclass ast () ())

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
;;; value that is not typically not just a Boolean value.
(defclass one-value-ast-mixin () ())

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

(cleavir-io:define-save-info immediate-ast
  (:value value))

(defmethod children ((ast immediate-ast))
  (declare (ignorable ast))
  '())

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

(cleavir-io:define-save-info constant-ast
  (:value value))

(defmethod children ((ast constant-ast))
  (declare (ignorable ast))
  '())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class GLOBAL-AST. 
;;; 
;;; A GLOBAL-AST represents a reference to a global FUNCTION, i.e., a
;;; name that is known to be associated with a function in the global
;;; environment.  Such a reference contains the name of the function
;;; and the TYPE of the function as it was declared in the context
;;; where the AST was created.

(defclass global-ast (ast one-value-ast-mixin)
  ((%name :initarg :name :reader name)
   (%function-type :initform t :initarg :function-type :accessor function-type)))

(defun make-global-ast (name)
  (make-instance 'global-ast :name name))

(cleavir-io:define-save-info global-ast
  (:name name)
  (:function-type function-type))

(defmethod children ((ast global-ast))
  (declare (ignorable ast))
  '())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class SPECIAL-AST.
;;; 
;;; A SPECIAL-AST represents a reference to a special variable.  Such
;;; a reference contains the name of the variable.

(defclass special-ast (ast one-value-ast-mixin)
  ((%name :initarg :name :reader name)))

(defun make-special-ast (name)
  (make-instance 'special-ast :name name))

(cleavir-io:define-save-info special-ast
  (:name name))

(defmethod children ((ast special-ast))
  (declare (ignorable ast))
  '())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class LEXICAL-AST.
;;; 
;;; A LEXICAL-AST represents a reference to a lexical variable.  Such
;;; a reference contains the name of the variable, but it is used only
;;; for debugging purposes and for the purpose of error reporting.

(defclass lexical-ast (ast one-value-ast-mixin)
  ((%name :initarg :name :reader name)))

(defun make-lexical-ast (name)
  (make-instance 'lexical-ast :name name))

(cleavir-io:define-save-info lexical-ast
  (:name name))

(defmethod children ((ast lexical-ast))
  (declare (ignorable ast))
  '())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class CALL-AST. 
;;;
;;; A CALL-AST represents a function call.  

(defclass call-ast (ast)
  ((%callee-ast :initarg :callee-ast :reader callee-ast)
   (%argument-asts :initarg :argument-asts :reader argument-asts)))

(defun make-call-ast (callee-ast argument-asts)
  (make-instance 'call-ast
    :callee-ast callee-ast
    :argument-asts argument-asts))

(cleavir-io:define-save-info call-ast
  (:callee-ast callee-ast)
  (:argument-asts argument-asts))

(defmethod children ((ast special-ast))
  (cons (callee-ast ast) (argument-asts ast)))

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
  ((%lambda-list :initarg :lambda-list :reader lambda-list)
   (%body-ast :initarg :body-ast :reader body-ast)))

(defun make-function-ast (body-ast lambda-list)
  (make-instance 'function-ast
    :body-ast body-ast
    :lambda-list lambda-list))

(cleavir-io:define-save-info function-ast
  (:lambda-list lambda-list)
  (:body-ast body-ast))

(defmethod children ((ast function-ast))
  (list (body-ast ast)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class PROGN-AST.

(defclass progn-ast (ast)
  ((%form-asts :initarg :form-asts :reader form-asts)))

(defun make-progn-ast (form-asts)
  (make-instance 'progn-ast
    :form-asts form-asts))

(cleavir-io:define-save-info function-ast
  (:form-asts form-asts))

(defmethod children ((ast progn-ast))
  (form-asts ast))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class BLOCK-AST.

(defclass block-ast (ast)
  ((%body-ast :initarg :body-ast :accessor body-ast)))

(defun make-block-ast (body-ast)
  (make-instance 'block-ast
    :body-ast body-ast))
  
(cleavir-io:define-save-info block-ast
  (:body-ast body-ast))

(defmethod children ((ast block-ast))
  (list (body-ast ast)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class RETURN-FROM-AST.

(defclass return-from-ast (ast)
  ((%block-ast :initarg :block-ast :reader block-ast)
   (%form-ast :initarg :form-ast :reader form-ast)))

(defun make-return-from-ast (block-ast form-ast)
  (make-instance 'return-from-ast
    :block-ast block-ast
    :form-ast form-ast))
  
(cleavir-io:define-save-info return-from-ast
  (:block-ast block-ast)
  (:form-ast form-ast))

(defmethod children ((ast return-from-ast))
  (list (block-ast ast) (form-ast ast)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class SETQ-AST.

(defclass setq-ast (ast)
  ((%lhs-ast :initarg :lhs-ast :reader lhs-ast)
   (%value-ast :initarg :value-ast :reader value-ast)))

(defun make-setq-ast (lhs-ast value-ast)
  (make-instance 'setq-ast
    :lhs-ast lhs-ast
    :value-ast value-ast))

(cleavir-io:define-save-info setq-ast
  (:lhs-ast lhs-ast)
  (:value-ast value-ast))

(defmethod children ((ast setq-ast))
  (list (lhs-ast ast) (value-ast ast)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class TAG-AST.

(defclass tag-ast (ast)
  ((%name :initarg :name :reader name)))

(defun make-tag-ast (name)
  (make-instance 'tag-ast
    :name name))

(cleavir-io:define-save-info tag-ast
  (:name name))

(defmethod children ((ast tag-ast))
  (declare (ignorable ast))
  '())

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
  ((%tag-ast :initarg :tag-ast :reader tag-ast)))

(defun make-go-ast (tag-ast)
  (make-instance 'go-ast
    :tag-ast tag-ast))

(cleavir-io:define-save-info go-ast
  (:tag-ast tag-ast))

(defmethod children ((ast go-ast))
  (list (tag-ast ast)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class THE-AST.

(defclass the-ast (ast)
  ((%form-ast :initarg :form-ast :reader form-ast)
   (%type-asts :initarg :type-asts :reader type-asts)
   (%value-type :initarg :value-type :reader value-type)))

(defun make-the-ast (form-ast &rest type-asts)
  (make-instance 'the-ast
    :form-ast form-ast
    :type-asts type-asts
    :value-type (mapcar #'value type-asts)))

(cleavir-io:define-save-info the-ast
  (:form-ast form-ast)
  (:type-asts type-asts)
  (:value-type value-type))

(defmethod children ((ast the-ast))
  (list* (form-ast ast) (type-asts ast)))

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
  ((%type-specifier :initarg :type-specifier :reader type-specifier)
   (%type-specifier-ast :initarg :type-specifier-ast :reader type-specifier-ast)
   (%form-ast :initarg :form-ast :reader form-ast)))

(defun make-typeq-ast (form-ast type-specifier-ast)
  (make-instance 'typeq-ast
    :form-ast form-ast
    :type-specifier-ast type-specifier-ast))

(defmethod initialize-instance :after ((ast typeq-ast) &key &allow-other-keys)
  (reinitialize-instance
   ast
   :type-specifier (value (type-specifier-ast ast))))

(cleavir-io:define-save-info typeq-ast
  (:type-specifier-ast type-specifier-ast)
  (:form-ast form-ast))

(defmethod children ((ast typeq-ast))
  (list (form-ast ast) (type-specifier-ast ast)))

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
  ((%form-ast :initarg :form-ast :reader form-ast)
   (%read-only-p :initarg :read-only-p :reader read-only-p)))

(defun make-load-time-value-ast (form-ast &optional read-only-p)
  (make-instance 'load-time-value-ast
    :form-ast form-ast
    :read-only-p read-only-p))

;;; Even though READ-ONLY-P is not a child of the AST, it needs to be
;;; saved when the AST is saved. 
(cleavir-io:define-save-info load-time-value-ast
  (:read-only-p read-only-p))

(defmethod children ((ast load-time-value-ast))
  (list (form-ast ast)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class IF-AST.
;;;
;;; This AST corresponds directly to the IF special operator.  It
;;; produces as many values as the AST in the THEN-AST or ELSE-AST
;;; produces, according to the value of the TEST AST.

(defclass if-ast (ast)
  ((%test-ast :initarg :test-ast :reader test-ast)
   (%then-ast :initarg :then-ast :reader then-ast)
   (%else-ast :initarg :else-ast :reader else-ast)))

(defun make-if-ast (test-ast then-ast else-ast)
  (make-instance 'if-ast
    :test-ast test-ast
    :then-ast then-ast
    :else-ast else-ast))

(cleavir-io:define-save-info if-ast
  (:test-ast test-ast)
  (:then-ast then-ast)
  (:else-ast else-ast))

(defmethod children ((ast if-ast))
  (list (test-ast ast) (then-ast ast) (else-ast ast)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class EQ-AST.
;;;
;;; This AST can be used to to test whether two objects are identical.
;;; It has two children.  This AST can only appear in the TEST
;;; position of an IF-AST.

(defclass eq-ast (ast)
  ((%arg1-ast :initarg :arg1-ast :reader arg1-ast)
   (%arg2-ast :initarg :arg2-ast :reader arg2-ast)))

(defun make-eq-ast (arg1-ast arg2-ast)
  (make-instance 'eq-ast
    :arg1-ast arg1-ast
    :arg2-ast arg2-ast))

(cleavir-io:define-save-info eq-ast
  (:arg1-ast arg1-ast)
  (:arg2-ast arg2-ast))

(defmethod children ((ast eq-ast))
  (list (arg1-ast ast) (arg2-ast ast)))
