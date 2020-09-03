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
;;; Variable *POLICY*. Default for :policy initarg.
;;; This is useful because every AST has a policy, but they're
;;; shared very heavily and generated all over the place.

(defvar *policy*)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class AST.  The base class for all AST classes.
;;;
;;; ORIGIN is a client-supplied object that is not interpreted by
;;; Cleavir.
;;; POLICY is the compilation policy in force for the AST.
;;; DYNAMIC-ENVIRONMENT is a lexical-ast representing the
;;; dynamic environment in force.

(defclass ast ()
  ((%origin :initform nil :initarg :origin :accessor origin)
   (%policy :initform *policy* :initarg :policy :accessor policy)))

;;; Policies must be saved
(cleavir-io:define-save-info ast
  (:origin origin)
  (:policy policy))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Mixin classes.

;;; This class is used as a superclass for ASTs that produce Boolean
;;; results, so are mainly used as the TEST-AST of an IF-AST.
(defclass boolean-ast-mixin () ())

;;; This class is used as a superclass for ASTs that produce results
;;; for BRANCH-AST.
(defclass multiway-ast-mixin () ())

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
;;; FLET and LABELS are like LET except that the symbols the bind are
;;; in the function namespace, but the distinciton between namespeces
;;; no longer exists in the AST.
;;; 
;;; A LAMBDA expression, either inside (FUNCTION (LAMBDA ...)) or when
;;; it is the CAR of a compound form, compiles into a FUNCTION-AST.
;;; The FUNCTION special form does not otherwise require an AST
;;; because the other form of the FUNCTION special form is just a
;;; conversion between namespaces and again, namespaces are no longer
;;; present in the AST.
;;;
;;; We also define ASTs that do not correspond to any Common Lisp
;;; special operators, because we simplify later code generation that
;;; way.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class IMMEDIATE-AST.
;;;
;;; This class represents constants that have a representation that
;;; allows them to be immediate values in the resulting machine code.
;;; Obviously, whether that is possible depends on the implementation.
;;;
;;; The value of the constant is represented as a possibly-negative
;;; integer that is the machine-code representation of the constant.

(defclass immediate-ast (one-value-ast-mixin side-effect-free-ast-mixin ast)
  ((%value :initarg :value :reader value)))

(defun make-immediate-ast (value &key origin (policy *policy*))
  (make-instance 'immediate-ast
    :origin origin :policy policy
    :value value))

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

(defclass constant-ast (one-value-ast-mixin side-effect-free-ast-mixin ast)
  ((%value :initarg :value :reader value)))

(defun make-constant-ast (value &key origin (policy *policy*))
  (make-instance 'constant-ast
    :origin origin :policy policy
    :value value))

(cleavir-io:define-save-info constant-ast
  (:value value))

(defmethod children ((ast constant-ast))
  (declare (ignorable ast))
  '())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class LEXICAL-AST.
;;; 
;;; A LEXICAL-AST represents a reference to a lexical variable.  Such
;;; a reference contains the name of the variable, but it is used only
;;; for debugging purposes and for the purpose of error reporting.

(defclass lexical-ast (one-value-ast-mixin side-effect-free-ast-mixin ast)
  ((%name :initarg :name :reader name)))

(defun make-lexical-ast (name &key origin (policy *policy*))
  (make-instance 'lexical-ast
    :origin origin :policy policy
    :name name))

(cleavir-io:define-save-info lexical-ast
  (:name name))

(defmethod children ((ast lexical-ast))
  (declare (ignorable ast))
  '())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class SYMBOL-VALUE-AST.
;;;
;;; This AST is generated from a reference to a special variable.

(defclass symbol-value-ast (one-value-ast-mixin side-effect-free-ast-mixin ast)
  ((%symbol-ast :initarg :symbol-ast :reader symbol-ast)))

(defun make-symbol-value-ast (symbol-ast &key origin (policy *policy*))
  (make-instance 'symbol-value-ast
    :origin origin :policy policy
    :symbol-ast symbol-ast))

(cleavir-io:define-save-info symbol-value-ast
  (:symbol-ast symbol-ast))

(defmethod children ((ast symbol-value-ast))
  (list (symbol-ast ast)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class CONSTANT-SYMBOL-VALUE-AST.

(defclass constant-symbol-value-ast (one-value-ast-mixin side-effect-free-ast-mixin ast)
  ((%name :initarg :name :reader name)))

(defun make-constant-symbol-value-ast (name &key origin (policy *policy*))
  (make-instance 'constant-symbol-value-ast
    :origin origin :policy policy
    :name name))

(cleavir-io:define-save-info constant-symbol-value-ast
  (:name name))

(defmethod children ((ast constant-symbol-value-ast))
  '())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class SET-SYMBOL-VALUE-AST.
;;;
;;; This AST is generated from an assignment to a special variable.

(defclass set-symbol-value-ast (no-value-ast-mixin ast)
  ((%symbol-ast :initarg :symbol-ast :reader symbol-ast)
   (%value-ast :initarg :value-ast :reader value-ast)))

(defun make-set-symbol-value-ast (symbol-ast value-ast &key origin (policy *policy*))
  (make-instance 'set-symbol-value-ast
    :origin origin :policy policy
    :symbol-ast symbol-ast
    :value-ast value-ast))

(cleavir-io:define-save-info set-symbol-value-ast
  (:symbol-ast symbol-ast)
  (:value-ast value-ast))

(defmethod children ((ast set-symbol-value-ast))
  (list (symbol-ast ast) (value-ast ast)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class SET-CONSTANT-SYMBOL-VALUE-AST.

(defclass set-constant-symbol-value-ast (no-value-ast-mixin ast)
  ((%name :initarg :name :reader name)
   (%value-ast :initarg :value-ast :reader value-ast)))

(defun make-set-constant-symbol-value-ast (name value-ast &key origin (policy *policy*))
  (make-instance 'set-constant-symbol-value-ast
    :origin origin :policy policy
    :name name
    :value-ast value-ast))

(cleavir-io:define-save-info set-constant-symbol-value-ast
  (:name name)
  (:value-ast value-ast))

(defmethod children ((ast set-constant-symbol-value-ast))
  (list (value-ast ast)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class FDEFINITION-AST.
;;;
;;; This AST is generated from a reference to a global function.

(defclass fdefinition-ast (one-value-ast-mixin side-effect-free-ast-mixin ast)
  (;; This slot contains an AST that produces the function name.
   (%name-ast :initarg :name-ast :reader name-ast)))

(defun make-fdefinition-ast (name-ast &key origin (policy *policy*))
  (make-instance 'fdefinition-ast
    :origin origin :policy policy
    :name-ast name-ast))

(cleavir-io:define-save-info fdefinition-ast
  (:name-ast name-ast))

(defmethod children ((ast fdefinition-ast))
  (list (name-ast ast)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class CONSTANT-FDEFINITION-AST.
;;;
;;; This AST is not yet generated from a reference to a global
;;; function, but migration to this AST is suggested.

(defclass constant-fdefinition-ast
    (one-value-ast-mixin side-effect-free-ast-mixin ast)
  (;; This slot contains the name of the function
   (%name :initarg :name :reader name)))

(defun make-constant-fdefinition-ast (name &key origin (policy *policy*))
  (make-instance 'constant-fdefinition-ast
    :origin origin :policy policy
    :name name))

(cleavir-io:define-save-info constant-fdefinition-ast
  (:name name))

(defmethod children ((ast constant-fdefinition-ast))
  '())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class CALL-AST. 
;;;
;;; A CALL-AST represents a function call.  

(defclass call-ast (ast)
  ((%callee-ast :initarg :callee-ast :reader callee-ast)
   (%argument-asts :initarg :argument-asts :reader argument-asts)
   (%inline :initarg :inline :initform nil :reader inline-declaration)
   (%attributes :initarg :attributes :reader attributes
                :initform (cleavir-attributes:default-attributes))))

(defun make-call-ast (callee-ast argument-asts
                      &key origin inline (policy *policy*)
                        (attributes
                         (cleavir-attributes:default-attributes)))
  (make-instance 'call-ast
    :origin origin :policy policy
    :callee-ast callee-ast
    :inline inline
    :argument-asts argument-asts
    :attributes attributes))

(cleavir-io:define-save-info call-ast
  (:callee-ast callee-ast)
  (:argument-asts argument-asts)
  (:inline inline-declaration)
  (:attributes attributes))

(defmethod children ((ast call-ast))
  (list* (callee-ast ast) (argument-asts ast)))

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
;;; ([r1 .. rl [&optional o1 ..om] [&rest r] [&key k1 .. kn &allow-other-keys]]])
;;;
;;; where: 
;;;
;;;   - Each ri is a LEXICAL-AST. 
;;;
;;;   - r is a LEXICAL-AST.
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

(defclass function-ast (one-value-ast-mixin side-effect-free-ast-mixin ast)
  ((%lambda-list :initarg :lambda-list :reader lambda-list)
   (%body-ast :initarg :body-ast :reader body-ast)
   ;; An alist from lexical ASTs to lists of pertinent declaration specifiers.
   ;; Since SPECIAL is otherwise handled, these are for optimization use only
   ;; and may be discarded at will.
   (%bound-declarations :initarg :bound-declarations :initform nil
                        :reader bound-declarations)
   ;; These three are intended for debugging/introspection.
   (%name :initarg :name :initform nil :accessor name)
   (%docstring :initarg :docstring :initform nil :reader docstring)
   (%original-lambda-list :initarg :original-lambda-list :initform nil
                          :reader original-lambda-list)
   (%attributes :initarg :attributes :reader attributes
                :initform (cleavir-attributes:default-attributes))))

(defun make-function-ast (body-ast lambda-list
                          &key name docstring original-lambda-list
                            bound-declarations
                            origin (policy *policy*)
                            (attributes
                             (cleavir-attributes:default-attributes)))
  (make-instance 'function-ast
    :origin origin :policy policy
    :name name :docstring docstring
    :original-lambda-list original-lambda-list
    :bound-declarations bound-declarations
    :body-ast body-ast
    :lambda-list lambda-list
    :attributes attributes))

(cleavir-io:define-save-info function-ast
  (:lambda-list lambda-list)
  (:body-ast body-ast)
  (:name name) (:docstring docstring)
  (:bound-declarations bound-declarations)
  (:original-lambda-list original-lambda-list)
  (:attributes attributes))

(defmethod children ((ast function-ast))
  (list* (body-ast ast)
         (loop for entry in (lambda-list ast)
               append (cond ((symbolp entry)
                             '())
                            ((consp entry)
                             (if (= (length entry) 2)
                                 entry
                                 (cdr entry)))
                            (t
                             (list entry))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class TOP-LEVEL-FUNCTION-AST.
;;;
;;; This AST is a subclass of FUNCTION-AST.  It is used when an AST is
;;; transformed by hoisting all the LOAD-TIME-VALUE-ASTs in the tree
;;; by turning them into LEXIAL-ASTs that are also required parameters
;;; of the TOP-LEVEL-FUNCTION-AST.
;;;
;;; This AST class supplies a slot that contains a list of the forms
;;; that were contained in the LOAD-TIME-VALUE-ASTs.  In order to
;;; evaluate the original AST, the transformed AST must be called with
;;; the values of those forms as arguments.

(defclass top-level-function-ast (function-ast)
  ((%forms :initarg :forms :reader forms)))

(defun make-top-level-function-ast (body-ast lambda-list forms
                                    &key origin (policy *policy*))
  (make-instance 'top-level-function-ast
    :origin origin :policy policy
    :body-ast body-ast
    :lambda-list lambda-list
    :forms forms))

(cleavir-io:define-save-info top-level-function-ast
  (:forms forms))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class PROGN-AST.

(defclass progn-ast (ast)
  ((%form-asts :initarg :form-asts :reader form-asts)))

(defun make-progn-ast (form-asts &key origin (policy *policy*))
  (make-instance 'progn-ast
    :origin origin :policy policy
    :form-asts form-asts))

(cleavir-io:define-save-info progn-ast
  (:form-asts form-asts))

(defmethod children ((ast progn-ast))
  (form-asts ast))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class BLOCK-AST.

(defclass block-ast (ast)
  ((%body-ast :initarg :body-ast :accessor body-ast)))

(defun make-block-ast (body-ast &key origin (policy *policy*))
  (make-instance 'block-ast
    :origin origin :policy policy
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

(defun make-return-from-ast (block-ast form-ast &key origin (policy *policy*))
  (make-instance 'return-from-ast
    :origin origin :policy policy
    :block-ast block-ast
    :form-ast form-ast))

(cleavir-io:define-save-info return-from-ast
  (:block-ast block-ast)
  (:form-ast form-ast))

(defmethod children ((ast return-from-ast))
  (list (form-ast ast)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class SETQ-AST.
;;; 
;;; This AST does not correspond exactly to the SETQ special operator,
;;; because the AST does not return a value.

(defclass setq-ast (no-value-ast-mixin ast)
  ((%lhs-ast :initarg :lhs-ast :reader lhs-ast)
   (%value-ast :initarg :value-ast :reader value-ast)))

(defun make-setq-ast (lhs-ast value-ast &key origin (policy *policy*))
  (make-instance 'setq-ast
    :origin origin :policy policy
    :lhs-ast lhs-ast
    :value-ast value-ast))

(cleavir-io:define-save-info setq-ast
  (:lhs-ast lhs-ast)
  (:value-ast value-ast))

(defmethod children ((ast setq-ast))
  (list (lhs-ast ast) (value-ast ast)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class MULTIPLE-VALUE-SETQ-AST.
;;; 
;;; This AST can be used to represent MULTIPLE-VALUE-BIND.  
;;;
;;; The LHS-ASTS is a list of lexical locations to be assigned to.
;;; FORM-AST represents a form to be evaluated, and the values of
;;; which will be assigned to the lexical locations in LHS-ASTS.  If
;;; the FORM-AST produces fewer values than there are lexical
;;; locations in LHS-ASTS, then NIL is assigned to the remaining
;;; lexical locations.  If there are more values there are lexical
;;; locations in LHS-ASTS, then the additional values are not
;;; assigned anywhere.
;;;
;;; Unlike the special operator, this AST returns all values from
;;; the form (including unassigned values). This allows it to be
;;; used in concert with MULTIPLE-VALUE-PROG1 to implement THE
;;; type checks, and potentially other things.

(defclass multiple-value-setq-ast (ast)
  ((%lhs-asts :initarg :lhs-asts :reader lhs-asts)
   (%form-ast :initarg :form-ast :reader form-ast)))

(defun make-multiple-value-setq-ast (lhs-asts form-ast &key origin (policy *policy*))
  (make-instance 'multiple-value-setq-ast
    :origin origin :policy policy
    :lhs-asts lhs-asts
    :form-ast form-ast))

(cleavir-io:define-save-info multiple-value-setq-ast
  (:lhs-asts lhs-asts)
  (:form-ast form-ast))

(defmethod children ((ast multiple-value-setq-ast))
  (cons (form-ast ast) (lhs-asts ast)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class TAG-AST.

(defclass tag-ast (ast)
  ((%name :initarg :name :reader name)))

(defun make-tag-ast (name &key origin (policy *policy*))
  (make-instance 'tag-ast
    :origin origin :policy policy
    :name name))

(cleavir-io:define-save-info tag-ast
  (:name name))

(defmethod children ((ast tag-ast))
  (declare (ignorable ast))
  '())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class TAGBODY-AST.

(defclass tagbody-ast (no-value-ast-mixin ast)
  ((%item-asts :initarg :item-asts :reader item-asts)))

(defun make-tagbody-ast (item-asts &key origin (policy *policy*))
  (make-instance 'tagbody-ast
    :origin origin :policy policy
    :item-asts item-asts))

(cleavir-io:define-save-info tagbody-ast
  (:item-asts item-asts))

(defmethod children ((ast tagbody-ast))
  (item-asts ast))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class GO-AST.

(defclass go-ast (ast)
  ((%tag-ast :initarg :tag-ast :reader tag-ast)))

(defun make-go-ast (tag-ast &key origin (policy *policy*))
  (make-instance 'go-ast
    :origin origin :policy policy
    :tag-ast tag-ast))

(cleavir-io:define-save-info go-ast
  (:tag-ast tag-ast))

(defmethod children ((ast go-ast)) nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class THE-AST.
;;;
;;; This AST can be generated by from the THE special operator, but
;;; also implicitly from type declarations and assignments to
;;; variables with type declarations.  
;;;
;;; This AST should be used only in situations where it is known that
;;; the value produced is of the correct type.  For situations where
;;; it is desirable to signal an error when there is a violation of
;;; the declared type, the TYPEQ-AST should be used instead.

(defclass the-ast (ast)
  ((%form-ast :initarg :form-ast :reader form-ast)
   (%required-types :initarg :required :reader required-types)
   (%optional-types :initarg :optional :reader optional-types)
   (%rest-type :initarg :rest :reader rest-type)))

(defun make-the-ast (form-ast required optional rest &key origin (policy *policy*))
  (make-instance 'the-ast
    :origin origin :policy policy
    :form-ast form-ast
    :required required
    :optional optional
    :rest rest))

(cleavir-io:define-save-info the-ast
  (:form-ast form-ast)
  (:required required-types)
  (:optional optional-types)
  (:rest rest-type))

(defmethod children ((ast the-ast))
  (list (form-ast ast)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class TYPEQ-AST.
;;;
;;; This AST can be thought of as a translation to an AST of a
;;; hypothetical special form (TYPEQ <form> <type-specifier>) which is
;;; like the function TYPEP, except that the type specifier is not
;;; evaluated.
;;;
;;; However, this AST can only occur in the conditional position of an
;;; IF-AST.
;;;
;;; Implementations that interpret the special form (THE <type>
;;; <form>) as an error if <form> is not of type <type> might generate
;;; a TYPEQ-AST contained in an IF-AST instead of a THE-AST, and to
;;; have the ELSE branch of the IF-AST call ERROR.

(defclass typeq-ast (boolean-ast-mixin ast)
  (;; This slot contains the type specifier as an S-expression.  When
   ;; this AST is compiled to HIR, the contents of this slot will be
   ;; transmitted to the TYPEQ-INSTRUCTION so that it can be used by
   ;; the type inference machinery.
   (%type-specifier :initarg :type-specifier :reader type-specifier)
   (%form-ast :initarg :form-ast :reader form-ast)))

(defun make-typeq-ast (form-ast type-specifier &key origin (policy *policy*))
  (make-instance 'typeq-ast
    :origin origin :policy policy
    :form-ast form-ast
    :type-specifier type-specifier))

(cleavir-io:define-save-info typeq-ast
  (:type-specifier type-specifier)
  (:form-ast form-ast))

(defmethod children ((ast typeq-ast))
  (list (form-ast ast)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class TYPEW-AST.
;;;
;;; This AST is used to communicate type information to inference passes.
;;; The TEST-AST is what will actually evaluate the form and determine
;;; which branch of code to proceed down; the form-ast and ctype are only
;;; meaningful to inference passes. This allows the computation of type
;;; tests to be done in a client-dependent way and independently of the
;;; inference annotations.
;;; In other words, the typew-ast could be replaced with its form-ast
;;; and this would have no effect on semantics, just weaken inference.

(defclass typew-ast (boolean-ast-mixin ast)
  ((%form-ast :initarg :form-ast :reader form-ast)
   (%ctype :initarg :ctype :reader ctype)
   (%test-ast :initarg :test-ast :reader test-ast)))

(defun make-typew-ast (form-ast ctype test-ast &key origin (policy *policy*))
  (make-instance 'typew-ast
    :origin origin :policy policy
    :form-ast form-ast :ctype ctype :test-ast test-ast))

(cleavir-io:define-save-info typew-ast
    (:form-ast form-ast)
  (:ctype ctype)
  (:test-ast test-ast))

(defmethod children ((ast typew-ast))
  (list (form-ast ast) (test-ast ast)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class THE-TYPEW-AST.
;;;
;;; This is like TYPEW-AST for the situation in which the form's value
;;; has been declared to be of the given type. The ELSE-AST is only
;;; used if type inference determines that the declaration is incorrect.

(defclass the-typew-ast (one-value-ast-mixin ast)
  ((%form-ast :initarg :form-ast :reader form-ast)
   (%ctype :initarg :ctype :reader ctype)
   (%else-ast :initarg :else-ast :reader else-ast)))

(defun make-the-typew-ast (form-ast ctype else-ast
                           &key origin (policy *policy*))
  (make-instance 'the-typew-ast
    :origin origin :policy policy
    :form-ast form-ast :ctype ctype :else-ast else-ast))

(cleavir-io:define-save-info the-typew-ast
    (:form-ast form-ast)
  (:ctype ctype)
  (:else-ast else-ast))

(defmethod children ((ast the-typew-ast))
  (list (form-ast ast) (else-ast ast)))

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

(defclass load-time-value-ast (one-value-ast-mixin ast)
  ((%form :initarg :form :reader form)
   (%read-only-p :initarg :read-only-p :reader read-only-p)))

(defun make-load-time-value-ast (form &optional read-only-p &key origin (policy *policy*))
  (make-instance 'load-time-value-ast
    :origin origin :policy policy
    :form form
    :read-only-p read-only-p))

;;; Even though READ-ONLY-P is not a child of the AST, it needs to be
;;; saved when the AST is saved. 
(cleavir-io:define-save-info load-time-value-ast
  (:form form)
  (:read-only-p read-only-p))

(defmethod children ((ast load-time-value-ast))
  '())

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

(defun make-if-ast (test-ast then-ast else-ast &key origin (policy *policy*))
  (make-instance 'if-ast
    :origin origin :policy policy
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
;;; Class BRANCH-AST.
;;;
;;; This class is a generalization of IF-AST. Based on the TEST-AST,
;;; one of the zero or more BRANCH-ASTs, or else the DEFAULT-AST,
;;; will be evaluated and its values returned.
;;;
;;; This AST can be used for example in the implementation of fast
;;; CASE or TYPECASE operations.

(defclass branch-ast (ast)
  ((%test-ast :initarg :test-ast :reader test-ast)
   (%branch-asts :initarg :branch-asts :reader branch-asts)
   (%default-ast :initarg :default-ast :reader default-ast)))

(defun make-branch-ast (test-ast branch-asts default-ast
                        &key origin (policy *policy*))
  (make-instance 'branch-ast
    :origin origin :policy policy
    :test-ast test-ast
    :branch-asts branch-asts :default-ast default-ast))

(cleavir-io:define-save-info branch-ast
  (:test-ast test-ast)
  (:branch-asts branch-asts)
  (:default-ast default-ast))

(defmethod children ((ast branch-ast))
  (list* (test-ast ast) (default-ast ast) (branch-asts ast)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class MULTIPLE-VALUE-CALL-AST.

(defclass multiple-value-call-ast (ast)
  ((%function-form-ast :initarg :function-form-ast :reader function-form-ast)
   (%form-asts :initarg :form-asts :reader form-asts)
   (%attributes :initarg :attributes :reader attributes
                :initform (cleavir-attributes:default-attributes))))

(defun make-multiple-value-call-ast
    (function-form-ast form-asts
     &key origin (policy *policy*)
       (attributes (cleavir-attributes:default-attributes)))
  (make-instance 'multiple-value-call-ast
    :origin origin :policy policy
    :function-form-ast function-form-ast
    :form-asts form-asts :attributes attributes))

(cleavir-io:define-save-info multiple-value-call-ast
  (:function-form-ast function-form-ast)
  (:form-asts form-asts)
  (:attributes attributes))

(defmethod children ((ast multiple-value-call-ast))
  (list* (function-form-ast ast) (form-asts ast)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class VALUES-AST.
;;;
;;; This corresponds directly to CLEAVIR-PRIMOP:VALUES,
;;; and CL:VALUES through it.

(defclass values-ast (ast)
  ((%argument-asts :initarg :argument-asts :reader argument-asts)))

(defun make-values-ast
    (argument-asts &key origin (policy *policy*))
  (make-instance 'values-ast
    :origin origin :policy policy
    :argument-asts argument-asts))

(cleavir-io:define-save-info values-ast
  (:argument-asts argument-asts))

(defmethod children ((ast values-ast))
  (argument-asts ast))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class MULTIPLE-VALUE-PROG1-AST.

(defclass multiple-value-prog1-ast (ast)
  ((%first-form-ast :initarg :first-form-ast :reader first-form-ast)
   ;; A list of ASTs
   (%form-asts :initarg :form-asts :reader form-asts)))

(defun make-multiple-value-prog1-ast (first-form-ast form-asts &key origin (policy *policy*))
  (make-instance 'multiple-value-prog1-ast
    :origin origin :policy policy
    :first-form-ast first-form-ast
    :form-asts form-asts))

(cleavir-io:define-save-info multiple-value-prog1-ast
  (:first-form-ast first-form-ast)
  (:form-asts form-asts))

(defmethod children ((ast multiple-value-prog1-ast))
  (cons (first-form-ast ast)
	(form-asts ast)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class MULTIPLE-VALUE-EXTRACT-AST

(defclass multiple-value-extract-ast (multiple-value-prog1-ast)
  ((%lhs-asts :initarg :lhs-asts :reader lhs-asts)))

(defun make-multiple-value-extract-ast
    (lhs-asts first-form-ast form-asts &key origin (policy *policy*))
  (make-instance 'multiple-value-extract-ast
    :origin origin :policy policy
    :lhs-asts lhs-asts
    :first-form-ast first-form-ast
    :form-asts form-asts))

(cleavir-io:define-save-info multiple-value-extract-ast
    (:lhs-asts lhs-asts))

(defmethod children ((ast multiple-value-extract-ast))
  (append (lhs-asts ast) (call-next-method)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class DYNAMIC-ALLOCATION-AST
;;;
;;; This AST is used to translate DYNAMIC-EXTENT declarations.
;;; Any allocation done by its form-ast may be done dynamically,
;;; i.e. with stack discipline. This means that the consequences
;;; are undefined if any value allocated by the form-ast escapes
;;; the local function.
;;; Note that this loses information from DYNAMIC-EXTENT, which
;;; does not allow escape from the form with the declaration.

(defclass dynamic-allocation-ast (one-value-ast-mixin ast)
  ((%form-ast :initarg :form-ast :reader form-ast)))

(defun make-dynamic-allocation-ast (form-ast &key origin (policy *policy*))
  (make-instance 'dynamic-allocation-ast
    :origin origin :policy policy
    :form-ast form-ast))

(cleavir-io:define-save-info dynamic-allocation-ast
  (:form-ast form-ast))

(defmethod children ((ast dynamic-allocation-ast))
  (list (form-ast ast)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class UNREACHABLE-AST.
;;;
;;; This AST indicates an unreachable control point.
;;; Control that leads inevitably from or to this AST is
;;; declared to be impossible.

(defclass unreachable-ast (ast) ())

(defun make-unreachable-ast (&key origin (policy *policy*))
  (make-instance 'unreachable-ast :origin origin :policy policy))

(defmethod children ((ast unreachable-ast)) nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class BIND-AST.
;;;
;;; This AST is used to create a dynamic binding for a symbol for the
;;; duration of the execution of the body.  It is generated as a
;;; result of a binding of a special variable in a LET, LET*, or a
;;; lambda list of a function. 

(defclass bind-ast (ast)
  ((%symbol :initarg :symbol :reader symbol)
   (%value-ast :initarg :value-ast :reader value-ast)
   (%body-ast :initarg :body-ast :reader body-ast)))

(defun make-bind-ast (symbol value-ast body-ast &key origin (policy *policy*))
  (make-instance 'bind-ast
    :origin origin :policy policy
    :symbol symbol
    :value-ast value-ast
    :body-ast body-ast))

(cleavir-io:define-save-info bind-ast
  (:symbol symbol)
  (:value-ast value-ast)
  (:body-ast body-ast))

(defmethod children ((ast bind-ast))
  (list (value-ast ast) (body-ast ast)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class EQ-AST.
;;;
;;; This AST can be used to to test whether two objects are identical.
;;; It has two children.  This AST can only appear in the TEST
;;; position of an IF-AST.

(defclass eq-ast (boolean-ast-mixin ast)
  ((%arg1-ast :initarg :arg1-ast :reader arg1-ast)
   (%arg2-ast :initarg :arg2-ast :reader arg2-ast)))

(defun make-eq-ast (arg1-ast arg2-ast &key origin (policy *policy*))
  (make-instance 'eq-ast
    :origin origin :policy policy
    :arg1-ast arg1-ast
    :arg2-ast arg2-ast))

(cleavir-io:define-save-info eq-ast
  (:arg1-ast arg1-ast)
  (:arg2-ast arg2-ast))

(defmethod children ((ast eq-ast))
  (list (arg1-ast ast) (arg2-ast ast)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class NEQ-AST.
;;;
;;; This AST can be used to to test whether two objects are distinct.
;;; It has two children.  This AST can only appear in the TEST
;;; position of an IF-AST. It is useful for using an AST that
;;; returns a value as a boolean ast.

(defclass neq-ast (boolean-ast-mixin ast)
  ((%arg1-ast :initarg :arg1-ast :reader arg1-ast)
   (%arg2-ast :initarg :arg2-ast :reader arg2-ast)))

(defun make-neq-ast (arg1-ast arg2-ast &key origin (policy *policy*))
  (make-instance 'neq-ast
    :origin origin :policy policy
    :arg1-ast arg1-ast
    :arg2-ast arg2-ast))

(cleavir-io:define-save-info neq-ast
  (:arg1-ast arg1-ast)
  (:arg2-ast arg2-ast))

(defmethod children ((ast neq-ast))
  (list (arg1-ast ast) (arg2-ast ast)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class CASE-AST.
;;;
;;; This AST can be used to select an execution path by
;;; comparing a given object against a fixed set of immediates.
;;; COMPAREES is a sequence of sequences of objects.
;;; If the primary value returned by the ARG-AST is EQ to one of
;;; the objects in the nth sequence, the nth branch is taken;
;;; if the value doesn't match any immediate the default branch
;;; is taken instead.
;;; This AST can only appear in the TEST position of a BRANCH-AST.

(defclass case-ast (multiway-ast-mixin ast)
  ((%arg-ast :initarg :arg-ast :reader arg-ast)
   (%comparees :initarg :comparees :reader comparees)))

(defun make-case-ast (arg-ast comparees &key origin (policy *policy*))
  (make-instance 'case-ast
    :origin origin :policy policy
    :arg-ast arg-ast :comparees comparees))

(cleavir-io:define-save-info case-ast
  (:arg-ast arg-ast)
  (:comparees comparees))

(defmethod children ((ast case-ast))
  (list (arg-ast ast)))
