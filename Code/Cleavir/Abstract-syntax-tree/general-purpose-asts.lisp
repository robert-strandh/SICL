(cl:in-package #:cleavir-ast)

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

(defclass constant-ast (ast
                        one-value-ast-mixin
                        side-effect-free-ast-mixin)
  ((%value :initarg :value :reader value)))

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

(defclass lexical-ast (ast
                       one-value-ast-mixin
                       side-effect-free-ast-mixin)
  ((%name :initarg :name :reader name)))

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

(defclass symbol-value-ast
    (ast one-value-ast-mixin side-effect-free-ast-mixin)
  ((%name-ast :initarg :name-ast :reader name-ast)))

(cleavir-io:define-save-info symbol-value-ast
  (:name-ast name-ast))

(defmethod children ((ast symbol-value-ast))
  (list (name-ast ast)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class SET-SYMBOL-VALUE-AST.

(defclass set-symbol-value-ast (ast no-value-ast-mixin)
  ((%name-ast :initarg :name-ast :reader name-ast)
   (%value-ast :initarg :value-ast :reader value-ast)))

(cleavir-io:define-save-info set-symbol-value-ast
  (:name-ast name-ast)
  (:value-ast value-ast))

(defmethod children ((ast set-symbol-value-ast))
  (list (name-ast ast) (value-ast ast)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class FDEFINITION-AST.
;;;
;;; This AST is generated from a reference to a global function.

(defclass fdefinition-ast
    (ast one-value-ast-mixin side-effect-free-ast-mixin)
  (;; This slot contains an AST that produces the function name.
   (%name-ast :initarg :name-ast :reader name-ast)))

(cleavir-io:define-save-info fdefinition-ast
  (:name-ast name-ast))

(defmethod children ((ast fdefinition-ast))
  (list (name-ast ast)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class SET-FDEFINITION-AST.

(defclass set-fdefinition-ast (ast no-value-ast-mixin)
  ((%name-ast :initarg :name-ast :reader name-ast)
   (%value-ast :initarg :value-ast :reader value-ast)))

(cleavir-io:define-save-info set-fdefinition-ast
  (:name-ast name-ast)
  (:value-ast value-ast))

(defmethod children ((ast set-fdefinition-ast))
  (list (name-ast ast) (value-ast ast)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class CALL-AST.
;;;
;;; A CALL-AST represents a function call.  

(defclass call-ast (ast)
  ((%callee-ast :initarg :callee-ast :reader callee-ast)
   (%argument-asts :initarg :argument-asts :reader argument-asts)))

(cleavir-io:define-save-info call-ast
  (:callee-ast callee-ast)
  (:argument-asts argument-asts))

(defmethod children ((ast call-ast))
  (list* (callee-ast ast) (argument-asts ast)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class NAMED-CALL-AST.
;;;
;;; This AST is similar to the CALL-AST.  It can be used by clients
;;; code that want to have a special representation for a function
;;; call where the function is explicitly named, as is the case for
;;; function-call forms.  This AST differs from the CALL-AST, in that
;;; it does not have a CALLEE child.  Instead, it has a NAME slot that
;;; is not an AST, and which contains the name of the function to
;;; call.

(defclass named-call-ast (ast)
  ((%callee-name :initarg :callee-name :reader callee-name)
   (%argument-asts :initarg :argument-asts :reader argument-asts)))

(cleavir-io:define-save-info named-call-ast
  (:callee-name callee-name)
  (:argument-asts argument-asts))

(defmethod children ((ast named-call-ast))
  (argument-asts ast))

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

(defclass function-ast (ast
                        one-value-ast-mixin
                        side-effect-free-ast-mixin)
  ((%lambda-list :initarg :lambda-list :reader lambda-list)
   (%body-ast :initarg :body-ast :reader body-ast)))

(cleavir-io:define-save-info function-ast
  (:lambda-list lambda-list)
  (:body-ast body-ast))

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

(cleavir-io:define-save-info top-level-function-ast
  (:forms forms))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class PROGN-AST.

(defclass progn-ast (ast)
  ((%form-asts :initarg :form-asts :reader form-asts)))

(cleavir-io:define-save-info progn-ast
  (:form-asts form-asts))

(defmethod children ((ast progn-ast))
  (form-asts ast))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class BLOCK-AST.

(defclass block-ast (ast)
  ;; FIXME: make this read-only and use REINITIALIZE-INSTANCE insteac.
  ((%body-ast :initarg :body-ast :accessor body-ast)))

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

(defclass setq-ast (ast no-value-ast-mixin)
  ((%lhs-ast :initarg :lhs-ast :reader lhs-ast)
   (%value-ast :initarg :value-ast :reader value-ast)))

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
;;; locations in LHS-ASTS, then the additional values are ignored.
;;;
;;; This AST does not return a value, so it must be compiled in a
;;; context where no value is required.

(defclass multiple-value-setq-ast (ast no-value-ast-mixin)
  ((%lhs-asts :initarg :lhs-asts :reader lhs-asts)
   (%form-ast :initarg :form-ast :reader form-ast)))

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

(cleavir-io:define-save-info tag-ast
  (:name name))

(defmethod children ((ast tag-ast))
  (declare (ignorable ast))
  '())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class TAGBODY-AST.

(defclass tagbody-ast (ast no-value-ast-mixin)
  ((%item-asts :initarg :item-asts :reader item-asts)))

(cleavir-io:define-save-info tagbody-ast
  (:item-asts item-asts))

(defmethod children ((ast tagbody-ast))
  (item-asts ast))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class GO-AST.

(defclass go-ast (ast)
  ((%tag-ast :initarg :tag-ast :reader tag-ast)))

(cleavir-io:define-save-info go-ast
  (:tag-ast tag-ast))

(defmethod children ((ast go-ast))
  (list (tag-ast ast)))

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
;;;
;;; It used to be the case that we would have an :AFTER method on
;;; INITIALIZE-INSTANCE that would compute the TYPE-SPECIFIER-AST slot
;;; from the TYPE-SPECIFIER slot.  However this technique will not
;;; work when ASTs are cloned, because it is assumed in the cloning
;;; code that an instance of the AST can be created without any
;;; initialization arguments.  So instead, we initialize the
;;; TYPE-SPECIFIER-AST slot with NIL and we compute the real value of
;;; it only when it is requested.

(defclass typeq-ast (ast boolean-ast-mixin)
  (;; This slot contains the type specifier as an S-expression.  When
   ;; this AST is compiled to HIR, the contents of this slot will be
   ;; transmitted to the TYPEQ-INSTRUCTION so that it can be used by
   ;; the type inference machinery.
   (%type-specifier :initarg :type-specifier :reader type-specifier)
   ;; This slot also contains the type specifier, but this time as a
   ;; LOAD-TIME-VALUE-AST.  The purpose of this AST is that it will be
   ;; hoisted so that the type specifier is provided as a load-time
   ;; constant to be used with TYPEP, should it turn out to be
   ;; necessary to use TYPEP at runtime to determine the type.
   (%type-specifier-ast :initform nil
			:initarg :type-specifier-ast
			:reader type-specifier-ast)
   (%form-ast :initarg :form-ast :reader form-ast)))

;; (defmethod type-specifier-ast :around ((ast typeq-ast))
;;   (let ((value (call-next-method)))
;;     (when (null value)
;;       (setq value
;;             (make-instance 'load-time-value-ast
;;               :form `',(type-specifier ast)
;;               :read-only-p t))
;;       (reinitialize-instance
;;        ast
;;        :type-specifier-ast value))
;;     value))

(cleavir-io:define-save-info typeq-ast
  (:type-specifier type-specifier)
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

(defclass load-time-value-ast (ast one-value-ast-mixin)
  ((%form-ast :initarg :form-ast :reader form-ast)
   (%read-only-p :initarg :read-only-p :reader read-only-p)))

;;; Even though READ-ONLY-P is not a child of the AST, it needs to be
;;; saved when the AST is saved. 
(cleavir-io:define-save-info load-time-value-ast
  (:form-ast form-ast)
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

(cleavir-io:define-save-info if-ast
  (:test-ast test-ast)
  (:then-ast then-ast)
  (:else-ast else-ast))

(defmethod children ((ast if-ast))
  (list (test-ast ast) (then-ast ast) (else-ast ast)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class MULTIPLE-VALUE-CALL-AST.

(defclass multiple-value-call-ast (ast)
  ((%function-form-ast :initarg :function-form-ast :reader function-form-ast)
   (%form-asts :initarg :form-asts :reader form-asts)))

(cleavir-io:define-save-info multiple-value-call-ast
  (:function-form-ast function-form-ast)
  (:form-asts form-asts))

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

(cleavir-io:define-save-info multiple-value-prog1-ast
  (:first-form-ast first-form-ast)
  (:form-asts form-asts))

(defmethod children ((ast multiple-value-prog1-ast))
  (cons (first-form-ast ast)
	(form-asts ast)))

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

(defclass dynamic-allocation-ast (ast one-value-ast-mixin)
  ((%form-ast :initarg :form-ast :reader form-ast)))

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

(defmethod children ((ast unreachable-ast)) nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class BIND-AST.
;;;
;;; This AST is used to create a dynamic binding for a variable for
;;; the duration of the execution of the body.  It is generated as a
;;; result of a binding of a special variable in a LET, LET*, or a
;;; lambda list of a function.

(defclass bind-ast (ast)
  ((%name-ast :initarg :name-ast :reader name-ast)
   (%value-ast :initarg :value-ast :reader value-ast)
   (%body-ast :initarg :body-ast :reader body-ast)))

(cleavir-io:define-save-info bind-ast
  (:name-ast name-ast)
  (:value-ast value-ast)
  (:body-ast body-ast))

(defmethod children ((ast bind-ast))
  (list (value-ast ast) (body-ast ast)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class UNWIND-PROTECT-AST.
;;;
;;; This AST is generated from an UNWIND-PROTECT form.  The protected
;;; form of the original form is turned into a corresponding AST,
;;; whereas the cleanup forms are wrapped in a LAMBDA expression so
;;; that those forms are executed as part of a thunk.

(defclass unwind-protect-ast (ast)
  ((%protected-form-ast :initarg :protected-form-ast :reader protected-form-ast)
   (%cleanup-thunk-ast :initarg :cleanup-thunk-ast :reader cleanup-thunk-ast)))

(cleavir-io:define-save-info unwind-protect-ast
  (:protected-form-ast protected-form-ast)
  (:cleanup-thunk-ast cleanup-thunk-ast))

(defmethod children ((ast unwind-protect-ast))
  (list (protected-form-ast ast) (cleanup-thunk-ast ast)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class EQ-AST.
;;;
;;; This AST can be used to to test whether two objects are identical.
;;; It has two children.  This AST can only appear in the TEST
;;; position of an IF-AST.

(defclass eq-ast (ast boolean-ast-mixin)
  ((%arg1-ast :initarg :arg1-ast :reader arg1-ast)
   (%arg2-ast :initarg :arg2-ast :reader arg2-ast)))

(cleavir-io:define-save-info eq-ast
  (:arg1-ast arg1-ast)
  (:arg2-ast arg2-ast))

(defmethod children ((ast eq-ast))
  (list (arg1-ast ast) (arg2-ast ast)))
