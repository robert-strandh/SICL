(in-package #:clvm-ast)

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class GLOBAL-AST. 
;;; 
;;; A GLOBAL-AST represents a reference to a global FUNCTION, i.e., a
;;; name that is known to be associated with a function in the global
;;; environment.  Such a reference contains the name of the function,
;;; the STORAGE cell where the function can be found, and the TYPE of
;;; the function as it was declared in the context where the AST was
;;; created.  The storage cell is a CONS cell where the CAR contains
;;; the function and the CDR is NIL.

(defclass global-ast (ast)
  ((%name :initarg :name :reader name)
   (%function-type :initarg :function-type :accessor function-type)
   (%storage :initarg :storage :reader storage)
   (%children :initform '() :allocation :class)))

(defun make-global-ast (name storage)
  (make-instance 'global-ast :name name :storage storage))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class SPECIAL-AST.
;;; 
;;; A SPECIAL-AST represents a reference to a special variable.  Such
;;; a reference contains the name of the variable, and the STORAGE
;;; cell where the global value of the variable can be found.  This
;;; value is used only if no binding of the variable is found in the
;;; dynamic environment.  The storage cell is a CONS cell where the
;;; CAR contains the global value and the CDR is NIL.

(defclass special-ast (ast)
  ((%name :initarg :name :reader name)
   (%storage :initarg :storage :reader storage)
   (%children :initform '() :allocation :class)))

(defun make-special-ast (name storage)
  (make-instance 'special-ast :name name :storage storage))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class LEXICAL-AST.
;;; 
;;; A LEXICAL-AST represents a reference to a lexical variable.  Such
;;; a reference contains the name of the variable, but it is used only
;;; for debugging perposes and for the purpose of error reporting.
;;;
;;; A lexical variable does not have any associated location or
;;; storage, because where it can be found might be different in
;;; different parts of the code.

(defclass lexical-ast (ast)
  ((%name :initarg :name :reader name)))

(defun make-lexical-ast (name)
  (make-instance 'lexical-ast :name name))

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
  (cdr (children ast)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class FUNCTION-AST.
;;;
;;; A function AST represents an explicit lambda expression, but also
;;; implicit lambda expressions such as the ones found in FLET and
;;; LABELS.

(defclass function-ast (ast)
  (;; If the function is known to have only required parameters, then
   ;; this value is the number of required parameters.  Otherwise,
   ;; this value is false.  A function is considered for inlining only
   ;; if it has only required parameters. 
   (%required-only-p :initarg :required-only-p :reader required-only-p)))

(defun make-function-ast (body-ast &optional required-only-p)
  (make-instance 'function-ast
    :children (list body-ast)
    :required-only-p required-only-p))

(defmethod body-ast ((ast function-ast))
  (first (children ast)))

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
;;;
;;; As with the TYPEQ-AST, we keep the list of types in a separate
;;; slot, so that even if the type specifiers stored in the children
;;; get replaced by a reference to the linkage vector, we still have
;;; access to the types for static type inference purposes. 

(defclass the-ast (ast)
  ((%type-specifiers :initarg :type-specifiers :reader type-specifiers)))

(defun make-the-ast (form-ast &rest types)
  (make-instance 'the-ast
    :children (list* form-ast types)
    :type-specifiers (mapcar #'value types)))

(defmethod form-ast ((ast the-ast))
  (first (children ast)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class LOAD-TIME-VALUE-AST.

(defclass load-time-value-ast (ast)
  ((%read-only-p :initarg :read-only-p :reader read-only-p)))

(defun make-load-time-value-ast (form-ast &optional read-only-p)
  (make-instance 'load-time-value-ast
    :children  (list form-ast)
    :read-only-p read-only-p))

(defmethod form-ast ((ast load-time-value-ast))
  (first (children ast)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class IF-AST.

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
