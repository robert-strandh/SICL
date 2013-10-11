(in-package #:sicl-ast)

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Retrieving the children of an AST.
;;;
;;; Since each node of an AST is different, and have different names
;;; for the sub-ASTs that it defines, it is not easy to retrieve the
;;; children of an arbitrary node.  Furthermore, the AST, despite the
;;; name, is not really a tree, and not even acyclic because of nodes
;;; such as RETURN-FROM and GO that contain references to ancestors.  
;;;
;;; For that reason, we define a method for each type of node in the
;;; AST that allows us to scan the entire AST without looking at
;;; details of each node, while avoiding infinite cycles by only
;;; looking "downwards" in the tree.

(defgeneric children (ast))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Drawing an AST. 
;;;
;;; We generate a Graphviz source file from the AST so that the AST
;;; can be presented in graph form for easy inspection.

(defparameter *table* nil)

(defun id (ast)
  (symbol-name (gethash ast *table*)))

(defgeneric stream-draw-ast (ast stream))

(defun draw-ast (ast filename)
  (with-open-file (stream filename :direction :output :if-exists :supersede)
    (format stream "digraph G {~%   ordering = out;~%")
    (let ((*table* (make-hash-table :test #'eq)))
      (stream-draw-ast ast stream))
    (format stream "}~%")))

(defmethod stream-draw-ast :around (ast stream)
  (when (null (gethash ast *table*))
    (setf (gethash ast *table*) (gensym))
    (format stream "  ~a [shape = box];~%"
	    (id ast))
    (call-next-method)
    (loop for child in (children ast)
	  do (stream-draw-ast child stream)
	     (format stream "   ~a -> ~a~%" (id ast) (id child)))))

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

(defmethod stream-draw-ast ((ast immediate-ast) stream)
  (format stream "   ~a [style = filled, fillcolor = aquamarine];~%" (id ast))
  (format stream "   ~a [label = \"~a\"];~%"
	  (id ast)
	  (value ast)))

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

(defmethod stream-draw-ast ((ast constant-ast) stream)
  (format stream "   ~a [style = filled, fillcolor = green];~%" (id ast))
  (format stream "   ~a [label = \"~a\"];~%"
	  (id ast)
	  (value ast)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class GLOBAL-AST. 
;;; 
;;; A GLOBAL-AST represents a reference to a global FUNCTION, i.e., a
;;; name that is known to be associated with a function in the global
;;; environment.  Such a reference contains the name of the function,
;;; and the STORAGE cell where the function can be found.  The storage
;;; cell is a CONS cell where the CAR contains the function and the
;;; CDR is NIL.
;;;
;;; We do not include information about the type of the function, nor
;;; whether the function is declared inline/notinline, because such
;;; information is not intrinsic properties of the function, and can
;;; vary according to the place where the function is called. 

(defclass global-ast (ast)
  ((%name :initarg :name :reader name)
   (%storage :initarg :storage :reader storage)
   (%children :initform '() :allocation :class)))

(defun make-global-ast (name storage)
  (make-instance 'global-ast :name name :storage storage))

(defmethod stream-draw-ast ((ast global-ast) stream)
  (format stream "   ~a [style = filled, fillcolor = cyan];~%" (id ast))
  (format stream "   ~a [label = \"~a\"];~%"
	  (id ast)
	  (name ast)))

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

(defmethod stream-draw-ast ((ast special-ast) stream)
  (format stream "   ~a [style = filled, fillcolor = magenta];~%" (id ast))
  (format stream "   ~a [label = \"~a\"];~%"
	  (id ast)
	  (name ast)))

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

(defmethod stream-draw-ast ((ast lexical-ast) stream)
  (format stream "   ~a [style = filled, fillcolor = yellow];~%" (id ast))
  (format stream "   ~a [label = \"~a\"];~%"
	  (id ast)
	  (name ast)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class CALL-AST. 
;;;
;;; A CALL-AST represents a function call.  
;;;
;;; The callee can be a function-ast or a location.  An argument can
;;; be any AST. 
;;;
;;; FIXME: Why can't the callee be any AST?

(defclass call-ast (ast)
  ())

(defun make-call-ast (callee-ast argument-asts)
  (make-instance 'call-ast
    :children (cons callee-ast argument-asts)))

(defmethod stream-draw-ast ((ast call-ast) stream)
  (format stream "   ~a [label = \"call\"];~%"
	  (id ast)))

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

(defmethod stream-draw-ast ((ast function-ast) stream)
  (format stream "   ~a [label = \"function\"];~%"
	  (id ast)))

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

(defmethod stream-draw-ast ((ast progn-ast) stream)
  (format stream "   ~a [label = \"progn\"];~%"
	  (id ast)))

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

(defmethod stream-draw-ast ((ast block-ast) stream)
  (format stream "   ~a [label = \"block\"];~%"
	  (id ast)))
  
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

(defmethod stream-draw-ast ((ast return-from-ast) stream)
  (format stream "   ~a [label = \"return-from\"];~%"
	  (id ast)))
  
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

(defmethod stream-draw-ast ((ast setq-ast) stream)
  (format stream "   ~a [label = \"setq\"];~%"
	  (id ast)))

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

(defmethod stream-draw-ast ((ast tag-ast) stream)
  (format stream "   ~a [label = \"~a\"];~%"
	  (id ast)
	  (name ast)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class TAGBODY-AST.

(defclass tagbody-ast (ast)
  ())

(defun make-tagbody-ast (items)
  (make-instance 'tagbody-ast
    :children items))

(defmethod stream-draw-ast ((ast tagbody-ast) stream)
  (format stream "   ~a [label = \"tagbody\"];~%"
	  (id ast)))

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

(defmethod stream-draw-ast ((ast go-ast) stream)
  (format stream "   ~a [label = \"go\"];~%"
	  (id ast)))

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
     
(defmethod stream-draw-ast ((ast the-ast) stream)
  (format stream "   ~a [label = \"the\"];~%"
	  (id ast)))

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

(defmethod stream-draw-ast ((ast load-time-value-ast) stream)
  (format stream "   ~a [label = \"~a\"];~%"
	  (id ast) (form-ast ast))
  (format stream "   ~a [style = filled, fillcolor = pink];~%"
	  (id ast)))

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

(defmethod stream-draw-ast ((ast if-ast) stream)
  (format stream "   ~a [label = \"if\"];~%"
	  (id ast)))

(defmethod test-ast ((ast if-ast))
  (first (children ast)))

(defmethod then-ast ((ast if-ast))
  (second (children ast)))

(defmethod else-ast ((ast if-ast))
  (third (children ast)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class TYPEQ-AST.
;;;
;;; The type specifier becomes both a child in the form of a
;;; CONSTANT-AST and the value of a separate slot.  The reason for
;;; this is that the child might be replaced by a reference to the
;;; linkage vector, but we still need the type specifier in order to
;;; do static type inference.

(defclass typeq-ast (ast)
  ((%type-specifier :initarg :type-specifier :reader type-specifier)))

(defun make-typeq-ast (form-ast type-specifier-ast)
  (make-instance 'typeq-ast
    :children (list form-ast type-specifier-ast)
    :type-specifier (sicl-ast:value type-specifier-ast)))

(defmethod stream-draw-ast ((ast typeq-ast) stream)
  (format stream "   ~a [label = \"typeq\"];~%"
	  (id ast)))

(defmethod form-ast ((ast typeq-ast))
  (first (children ast)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class LOAD-CAR-AST.

(defclass load-car-ast (ast)
  ())

(defun make-load-car-ast (form-ast)
  (make-instance 'load-car-ast
    :children (list form-ast)))

(defmethod stream-draw-ast ((ast load-car-ast) stream)
  (format stream "   ~a [label = \"load-car\"];~%"
	  (id ast)))

(defmethod form-ast ((ast load-car-ast))
  (first (children ast)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class STORE-CAR-AST.

(defclass store-car-ast (ast)
  ())
   
(defun make-store-car-ast (form-ast value-ast)
  (make-instance 'store-car-ast
    :children (list form-ast value-ast)))

(defmethod stream-draw-ast ((ast store-car-ast) stream)
  (format stream "   ~a [label = \"store-car\"];~%"
	  (id ast)))

(defmethod form-ast ((ast store-car-ast))
  (first (children ast)))

(defmethod value-ast ((ast store-car-ast))
  (second (children ast)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class LOAD-CDR-AST.

(defclass load-cdr-ast (ast)
  ())

(defun make-load-cdr-ast (form-ast)
  (make-instance 'load-cdr-ast
    :children (list form-ast)))

(defmethod stream-draw-ast ((ast load-cdr-ast) stream)
  (format stream "   ~a [label = \"load-cdr\"];~%"
	  (id ast)))

(defmethod form-ast ((ast load-cdr-ast))
  (first (children ast)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class STORE-CDR-AST.

(defclass store-cdr-ast (ast)
  ())

(defun make-store-cdr-ast (form-ast value-ast)
  (make-instance 'store-cdr-ast
    :children (list form-ast value-ast)))

(defmethod stream-draw-ast ((ast store-cdr-ast) stream)
  (format stream "   ~a [label = \"store-cdr\"];~%"
	  (id ast)))

(defmethod form-ast ((ast store-cdr-ast))
  (first (children ast)))

(defmethod value-ast ((ast store-cdr-ast))
  (second (children ast)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class LOAD-CLASS-AST.

(defclass load-class-ast (ast)
  ())

(defun make-load-class-ast (form-ast)
  (make-instance 'load-class-ast
    :children (list form-ast)))

(defmethod stream-draw-ast ((ast load-class-ast) stream)
  (format stream "   ~a [label = \"load-class\"];~%"
	  (id ast)))

(defmethod form-ast ((ast load-class-ast))
  (first (children ast)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class STORE-CLASS-AST.

(defclass store-class-ast (ast)
  ())

(defun make-store-class-ast (form-ast value-ast)
  (make-instance 'store-class-ast
    :children (list form-ast value-ast)))

(defmethod stream-draw-ast ((ast store-class-ast) stream)
  (format stream "   ~a [label = \"store-class\"];~%"
	  (id ast)))

(defmethod form-ast ((ast store-class-ast))
  (first (children ast)))

(defmethod value-ast ((ast store-class-ast))
  (second (children ast)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class LOAD-CONTENTS-AST.

(defclass load-contents-ast (ast)
  ())

(defun make-load-contents-ast (form-ast offset-ast)
  (make-instance 'load-contents-ast
    :children (list form-ast offset-ast)))

(defmethod stream-draw-ast ((ast load-contents-ast) stream)
  (format stream "   ~a [label = \"load-contents\"];~%"
	  (id ast)))

(defmethod form-ast ((ast load-contents-ast))
  (first (children ast)))

(defmethod offset-ast ((ast load-contents-ast))
  (second (children ast)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class STORE-CONTENTS-AST.

(defclass store-contents-ast (ast)
  ())

(defun make-store-contents-ast (form-ast offset-ast value-ast)
  (make-instance 'store-contents-ast
    :children (list form-ast offset-ast value-ast)))

(defmethod stream-draw-ast ((ast store-contents-ast) stream)
  (format stream "   ~a [label = \"store-contents\"];~%"
	  (id ast)))

(defmethod form-ast ((ast store-contents-ast))
  (first (children ast)))

(defmethod offset-ast ((ast store-contents-ast))
  (second (children ast)))

(defmethod value-ast ((ast store-contents-ast))
  (third (children ast)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class LOAD-CONSTANT-AST.

(defclass load-constant-ast (ast)
  ((%offset :initarg :offset :reader offset)))

(defun make-load-constant-ast (offset)
  (make-instance 'load-constant-ast :offset offset))

(defmethod stream-draw-ast ((ast load-constant-ast) stream)
  (format stream "   ~a [style = filled, fillcolor = green];~%" (id ast))
  (format stream "   ~a [label = \"~a\"];~%"
	  (id ast)
	  (offset ast)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class LOAD-GLOBAL-AST.

(defclass load-global-ast (ast)
  ((%offset :initarg :offset :reader offset)))

(defun make-load-global-ast (offset)
  (make-instance 'load-global-ast :offset offset))

(defmethod stream-draw-ast ((ast load-global-ast) stream)
  (format stream "   ~a [style = filled, fillcolor = cyan];~%" (id ast))
  (format stream "   ~a [label = \"~a\"];~%"
	  (id ast)
	  (offset ast)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; AST classes for low-level operators.
;;;

(defclass arguments-mixin ()
  ())

(defmethod argument-asts ((ast arguments-mixin))
  (children ast))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class WORD-AST.
;;;
;;; Arguments (1):
;;;
;;;   * An Lisp integer that represents a machine integer.  So for a
;;;     32-bit machine, the Lisp integer can be in the range between
;;;     -2^31 and 2^31 - 1.
;;;
;;; Values (1):
;;;
;;;   * The same as the argument.  In other words, it is a no-op AST,
;;;     and its sole purpose is to make it possible to distinguish
;;;     between Lisp integers and machine integers.  A Lisp integer
;;;     would be represented as a CONSTANT-AST rather than a WORD-AST,
;;;     at least in early phases of the compilation process. 

(defclass word-ast (ast)
  ((%value :initarg :value :reader value)))

(defun make-word-ast (value)
  (make-instance 'word-ast :value value))

(defmethod stream-draw-ast ((ast word-ast) stream)
  (format stream "   ~a [label = \"~d\"];~%" (id ast) (value ast))
  (format stream "   ~a [style = filled, fillcolor = lightblue];~%" (id ast)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class MEMREF-AST.
;;; 
;;; Load a word from memory.
;;; 
;;; Arguments (1):
;;;
;;;   * An address of a word in memory.
;;;
;;; Values (1):
;;;  
;;;   * The contents of memory at that address. 

(defclass memref-ast (ast arguments-mixin)
  ())

(defun make-memref-ast (argument-ast)
  (make-instance 'memref-ast
    :children (list argument-ast)))

(defmethod stream-draw-ast ((ast memref-ast) stream)
  (format stream "   ~a [label = \"memref\"];~%"
	  (id ast)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class MEMSET-AST.
;;;
;;; Store a word into memory.
;;;
;;; Arguments (2):
;;;
;;;   * An address of a word in memory.
;;;   * A value to store at that address.
;;;
;;; No values.

(defclass memset-ast (ast arguments-mixin)
  ())

(defun make-memset-ast (argument-asts)
  (make-instance 'memset-ast
    :children argument-asts))

(defmethod stream-draw-ast ((ast memset-ast) stream)
  (format stream "   ~a [label = \"memset\"];~%"
	  (id ast)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class U+-AST.
;;;
;;; Unsigned integer addition.
;;;
;;; Arguments (2 or 3):
;;;
;;;   * First operand, an unsigned integer.
;;;   * Second operand, an unsigned integer.
;;;   * A Boolean indicating carry in (optional).
;;;
;;; Values (2):
;;;
;;;   * The first (primary) value is the sum of the operands modulo
;;;     2^w where w is the word length of the machine.  
;;;
;;;   * The second value is true if and only if the operation resulted
;;;     in a carry.

(defclass u+-ast (ast arguments-mixin)
  ())

(defun make-u+-ast (argument-asts)
  (make-instance 'u+-ast
    :children argument-asts))

(defmethod stream-draw-ast ((ast u+-ast) stream)
  (format stream "   ~a [label = \"u+\"];~%"
	  (id ast)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class U--AST.
;;;
;;; Unsigned integer subtraction.
;;;
;;; Arguments (2 or 3):
;;;
;;;   * First operand, an unsigned integer.
;;;   * Second operand, an unsigned integer.
;;;   * A Boolean indicating carry in (optional).
;;;
;;; Values (2):
;;;
;;;   * The difference of the first two operands modulo word size
;;;
;;;   * A Boolean indicating carry out.  Carry out happens when the 
;;;     second operand is larger than the first.  

(defclass u--ast (ast arguments-mixin)
  ())

(defun make-u--ast (argument-asts)
  (make-instance 'u--ast
    :children argument-asts))

(defmethod stream-draw-ast ((ast u--ast) stream)
  (format stream "   ~a [label = \"u-\"];~%"
	  (id ast)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class S+-AST.
;;;
;;; Signed integer addition.
;;;
;;; Arguments (2):
;;;
;;;   * First operand, a signed integer.
;;;   * Second operand, a signed integer.
;;;
;;; Values (2):
;;;
;;;   * The sum of the first two operands modulo word size
;;;   * A Boolean indicating overflow.

(defclass s+-ast (ast arguments-mixin)
  ())

(defun make-s+-ast (argument-asts)
  (make-instance 's+-ast
    :children argument-asts))

(defmethod stream-draw-ast ((ast s+-ast) stream)
  (format stream "   ~a [label = \"s+\"];~%"
	  (id ast)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class S--AST.
;;;
;;; Signed integer subtraction.
;;;
;;; Arguments (2):
;;;
;;;   * First operand, a signed integer.
;;;   * Second operand, a signed integer.
;;;
;;; Values (2):
;;;
;;;   * The difference of the first two operands modulo word size
;;;   * A Boolean indicating overflow.

(defclass s--ast (ast arguments-mixin)
  ())

(defun make-s--ast (argument-asts)
  (make-instance 's--ast
    :children argument-asts))

(defmethod stream-draw-ast ((ast s--ast) stream)
  (format stream "   ~a [label = \"s-\"];~%"
	  (id ast)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class NEG-AST.
;;;
;;; Arithmetic 2-complement negation.
;;;
;;; Arguments (1):
;;;
;;;   * A signed integer.
;;;
;;; Values (2):
;;;
;;;   * The negative value of the argument. 
;;;   * A Boolean indicating overflow.

(defclass neg-ast (ast arguments-mixin)
  ())

(defun make-neg-ast (argument-asts)
  (make-instance 'neg-ast
    :children argument-asts))

(defmethod stream-draw-ast ((ast neg-ast) stream)
  (format stream "   ~a [label = \"neg\"];~%"
	  (id ast)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class &-AST.
;;;
;;; Bitwise logical AND.
;;;
;;; Arguments (2):
;;;
;;;   * First operand, a word.
;;;   * Second operand, a word.
;;;
;;; Values (1):
;;;
;;;   * The bitwise logical AND between the two words.

(defclass &-ast (ast arguments-mixin)
  ())

(defun make-&-ast (argument-asts)
  (make-instance '&-ast
    :children argument-asts))

(defmethod stream-draw-ast ((ast &-ast) stream)
  (format stream "   ~a [label = \"&\"];~%"
	  (id ast)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class IOR-AST.
;;;
;;; Bitwise logical inclusive OR.
;;;
;;; Arguments (2):
;;;
;;;   * First operand, a word.
;;;   * Second operand, a word.
;;;
;;; Values (1):
;;;
;;;   * The bitwise logical inclusive OR between the two words.

(defclass ior-ast (ast arguments-mixin)
  ())

(defun make-ior-ast (argument-asts)
  (make-instance 'ior-ast
    :children argument-asts))

(defmethod stream-draw-ast ((ast ior-ast) stream)
  (format stream "   ~a [label = \"ior\"];~%"
	  (id ast)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class XOR-AST.
;;;
;;; Bitwise logical exclusive OR.
;;;
;;; Arguments (2):
;;;
;;;   * First operand, a word.
;;;   * Second operand, a word.
;;;
;;; Values (1):
;;;
;;;   * The bitwise logical exclusive OR between the two words.

(defclass xor-ast (ast arguments-mixin)
  ())

(defun make-xor-ast (argument-asts)
  (make-instance 'xor-ast
    :children argument-asts))

(defmethod stream-draw-ast ((ast xor-ast) stream)
  (format stream "   ~a [label = \"xor\"];~%"
	  (id ast)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class ~-AST.
;;;
;;; Bitwise logical inverse.
;;;
;;; Arguments (1):
;;;
;;;   * A word.
;;;
;;; Values (1):
;;;
;;;   * The bitwise logical inverse of the argument.

(defclass ~-ast (ast arguments-mixin)
  ())

(defun make-~-ast (argument-asts)
  (make-instance '~-ast
    :children argument-asts))

(defmethod stream-draw-ast ((ast ~-ast) stream)
  (format stream "   ~a [label = \"~\"];~%"
	  (id ast)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class ==-AST.
;;;
;;; Word equality.
;;;
;;; Arguments (2):
;;;
;;;   * First operand, a word.
;;;   * Second operand, a word.
;;;
;;; Values (1):
;;;
;;;   * A Boolean indicating whether the two are the same.

(defclass ==-ast (ast arguments-mixin)
  ())

(defun make-==-ast (argument-asts)
  (make-instance '==-ast
    :children argument-asts))

(defmethod stream-draw-ast ((ast ==-ast) stream)
  (format stream "   ~a [label = \"==\"];~%"
	  (id ast)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class S<-AST.
;;;
;;; Signed integer less than.
;;;
;;; Arguments (2):
;;;
;;;   * First operand, a signed integer.
;;;   * Second operand, a signed integer.
;;;
;;; Values (1):
;;;
;;;   * A Boolean indicating whether the first is less than the second.

(defclass s<-ast (ast arguments-mixin)
  ())

(defun make-s<-ast (argument-asts)
  (make-instance 's<-ast
    :children argument-asts))

(defmethod stream-draw-ast ((ast s<-ast) stream)
  (format stream "   ~a [label = \"s<\"];~%"
	  (id ast)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class S<=-AST.
;;;
;;; Signed integer less than or equal.
;;;
;;; Arguments (2):
;;;
;;;   * First operand, a signed integer.
;;;   * Second operand, a signed integer.
;;;
;;; Values (1):
;;;
;;;   * A Boolean indicating whether the first is less than or equal
;;;     to the second. 

(defclass s<=-ast (ast arguments-mixin)
  ())

(defun make-s<=-ast (argument-asts)
  (make-instance 's<=-ast
    :children argument-asts))

(defmethod stream-draw-ast ((ast s<=-ast) stream)
  (format stream "   ~a [label = \"s<=\"];~%"
	  (id ast)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class U<-AST.
;;;
;;; Unsigned integer less than.
;;;
;;; Arguments (2):
;;;
;;;   * First operand, an unsigned integer.
;;;   * Second operand, an unsigned integer.
;;;
;;; Values (1):
;;;
;;;   * A Boolean indicating whether the first is less than the second.

(defclass u<-ast (ast arguments-mixin)
  ())

(defun make-u<-ast (argument-asts)
  (make-instance 'u<-ast
    :children argument-asts))

(defmethod stream-draw-ast ((ast u<-ast) stream)
  (format stream "   ~a [label = \"u<\"];~%"
	  (id ast)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class U<=-AST.
;;;
;;; Unsigned integer less than or equal.
;;;
;;; Arguments (2):
;;;
;;;   * First operand, an unsigned integer.
;;;   * Second operand, an unsigned integer.
;;;
;;; Values (1):
;;;
;;;   * A Boolean indicating whether the first is less than or equal
;;;     to the second. 

(defclass u<=-ast (ast arguments-mixin)
  ())

(defun make-u<=-ast (argument-asts)
  (make-instance 'u<=-ast
    :children argument-asts))

(defmethod stream-draw-ast ((ast u<=-ast) stream)
  (format stream "   ~a [label = \"u<=\"];~%"
	  (id ast)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class ARGCOUNT-AST.
;;;
;;; The value is the argument count to a function, represented as a
;;; fixnum.

(defclass argcount-ast (ast)
  ())

(defun make-argcount-ast ()
  (make-instance 'argcount-ast))

(defmethod stream-draw-ast ((ast argcount-ast) stream)
  (format stream "   ~a [style = filled, fillcolor = orange];~%" (id ast))
  (format stream "   ~a [label = \"AC\"];~%"
	  (id ast)))

(defmethod children ((ast argcount-ast))
  '())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class ARG-AST.
;;;
;;; The value is one argument to a function.  The argument can be any
;;; AST, but it is typically very simple, such as a constant or a
;;; lexical variable.  The argument is interpreted as a fixnum. 

(defclass arg-ast (ast)
  ())

(defun make-arg-ast (index-ast)
  (make-instance 'arg-ast
    :children (list index-ast)))

(defmethod stream-draw-ast ((ast arg-ast) stream)
  (format stream "   ~a [style = filled, fillcolor = orange];~%" (id ast))
  (format stream "   ~a [label = \"arg\"];~%"
	  (id ast)))

(defmethod index-ast ((ast arg-ast))
  (first (children ast)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class HALT-AST.
;;;
;;; Halt the processor.

(defclass halt-ast (ast)
  ())

(defun make-halt-ast ()
  (make-instance 'halt-ast))

(defmethod stream-draw-ast ((ast halt-ast) stream)
  (format stream "   ~a [label = \"halt\"];~%"
	  (id ast)))
