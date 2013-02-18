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
    (call-next-method)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class AST.  The base class for all AST classes.

(defclass ast () ())

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
;;; We also define ASTs that do not correspond to any Common Lisp
;;; special operators, because we simplify later code generation that
;;; way.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class CONSTANT-AST. 
;;;
;;; A CONSTANT-AST represents a constant expression.  A constant
;;; expression is one of the following:
;;; 
;;;   * A literal: any non-symbol atom.
;;;   
;;;   * A self-evaluating symbol: NIL, T, or a keyword symbol.
;;;
;;;   * A Symbols that have been define to be constant by the use of
;;;     DEFCONSTANT.  The value stored in the AST is not the symbol
;;;     itself, but the constant value of it.
;;;
;;;   * A quoted expression.  The value stored in the AST is the
;;;     expression stripped of the quoting form.

(defclass constant-ast (ast)
  ((%value :initarg :value :reader value)))

(defun make-constant-ast (value)
  (make-instance 'constant-ast :value value))

(defmethod stream-draw-ast ((ast constant-ast) stream)
  (format stream "   ~a [label = \"~a\"];~%"
	  (id ast)
	  (value ast)))

(defmethod children ((ast constant-ast))
  '())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class CALL-AST. 
;;;
;;; A CALL-AST represents a function call.  
;;;
;;; The callee can be a function-ast or a location.  An argument can
;;; be any AST. 

(defclass call-ast (ast)
  ((%callee-ast :initarg :callee-ast :reader callee-ast)
   (%argument-asts :initarg :argument-asts :reader argument-asts)))

(defun make-call-ast (callee-ast argument-asts)
  (make-instance 'call-ast
    :callee-ast callee-ast
    :argument-asts argument-asts))

(defmethod stream-draw-ast ((ast call-ast) stream)
  (format stream "   ~a [label = \"call\"];~%"
	  (id ast))
  (let ((location (callee-ast ast)))
    (stream-draw-ast location stream)
    (format stream "   ~a -> ~a~%" (id ast) (id location)))
  (loop for child in (argument-asts ast)
	do (stream-draw-ast child stream)
	   (format stream "   ~a -> ~a~%"
		   (id ast) (id child))))

(defmethod children ((ast call-ast))
  (cons (callee-ast ast)
	(argument-asts ast)))

;;; An &optional entry is one of:
;;;
;;;  * (lexical-location init-form-ast)
;;;  * (lexical-location init-form-ast lexical-location)
;;;
;;; Here, init-form-ast is an AST resulting from the conversion of an
;;; initialization form.  In the second variant, the last lexical
;;; location is assigned a Boolean value according to whether an
;;; optional argument was given.
;;;
;;; A &key entry is one of:
;;;
;;;  * ((keyword lexical-location) init-form-ast)
;;;  * ((keyword lexical-location) init-form-ast lexical-location)
;;;
;;; As with the &optional entry, init-form-ast is an AST resulting
;;; from the conversion of an initialization form.  In the second
;;; variant, the last lexical location is assigned a Boolean value
;;; according to whether an optional argument was given.
;;; 
;;; An &aux entry is of the form:
;;;
;;;  * (lexical-location init-form-ast)

;;; FIXME: handle special variables in lambda list. 

(defclass lambda-list ()
  (;; A possibly empty list of lexical locations.
   (%required :initarg :required :reader required)
   ;; A possibly empty list of optional entries. 
   (%optional :initarg :optional :reader optional)
   ;; Either NIL or a single lexical location. 
   (%rest-body :initarg :rest-body :reader rest-body)
   ;; Either:
   ;;  * :none, meaning &key was not given at all,
   ;;  * a possibly empty list of &key entries.
   (%keys :initarg :keys :reader keys)
   ;; Either:
   ;;  * nil, meaning &allow-other-keys was not given at all,
   ;;  * t, meaning &allow-other-keys was given.
   (%allow-other-keys :initarg :allow-other-keys :reader allow-other-keys)
   ;; A possibly empty list of &aux entries.
   (%aux :initarg :aux :reader aux)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class FUNCTION-AST.
;;;
;;; A function AST represents an explicit lambda expression, but also
;;; implicit lambda expressions such as the ones found in FLET and
;;; LABELS.

(defclass function-ast (ast)
  ((%lambda-list :initarg :lambda-list :reader lambda-list)
   (%body-ast :initarg :body-ast :reader body-ast)))

(defun make-function-ast (lambda-list body-ast)
  (make-instance 'function-ast
    :lambda-list lambda-list
    :body-ast body-ast))

(defmethod stream-draw-ast ((ast function-ast) stream)
  (format stream "   ~a [label = \"function\"];~%"
	  (id ast))
  (loop for param in (required (lambda-list ast))
	do (stream-draw-ast param stream)
	   (format stream "   ~a -> ~a~%"
		   (id ast)
		   (id param)))
  (stream-draw-ast (body-ast ast) stream)
  (format stream "   ~a -> ~a~%"
	  (id ast)
	  (id (body-ast ast))))
  
(defmethod children ((ast function-ast))
  (list (body-ast ast)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class MULTIPLE-VALUE-PROG1-AST.
;;;
;;; The HyperSpec says that MULTIPLE-VALUE-PROG1 takes at least one
;;; argument.  All the arguments are forms.  The first argument form
;;; is evaluated first and all the values are saved.  Then the
;;; remaining forms are evaluated from left to right and their values
;;; are discarded.  

(defclass multiple-value-prog1-ast (ast)
  ((%first-ast :initarg :first-ast :reader first-ast)
   (%body-asts :initarg :body-asts :reader body-asts)))

(defun make-multiple-value-prog1-ast (first-ast body-asts)
  (make-instance 'multiple-value-prog1-ast
    :first-ast first-ast
    :body-asts body-asts))

(defmethod stream-draw-ast ((ast multiple-value-prog1-ast) stream)
  (declare (ignore stream))
  (error "don't know how to draw a multiple-value-prog1-ast yet"))

(defmethod children ((ast multiple-value-prog1-ast))
  (cons (first-ast ast)
	(body-asts ast)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class MULTIPLE-VALUE-CALL-AST.
;;; 
;;; The HyperSpec says that MULTIPLE-VALUE-CALL takes at least one
;;; argument.  All the arguments are forms.  The first argument is
;;; evaluated first, and the result must be a function.  Then the
;;; remaining forms are evaluated from left to right and all the
;;; values from each form are then used as arguments to the function.
;;; In other words, the number of arguments passed to the function are
;;; the sum of the number of values of each argument form (except the
;;; first).

(defclass multiple-value-call-ast (ast)
  ((%callee-ast :initarg :callee-ast :reader callee-ast)
   (%argument-asts :initarg :argument-asts :reader argument-asts)))

(defun make-multiple-value-call-ast (callee-ast argument-asts)
  (make-instance 'multiple-value-call-ast
    :callee-ast callee-ast
    :argument-asts argument-asts))

;;; FIXME: define a method on STREAM-DRAW-AST specialized to PROGV-AST.

(defmethod children ((ast multiple-value-call-ast))
  (cons (callee-ast ast)
	(argument-asts ast)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class PROGN-AST.

(defclass progn-ast (ast)
  ((%form-asts :initarg :form-asts :reader form-asts)))

(defun make-progn-ast (form-asts)
  (make-instance 'progn-ast
    :form-asts form-asts))

(defmethod stream-draw-ast ((ast progn-ast) stream)
  (format stream "   ~a [label = \"progn\"];~%"
	  (id ast))
  (loop for child in (form-asts ast)
	do (stream-draw-ast child stream)
	   (format stream "   ~a -> ~a~%"
		   (id ast) (id child))))

(defmethod children ((ast progn-ast))
  (form-asts ast))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class PROGV-AST.

(defclass progv-ast (ast)
  ((%symbols-ast :initarg :symbols-ast :reader symbols-ast)
   (%vals-ast :initarg :vals-ast :reader vals-ast)
   (%body-ast :initarg :body-ast :reader body-ast)))

(defun make-progv-ast (symbols-ast vals-ast body-ast)
  (make-instance 'progv-ast
    :symbols-ast symbols-ast
    :vals-ast vals-ast
    :body-ast body-ast))

;;; FIXME: define a method on STREAM-DRAW-AST specialized to PROGV-AST.

(defmethod children ((ast progv-ast))
  (append (symbols-ast ast)
	  (vals-ast ast)
	  (list (body-ast ast))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class BLOCK-AST.

(defclass block-ast (ast)
  ((%body-ast :initarg :body-ast :accessor body-ast)))

(defun make-block-ast (body-ast)
  (make-instance 'block-ast
    :body-ast body-ast))

(defmethod stream-draw-ast ((ast block-ast) stream)
  (format stream "   ~a [label = \"block\"];~%"
	  (id ast))
  (stream-draw-ast (body-ast ast) stream)
  (format stream "   ~a -> ~a~%"
	  (id ast)
	  (id (body-ast ast))))
  
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

(defmethod stream-draw-ast ((ast return-from-ast) stream)
  (format stream "   ~a [label = \"return-from\"];~%"
	  (id ast))
  (format stream "   ~a -> ~a~%"
	  (id ast)
	  (id (block-ast ast)))
  (stream-draw-ast (form-ast ast) stream)
  (format stream "   ~a -> ~a~%"
	  (id ast)
	  (id (form-ast ast))))
  
(defmethod children ((ast return-from-ast))
  (list (form-ast ast)))

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

(defmethod stream-draw-ast ((ast setq-ast) stream)
  (format stream "   ~a [label = \"setq\"];~%"
	  (id ast))
  (stream-draw-ast (lhs-ast ast) stream)
  (format stream "   ~a -> ~a~%"
	  (id ast)
	  (id (lhs-ast ast)))
  (stream-draw-ast (value-ast ast) stream)
  (format stream "   ~a -> ~a~%"
	  (id ast)
	  (id (value-ast ast))))

(defmethod children ((ast setq-ast))
  (list (lhs-ast ast)
	(value-ast ast)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class TAG-AST.

(defclass tag-ast (ast)
  ((%name :initarg :name :reader name)))

(defun make-tag-ast (name)
  (make-instance 'tag-ast
    :name name))

(defmethod children ((ast tag-ast))
  '())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class TAGBODY-AST.

(defclass tagbody-ast (ast)
  ((%items :initarg :items :reader items)))

(defun make-tagbody-ast (items)
  (make-instance 'tagbody-ast
    :items items))

(defmethod stream-draw-ast ((ast tagbody-ast) stream)
  (format stream "   ~a [label = \"tagbody\"];~%"
	  (id ast))
  (loop for item in (items ast)
	do (stream-draw-ast item stream)
	   (format stream "   ~a -> ~a~%"
		   (id ast) (id item))))

(defmethod children ((ast tagbody-ast))
  (items ast))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class GO-AST.

(defclass go-ast (ast)
  ((%tag-ast :initarg :tag-ast :reader tag-ast)))

(defun make-go-ast (tag-ast)
  (make-instance 'go-ast
    :tag-ast tag-ast))

(defmethod stream-draw-ast ((ast go-ast) stream)
  (format stream "   ~a [label = \"go\"];~%"
	  (id ast))
  (stream-draw-ast (tag-ast ast) stream)
  (format stream "   ~a -> ~a~%"
	  (id ast) (id (tag-ast ast))))

(defmethod children ((ast go-ast))
  '())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class THE-AST.

(defclass the-ast (ast)
  ((%value-type :initarg :value-type :reader value-type)
   (%form-ast :initarg :form-ast :reader form-ast)))

(defun make-the-ast (value-type form-ast)
  (make-instance 'the-ast
    :value-type value-type
    :form-ast form-ast))

;;; FIXME: define a method on STREAM-DRAW-AST specialized to THE-AST.

(defmethod children ((ast the-ast))
  (list (form-ast ast)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class CATCH-AST.

(defclass catch-ast (ast)
  ((%tag-ast :initarg :tag-ast :reader tag-ast)
   (%body-ast :initarg :body-ast :reader body-ast)))

(defun make-catch-ast (tag-ast body-ast)
  (make-instance 'catch-ast
    :tag-ast tag-ast
    :body-ast body-ast))

;;; FIXME: define a method on STREAM-DRAW-AST specialized to CATCH-AST.

(defmethod children ((ast catch-ast))
  (list (tag-ast ast)
	(body-ast ast)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class THROW-AST.

(defclass throw-ast (ast)
  ((%tag-ast :initarg :tag-ast :reader tag-ast)
   (%form-ast :initarg :form-ast :reader form-ast)))

(defun make-throw-ast (tag-ast form-ast)
  (make-instance 'throw-ast
    :tag-ast tag-ast
    :form-ast form-ast))

;;; FIXME: define a method on STREAM-DRAW-AST specialized to THROW-AST.

(defmethod children ((ast throw-ast))
  (list (tag-ast ast) (form-ast ast)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class UNWIND-PROTECT-AST.

(defclass unwind-protect-ast (ast)
  ((%protected-ast :initarg :protected-ast :reader protected-ast)
   (%cleanup-form-asts :initarg :cleanup-form-asts :reader cleanup-form-asts)))

(defun make-unwind-protect-ast (protected-ast cleanup-form-asts)
  (make-instance 'unwind-protect-ast
    :protected-ast protected-ast
    :cleanup-form-asts cleanup-form-asts))

;;; FIXME: define a method on STREAM-DRAW-AST specialized to
;;; UNWIND-PROTECT-AST.

(defmethod children ((ast unwind-protect-ast))
  (cons (protected-ast ast)
	(cleanup-form-asts ast)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class EVAL-WHEN-AST.

(defclass eval-when-ast (ast)
  ((%situations :initarg :situations :reader situations)
   (%body-ast :initarg :body-ast :reader body-ast)))

(defun make-eval-when-ast (situations body-ast)
  (make-instance 'eval-when-ast
    :situations situations
    :body-ast body-ast))

;;; FIXME: define a method on STREAM-DRAW-AST specialized to EVAL-WHEN-AST.

(defmethod children ((ast eval-when-ast))
  (list (body-ast ast)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class LOAD-TIME-VALUE-AST.

(defclass load-time-value-ast (ast)
  ((%form-ast :initarg :form-ast :reader form-ast)
   (%read-only-p :initarg :read-only-p :reader read-only-p)))

(defun make-load-time-value-ast (form-ast read-only-p)
  (make-instance 'load-time-value-ast
    :form-ast form-ast
    :read-only-p read-only-p))

;;; FIXME: define a method on STREAM-DRAW-AST specialized to
;;; LOAD-TIME-VALUE-AST.

(defmethod children ((ast load-time-value-ast))
  (list (form-ast ast)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class IF-AST.

(defclass if-ast (ast)
  ((%test-ast :initarg :test-ast :reader test-ast)
   (%then-ast :initarg :then-ast :reader then-ast)
   (%else-ast :initarg :else-ast :reader else-ast)))

(defun make-if-ast (test-ast then-ast else-ast)
  (make-instance 'if-ast
    :test-ast test-ast
    :then-ast then-ast
    :else-ast else-ast))

(defmethod stream-draw-ast ((ast if-ast) stream)
  (format stream "   ~a [label = \"if\"];~%"
	  (id ast))
  (stream-draw-ast (test-ast ast) stream)
  (stream-draw-ast (then-ast ast) stream)
  (stream-draw-ast (else-ast ast) stream)
  (format stream "   ~a -> ~a~%"
	  (id ast)
	  (id (test-ast ast)))
  (format stream "   ~a -> ~a~%"
	  (id ast)
	  (id (then-ast ast)))
  (format stream "   ~a -> ~a~%"
	  (id ast)
	  (id (else-ast ast))))

(defmethod children ((ast if-ast))
  (list (test-ast ast)
	(then-ast ast)
	(else-ast ast)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Define a method on STREAM-DRAW-AST and on CHILDREN for a
;;; LEXICAL-LOCATION-INFO, GLOBAL-LOCATION-INFO and
;;; SPECIAL-LOCATION-INFO objects from the environment, because they
;;; can be elements of an AST.

(defun draw-location (location stream &optional (color "blue"))
  (when (null (gethash location *table*))
    (setf (gethash location *table*) (gensym))
    (format stream
	    "  ~a [label = \"~a\"];~%"
	    (id location) (sicl-env:name location))
    (format stream
	    "  ~a [style = filled, fillcolor = ~a];~%"
	    (id location) color)))

(defmethod stream-draw-ast ((ast sicl-env:lexical-location-info) stream)
  (format stream "   ~a [label = \"lexical\"];~%"
	  (id ast))
  (draw-location (sicl-env:location ast) stream "yellow")
  (format stream "   ~a -> ~a~%"
	  (id ast)
	  (id (sicl-env:location ast))))
  
(defmethod children ((ast sicl-env:lexical-location-info))
  '())

(defmethod stream-draw-ast ((ast sicl-env:global-location-info) stream)
  (format stream "   ~a [label = \"global\"];~%"
	  (id ast))
  (draw-location (sicl-env:location ast) stream "green")
  (format stream "   ~a -> ~a~%"
	  (id ast)
	  (id (sicl-env:location ast))))
  
(defmethod children ((ast sicl-env:global-location-info))
  '())

(defmethod stream-draw-ast ((ast sicl-env:special-location-info) stream)
  (format stream "   ~a [label = \"special\"];~%"
	  (id ast))
  (draw-location (sicl-env:location ast) stream "red")
  (format stream "   ~a -> ~a~%"
	  (id ast)
	  (id (sicl-env:location ast))))

(defmethod children ((ast sicl-env:special-location-info))
  '())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; AST classes for low-level operators.
;;;

(defclass arguments-mixin ()
  ((%argument-asts :initarg :argument-asts :accessor argument-asts)))

(defmethod children ((ast arguments-mixin))
  (argument-asts ast))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class WORD.
;;;
;;; Immediate word value. 
;;;
;;; No arguments.
;;;
;;; Values: (1):
;;;
;;;   * The positive or negative value truncated to the 
;;;     number of bits in a word on the target platform.

(defclass word-ast (ast)
  ((%value :initarg :value :reader value)))

(defun make-word-ast (value)
  (make-instance 'word-ast :value value))

(defmethod children ((ast word-ast))
  '())

(defmethod stream-draw-ast ((ast word-ast) stream)
  (format stream "   ~a [label = \"~d\"];~%" (id ast) (value ast))
  (format stream "   ~a [style = filled, fillcolor = blue];~%" (id ast)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class MEMALLOC-AST.
;;;
;;; Allocate a sequence of bytes of memory.
;;;
;;; Arguments (1):
;;;
;;;   * The number of bytes to be allocated.
;;;
;;; Values (1):
;;;
;;;   * A pointer to the first byte allocated.

(defclass memalloc-ast (ast arguments-mixin)
  ())

(defun make-memalloc-ast (argument-asts)
  (make-instance 'memalloc-ast
    :argument-asts argument-asts))

(defmethod stream-draw-ast ((ast memalloc-ast) stream)
  (format stream "   ~a [label = \"memalloc\"];~%"
	  (id ast))
  (loop for argument-ast in (argument-asts ast)
	do (stream-draw-ast argument-ast stream)
	   (format stream "   ~a -> ~a~%" (id ast) (id argument-ast))))

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

(defun make-memref-ast (argument-asts)
  (make-instance 'memref-ast
    :argument-asts argument-asts))

(defmethod stream-draw-ast ((ast memref-ast) stream)
  (format stream "   ~a [label = \"memref\"];~%"
	  (id ast))
  (loop for argument-ast in (argument-asts ast)
	do (stream-draw-ast argument-ast stream)
	   (format stream "   ~a -> ~a~%" (id ast) (id argument-ast))))

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
    :argument-asts argument-asts))

(defmethod stream-draw-ast ((ast memset-ast) stream)
  (format stream "   ~a [label = \"memset\"];~%"
	  (id ast))
  (loop for argument-ast in (argument-asts ast)
	do (stream-draw-ast argument-ast stream)
	   (format stream "   ~a -> ~a~%" (id ast) (id argument-ast))))

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

(defclass u+-ast (ast arguments-mixin)
  ())

(defun make-u+-ast (argument-asts)
  (make-instance 'u+-ast
    :argument-asts argument-asts))

(defmethod stream-draw-ast ((ast u+-ast) stream)
  (format stream "   ~a [label = \"u+\"];~%"
	  (id ast))
  (loop for argument-ast in (argument-asts ast)
	do (stream-draw-ast argument-ast stream)
	   (format stream "   ~a -> ~a~%" (id ast) (id argument-ast))))

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
;;;   * A Boolean indicating carry out.

(defclass u--ast (ast arguments-mixin)
  ())

(defun make-u--ast (argument-asts)
  (make-instance 'u--ast
    :argument-asts argument-asts))

(defmethod stream-draw-ast ((ast u--ast) stream)
  (format stream "   ~a [label = \"u-\"];~%"
	  (id ast))
  (loop for argument-ast in (argument-asts ast)
	do (stream-draw-ast argument-ast stream)
	   (format stream "   ~a -> ~a~%" (id ast) (id argument-ast))))

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
    :argument-asts argument-asts))

(defmethod stream-draw-ast ((ast s+-ast) stream)
  (format stream "   ~a [label = \"s+\"];~%"
	  (id ast))
  (loop for argument-ast in (argument-asts ast)
	do (stream-draw-ast argument-ast stream)
	   (format stream "   ~a -> ~a~%" (id ast) (id argument-ast))))

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
    :argument-asts argument-asts))

(defmethod stream-draw-ast ((ast s--ast) stream)
  (format stream "   ~a [label = \"s-\"];~%"
	  (id ast))
  (loop for argument-ast in (argument-asts ast)
	do (stream-draw-ast argument-ast stream)
	   (format stream "   ~a -> ~a~%" (id ast) (id argument-ast))))

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
    :argument-asts argument-asts))

(defmethod stream-draw-ast ((ast neg-ast) stream)
  (format stream "   ~a [label = \"neg\"];~%"
	  (id ast))
  (loop for argument-ast in (argument-asts ast)
	do (stream-draw-ast argument-ast stream)
	   (format stream "   ~a -> ~a~%" (id ast) (id argument-ast))))

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
    :argument-asts argument-asts))

(defmethod stream-draw-ast ((ast &-ast) stream)
  (format stream "   ~a [label = \"&\"];~%"
	  (id ast))
  (loop for argument-ast in (argument-asts ast)
	do (stream-draw-ast argument-ast stream)
	   (format stream "   ~a -> ~a~%" (id ast) (id argument-ast))))

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
    :argument-asts argument-asts))

(defmethod stream-draw-ast ((ast ior-ast) stream)
  (format stream "   ~a [label = \"ior\"];~%"
	  (id ast))
  (loop for argument-ast in (argument-asts ast)
	do (stream-draw-ast argument-ast stream)
	   (format stream "   ~a -> ~a~%" (id ast) (id argument-ast))))

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
    :argument-asts argument-asts))

(defmethod stream-draw-ast ((ast xor-ast) stream)
  (format stream "   ~a [label = \"xor\"];~%"
	  (id ast))
  (loop for argument-ast in (argument-asts ast)
	do (stream-draw-ast argument-ast stream)
	   (format stream "   ~a -> ~a~%" (id ast) (id argument-ast))))

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
    :argument-asts argument-asts))

(defmethod stream-draw-ast ((ast ~-ast) stream)
  (format stream "   ~a [label = \"~\"];~%"
	  (id ast))
  (loop for argument-ast in (argument-asts ast)
	do (stream-draw-ast argument-ast stream)
	   (format stream "   ~a -> ~a~%" (id ast) (id argument-ast))))

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
    :argument-asts argument-asts))

(defmethod stream-draw-ast ((ast ==-ast) stream)
  (format stream "   ~a [label = \"==\"];~%"
	  (id ast))
  (loop for argument-ast in (argument-asts ast)
	do (stream-draw-ast argument-ast stream)
	   (format stream "   ~a -> ~a~%" (id ast) (id argument-ast))))

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
    :argument-asts argument-asts))

(defmethod stream-draw-ast ((ast s<-ast) stream)
  (format stream "   ~a [label = \"s<\"];~%"
	  (id ast))
  (loop for argument-ast in (argument-asts ast)
	do (stream-draw-ast argument-ast stream)
	   (format stream "   ~a -> ~a~%" (id ast) (id argument-ast))))

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
    :argument-asts argument-asts))

(defmethod stream-draw-ast ((ast s<=-ast) stream)
  (format stream "   ~a [label = \"s<=\"];~%"
	  (id ast))
  (loop for argument-ast in (argument-asts ast)
	do (stream-draw-ast argument-ast stream)
	   (format stream "   ~a -> ~a~%" (id ast) (id argument-ast))))

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
    :argument-asts argument-asts))

(defmethod stream-draw-ast ((ast u<-ast) stream)
  (format stream "   ~a [label = \"u<\"];~%"
	  (id ast))
  (loop for argument-ast in (argument-asts ast)
	do (stream-draw-ast argument-ast stream)
	   (format stream "   ~a -> ~a~%" (id ast) (id argument-ast))))

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
    :argument-asts argument-asts))

(defmethod stream-draw-ast ((ast u<=-ast) stream)
  (format stream "   ~a [label = \"u<=\"];~%"
	  (id ast))
  (loop for argument-ast in (argument-asts ast)
	do (stream-draw-ast argument-ast stream)
	   (format stream "   ~a -> ~a~%" (id ast) (id argument-ast))))

