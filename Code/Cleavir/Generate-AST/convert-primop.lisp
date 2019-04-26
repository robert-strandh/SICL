(cl:in-package #:cleavir-generate-ast)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting CLEAVIR-PRIMOP:EQ.

(defmethod convert-special
    ((symbol (eql 'cleavir-primop:eq)) form env system)
  (db origin (op arg1 arg2) form
    (declare (ignore op))
    (cleavir-ast:make-eq-ast
     (convert arg1 env system)
     (convert arg2 env system))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting CLEAVIR-PRIMOP:TYPEQ.

(defmethod convert-special
    ((symbol (eql 'cleavir-primop:typeq)) form env system)
  (db origin (op arg1 arg2) form
    (declare (ignore op))
    (cleavir-ast:make-typeq-ast
     (convert arg1 env system)
     arg2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting CLEAVIR-PRIMOP:CAR.

(defmethod convert-special
    ((symbol (eql 'cleavir-primop:car)) form env system)
  (db origin (op arg) form
    (declare (ignore op))
    (cleavir-ast:make-car-ast (convert arg env system))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting CLEAVIR-PRIMOP:CDR.

(defmethod convert-special
    ((symbol (eql 'cleavir-primop:cdr)) form env system)
  (db origin (op arg) form
    (declare (ignore op))
    (cleavir-ast:make-cdr-ast (convert arg env system))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting CLEAVIR-PRIMOP:RPLACA.

(defmethod convert-special
    ((symbol (eql 'cleavir-primop:rplaca)) form env system)
  (db origin (op arg1 arg2) form
    (declare (ignore op))
    (cleavir-ast:make-rplaca-ast (convert arg1 env system)
				 (convert arg2 env system))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting CLEAVIR-PRIMOP:RPLACD.

(defmethod convert-special
    ((symbol (eql 'cleavir-primop:rplacd)) form env system)
  (db origin (op arg1 arg2) form
    (declare (ignore op))
    (cleavir-ast:make-rplacd-ast (convert arg1 env system)
				 (convert arg2 env system))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting CLEAVIR-PRIMOP:FIXNUM-ARITHMETIC.

(defmethod convert-special
    ((symbol (eql 'cleavir-primop:fixnum-arithmetic)) form env system)
  (cleavir-code-utilities:check-form-proper-list form)
  (cleavir-code-utilities:check-argcount form 4 4)
  (destructuring-bind (variable operation normal overflow) (cdr form)
    (assert (symbolp variable))
    (let ((new-env (cleavir-env:add-lexical-variable env variable)))
      (cleavir-ast:make-if-ast (convert operation new-env system)
			       (convert normal new-env system)
			       (convert overflow new-env system)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting CLEAVIR-PRIMOP:FIXNUM-ADD.

(defmethod convert-special
    ((symbol (eql 'cleavir-primop:fixnum-add)) form env system)
  (db origin (op arg1 arg2 variable) form
    (declare (ignore op))
    (cleavir-ast:make-fixnum-add-ast (convert arg1 env system)
				     (convert arg2 env system)
				     (convert variable env system))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting CLEAVIR-PRIMOP:FIXNUM-SUB.

(defmethod convert-special
    ((symbol (eql 'cleavir-primop:fixnum-sub)) form env system)
  (db origin (op arg1 arg2 variable) form
    (declare (ignore op))
    (cleavir-ast:make-fixnum-sub-ast (convert arg1 env system)
				     (convert arg2 env system)
				     (convert variable env system))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting CLEAVIR-PRIMOP:FIXNUM-LESS.

(defmethod convert-special
    ((symbol (eql 'cleavir-primop:fixnum-less)) form env system)
  (destructuring-bind (arg1 arg2) (cdr form)
    (make-instance 'cleavir-ast:fixnum-less-ast
      :arg1-ast (convert arg1 env system)
      :arg2-ast (convert arg2 env system))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting CLEAVIR-PRIMOP:FIXNUM-NOT-GREATER.

(defmethod convert-special
    ((symbol (eql 'cleavir-primop:fixnum-not-greater)) form env system)
  (destructuring-bind (arg1 arg2) (cdr form)
    (make-instance 'cleavir-ast:fixnum-not-greater-ast
      :arg1-ast (convert arg1 env system)
      :arg2-ast (convert arg2 env system))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting CLEAVIR-PRIMOP:FIXNUM-GREATER.

(defmethod convert-special
    ((symbol (eql 'cleavir-primop:fixnum-greater)) form env system)
  (destructuring-bind (arg1 arg2) (cdr form)
    (make-instance 'cleavir-ast:fixnum-greater-ast
      :arg1-ast (convert arg1 env system)
      :arg2-ast (convert arg2 env system))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting CLEAVIR-PRIMOP:FIXNUM-NOT-LESS.

(defmethod convert-special
    ((symbol (eql 'cleavir-primop:fixnum-not-less)) form env system)
  (destructuring-bind (arg1 arg2) (cdr form)
    (make-instance 'cleavir-ast:fixnum-not-less-ast
      :arg1-ast (convert arg1 env system)
      :arg2-ast (convert arg2 env system))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting CLEAVIR-PRIMOP:FIXNUM-EQUAL.

(defmethod convert-special
    ((symbol (eql 'cleavir-primop:fixnum-equal)) form env system)
  (destructuring-bind (arg1 arg2) (cdr form)
    (make-instance 'cleavir-ast:fixnum-equal-ast
      :arg1-ast (convert arg1 env system)
      :arg2-ast (convert arg2 env system))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting CLEAVIR-PRIMOP:LET-UNINITIALIZED.
;;;
;;; A form using the operator LET-UNINITIALIZED has the following
;;; syntax:
;;;
;;; (CLEAVIR-PRIMOP:LET-UNINITIALIZED (var*) form*)
;;;
;;; It sole purpose is to create a lexical environment for the
;;; variables in which the forms are evaluated.  An absolute
;;; requirement is that the variables must be assigned to before they
;;; are used in the forms, or else things will fail spectacularly.

(defmethod convert-special
    ((symbol (eql 'cleavir-primop:let-uninitialized)) form env system)
  (destructuring-bind (variables &rest body) (rest form)
    (let ((new-env env))
      (loop for variable in variables
	    for variable-ast = (cleavir-ast:make-lexical-ast variable)
	    do (setf new-env
		     (cleavir-env:add-lexical-variable
		      new-env variable variable-ast)))
      (process-progn (convert-sequence body new-env system)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting floating point 2-argument arithmetic.
;;;
;;; These primitive operations are used in the implementation of
;;; the Common Lisp functions +, -, *, and /. They all take two
;;; argument, both forms that must evaluate to values of the
;;; appropriate float type.
;;;
;;; The value of a form with this operator is a float of the
;;; type, representing the results of the operation on the two
;;; arguments.
;;;
;;; If the result is greater than the value of the constant
;;; MOST-POSITIVE-something-FLOAT, where "something" is the
;;; float type, an error of type FLOATING-POINT-OVERFLOW is
;;; signaled. Similarly if the result is less than
;;; MOST-NEGATIVE-something-FLOAT.

(defmacro define-float-binop (primop ast)
  `(defmethod convert-special
       ((symbol (eql ',primop)) form env system)
     (db origin (op type arg1 arg2) form
       (declare (ignore op))
       (make-instance ',ast
         :subtype type
         :arg1-ast (convert arg1 env system)
         :arg2-ast (convert arg2 env system)
         :origin origin))))


(define-float-binop cleavir-primop:float-add
  cleavir-ast:float-add-ast)
(define-float-binop cleavir-primop:float-sub
  cleavir-ast:float-sub-ast)
(define-float-binop cleavir-primop:float-mul
  cleavir-ast:float-mul-ast)
(define-float-binop cleavir-primop:float-div
  cleavir-ast:float-div-ast)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting floating point comparisons.
;;;
;;; These primitive operations are used in the implementation of
;;; the Common Lisp functions <, <=, =, >=, and >. They take two
;;; arguments, both forms that must evaluate to floats of the
;;; appropriate type. They can only appear as the TEST-FORM in
;;; the special form IF.
;;;
;;; If the comparison is met, the THEN branch of the IF form
;;; is evaluated. Otherwise, the ELSE branch.
;;;
;;; For example, with SHORT-FLOAT-LESS, if the value of the
;;; first form, which is a short float, is strictly less than the
;;; value of the second form, also a short float, the THEN branch
;;; is taken, and otherwise the ELSE branch is taken.

(define-float-binop cleavir-primop:float-less
  cleavir-ast:float-less-ast)
(define-float-binop cleavir-primop:float-not-greater
  cleavir-ast:float-not-greater-ast)
(define-float-binop cleavir-primop:float-equal
  cleavir-ast:float-equal-ast)
(define-float-binop cleavir-primop:float-not-less
  cleavir-ast:float-not-less-ast)
(define-float-binop cleavir-primop:float-greater
  cleavir-ast:float-greater-ast)

(defmacro define-float-unop (primop ast)
  `(defmethod convert-special
       ((symbol (eql ',primop)) form env system)
     (db origin (op type arg) form
       (declare (ignore op))
       (make-instance ',ast
         :subtype type
         :arg-ast (convert arg env system)
         :origin origin))))

(define-float-unop cleavir-primop:float-sin
  cleavir-ast:float-sin-ast)
(define-float-unop cleavir-primop:float-cos
  cleavir-ast:float-cos-ast)
(define-float-unop cleavir-primop:float-sqrt
  cleavir-ast:float-sqrt-ast)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting CLEAVIR-PRIMOP:COERCE.
;;;
;;; This primop is used to convert between number types. It takes
;;; three arguments. The first two are number types, and the last
;;; is a form, the only argument that is evaluated.
;;;
;;; The value of the form must be of the number type. It is
;;; converted into a new value of the second type.
;;;
;;; This primop can be used for implementing CL:COERCE and CL:FLOAT,
;;; as well as for arithmetic contagion.

(defmethod convert-special
    ((symbol (eql 'cleavir-primop:coerce)) form env system)
  (db origin (op type1 type2 form) form
    (declare (ignore op))
    (make-instance 'cleavir-ast:coerce-ast
     :from type1 :to type2
     :arg-ast (convert form env system)
     :origin origin)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting CLEAVIR-PRIMOP:FUNCALL.
;;;
;;; This primop is similar to the function CL:FUNCALL.  The difference
;;; is that the primop does not allow a function NAME as its first
;;; argument.  It has to be a form that evaluates to a function.
;;;
;;; In order to inline CL:FUNCALL, a possible strategy would be to
;;; define a compiler macro on that function that expands to a form
;;; that turns the first argument into a function if it is not already
;;; a function and then calls this primop.

(defmethod convert-special
    ((symbol (eql 'cleavir-primop:funcall)) form env system)
  (destructuring-bind (function-form . argument-forms) (rest form)
    (cleavir-ast:make-call-ast
     (convert function-form env system)
     (loop for argument-form in argument-forms
	   collect (convert argument-form env system)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting CLEAVIR-PRIMOP:MULTIPLE-VALUE-CALL.
;;;
;;; This primop is similar to the special operator CL:MULTIPLE-VALUE-CALL.
;;; The difference is that the primop does not allow a function NAME
;;; as its first argument.  It has to be a form that evaluates to a
;;; function.
;;;
;;; CL:MULTIPLE-VALUE-CALL can be defined as a macro expanding into
;;; a form that turns the first argument into a function if it is not
;;; already a function and then calls this primop.

(defmethod convert-special
    ((symbol (eql 'cleavir-primop:multiple-value-call)) form env system)
  (destructuring-bind (function-form . argument-forms) (rest form)
    (cleavir-ast:make-multiple-value-call-ast
     (convert function-form env system)
     (loop for argument-form in argument-forms
           collect (convert argument-form env system)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting CLEAVIR-PRIMOP:SLOT-READ.
;;;
;;; This primop takes two arguments.  The first argument is a form
;;; that must evaluate to a standard instance.  The second argument is
;;; a form that must evaluate to a fixnum and that indicates the slot
;;; number to be read.

(defmethod convert-special
    ((symbol (eql 'cleavir-primop:slot-read)) form env system)
  (destructuring-bind (instance-form slot-number-form) (rest form)
    (cleavir-ast:make-slot-read-ast
     (convert instance-form env system)
     (convert slot-number-form env system))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting CLEAVIR-PRIMOP:SLOT-WRITE.
;;;
;;; This primop takes three arguments.  The first argument is a form
;;; that must evaluate to a standard instance.  The second argument is
;;; a form that must evaluate to a fixnum and that indicates the slot
;;; number to be written.  The third argument is a form that evaluates
;;; to the object that will be written to the slot.

(defmethod convert-special
    ((symbol (eql 'cleavir-primop:slot-write)) form env system)
  (destructuring-bind (instance-form slot-number-form value-form) (rest form)
    (cleavir-ast:make-slot-write-ast
     (convert instance-form env system)
     (convert slot-number-form env system)
     (convert value-form env system))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting CLEAVIR-PRIMOP:AREF.
;;;
;;; This primop takes five arguments. The first and second are an
;;; array and an index into it, as forms. The remainder of the
;;; arguments are not evaluated. The third is the actual element
;;; type of the array. The fourth is whether the array is actually
;;; simple. The fifth is whether the value in the array is already
;;; boxed.

(defmethod convert-special
    ((symbol (eql 'cleavir-primop:aref)) form env system)
  (db origin (array-form index-form type simple-p boxed-p)
      (rest form)
    (make-instance 'cleavir-ast:aref-ast
      :array-ast (convert array-form env system)
      :index-ast (convert index-form env system)
      :element-type type
      :simple-p simple-p
      :boxed-p boxed-p
      :origin origin)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting CLEAVIR-PRIMOP:ASET.
;;;
;;; This primop takes six arguments. The first two are an array and
;;; an index into it. The third is the object to be written in. The
;;; fourth and fifth are as above. The sixth is whether the objects
;;; in the array are boxed.
;;;
;;; Forms using this primitive operation must occur in a context
;;; that does not require a value, such as in a PROGN other than as
;;; the last form.

(defmethod convert-special
    ((symbol (eql 'cleavir-primop:aset)) form env system)
  (db origin (array-form index-form object-form
			 type simple-p boxed-p)
      (rest form)
    (make-instance 'cleavir-ast:aset-ast
      :array-ast (convert array-form env system)
      :index-ast (convert index-form env system)
      :element-ast (convert object-form env system)
      :element-type type
      :simple-p simple-p
      :boxed-p boxed-p
      :origin origin)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting CLEAVIR-PRIMOP:VALUES.
;;;
;;; This primitive operation can be used to inline CL:VALUES.
;;; That is, (cl:values ...) and (cleavir-primop:values ...) are
;;;  equivalent. The difference is that CL:VALUES is a function.
;;; This will compile down into a FIXED-TO-MULTIPLE-INSTRUCTION.

(defmethod convert-special
    ((symbol (eql 'cleavir-primop:values)) form env system)
  (db origin (op . arguments) form
    (declare (ignore op))
    (make-instance 'cleavir-ast:values-ast
     :argument-asts (mapcar
		     (lambda (form) (convert form env system))
		     arguments)
     :origin origin)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting CLEAVIR-PRIMOP:UNREACHABLE.
;;;
;;; Indicates that execution of the form should be impossible.

(defmethod convert-special
    ((symbol (eql 'cleavir-primop:unreachable)) form env system)
  (declare (ignore form env system))
  ;; should have :origin, i guess?
  (make-instance 'cleavir-ast:unreachable-ast))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting CLEAVIR-PRIMOP:AST.
;;;
;;; This allows ASTs produced by other means to be inserted into
;;; code which is then converted again.

(defmethod convert-special
    ((symbol (eql 'cleavir-primop:ast)) form env system)
  (declare (ignore env system))
  (db origin (ast) (rest form) ast))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting CLEAVIR-PRIMOP:MULTIPLE-VALUE-SETQ.
;;;
;;; Assigns the given variables to the values of the form.
;;; Returns all values of the form.
;;; The variables must be lexical.

(defmethod convert-special
    ((symbol (eql 'cleavir-primop:multiple-value-setq)) form env system)
  (db origin (op variables form) form
    (declare (ignore op))
    (let ((lexes (mapcar (lambda (var) (variable-info env var)) variables)))
      (assert (every (lambda (l)
                       (typep l 'cleavir-env:lexical-variable-info))
                     lexes))
      (cleavir-ast:make-multiple-value-setq-ast
       (mapcar #'cleavir-env:identity lexes) ; get actual lexical ASTs
       (convert form env system)
       :origin origin))))
