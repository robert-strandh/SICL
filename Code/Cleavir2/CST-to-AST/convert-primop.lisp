(cl:in-package #:cleavir-cst-to-ast)

(defun check-simple-primop-syntax (cst argument-count)
  (check-cst-proper-list cst 'form-must-be-proper-list)
  (check-argument-count cst argument-count argument-count))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting CLEAVIR-PRIMOP:AST.
;;;
;;; This allows ASTs produced by other means to be inserted into
;;; code which is then converted again.

(defmethod convert-special
    (client (symbol (eql 'cleavir-primop:ast)) cst environment)
  (declare (ignore client environment))
  (check-simple-primop-syntax cst 1)
  (cst:db origin (primop-cst ast-cst) cst
    (declare (ignore primop-cst))
    (cst:raw ast-cst)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting CLEAVIR-PRIMOP:EQ.

(defmethod convert-special
    (client (symbol (eql 'cleavir-primop:eq)) cst environment)
  (check-simple-primop-syntax cst 2)
  (cst:db origin (eq-cst arg1-cst arg2-cst) cst
    (declare (ignore eq-cst))
    (cleavir-ast:make-ast 'cleavir-ast:eq-ast
     :arg1-ast (convert client arg1-cst environment)
     :arg2-ast (convert client arg2-cst environment))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting CLEAVIR-PRIMOP:TYPEQ.

(defmethod convert-special
    (client (symbol (eql 'cleavir-primop:typeq)) cst environment)
  (check-simple-primop-syntax cst 2)
  (cst:db origin (typeq-cst arg1-cst arg2-cst) cst
    (declare (ignore typeq-cst))
    (cleavir-ast:make-ast 'cleavir-ast:typeq-ast
     :form-ast (convert client arg1-cst environment)
     :type-specifier (cst:raw arg2-cst))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting CLEAVIR-PRIMOP:VALUES.
;;;
;;; This primitive operation can be used to inline CL:VALUES.  That
;;; is, (cl:values ...) and (cleavir-primop:values ...) are
;;; equivalent. The difference is that CL:VALUES is a function.  In
;;; the resulting HIR code, the use of this operation will appear as a
;;; FIXED-TO-MULTIPLE-INSTRUCTION.

(defmethod convert-special
    (client (symbol (eql 'cleavir-primop:values)) cst environment)
  (check-cst-proper-list cst 'form-must-be-proper-list)
  (cst:db origin (values-cst . arguments-cst) cst
    (declare (ignore values-cst))
    (cleavir-ast:make-ast 'cleavir-ast:values-ast
     :argument-asts (mapcar
                     (lambda (cst) (convert client cst environment))
                     (cst:listify arguments-cst)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting CLEAVIR-PRIMOP:MULTIPLE-VALUE-SETQ
;;;
;;; This primitive operation can be used to compile
;;; CL:MULTIPLE-VALUE-SETQ. Unlike that operator, it requires all
;;; the variables to be lexical, and can only be used in a no-values
;;; context. In the result HIR code, the use of this operation will
;;; appear as a MULTIPLE-TO-FIXED-INSTRUCTION.

(defmethod convert-special
    (client (symbol (eql 'cleavir-primop:multiple-value-setq)) cst environment)
  (check-cst-proper-list cst 'form-must-be-proper-list)
  (check-argument-count cst 2 2)
  (cst:db origin (mvs-cst variables-cst form-cst) cst
    (declare (ignore mvs-cst))
    (assert (cst:proper-list-p variables-cst))
    (let ((lexes
            (loop for var in (cst:raw variables-cst)
                  do (assert (symbolp var))
                  collect (let ((info (trucler:describe-variable client environment var)))
                            (assert (typep info 'trucler:lexical-variable-description))
                            info))))
      (cleavir-ast:make-ast 'cleavir-ast:multiple-value-setq-ast
       :lhs-asts (mapcar #'trucler:identity lexes)
       :form-ast (convert client form-cst environment)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting CLEAVIR-PRIMOP:CONSP.

(defmethod convert-special
    (client (symbol (eql 'cleavir-primop:consp)) cst environment)
  (check-simple-primop-syntax cst 1)
  (cst:db origin (consp-cst object-cst) cst
    (declare (ignore consp-cst))
    (cleavir-ast:make-ast 'cleavir-ast:consp-ast
      :object-ast (convert client object-cst environment))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting CLEAVIR-PRIMOP:CAR.

(defmethod convert-special
    (client (symbol (eql 'cleavir-primop:car)) cst environment)
  (check-simple-primop-syntax cst 1)
  (cst:db origin (car-cst arg-cst) cst
    (declare (ignore car-cst))
    (cleavir-ast:make-ast 'cleavir-ast:car-ast
      :cons-ast (convert client arg-cst environment))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting CLEAVIR-PRIMOP:CDR.

(defmethod convert-special
    (client (symbol (eql 'cleavir-primop:cdr)) cst environment)
  (check-simple-primop-syntax cst 1)
  (cst:db origin (cdr-cst arg-cst) cst
    (declare (ignore cdr-cst))
    (cleavir-ast:make-ast 'cleavir-ast:cdr-ast
      :cons-ast (convert client arg-cst environment))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting CLEAVIR-PRIMOP:RPLACA.

(defmethod convert-special
    (client (symbol (eql 'cleavir-primop:rplaca)) cst environment)
  (check-simple-primop-syntax cst 2)
  (cst:db origin (rplaca-cst arg1-cst arg2-cst) cst
    (declare (ignore rplaca-cst))
    (cleavir-ast:make-ast 'cleavir-ast:rplaca-ast
      :cons-ast (convert client arg1-cst environment)
      :object-ast (convert client arg2-cst environment))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting CLEAVIR-PRIMOP:RPLACD.

(defmethod convert-special
    (client (symbol (eql 'cleavir-primop:rplacd)) cst environment)
  (check-simple-primop-syntax cst 2)
  (cst:db origin (rplacd-cst arg1-cst arg2-cst) cst
    (declare (ignore rplacd-cst))
    (cleavir-ast:make-ast 'cleavir-ast:rplacd-ast
      :cons-ast (convert client arg1-cst environment)
      :object-ast (convert client arg2-cst environment))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting CLEAVIR-PRIMOP:FIXNUMP.

(defmethod convert-special
    (client (symbol (eql 'cleavir-primop:fixnump)) cst environment)
  (check-simple-primop-syntax cst 1)
  (cst:db origin (fixnump-cst object-cst) cst
    (declare (ignore fixnump-cst))
    (cleavir-ast:make-ast 'cleavir-ast:fixnump-ast
      :object-ast (convert client object-cst environment))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting CLEAVIR-PRIMOP:FIXNUM-ADD.

(defmethod convert-special
    (client (symbol (eql 'cleavir-primop:fixnum-add)) cst environment)
  (check-simple-primop-syntax cst 3)
  (cst:db origin (add-cst arg1-cst arg2-cst variable-cst) cst
    (declare (ignore add-cst))
    (cleavir-ast:make-ast 'cleavir-ast:fixnum-add-ast
      :arg1-ast (convert client arg1-cst environment)
      :arg2-ast (convert client arg2-cst environment)
      :variable-ast (convert client variable-cst environment))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting CLEAVIR-PRIMOP:FIXNUM-SUB.

(defmethod convert-special
    (client (symbol (eql 'cleavir-primop:fixnum-sub)) cst environment)
  (check-simple-primop-syntax cst 3)
  (cst:db origin (sub-cst arg1-cst arg2-cst variable-cst) cst
    (declare (ignore sub-cst))
    (cleavir-ast:make-ast 'cleavir-ast:fixnum-sub-ast
      :arg1-ast (convert client arg1-cst environment)
      :arg2-ast (convert client arg2-cst environment)
      :variable-ast (convert client variable-cst environment))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting CLEAVIR-PRIMOP:FIXNUM-DIVIDE.

(defmethod convert-special
    (client (symbol (eql 'cleavir-primop:fixnum-divide)) cst environment)
  (check-simple-primop-syntax cst 2)
  (cst:db origin (divide-cst dividend-cst divisor-cst) cst
    (declare (ignore divide-cst))
    (cleavir-ast:make-ast 'cleavir-ast:fixnum-divide-ast
      :dividend-ast (convert client dividend-cst environment)
      :divisor-ast (convert client divisor-cst environment))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting CLEAVIR-PRIMOP:FIXNUM-LESS.

(defmethod convert-special
    (client (symbol (eql 'cleavir-primop:fixnum-less)) cst environment)
  (check-simple-primop-syntax cst 2)
  (cst:db origin (less-cst arg1-cst arg2-cst) cst
    (declare (ignore less-cst))
    (cleavir-ast:make-ast 'cleavir-ast:fixnum-less-ast
      :arg1-ast (convert client arg1-cst environment)
      :arg2-ast (convert client arg2-cst environment))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting CLEAVIR-PRIMOP:FIXNUM-NOT-GREATER.

(defmethod convert-special
    (client (symbol (eql 'cleavir-primop:fixnum-not-greater)) cst environment)
  (check-simple-primop-syntax cst 2)
  (cst:db origin (not-greater-cst arg1-cst arg2-cst) cst
    (declare (ignore not-greater-cst))
    (cleavir-ast:make-ast 'cleavir-ast:fixnum-not-greater-ast
      :arg1-ast (convert client arg1-cst environment)
      :arg2-ast (convert client arg2-cst environment))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting CLEAVIR-PRIMOP:FIXNUM-GREATER.

(defmethod convert-special
    (client (symbol (eql 'cleavir-primop:fixnum-greater)) cst environment)
  (check-simple-primop-syntax cst 2)
  (cst:db origin (greater-cst arg1-cst arg2-cst) cst
    (declare (ignore greater-cst))
    (cleavir-ast:make-ast 'cleavir-ast:fixnum-greater-ast
      :arg1-ast (convert client arg1-cst environment)
      :arg2-ast (convert client arg2-cst environment))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting CLEAVIR-PRIMOP:FIXNUM-NOT-LESS.

(defmethod convert-special
    (client (symbol (eql 'cleavir-primop:fixnum-not-less)) cst environment)
  (check-simple-primop-syntax cst 2)
  (cst:db origin (not-less-cst arg1-cst arg2-cst) cst
    (declare (ignore not-less-cst))
    (cleavir-ast:make-ast 'cleavir-ast:fixnum-not-less-ast
      :arg1-ast (convert client arg1-cst environment)
      :arg2-ast (convert client arg2-cst environment))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting CLEAVIR-PRIMOP:FIXNUM-EQUAL.

(defmethod convert-special
    (client (symbol (eql 'cleavir-primop:fixnum-equal)) cst environment)
  (check-simple-primop-syntax cst 2)
  (cst:db origin (equal-cst arg1-cst arg2-cst) cst
    (declare (ignore equal-cst))
    (cleavir-ast:make-ast 'cleavir-ast:fixnum-equal-ast
      :arg1-ast (convert client arg1-cst environment)
      :arg2-ast (convert client arg2-cst environment))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting CLEAVIR-PRIMOP:CHARACTERP.

(defmethod convert-special
    (client (symbol (eql 'cleavir-primop:characterp)) cst environment)
  (check-simple-primop-syntax cst 1)
  (cst:db origin (characterp-cst object-cst) cst
    (declare (ignore characterp-cst))
    (cleavir-ast:make-ast 'cleavir-ast:characterp-ast
      :object-ast (convert client object-cst environment))))

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
    (client (symbol (eql 'cleavir-primop:let-uninitialized)) cst environment)
  (check-cst-proper-list cst 'form-must-be-proper-list)
  (check-argument-count cst 1 nil)
  (cst:db origin (let-cst variables-cst . body-cst) cst
    (declare (ignore let-cst))
    (assert (cst:proper-list-p variables-cst))
    (assert (every #'symbolp (cst:raw variables-cst)))
    (let ((new-env environment))
      (loop for rest-cst = variables-cst then (cst:rest rest-cst)
            until (cst:null rest-cst)
            do (let* ((variable-cst (cst:first rest-cst))
                      (variable (cst:raw variable-cst))
                      (variable-ast (cleavir-ast:make-ast 'cleavir-ast:lexical-ast
                                      :name variable)))
                 (setf new-env
                       (trucler:add-lexical-variable
                        client new-env variable variable-ast))))
      (process-progn (convert-sequence client body-cst new-env)))))

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
    (client (symbol (eql 'cleavir-primop:funcall)) cst environment)
  (check-cst-proper-list cst 'form-must-be-proper-list)
  (check-argument-count cst 1 nil)
  (cst:db origin (funcall-cst function-cst . arguments-cst) cst
    (declare (ignore funcall-cst))
    (cleavir-ast:make-ast 'cleavir-ast:call-ast
     :callee-ast (convert client function-cst environment)
     :argument-asts (loop for remaining = arguments-cst
                            then (cst:rest remaining)
                          until (cst:null remaining)
                          collect (convert client (cst:first remaining) environment)))))

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
    (client (symbol (eql 'cleavir-primop:multiple-value-call)) cst environment)
  (check-cst-proper-list cst 'form-must-be-proper-list)
  (check-argument-count cst 1 nil)
  (cst:db origin (multiple-value-call-cst function-cst . arguments-cst) cst
    (declare (ignore multiple-value-call-cst))
    (cleavir-ast:make-ast 'cleavir-ast:multiple-value-call-ast
     :function-form-ast (convert client function-cst environment)
     :form-asts (loop for remaining = arguments-cst then (cst:rest remaining)
                      until (cst:null remaining)
                      collect (convert client (cst:first remaining) environment)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting CLEAVIR-PRIMOP:STANDARD-OBJECT-P.

(defmethod convert-special
    (client (symbol (eql 'cleavir-primop:standard-object-p)) cst environment)
  (check-simple-primop-syntax cst 1)
  (cst:db origin (standard-object-p-cst object-cst) cst
    (declare (ignore standard-object-p-cst))
    (cleavir-ast:make-ast 'cleavir-ast:standard-object-p-ast
      :object-ast (convert client object-cst environment))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting CLEAVIR-PRIMOP:NOOK-READ.
;;;
;;; This primop takes two arguments.  The first argument is a form
;;; that must evaluate to a standard object.  The second argument is
;;; a form that must evaluate to a fixnum and that indicates the nook
;;; number to be read.

(defmethod convert-special
    (client (symbol (eql 'cleavir-primop:nook-read)) cst environment)
  (check-simple-primop-syntax cst 2)
  (cst:db origin (nook-read-cst instance-cst nook-number-cst) cst
    (declare (ignore nook-read-cst))
    (cleavir-ast:make-ast 'cleavir-ast:nook-read-ast
     :object-ast (convert client instance-cst environment)
     :nook-number-ast (convert client nook-number-cst environment))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting CLEAVIR-PRIMOP:NOOK-WRITE.
;;;
;;; This primop takes three arguments.  The first argument is a form
;;; that must evaluate to a standard object.  The second argument is
;;; a form that must evaluate to a fixnum and that indicates the nook
;;; number to be written.  The third argument is a form that evaluates
;;; to the object that will be written to the nook.

(defmethod convert-special
    (client (symbol (eql 'cleavir-primop:nook-write)) cst environment)
  (check-simple-primop-syntax cst 3)
  (cst:db origin (nook-write-cst instance-cst nook-number-cst value-cst) cst
    (declare (ignore nook-write-cst))
    (cleavir-ast:make-ast 'cleavir-ast:nook-write-ast
     :object-ast (convert client instance-cst environment)
     :nook-number-ast (convert client nook-number-cst environment)
     :value-ast (convert client value-cst environment))))

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
    (client (symbol (eql 'cleavir-primop:aref)) cst environment)
  (check-simple-primop-syntax cst 5)
  (cst:db origin (aref-cst array-cst index-cst type-cst simple-p-cst boxed-p-cst) cst
    (declare (ignore aref-cst))
    (cleavir-ast:make-ast 'cleavir-ast:aref-ast
      :array-ast (convert client array-cst environment)
      :index-ast (convert client index-cst environment)
      :element-type (cst:raw type-cst)
      :simple-p (cst:raw simple-p-cst)
      :boxed-p (cst:raw boxed-p-cst))))

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
    (client (symbol (eql 'cleavir-primop:aset)) cst environment)
  (check-simple-primop-syntax cst 6)
  (cst:db origin (aset-cst array-cst index-cst object-cst type-cst simple-p-cst boxed-p-cst)
      cst
    (declare (ignore aset-cst))
    (cleavir-ast:make-ast 'cleavir-ast:aset-ast
      :array-ast (convert client array-cst environment)
      :index-ast (convert client index-cst environment)
      :element-ast (convert client object-cst environment)
      :element-type (cst:raw type-cst)
      :simple-p (cst:raw simple-p-cst)
      :boxed-p (cst:raw boxed-p-cst))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting CLEAVIR-PRIMOP:SINGLE-FLOAT-P.

(defmethod convert-special
    (client (symbol (eql 'cleavir-primop:single-float-p)) cst environment)
  (check-simple-primop-syntax cst 1)
  (cst:db origin (single-float-p-cst object-cst) cst
    (declare (ignore single-float-p-cst))
    (cleavir-ast:make-ast 'cleavir-ast:single-float-p-ast
      :object-ast (convert client object-cst environment))))

;;; The following macro is used to generate a method on
;;; CONVERT-SPECIAL for binary floating-point primops.
(defmacro define-float-binop (primop ast)
  `(defmethod convert-special
       (client (symbol (eql ',primop)) cst environment)
     (check-simple-primop-syntax cst 3)
     (cst:db origin (op-cst type-cst arg1-cst arg2-cst) cst
       (declare (ignore op-cst))
       (cleavir-ast:make-ast ',ast
         :subtype (cst:raw type-cst)
         :arg1-ast (convert client arg1-cst environment)
         :arg2-ast (convert client arg2-cst environment)))))

(define-float-binop cleavir-primop:float-add
  cleavir-ast:float-add-ast)
(define-float-binop cleavir-primop:float-sub
  cleavir-ast:float-sub-ast)
(define-float-binop cleavir-primop:float-mul
  cleavir-ast:float-mul-ast)
(define-float-binop cleavir-primop:float-div
  cleavir-ast:float-div-ast)
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

;;; The following macro is used to generate a method on
;;; CONVERT-SPECIAL for unatry floating-point primops.
(defmacro define-float-unop (primop ast)
  `(defmethod convert-special
       (client (symbol (eql ',primop)) cst environment)
     (check-simple-primop-syntax cst 2)
     (cst:db origin (op-cst type-cst arg-cst) cst
       (declare (ignore op-cst))
       (cleavir-ast:make-ast ',ast
         :subtype (cst:raw type-cst)
         :arg-ast (convert client arg-cst environment)))))

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
    (client (symbol (eql 'cleavir-primop:coerce)) cst environment)
  (check-simple-primop-syntax cst 3)
  (cst:db origin (op-cst type1-cst type2-cst form-cst) cst
    (declare (ignore op-cst))
    (cleavir-ast:make-ast 'cleavir-ast:coerce-ast
     :from (cst:raw type1-cst) :to (cst:raw type2-cst)
     :arg-ast (convert client form-cst environment))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting CLEAVIR-PRIMOP:UNREACHABLE.
;;;
;;; Recall that this primop indicates that execution of the form
;;; should be impossible.

(defmethod convert-special
    (client (symbol (eql 'cleavir-primop:unreachable)) cst environment)
  (declare (ignore client environment))
  (check-simple-primop-syntax cst 0)
  (cleavir-ast:make-ast 'cleavir-ast:unreachable-ast))
