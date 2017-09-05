(cl:in-package #:cleavir-cst-to-ast)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting CLEAVIR-PRIMOP:AST.
;;;
;;; This allows ASTs produced by other means to be inserted into
;;; code which is then converted again.

(defmethod convert-special
    ((symbol (eql 'cleavir-primop:ast)) cst env system)
  (declare (ignore env system))
  (cst:db origin (primop ast) cst
    (declare (ignore primop))
    ast))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting CLEAVIR-PRIMOP:EQ.

(defmethod convert-special
    ((symbol (eql 'cleavir-primop:eq)) cst env system)
  (cst:db origin (eq-cst arg1-cst arg2-cst) cst
    (declare (ignore eq-cst))
    (cleavir-ast:make-eq-ast
     (convert arg1-cst env system)
     (convert arg2-cst env system)
     :origin origin)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting CLEAVIR-PRIMOP:TYPEQ.

(defmethod convert-special
    ((symbol (eql 'cleavir-primop:typeq)) cst env system)
  (cst:db origin (typeq-cst arg1-cst arg2-cst) cst
    (declare (ignore typeq-cst))
    (cleavir-ast:make-typeq-ast
     (convert arg1-cst env system)
     (cst:raw arg2-cst)
     :origin origin)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting CLEAVIR-PRIMOP:CAR.

(defmethod convert-special
    ((symbol (eql 'cleavir-primop:car)) cst env system)
  (cst:db origin (car-cst arg-cst) cst
    (declare (ignore car-cst))
    (cleavir-ast:make-car-ast (convert arg-cst env system)
                              :origin origin)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting CLEAVIR-PRIMOP:CDR.

(defmethod convert-special
    ((symbol (eql 'cleavir-primop:cdr)) cst env system)
  (cst:db origin (cdr-cst arg-cst) cst
    (declare (ignore cdr-cst))
    (cleavir-ast:make-cdr-ast (convert arg-cst env system)
                              :origin origin)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting CLEAVIR-PRIMOP:RPLACA.

(defmethod convert-special
    ((symbol (eql 'cleavir-primop:rplaca)) cst env system)
  (cst:db origin (rplaca-cst arg1-cst arg2-cst) cst
    (declare (ignore rplaca-cst))
    (cleavir-ast:make-rplaca-ast (convert arg1-cst env system)
                                 (convert arg2-cst env system)
                                 :origin origin)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting CLEAVIR-PRIMOP:RPLACD.

(defmethod convert-special
    ((symbol (eql 'cleavir-primop:rplacd)) cst env system)
  (cst:db origin (rplacd-cst arg1-cst arg2-cst) cst
    (declare (ignore rplacd-cst))
    (cleavir-ast:make-rplacd-ast (convert arg1-cst env system)
                                 (convert arg2-cst env system)
                                 :origin origin)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting CLEAVIR-PRIMOP:FIXNUM-ADD.

(defmethod convert-special
    ((symbol (eql 'cleavir-primop:fixnum-add)) cst env system)
  (cst:db origin (add-cst arg1-cst arg2-cst variable-cst) cst
    (declare (ignore add-cst))
    (cleavir-ast:make-fixnum-add-ast (convert arg1-cst env system)
                                     (convert arg2-cst env system)
                                     (convert variable-cst env system)
                                     :origin origin)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting CLEAVIR-PRIMOP:FIXNUM-SUB.

(defmethod convert-special
    ((symbol (eql 'cleavir-primop:fixnum-sub)) cst env system)
  (cst:db origin (sub-cst arg1-cst arg2-cst variable-cst) cst
    (declare (ignore sub-cst))
    (cleavir-ast:make-fixnum-sub-ast (convert arg1-cst env system)
                                     (convert arg2-cst env system)
                                     (convert variable-cst env system)
                                     :origin origin)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting CLEAVIR-PRIMOP:FIXNUM-LESS.

(defmethod convert-special
    ((symbol (eql 'cleavir-primop:fixnum-less)) cst env system)
  (cst:db origin (less-cst arg1-cst arg2-cst) cst
    (declare (ignore less-cst))
    (make-instance 'cleavir-ast:fixnum-less-ast
      :arg1-ast (convert arg1-cst env system)
      :arg2-ast (convert arg2-cst env system)
      :origin origin)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting CLEAVIR-PRIMOP:FIXNUM-NOT-GREATER.

(defmethod convert-special
    ((symbol (eql 'cleavir-primop:fixnum-not-greater)) cst env system)
  (cst:db origin (not-greater-cst arg1-cst arg2-cst) cst
    (declare (ignore not-greater-cst))
    (make-instance 'cleavir-ast:fixnum-not-greater-ast
      :arg1-ast (convert arg1-cst env system)
      :arg2-ast (convert arg2-cst env system)
      :origin origin)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting CLEAVIR-PRIMOP:FIXNUM-GREATER.

(defmethod convert-special
    ((symbol (eql 'cleavir-primop:fixnum-greater)) cst env system)
  (cst:db origin (greater-cst arg1-cst arg2-cst) cst
    (declare (ignore greater-cst))
    (make-instance 'cleavir-ast:fixnum-greater-ast
      :arg1-ast (convert arg1-cst env system)
      :arg2-ast (convert arg2-cst env system)
      :origin origin)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting CLEAVIR-PRIMOP:FIXNUM-NOT-LESS.

(defmethod convert-special
    ((symbol (eql 'cleavir-primop:fixnum-not-less)) cst env system)
  (cst:db origin (not-less-cst arg1-cst arg2-cst) cst
    (declare (ignore not-less-cst))
    (make-instance 'cleavir-ast:fixnum-not-less-ast
      :arg1-ast (convert arg1-cst env system)
      :arg2-ast (convert arg2-cst env system)
      :origin origin)))

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
    ((symbol (eql 'cleavir-primop:let-uninitialized)) cst env system)
  (cst:db origin (let-cst variables-cst body-csts) cst
    (declare (ignore let-cst))
    (let ((new-env env))
      (loop for rest-cst = variables-cst then (cst:rest rest-cst)
            until (cst:null rest-cst)
            do (let* ((variable-cst (cst:first rest-cst))
                      (variable-ast (cleavir-ast:make-lexical-ast
                                     (cst:raw variable-cst)
                                     :origin (cst:source variables-cst))))
                 (setf new-env
                       (cleavir-env:add-lexical-variable
                        new-env (cst:raw variable-cst) variable-ast))))
      (process-progn (convert-sequence (cst:listify body-csts) new-env system)))))

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
    ((symbol (eql 'cleavir-primop:funcall)) cst env system)
  (cst:db origin (funcall-cst function-cst . arguments-cst) cst
    (declare (ignore funcall-cst))
    (cleavir-ast:make-call-ast
     (convert function-cst env system)
     (loop for remaining = arguments-cst then (cst:rest remaining)
           until (cst:null remaining)
           collect (convert (cst:first remaining) env system))
     :origin origin)))

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
    ((symbol (eql 'cleavir-primop:multiple-value-call)) cst env system)
  (cst:db origin (multiple-value-call-cst function-cst . arguments-cst) cst
    (declare (ignore multiple-value-call-cst))
    (cleavir-ast:make-multiple-value-call-ast
     (convert function-cst env system)
     (loop for remaining = arguments-cst then (cst:rest remaining)
           until (cst:null remaining)
           collect (convert (cst:first remaining) env system))
     :origin origin)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting CLEAVIR-PRIMOP:SLOT-READ.
;;;
;;; This primop takes two arguments.  The first argument is a form
;;; that must evaluate to a standard instance.  The second argument is
;;; a form that must evaluate to a fixnum and that indicates the slot
;;; number to be read.

(defmethod convert-special
    ((symbol (eql 'cleavir-primop:slot-read)) cst env system)
  (cst:db origin (slot-read-cst instance-cst slot-number-cst) cst
    (declare (ignore slot-read-cst))
    (cleavir-ast:make-slot-read-ast
     (convert instance-cst env system)
     (convert slot-number-cst env system)
     :origin origin)))

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
    ((symbol (eql 'cleavir-primop:slot-write)) cst env system)
  (cst:db origin (slot-write-cst instance-cst slot-number-cst value-cst) cst
    (declare (ignore slot-write-cst))
    (cleavir-ast:make-slot-write-ast
     (convert instance-cst env system)
     (convert slot-number-cst env system)
     (convert value-cst env system)
     :origin origin)))

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
    ((symbol (eql 'cleavir-primop:aref)) cst env system)
  (cst:db origin (array-cst index-cst type simple-p boxed-p) cst
    (make-instance 'cleavir-ast:aref-ast
      :array-ast (convert array-cst env system)
      :index-ast (convert index-cst env system)
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
    ((symbol (eql 'cleavir-primop:aset)) cst env system)
  (cst:db origin (array-cst index-cst object-cst type simple-p boxed-p)
      cst
    (make-instance 'cleavir-ast:aset-ast
      :array-ast (convert array-cst env system)
      :index-ast (convert index-cst env system)
      :element-ast (convert object-cst env system)
      :element-type type
      :simple-p simple-p
      :boxed-p boxed-p
      :origin origin)))
