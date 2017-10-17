(cl:in-package #:cleavir-cst-to-ast)

;;; Check that the syntax of a special form is correct.  The special
;;; form is represented as a CST.  OPERATOR is the name of the special
;;; operator of the special form.  Primary methods on this generic
;;; function signal an error when an incorrect syntax is detected.  An
;;; :AROUND method proposes restarts that replace the CST by one that
;;; signals an error at run time.  The replacement CST is returned by
;;; the :AROUND method.
(defgeneric check-special-form-syntax (operator cst))

(defmethod check-special-form-syntax :around (operator cst)
  (restart-case (progn (call-next-method) cst)
    (recover ()
      :report "Recover by replacing form by a call to ERROR."
      (cst:cst-from-expression
       '(error 'run-time-program-error
         :expr (cst:raw cst)
         :origin (cst:source cst))))))

;;; Take a CST, check whether it represents a proper list.  If it does
;;; not represent ERROR-TYPE is a symbol that is passed to ERROR.
(defun check-cst-proper-list (cst error-type)
  (unless  (cst:proper-list-p cst)
    (error error-type
           :expr (cst:raw cst)
           :origin (cst:source cst))))

;;; Check that the number of arguments greater than or equal to MIN
;;; and less than or equal to MAX.  When MAX is NIL, then there is no
;;; upper bound on the number of arguments.  If the argument count is
;;; wrong, then signal an error.  It is assumed that CST represents a
;;; proper list, so this must be checked first by the caller.
(defun check-argument-count (cst min max)
  (let ((count (1- (length (cst:raw cst)))))
    (unless (and (>= count min)
                 (or (null max)
                     (<= count max)))
      (error 'incorrect-number-of-arguments
             :expr (cst:raw cst)
             :expected-min min
             :expected-max max
             :observed count
             :origin (cst:source cst)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Checking QUOTE.

(defmethod check-special-form-syntax ((operator (eql 'quote)) cst)
  ;; The code in this method has been moved to the corresponding
  ;; method of convert-special.  Ultimately, the code of every method
  ;; on CHECK-SPECIAL-FORM-SYNTAX will be moved.
  (declare (ignore cst))
  nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Checking BLOCK.

(defmethod check-special-form-syntax ((operator (eql 'block)) cst)
  ;; The code in this method has been moved to the corresponding
  ;; method of convert-special.  Ultimately, the code of every method
  ;; on CHECK-SPECIAL-FORM-SYNTAX will be moved.
  (declare (ignore cst))
  nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Checking EVAL-WHEN.

(defmethod check-special-form-syntax ((operator (eql 'eval-when)) cst)
  ;; The code in this method has been moved to the corresponding
  ;; method of convert-special.  Ultimately, the code of every method
  ;; on CHECK-SPECIAL-FORM-SYNTAX will be moved.
  (declare (ignore cst))
  nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Checking FLET and LABELS.

(defmethod check-special-form-syntax ((operator (eql 'flet)) cst)
  ;; The code in this method has been moved to the corresponding
  ;; method of convert-special.  Ultimately, the code of every method
  ;; on CHECK-SPECIAL-FORM-SYNTAX will be moved.
  (declare (ignore cst))
  nil)

(defmethod check-special-form-syntax ((operator (eql 'labels)) cst)
  ;; The code in this method has been moved to the corresponding
  ;; method of convert-special.  Ultimately, the code of every method
  ;; on CHECK-SPECIAL-FORM-SYNTAX will be moved.
  (declare (ignore cst))
  nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Checking FUNCTION.

(defun proper-function-name-p (name-cst)
  (let ((name (cst:raw name-cst)))
    (or (symbolp name)
        (and (cleavir-code-utilities:proper-list-p name)
             (= (length name) 2)
             (eq (car name) 'setf)
             (symbolp (cadr name))))))

(defmethod check-special-form-syntax ((operator (eql 'function)) cst)
  ;; The code in this method has been moved to the corresponding
  ;; method of convert-special.  Ultimately, the code of every method
  ;; on CHECK-SPECIAL-FORM-SYNTAX will be moved.
  (declare (ignore cst))
  nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Checking GO.

(defmethod check-special-form-syntax ((operator (eql 'go)) cst)
  ;; The code in this method has been moved to the corresponding
  ;; method of convert-special.  Ultimately, the code of every method
  ;; on CHECK-SPECIAL-FORM-SYNTAX will be moved.
  (declare (ignore cst))
  nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Checking IF.

(defmethod check-special-form-syntax ((operator (eql 'if)) cst)
  ;; The code in this method has been moved to the corresponding
  ;; method of convert-special.  Ultimately, the code of every method
  ;; on CHECK-SPECIAL-FORM-SYNTAX will be moved.
  (declare (ignore cst))
  nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Checking LET and LET*

(defmethod check-special-form-syntax ((operator (eql 'let)) cst)
  ;; The code in this method has been moved to the corresponding
  ;; method of convert-special.  Ultimately, the code of every method
  ;; on CHECK-SPECIAL-FORM-SYNTAX will be moved.
  (declare (ignore cst))
  nil)

(defmethod check-special-form-syntax ((operator (eql 'let*)) cst)
  ;; The code in this method has been moved to the corresponding
  ;; method of convert-special.  Ultimately, the code of every method
  ;; on CHECK-SPECIAL-FORM-SYNTAX will be moved.
  (declare (ignore cst))
  nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Checking LOAD-TIME-VALUE.

(defmethod check-special-form-syntax ((operator (eql 'load-time-value)) cst)
  ;; The code in this method has been moved to the corresponding
  ;; method of convert-special.  Ultimately, the code of every method
  ;; on CHECK-SPECIAL-FORM-SYNTAX will be moved.
  (declare (ignore cst))
  nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Checking LOCALLY.

(defmethod check-special-form-syntax ((operator (eql 'locally)) cst)
  ;; The code in this method has been moved to the corresponding
  ;; method of convert-special.  Ultimately, the code of every method
  ;; on CHECK-SPECIAL-FORM-SYNTAX will be moved.
  (declare (ignore cst))
  nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Checking MACROLET.

(defmethod check-special-form-syntax ((operator (eql 'macrolet)) cst)
  ;; The code in this method has been moved to the corresponding
  ;; method of convert-special.  Ultimately, the code of every method
  ;; on CHECK-SPECIAL-FORM-SYNTAX will be moved.
  (declare (ignore cst))
  nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Checking MULTIPLE-VALUE-CALL.

(defmethod check-special-form-syntax ((operator (eql 'multiple-value-call)) cst)
  ;; The code in this method has been moved to the corresponding
  ;; method of convert-special.  Ultimately, the code of every method
  ;; on CHECK-SPECIAL-FORM-SYNTAX will be moved.
  (declare (ignore cst))
  nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Checking MULTIPLE-VALUE-PROG1.

(defmethod check-special-form-syntax ((operator (eql 'multiple-value-prog1)) cst)
  ;; The code in this method has been moved to the corresponding
  ;; method of convert-special.  Ultimately, the code of every method
  ;; on CHECK-SPECIAL-FORM-SYNTAX will be moved.
  (declare (ignore cst))
  nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Checking PROGN.

(defmethod check-special-form-syntax ((operator (eql 'progn)) cst)
  ;; The code in this method has been moved to the corresponding
  ;; method of convert-special.  Ultimately, the code of every method
  ;; on CHECK-SPECIAL-FORM-SYNTAX will be moved.
  (declare (ignore cst))
  nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Checking PROGV.

(defmethod check-special-form-syntax ((operator (eql 'progv)) cst)
  ;; The code in this method has been moved to the corresponding
  ;; method of convert-special.  Ultimately, the code of every method
  ;; on CHECK-SPECIAL-FORM-SYNTAX will be moved.
  (declare (ignore cst))
  nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Checking RETURN-FROM.

(defmethod check-special-form-syntax ((operator (eql 'return-from)) cst)
  ;; The code in this method has been moved to the corresponding
  ;; method of convert-special.  Ultimately, the code of every method
  ;; on CHECK-SPECIAL-FORM-SYNTAX will be moved.
  (declare (ignore cst))
  nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Checking SETQ.

(defmethod check-special-form-syntax ((operator (eql 'setq)) cst)
  ;; The code in this method has been moved to the corresponding
  ;; method of convert-special.  Ultimately, the code of every method
  ;; on CHECK-SPECIAL-FORM-SYNTAX will be moved.
  (declare (ignore cst))
  nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Checking SYMBOL-MACROLET.
;;;
;;; FIXME: syntax check bindings

(defmethod check-special-form-syntax ((head (eql 'symbol-macrolet)) cst)
  ;; The code in this method has been moved to the corresponding
  ;; method of convert-special.  Ultimately, the code of every method
  ;; on CHECK-SPECIAL-FORM-SYNTAX will be moved.
  (declare (ignore cst))
  nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Checking TAGBODY.

(defmethod check-special-form-syntax ((head (eql 'tagbody)) cst)
  (check-cst-proper-list cst 'form-must-be-proper-list))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Checking THE.

(defmethod check-special-form-syntax ((head (eql 'the)) cst)
  (check-cst-proper-list cst 'form-must-be-proper-list)
  (check-argument-count cst 2 2))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Checking THROW

(defmethod check-special-form-syntax ((head (eql 'throw)) cst)
  ;; The code in this method has been moved to the corresponding
  ;; method of convert-special.  Ultimately, the code of every method
  ;; on CHECK-SPECIAL-FORM-SYNTAX will be moved.
  (declare (ignore cst))
  nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Checking UNWIND-PROTECT

(defmethod check-special-form-syntax ((head (eql 'unwind-protect)) cst)
  ;; The code in this method has been moved to the corresponding
  ;; method of convert-special.  Ultimately, the code of every method
  ;; on CHECK-SPECIAL-FORM-SYNTAX will be moved.
  (declare (ignore cst))
  nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Checking CATCH.

(defmethod check-special-form-syntax ((head (eql 'catch)) cst)
  ;; The code in this method has been moved to the corresponding
  ;; method of convert-special.  Ultimately, the code of every method
  ;; on CHECK-SPECIAL-FORM-SYNTAX will be moved.
  (declare (ignore cst))
  nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Syntax checks for the PRIMOPs.

;;; This macro can be used to define a simple syntax-check method,
;;; where the form must be a proper list and it has a fixed number of
;;; arguments.
(defmacro define-simple-check (operation argcount)
  (declare (ignore argcount))
  `(defmethod check-special-form-syntax ((operator (eql ',operation)) cst)
     ;; The code in this method has been moved to the corresponding
     ;; method of convert-special.  Ultimately, the code of every method
     ;; on CHECK-SPECIAL-FORM-SYNTAX will be moved.
     (declare (ignore cst))
     nil))

(define-simple-check cleavir-primop:eq 2)
(define-simple-check cleavir-primop:car 1)
(define-simple-check cleavir-primop:cdr 1)
(define-simple-check cleavir-primop:rplaca 2)
(define-simple-check cleavir-primop:rplacd 2)
(define-simple-check cleavir-primop:slot-read 2)
(define-simple-check cleavir-primop:slot-write 3)
(define-simple-check cleavir-primop:coerce 3)
(define-simple-check cleavir-primop:fixnum-less 2)
(define-simple-check cleavir-primop:fixnum-not-greater 2)
(define-simple-check cleavir-primop:fixnum-greater 2)
(define-simple-check cleavir-primop:fixnum-not-less 2)
(define-simple-check cleavir-primop:fixnum-equal 2)
(define-simple-check cleavir-primop:aref 5)
(define-simple-check cleavir-primop:aset 6)
(define-simple-check cleavir-primop:float-add 3)
(define-simple-check cleavir-primop:float-sub 3)
(define-simple-check cleavir-primop:float-mul 3)
(define-simple-check cleavir-primop:float-div 3)
(define-simple-check cleavir-primop:float-less 3)
(define-simple-check cleavir-primop:float-not-greater 3)
(define-simple-check cleavir-primop:float-equal 3)
(define-simple-check cleavir-primop:float-not-less 3)
(define-simple-check cleavir-primop:float-greater 3)
(define-simple-check cleavir-primop:float-sin 2)
(define-simple-check cleavir-primop:float-cos 2)
(define-simple-check cleavir-primop:float-sqrt 2)
(define-simple-check cleavir-primop:unreachable 0)
(define-simple-check cleavir-primop:ast 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Checking LET-UNINITIALIZED.

(defmethod check-special-form-syntax
    ((operator (eql 'cleavir-primop:let-uninitialized)) cst)
  ;; The code in this method has been moved to the corresponding
  ;; method of convert-special.  Ultimately, the code of every method
  ;; on CHECK-SPECIAL-FORM-SYNTAX will be moved.
  (declare (ignore cst))
  nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Checking FUNCALL.

(defmethod check-special-form-syntax
    ((operator (eql 'cleavir-primop:funcall)) cst)
  ;; The code in this method has been moved to the corresponding
  ;; method of convert-special.  Ultimately, the code of every method
  ;; on CHECK-SPECIAL-FORM-SYNTAX will be moved.
  (declare (ignore cst))
  nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Checking MULTIPLE-VALUE-CALL (primop).

(defmethod check-special-form-syntax
    ((operator (eql 'cleavir-primop:multiple-value-call)) cst)
  ;; The code in this method has been moved to the corresponding
  ;; method of convert-special.  Ultimately, the code of every method
  ;; on CHECK-SPECIAL-FORM-SYNTAX will be moved.
  (declare (ignore cst))
  nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Checking VALUES.

(defmethod check-special-form-syntax
    ((operator (eql 'cleavir-primop:values)) cst)
  ;; The code in this method has been moved to the corresponding
  ;; method of convert-special.  Ultimately, the code of every method
  ;; on CHECK-SPECIAL-FORM-SYNTAX will be moved.
  (declare (ignore cst))
  nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Checking TYPEQ.

(defmethod check-special-form-syntax
    ((operator (eql 'cleavir-primop:typeq)) cst)
  ;; The code in this method has been moved to the corresponding
  ;; method of convert-special.  Ultimately, the code of every method
  ;; on CHECK-SPECIAL-FORM-SYNTAX will be moved.
  (declare (ignore cst))
  nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Checking FIXNUM-ADD

(defmethod check-special-form-syntax
    ((operator (eql 'cleavir-primop:fixnum-add)) cst)
  ;; The code in this method has been moved to the corresponding
  ;; method of convert-special.  Ultimately, the code of every method
  ;; on CHECK-SPECIAL-FORM-SYNTAX will be moved.
  (declare (ignore cst))
  nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Checking FIXNUM-SUB

(defmethod check-special-form-syntax
    ((operator (eql 'cleavir-primop:fixnum-sub)) cst)
  ;; The code in this method has been moved to the corresponding
  ;; method of convert-special.  Ultimately, the code of every method
  ;; on CHECK-SPECIAL-FORM-SYNTAX will be moved.
  (declare (ignore cst))
  nil)
