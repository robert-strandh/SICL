(cl:in-package #:cleavir-walker)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function WALK-FORM.  
;;;
;;; This generic function is the main entry point for the code walker.

(defgeneric walk-form (form walker environment))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function WALK-SEQUENCE.  
;;;
;;; This generic function is called by the code walker in order to
;;; process a sequences of forms such as the forms of a body that is
;;; treated as a PROGN.

(defgeneric walk-sequence (sequence walker environment))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function WALK-SELF-EVALUATING.  
;;;
;;; This generic function is called by the code walker in order to
;;; process a self-evaluating object.

(defgeneric walk-self-evaluating (form walker environment))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function WALK-SYMBOL.  
;;;
;;; This generic function is called by the code walker in order to
;;; process a symbol.

(defgeneric walk-symbol (form walker environment))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function WALK-VARIABLE-INFO.  
;;;
;;; This generic function is called by the code walker in order to
;;; process a symbol according to the INFO instance describing the
;;; nature of the symbol as a variable name. 

(defgeneric walk-variable-info (form info walker environment))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function WALK-COMPOUND.  
;;;
;;; This generic function is called by the code walker in order to
;;; process a compound form.

(defgeneric walk-compound (form walker environment))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function WALK-FUNCTION-INFO.  
;;;
;;; This generic function is called by the code walker in order to
;;; process a compound form where the first element is a symbol.

(defgeneric walk-function-info (form info walker environment))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function WALK-LAMBDA-CALL.
;;;
;;; This function is called by the code walker in order to process a
;;; compound form where the first element is a CONS where the CAR is
;;; the symbol LAMBDA.

(defgeneric walk-lambda-call (form walker env))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Default method on WALK-SEQUENCE.

(defmethod walk-sequence (sequence walker env)
  (loop for form in sequence
	collect (walk-form form walker sequence)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Default method on WALK-SYMBOL.

(defmethod walk-symbol (form walker env)
  (let ((info (cleavir-env:variable-info env form)))
    (walk-variable-info form info walker env)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Default method on WALK-FORM.

(defmethod walk-form (form walker env)
  (cond ((and (not (consp form)) (not (symbolp form)))
	 (walk-self-evaluating form walker env))
	((symbolp form)
	 (walk-symbol form walker env))
	(t
	 (walk-compound form walker env))))
