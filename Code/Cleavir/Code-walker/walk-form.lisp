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
