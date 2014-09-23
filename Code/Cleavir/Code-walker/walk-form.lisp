(cl:in-package #:cleavir-walker)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function WALK-FORM.  
;;;
;;; This generic function is the main entry point for the code walker.

(defgeneric walk-form (form walker environment))
