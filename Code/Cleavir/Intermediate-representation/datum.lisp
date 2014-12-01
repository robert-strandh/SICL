(cl:in-package #:cleavir-ir)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class DATUM.  
;;;
;;; This is the root class of all different kinds of data. 

(defclass datum ()
  ((%defining-instructions :initform '() :accessor defining-instructions)
   (%using-instructions :initform '() :accessor using-instructions)))
