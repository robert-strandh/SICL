(cl:in-package #:cleavir-ir)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class DATUM.  
;;;
;;; This is the root class of all different kinds of data. 

(defclass datum ()
  ((%defining-instructions :initform '() :accessor defining-instructions)
   (%using-instructions :initform '() :accessor using-instructions)))

;;; Replace a datum with another in the instruction graph.
(defun replace-datum (new old)
  (loop for define in (cleavir-ir:defining-instructions old)
        do (cleavir-ir:substitute-output new old define))
  (loop for use in (cleavir-ir:using-instructions old)
        do (cleavir-ir:substitute-input new old use)))
