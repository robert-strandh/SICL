(cl:in-package #:sicl-run-time)

;;; This function is called by code that replaces the
;;; INITIALIZE-VALUES-INSTRUCTION.  That instruction has a single
;;; output which is a lexical location that will hold all the multiple
;;; values to be used when the function passed to MULTIPLE-VALUE-CALL
;;; is invoked.  In order to allow for the APPEND-VALUES-INSTRUCTION
;;; to append values to that lexical location, we make it contain a
;;; list of length 51.  Recall that 50 is the limit allowed by the
;;; standard for the number of arguments to a function.  The first
;;; CONS cell of the list initially have both its CAR and its CDR
;;; pointing to the second CONS cell.  The CAR is then used by the
;;; APPEND-VALUES-INSTRUCTION as a pointer into the remaining list.
;;; The CDR holds the beginning of the list.
(defun initialize-return-values ()
  (let ((argument-list (make-list 50)))
    (cons argument-list argument-list)))
