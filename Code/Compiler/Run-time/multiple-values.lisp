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
(defun initialize-values ()
  (let ((argument-list (make-list 50)))
    (cons argument-list argument-list)))

;;; When this function is called, the VALUES contains the 51-element
;;; list described above.  We need to extract the values that were
;;; appended by the APPEND-VALUES-INSTRUCTION and then APPLY the
;;; function.
(defun call-with-values (function values)
  (apply function
         (loop with end = (car values)
               for cell = (cdr values) then (cdr cell)
               until (eq cell end)
               collect (car cell))))

;;; This function is called as a result of the HIR instruction
;;; SAVE-VALUES-INSTRUCTION.  That instruction has no inputs and a
;;; single output which is a new dynamic environment.  It conses an
;;; entry that holds all the values in the globally reserved place for
;;; them, so that these values can later be restored.
(defun save-values ()
  ;; FIXME: define the function
  nil)

(defun restore-values ()
  ;; FIXME: define the function
  nil)
