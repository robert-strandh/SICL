;;;; src/debugger.lisp

(in-package #:portable-condition-system)

;;; DEFINE-COMMAND

(defgeneric run-debugger-command (command stream condition &rest arguments))

(defmethod run-debugger-command (command stream condition &rest arguments)
  (declare (ignore arguments))
  (format stream "~&;; ~S is not a recognized command.
;; Type :HELP for available commands.~%" command))

(defmacro define-command (name (stream condition &rest arguments) &body body)
  (check-type name keyword)
  (let ((command-var (gensym "COMMAND"))
        (arguments-var (gensym "ARGUMENTS")))
    (multiple-value-bind (real-body declarations documentation)
        (parse-body body :documentation t)
      `(defmethod run-debugger-command
           ((,command-var (eql ,name)) ,stream ,condition &rest ,arguments-var)
         ,@(when documentation `(,documentation))
         ,@declarations
         (destructuring-bind ,arguments ,arguments-var ,@real-body)))))

;;; Debugger commands

(defvar *debug-level* 0)

(define-command :eval (stream condition &optional form)
  (let ((level *debug-level*))
    (with-simple-restart (abort "Return to debugger level ~D." level)
      (let* ((real-form (or form (read stream)))
             (- real-form)
             (values (multiple-value-list (eval real-form))))
        (format stream "~&~{~S~^~%~}" values)
        (values values real-form)))))

(define-command :report (stream condition &optional (level *debug-level*))
  (format stream "~&;; Debugger level ~D entered on ~S:~%"
          level (type-of condition))
  (handler-case (let* ((report (princ-to-string condition))
                       (lines (split-sequence #\Newline report
                                              :remove-empty-subseqs t)))
                  (format stream "~&~{;; ~A~%~}" lines))
    (error () (format stream "~&;; #<error while reporting condition>~%"))))

(define-command :condition (stream condition)
  (run-debugger-command :eval stream condition condition))

(defun restart-max-name-length (restarts)
  (flet ((name-length (restart) (length (string (restart-name restart)))))
    (if restarts (reduce #'max (mapcar #'name-length restarts)) 0)))

(define-command :restarts (stream condition)
  (let ((restarts (compute-restarts condition)))
    (cond (restarts
           (format stream "~&;; Available restarts:~%")
           (loop with max-name-length = (restart-max-name-length restarts)
                 for i from 0
                 for restart in restarts
                 for report = (handler-case (princ-to-string restart)
                                (error () "#<error while reporting restart>"))
                 for restart-name = (or (restart-name restart) "")
                 do (format stream ";; ~2,' D: [~vA] ~A~%"
                            i max-name-length restart-name report)))
          (t (format stream "~&;; No available restarts.~%")))))

(define-command :restart (stream condition &optional n)
  (let* ((n (or n (read stream)))
         (restart (nth n (compute-restarts condition))))
    (if restart
        (invoke-restart-interactively restart)
        (format stream "~&;; There is no restart with number ~D.~%" n))))

(defun debugger-invoke-restart (name stream condition)
  (let ((restart (find-restart name condition)))
    (if restart
        (invoke-restart-interactively restart)
        (format stream "~&;; There is no active ~A restart.~%" name))))

(define-command :abort (stream condition)
  (debugger-invoke-restart 'abort stream condition))

(define-command :q (stream condition)
  (debugger-invoke-restart 'abort stream condition))

(define-command :continue (stream condition)
  (debugger-invoke-restart 'continue stream condition))

(define-command :c (stream condition)
  (debugger-invoke-restart 'continue stream condition))

(defvar *help-hooks* '())

(define-command :help (stream condition)
  (format stream "~&~
;; This is the standard debugger of the Portable Condition System.
;; The debugger read-eval-print loop supports the standard REPL variables:
;;   *   **   ***   +   ++   +++   /   //   ///   -
;;
;; Available debugger commands:
;;  :HELP              Show this text.
;;  :EVAL <form>       Evaluate a form typed after the :EVAL command.
;;  :REPORT            Report the condition the debugger was invoked with.
;;  :CONDITION         Return the condition the debugger was invoked with.
;;  :RESTARTS          Print available restarts.
;;  :RESTART <n>, <n>  Invoke a restart with the given number.")
  (when (find-restart 'abort condition)
    (format stream "~&;;  :ABORT, :Q         Invoke an ABORT restart.~%"))
  (when (find-restart 'continue condition)
    (format stream "~&;;  :CONTINUE, :C      Invoke a CONTINUE restart.~%"))
  (dolist (hook *help-hooks*)
    (funcall hook condition stream))
  (format stream "~&~
;;
;; Any non-keyword non-integer form is evaluated.~%"))

;;; Debugger implementation

(defun read-eval-print-command (stream condition)
  (format stream "~&[~D] Debug> "*debug-level*)
  (let* ((thing (read stream)))
    (multiple-value-bind (values actual-thing)
        (typecase thing
          (keyword (run-debugger-command thing stream condition))
          (integer (run-debugger-command :restart stream condition thing))
          (t (run-debugger-command :eval stream condition thing)))
      (unless actual-thing (setf actual-thing thing))
      (prog1 values
        (shiftf /// // / values)
        (shiftf *** ** * (first values))
        (shiftf +++ ++ + actual-thing)))))

(defun standard-debugger (condition &optional (stream *debug-io*))
  "Implements the standard debugger."
  (let ((*debug-level* (1+ *debug-level*)))
    (run-debugger-command :report stream condition)
    (format stream "~&;; Type :HELP for available commands.~%")
    (loop (read-eval-print-command stream condition))))

;;; Debugger interface

(defvar *debugger-hook* nil)

(defgeneric invoke-debugger (condition))

(defmethod invoke-debugger ((condition condition))
  (when *debugger-hook*
    (let ((hook *debugger-hook*)
          (*debugger-hook* nil))
      (funcall hook condition hook)))
  (standard-debugger condition))

(defun break (&optional (format-control "Break") &rest format-arguments)
  (let ((*debugger-hook* nil))
    (with-simple-restart (continue "Return from BREAK.")
      (invoke-debugger
       (make-condition 'simple-condition
                       :format-control format-control
                       :format-arguments format-arguments))))
  nil)
