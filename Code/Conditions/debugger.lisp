(cl:in-package #:sicl-conditions)

(defun debugger (condition)
  (loop (format *debug-io* "Debug> ")
        (finish-output *debug-io*)
        (let ((command (read *debug-io*)))
          (case command
            (:bt
             (funcall (find-symbol (symbol-name '#:inspect)
                                   '#:sicl-backtrace-inspector)
                      sicl-hir-interpreter:*call-stack*))))))

(defgeneric invoke-debugger (condition))

(defmethod invoke-debugger ((condition condition))
  (unless (null *debugger-hook*)
    (let ((debugger-hook *debugger-hook*)
          (*debugger-hook* nil))
      (funcall debugger-hook condition debugger-hook)))
  (debugger condition))
