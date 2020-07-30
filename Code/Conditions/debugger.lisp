(cl:in-package #:sicl-conditions)

(defun backtrace ()
  "This function will be redefined")

(defun debugger (condition)
  (format *debug-io*
          "Debugger entered on ~s~%" condition)
  (loop (format *debug-io* "Debug> ")
        (finish-output *debug-io*)
        (let ((command (read *debug-io*)))
          (case command
            (:bt (backtrace))))))

(defgeneric invoke-debugger (condition))

(defmethod invoke-debugger ((condition condition))
  (unless (null *debugger-hook*)
    (let ((debugger-hook *debugger-hook*)
          (*debugger-hook* nil))
      (funcall debugger-hook condition debugger-hook)))
  (debugger condition))
