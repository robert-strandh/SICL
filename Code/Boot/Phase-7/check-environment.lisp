(cl:in-package #:sicl-boot-phase-7)

(defun check-undefined-functions (e5)
  (loop for name being each hash-key of (clostrum-basic::functions e5)
        when (null (env:fboundp (env:client e5) e5 name))
          do (format t "Unbound function: ~s~%" name)))
