(cl:in-package #:sicl-new-boot-phase-5)

(eval-when (:compile-toplevel) (sb:enable-parcl-symbols client))

(defvar *call-history-function*)

(defun operator-is-a-fresh-generic-function-p (operator)
  (and (typep operator 'sb:header)
       (eq (sb:class operator) *standard-generic-function-class*)
       (null (funcall *call-history-function* operator))))

(defun set-default-discriminating-functions (client e4)
  (let* ((*standard-generic-function-class*
           (clo:find-class client e4 'standard-generic-function))
         (*call-history-function*
           (clo:fdefinition client e4 @clostrophilia:call-history))
         (table (clostrum-basic::functions e4))
         (symbol @clostrophilia:compute-and-install-discriminating-function)
         (compute-and-install-discriminating-function-function
           (clo:fdefinition client e4 symbol)))
    (loop for entry being each hash-value of table using (hash-key name)
          for cell = (clostrum-basic::cell entry)
          for operator = (car cell)
          when (operator-is-a-fresh-generic-function-p operator)
            do ; (format *trace-output* "Processing ~s~%" name)
               (funcall compute-and-install-discriminating-function-function
                        operator))))

         
