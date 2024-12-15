(cl:in-package #:sicl-new-boot-phase-5)

(eval-when (:compile-toplevel) (sb:enable-parcl-symbols client))

(defvar *call-history-function*)

(defvar *setf-call-history-function*)

(defun generic-function-is-fresh-p (generic-function)
  (null (funcall *call-history-function* generic-function)))

(defun operator-is-a-candidate-p (name operator)
  (and (operator-is-a-generic-function-p operator)
       (or (generic-function-is-fresh-p operator)
           (let* ((symbol (if (symbolp name) name (second name)))
                  (table (sb::symbol-package sb::*boot*))
                  (package (gethash symbol table))
                  (name (parcl-low:name (make-instance 'client) package)))
             (string= name "CTYPE")))))

(defun set-default-discriminating-functions (client e4)
  (let* ((*standard-generic-function-class*
           (clo:find-class client e4 'standard-generic-function))
         (*call-history-function*
           (clo:fdefinition client e4 @clostrophilia:call-history))
         (*setf-call-history-function*
           (clo:fdefinition
            client e4 (list 'setf @clostrophilia:call-history)))
         (table (clostrum-basic::functions e4))
         (symbol @clostrophilia:compute-and-install-discriminating-function)
         (compute-and-install-discriminating-function-function
           (clo:fdefinition client e4 symbol)))
    (loop for entry being each hash-value of table using (hash-key name)
          for cell = (clostrum-basic::cell entry)
          for operator = (car cell)
          when (operator-is-a-candidate-p name operator)
            do ; (format *trace-output* "Processing ~s~%" name)
               (funcall *setf-call-history-function*
                        '() operator)
               (funcall compute-and-install-discriminating-function-function
                        operator))))

         
