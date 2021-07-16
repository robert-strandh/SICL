(cl:in-package #:cleavir-code-utilities)

(defun check-argcount (form min-argcount max-argcount)
  (when (< (length (cdr form)) min-argcount)
    (error 'invalid-number-of-arguments
           :form form
           :min-argcount min-argcount
           :max-argcount max-argcount))
  (when (and (not (null max-argcount))
             (> (length (cdr form)) max-argcount)
    (error 'invalid-number-of-arguments
           :form form
           :min-argcount min-argcount
           :max-argcount max-argcount))))
