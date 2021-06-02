(cl:in-package #:sicl-stream)

(defmacro with-open-stream ((variable stream-form) &body body)
  (multiple-value-bind (declarations forms)
      (cleavir-code-utilities:separate-ordinary-body body)
    `(let ((,variable ,stream-form))
       ,@declarations
       (unwind-protect (progn ,@forms)
         (close ,variable)))))
