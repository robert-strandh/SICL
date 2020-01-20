(cl:in-package #:sicl-hash-table)

(defvar *default-hash-table-class*)

(defun make-hash-table (&rest rest
                        &key (test 'eql) size rehash-size rehash-threshold
                             (class *default-hash-table-class*))
  (declare (ignore test size rehash-size rehash-threshold))
  (apply #'make-instance class
         (append (loop for (keyword value) on rest by #'cddr
                       unless (eql keyword :class)
                         collect keyword and collect value))))
