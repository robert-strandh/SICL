(cl:in-package #:sicl-list-hash-table)

(defmethod gethash (key (hash-table list-hash-table) &optional default)
  (declare (ignore default))
  (with-accessors ((contents contents)
                   (test hash-table-test))
      hash-table
    (let ((entry (assoc key contents :key #'car :test test)))
      (if (null entry)
          (values nil nil)
          (values (cdr entry) t)))))
