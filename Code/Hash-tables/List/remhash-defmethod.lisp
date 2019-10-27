(cl:in-package #:sicl-list-hash-table)

(defmethod remhash (key (hash-table list-hash-table))
  (with-accessors ((contents contents)
                   (test hash-table-test))
      (let ((position (position key contents :key #'car :test test)))
        (if (null position)
            nil
            (progn (setf contents
                         (nconc (subseq contents 0 position)
                                (nthcdr (1+ position) contents)))
                   t)))))
