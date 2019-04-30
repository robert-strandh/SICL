(cl:in-package #:sicl-hir-to-cl)

(defun sort-functions (initial-instruction)
  (let ((table (make-hash-table :test #'eq)))
    (cleavir-ir:map-instructions-with-owner
     (lambda (instruction owner)
       (when (null (nth-value 1 (gethash owner table)))
         (setf (gethash owner table) '()))
       (when (typep instruction 'cleavir-ir:enclose-instruction)
         (push (cleavir-ir:code instruction)
               (gethash owner table))))
     initial-instruction)
    (let ((result '()))
      (maphash (lambda (key value)
                 (push (cons key value) result))
               table)
      (loop until (null result)
            for candidate = (find-if (lambda (list) (null (rest list))) result)
            collect (first candidate)
            do (setf result (delete candidate result))
               (loop for remaining in result
                     do (setf (rest remaining)
                              (delete (first candidate) (rest remaining))))))))

