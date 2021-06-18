(cl:in-package #:cleavir-type-inference)

(defun bag-join (&rest bags)
  (when (null bags)
    (return-from bag-join nil)) ; an empty bag.
  (loop for b in bags
        do (assert (valid-bag-p b)))
  (loop for (location . type-descriptor) in (first bags)
        collect
        (cons location
              (loop with result = type-descriptor
                    for b in (rest bags)
                    for r = (assoc location b :test #'eq)
                    for type = (cdr r)
                    do (setf result
                             (etypecase location
                               (cleavir-ir:lexical-location
                                (binary-join type-descriptor type))
                               (cleavir-ir:values-location
                                (values-binary-join type-descriptor type))))
                    finally (return result)))))
