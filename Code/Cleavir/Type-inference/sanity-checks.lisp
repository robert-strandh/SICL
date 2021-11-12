(cl:in-package #:cleavir-type-inference)

;;; Things we track.
(deftype typed-location ()
  '(or cleavir-ir:lexical-location cleavir-ir:values-location))

(defun valid-bag-p (bag)
  (and (every (lambda (restriction)
                (and (consp restriction)
                     (typep (first restriction) 'typed-location)))
              bag)
       (= (length bag)
          (length (remove-duplicates bag :test #'eq :key #'first)))))
