(cl:in-package #:sicl-sequence)

(defmethod map (result-type function sequence &rest more-sequences)
  (labels ((map-aux (writer terminate)
             (let ((readers (make-array (1+ (length more-sequences)))))
               (setf (elt readers 0)
                     (make-sequence-reader sequence 0 nil nil terminate))
               (loop for index from 1 and sequence in more-sequences do
                 (setf (elt readers index)
                       (make-sequence-reader sequence 0 nil nil terminate)))
               (apply-to-sequence-iterators writer function readers))))
    (case result-type
      ((nil)
       (map-aux
        (lambda (elt)
          (declare (ignore elt)))
        (lambda (n)
          (declare (ignore n))
          (return-from map nil))))
      ((list)
       (sicl-utilities:with-collectors ((result collect))
         (map-aux
          (lambda (elt)
            (collect elt))
          (lambda (n)
            (declare (ignore n))
            (return-from map (result))))))
      (otherwise
       (let ((result-sequence
               (make-sequence
                result-type
                (reduce #'max more-sequences
                        :key #'length
                        :initial-value (length sequence)))))
         (map-aux
          (make-sequence-writer result-sequence 0 nil nil #'identity)
          (lambda (n)
            (declare (ignore n))
            (return-from map result-sequence))))))))

(seal-domain #'map '(t t t))
