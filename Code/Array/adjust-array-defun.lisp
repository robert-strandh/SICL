(cl:in-package #:sicl-array)

(defun copy-elements (from-array to-array dimensions)
  (let ((rank (length dimensions)))
    (case rank
      (0
       (setf (aref to-array) (aref from-array)))
      (1
       (loop for index from 0 below (first dimensions)
             do (setf (aref to-array index)
                      (aref from-array index))))
      (2
       (loop for index1 from 0 below (first dimensions)
             do (loop for index2 from 0 below (second dimensions)
                      do (setf (aref to-array index1)
                               (aref from-array index2)))))
      (otherwise
       (labels ((aux (indices-so-far remaining-dimensions)
                  (if (null remaining-dimensions)
                      (apply #'(setf aref)
                             (apply #'aref from-array indices-so-far)
                             to-array indices-so-far)
                      (loop for index from 0 below (first remaining-dimensions)
                            do (aux (cons index indices-so-far)
                                    (rest remaining-dimensions))))))
         (aux '() (reverse dimensions)))))))
