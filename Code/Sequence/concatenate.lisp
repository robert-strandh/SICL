(cl:in-package #:sicl-sequence)

(defun concatenate (result-type &rest sequences)
  (multiple-value-bind (prototype length-constraint)
      (reify-sequence-type-specifier result-type)
    (let ((result (apply #'concatenate-sequence-like prototype sequences)))
      (unless (or (not length-constraint)
                  (and (integerp length-constraint)
                       (= (length result) length-constraint))
                  (typep result result-type))
        (error "Failed to concatenate ~:[the~;zero~] sequence~P~@
                      ~{ ~S~%~}into a sequence of type ~S."
               (null sequences)
               (length sequences)
               sequences
               result-type))
      result)))

(defmethod concatenate-sequence-like ((list list) &rest sequences)
  (sicl-utilities:with-collectors ((result collect))
    (loop for sequence in sequences do
      (map nil #'collect sequence))
    (result)))

(seal-domain #'concatenate-sequence-like '(list))

(replicate-for-each-vector-class #1=#:vector-class
  (defmethod concatenate-sequence-like ((vector #1#) &rest sequences)
    (let* ((length (loop for sequence in sequences sum (length sequence)))
           (result (make-sequence-like vector length))
           (index 0))
      (declare (array-length length index))
      (declare (type #1# result))
      (loop for sequence in sequences do
        (typecase sequence
          (#1#
           (let ((amount (length sequence)))
             (loop for offset below amount do
               (setf (elt result (+ index offset))
                     (elt sequence offset)))
             (incf index amount)))
          (list
           (loop for element in sequence do
             (setf (elt result index)
                   element)
             (incf index)))
          (otherwise
           (with-scan-buffers (scan-buffer)
             (multiple-value-bind (scanner state)
                 (make-sequence-scanner sequence)
               (declare (sequence-scanner scanner))
               (loop
                 (multiple-value-bind (amount new-state)
                     (funcall scanner sequence state scan-buffer)
                   (when (zerop amount) (return))
                   (loop for offset below amount do
                     (setf (elt result (+ index offset))
                           (svref scan-buffer offset)))
                   (setf state new-state)
                   (incf index amount))))))))
      result)))

(seal-domain #'concatenate-sequence-like '(vector))
