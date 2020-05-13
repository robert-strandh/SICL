(cl:in-package #:sicl-sequence)

(defun concatenate (result-type &rest sequences)
  (multiple-value-bind (prototype length)
      (reify-sequence-type-specifier result-type)
    (let ((result (apply #'concatenate-into-sequence-like prototype sequences)))
      (when length
        (unless (or (and (integerp length) (= (length result) length))
                    (typep result result-type))
          (error "Failed to concatenate ~:[the~;~] ~R sequences~@
                      ~{ ~S~%~}into a sequence of type ~S."
                 (null sequences)
                 (length sequences)
                 sequences
                 result-type)))
      result)))

(defmethod concatenate-into-sequence-like ((list list) &rest sequences)
  (sicl-utilities:with-collectors ((result collect))
    (loop for sequence in sequences do
      (map nil collect sequence))
    (result)))

(replicate-for-each-vector-class #1=#:vector-class
  (defmethod concatenate-into-sequence-like ((vector #1#) &rest sequences)
    (let* ((length (loop for sequence in sequences sum (length sequence)))
           (result (make-sequence-like vector length))
           (index 0))
      (declare (array-length length index))
      (declare (type #1# result))
      (with-scan-buffers (scan-buffer)
        (loop for sequence in sequences do
          (typecase sequence
            (#1#
             (let ((amount (length sequence)))
               (loop for offset below amount do
                 (setf (elt result (+ index offset))
                       (elt sequence offset)))
               (incf index amount)))
            (list
             (loop for element in sequence
                   for offset of-type array-length from 0 do
                     (setf (elt result (+ index offset))
                           element)))
            (otherwise
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
