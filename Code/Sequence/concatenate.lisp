(cl:in-package #:sicl-sequence)

(defun concatenate (result-type &rest sequences)
  (with-reified-result-type (prototype result-type)
    (apply #'concatenate-sequence-like prototype sequences)))

(define-compiler-macro concatenate
    (&whole form result-type &rest sequences &environment env)
  (if (constantp result-type)
      (let ((type (eval result-type)))
        `(the ,type
              (concatenate-sequence-like
               ',(reify-sequence-type-specifier type env)
               ,@sequences)))
      form))

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
          (simple-vector #2=
           (let ((amount (length sequence)))
             (loop for offset below amount do
               (setf (elt result (+ index offset))
                     (elt sequence offset)))
             (incf index amount)))
          (#1# #2#)
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
