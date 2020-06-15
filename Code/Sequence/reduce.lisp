(cl:in-package #:sicl-sequence)

(defmethod reduce
    (function (list list)
     &key key from-end (start 0) end (initial-value nil initial-value-p))
  (let ((function (function-designator-function function))
        (rest (nthcdr start list)))
    (declare (function function))
    (if (or (endp rest) (eql start end))
        (if initial-value-p initial-value (funcall function))
        (with-key-function (key key)
          (if (not from-end)
              ;; Forward.
              (if initial-value-p
                  (let ((value initial-value))
                    (if (null end)
                        (loop for element in rest
                              do (setf value (funcall function value (key element))))
                        (loop for element in rest
                              for counter fixnum below (- end start)
                              do (setf value (funcall function value (key element)))))
                    value)
                  (let ((value (key (first rest))))
                    (if (null end)
                        (loop for element in (rest rest)
                              do (setf value (funcall function value (key element))))
                        (loop for element in (rest rest)
                              for counter fixnum below (- end start 1)
                              do (setf value (funcall function value (key element)))))
                    value))
              ;; Backward.
              (if initial-value-p
                  (let ((value initial-value))
                    (for-each-relevant-cons (cons index list start end t)
                      (setf value (funcall function (key (car cons)) value)))
                    value)
                  (let ((value '.no-value.))
                    (for-each-relevant-cons (cons index list start end t)
                      (let ((current (key (car cons))))
                        (if (eq value '.no-value.)
                            (setf value current)
                            (setf value (funcall function current value)))))
                    value)))))))

(seal-domain #'reduce '(t list))

(replicate-for-each-vector-class #1=#:vector-class
  (defmethod reduce
      (function (vector #1#)
       &key key from-end (start 0) end (initial-value nil initial-value-p))
    (let ((function (function-designator-function function)))
      (declare (function function))
      (multiple-value-bind (start end)
          (canonicalize-start-and-end vector start end)
        (if (= start end)
            (if initial-value-p initial-value (funcall function))
            (with-key-function (key key)
              (if (not from-end)
                  ;; Forward.
                  (multiple-value-bind (value initial)
                      (if initial-value-p
                          (values initial-value start)
                          (values (key (elt vector start)) (1+ start)))
                    (loop for index fixnum from initial below end do
                      (setf value (funcall function value (key (elt vector index)))))
                    value)
                  ;; Backward.
                  (multiple-value-bind (value initial)
                      (if initial-value-p
                          (values initial-value (- end 1))
                          (values (key (elt vector (1- end))) (- end 2)))
                    (loop for index fixnum downfrom initial to start do
                      (setf value (funcall function (key (elt vector index)) value)))
                    value))))))))

(seal-domain #'reduce '(t vector))
