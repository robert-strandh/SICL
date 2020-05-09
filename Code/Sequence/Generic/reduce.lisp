(cl:in-package #:sicl-sequence)

(defmethod reduce
    (function (list list)
     &key key from-end (start 0) end (initial-value nil initial-value-p))
  (setf function (function-designator-function function))
  (if (endp list)
      (if initial-value-p initial-value (funcall function))
      (with-key-function (key key)
        (if (not from-end)
            ;; Forward.
            (multiple-value-bind (value rest)
                (if initial-value-p
                    (values initial-value list)
                    (values (key (first list)) (rest list)))
              (for-each-relevant-cons (cons index rest start end)
                (setf value (funcall function value (key (car cons)))))
              value)
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
                  value))))))

(seal-domain #'reduce '(t list))

(replicate-for-each-relevant-vectoroid #1=#:vectoroid
  (defmethod reduce
      (function (vector #1#)
       &key key from-end (start 0) end (initial-value nil initial-value-p))
    (setf function (function-designator-function function))
    (if (zerop (length vector))
        (if initial-value-p initial-value (funcall function))
        (multiple-value-bind (start end)
            (canonicalize-start-and-end vector start end)
          (with-key-function (key key)
            (if (not from-end)
                ;; Forward.
                (multiple-value-bind (value start)
                    (if initial-value-p
                        (values initial-value start)
                        (values (key (elt vector start)) (1+ start)))
                  (loop for index from start below end do
                    (setf value (funcall function value (key (elt vector index)))))
                  value)
                ;; Backward.
                (multiple-value-bind (value end)
                    (if initial-value-p
                        (values initial-value end)
                        (values (key (elt vector (1- end))) (1- end)))
                  (loop for index downfrom (1- end) to start do
                    (setf value (funcall function (key (elt vector index)) value)))
                  value)))))))

(seal-domain #'reduce '(t vector))
