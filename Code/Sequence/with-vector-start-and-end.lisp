(cl:in-package #:sicl-sequence)

(defmacro with-vector-start-and-end
    ((start-name end-name &optional (length-name nil length-name-p))
     (vector start end)
     &body body)
  `(multiple-value-bind (,start-name ,end-name ,@(when length-name-p `(,length-name)))
       (canonicalize-vector-start-and-end ,vector ,start ,end)
     (declare (vector-length ,start-name ,end-name ,@(when length-name-p `(,length-name))))
     ,@body))

(declaim (inline canonicalize-vector-start-and-end))
(defun canonicalize-vector-start-and-end (vector start end)
  (declare (vector vector))
  (let* ((length (the vector-length (length vector)))
         (start
           (typecase start
             (vector-length start)
             (otherwise
              (error 'invalid-start-index
                     :expected-type 'vector-length
                     :datum start
                     :in-sequence vector))))
         (end
           (typecase end
             (null length)
             (vector-length end)
             (otherwise
              (error 'invalid-end-index
                     :expected-type `(or null (integer 0 ,length))
                     :datum end
                     :in-sequence vector)))))
    (unless (<= start end)
      (error 'end-less-than-start
             :datum start
             :expected-type `(integer 0 ,end)
             :end-index end
             :in-sequence vector))
    (values start end length)))
