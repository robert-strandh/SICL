(cl:in-package #:sicl-sequence)

(defmacro with-list-start-and-end
    ((start-name end-name &optional (length-name nil length-name-p))
     (list start end)
     &body body)
  `(multiple-value-bind (,start-name ,end-name ,@(when length-name-p `(,length-name)))
       (canonicalize-list-start-and-end ,list ,start ,end)
     (declare (list-length ,start-name ,end-name ,@(when length-name-p `(,length-name))))
     ,@body))

(declaim (inline canonicalize-list-start-and-end))
(defun canonicalize-list-start-and-end (list start end)
  (declare (list list))
  (let* ((length (the list-length (length list)))
         (start
           (typecase start
             (list-length start)
             (otherwise
              (error 'invalid-start-index
                     :expected-type 'list-length
                     :datum start
                     :in-sequence list))))
         (end
           (typecase end
             (null length)
             (list-length end)
             (otherwise
              (error 'invalid-end-index
                     :expected-type `(or null (integer 0 ,length))
                     :datum end
                     :in-sequence list)))))
    (unless (<= start end)
      (error 'end-less-than-start
             :datum start
             :expected-type `(integer 0 ,end)
             :end-index end
             :in-sequence list))
    (values start end length)))
