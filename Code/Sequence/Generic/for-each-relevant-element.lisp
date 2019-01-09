(cl:in-package #:sicl-sequence)

(defun canonicalize-start-and-end (sequence start end)
  (let* ((length (non-list-length sequence))
         (start (typecase start
                  (null 0)
                  ((integer 0) start)
                  (otherwise (error 'invalid-start-index-type
                                    :expected-type '(or null (integer 0))
                                    :datum start
                                    :sequence sequence))))
         (end (typecase end
                (null length)
                (integer end)
                (otherwise (error 'invalid-end-index-type
                                  :expected-type '(or null integer)
                                  :datum end
                                  :sequence sequence)))))
    (unless (<= end length)
      (error 'invalid-end-index
             :datum end
             :expected-type `(integer 0 ,length)
             :in-sequence sequence))
    (unless (<= start end)
      (error 'end-less-than-start
             :datum start
             :expected-type `(integer 0 ,end)
             :end-index end
             :in-sequence sequence))
    (values start end)))

(defmacro for-each-relevant-element
    ((element-var index-var sequence start end from-end) &body body)
  (let ((sequence-var (gensym))
        (start-var (gensym))
        (end-var (gensym)))
    `(let ((,sequence-var ,sequence))
       (multiple-value-bind (,start-var ,end-var)
           (canonicalize-start-and-end ,sequence-var ,start ,end)
         (declare (type fixnum ,start-var ,end-var))
         (if ,from-end
             (loop for ,index-var of-type fixnum downfrom (1- ,end-var) to ,start-var
                   do (let ((,element-var (non-list-elt ,sequence-var ,index-var)))
                        ,@body))
             (loop for ,index-var of-type fixnum from ,start-var below ,end-var
                   do (let ((,element-var (non-list-elt ,sequence-var ,index-var)))
                        ,@body)))))))
