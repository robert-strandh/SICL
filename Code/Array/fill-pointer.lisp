(cl:in-package #:sicl-array)

(defgeneric fill-pointer (vector))

(defgeneric (setf fill-pointer) (new-fill-pointer vector))

(defmethod fill-pointer ((vector vector))
  (let ((fill-pointer (vector-fill-pointer vector)))
    (if (null fill-pointer)
        ;; FIXME: signal a more specific condition
        (error 'type-error
               :datum vector
               :expected-type 'vector)
        fill-pointer)))

(defmethod (setf fill-pointer) (new-fill-pointer (vector vector))
  (cond ((null (vector-fill-pointer vector))
         ;; FIXME: signal a more specific condition
         (error 'type-error
                :datum vector
                :expected-type 'vector))
        ((not (integerp new-fill-pointer))
         (error 'type-error
                :datum new-fill-pointer
                :expected-type 'integer))
        ((or (minusp new-fill-pointer)
             (> new-fill-pointer (array-total-size vector)))
         (error 'type-error
                :datum new-fill-pointer
                :expected-type `(integer 0 ,(array-total-size vector))))
        (t
         (setf (vector-fill-pointer vector) new-fill-pointer))))
