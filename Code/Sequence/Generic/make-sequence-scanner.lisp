(cl:in-package #:sicl-sequence)

(defconstant +scan-buffer-length+ 8)

(deftype scan-buffer ()
  `(simple-vector ,+scan-buffer-length+))

(deftype scan-index ()
  `(integer 0 (,(- array-total-size-limit +scan-buffer-length+))))

(defmacro with-scan-buffers ((&rest scan-buffer-names) &body body)
  `(let ,(loop for scan-buffer-name in scan-buffer-names
               collect `(,scan-buffer-name (make-array +scan-buffer-length+)))
     (declare (dynamic-extent ,@scan-buffer-names))
     (declare (scan-buffer ,@scan-buffer-names))
     ,@body))

(defmethod make-sequence-scanner ((list list))
  (values
   (lambda (list state scan-buffer)
     (declare (ignore list))
     (declare (list list state))
     (declare (scan-buffer scan-buffer))
     (declare (optimize (speed 3) (safety 0)))
     (loop for index of-type fixnum below (length scan-buffer) do
       (if (endp state)
           (return (values index state))
           (setf (svref scan-buffer index)
                 (pop state)))
           finally (return (values (length scan-buffer) state))))
   list))

(replicate-for-each-vector-class #1=#:vector-class
  (defmethod make-sequence-scanner ((vector #1#))
    (values
     (lambda (vector state scan-buffer)
       (declare (type #1# vector))
       (declare (scan-index state))
       (declare (scan-buffer scan-buffer))
       (declare (optimize (speed 3) (safety 0)))
       (let ((n (min (- (the array-length (length vector)) state)
                     (length scan-buffer))))
         (loop for index of-type fixnum below n do
           (setf (svref scan-buffer index)
                 (elt vector (+ state index))))
         (values n (+ state n))))
     0)))
