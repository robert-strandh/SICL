(cl:in-package #:sicl-sequence)

;;; A sequence scanner is a function that takes a sequence, a state, and a
;;; scan buffer.  The scanner fills the scan buffer with the elements of
;;; the sequence, starting from a position designated by the supplied
;;; state.  The scanner returns two values - the number of elements that
;;; have been copied to the buffer, and a new state that designates the
;;; part of the sequence right after those that have just been copied.

(defconstant +scan-buffer-length+ 8)

(deftype scan-buffer ()
  `(simple-vector ,+scan-buffer-length+))

(deftype scan-amount ()
  `(integer 0 ,+scan-buffer-length+))

(deftype sequence-scanner ()
  '(function (sequence t scan-buffer) (values scan-amount t &optional)))

(defmacro with-scan-buffers ((&rest scan-buffer-names) &body body)
  `(let ,(loop for scan-buffer-name in scan-buffer-names
               collect `(,scan-buffer-name (make-array +scan-buffer-length+)))
     (declare (dynamic-extent ,@scan-buffer-names))
     (declare (scan-buffer ,@scan-buffer-names))
     ,@body))

(defmethod make-sequence-scanner ((list list))
  (values
   (the sequence-scanner
    (lambda (list state scan-buffer)
      (declare (ignore list))
      (declare (list list state))
      (declare (scan-buffer scan-buffer))
      (loop for index of-type fixnum below (length scan-buffer) do
        (if (endp state)
            (return (values index state))
            (setf (svref scan-buffer index)
                  (pop state)))
            finally (return (values state (length scan-buffer))))))
   list))

(replicate-for-each-vector-class #1=#:vector-class
  (defmethod make-sequence-scanner ((vector #1#))
    (values
     (the sequence-scanner
      (lambda (vector state scan-buffer)
        (declare (type #1# vector))
        (declare (array-length state))
        (declare (scan-buffer scan-buffer))
        (let ((n (min (- (length vector) state)
                      (length scan-buffer))))
          (loop for index of-type fixnum below n do
            (setf (svref scan-buffer index)
                  (elt vector (+ state index))))
          (values n (+ state n)))))
     0)))
