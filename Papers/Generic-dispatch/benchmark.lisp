;;; 1. Define the following class and the instance of it:

(defclass c () ((%x :initarg :x :reader x)))

(defparameter *i* (make-instance 'c :x 1))

;;; 2. Define this function.  Make sure it is compiled. 

(defun f ()
  (declare (optimize (safety 0) (speed 3) (debug 0)))
  (loop with i = *i*
        repeat 10000
        do (loop repeat 100000
                 do (x i))))

(compile 'f)

;;; 3. Run the following test.  It will probably take at least 10
;;;    seconds.  It may take minutes if your implementation is slow.

(time (f))

;;; 4. Send email to robert.strandh@gmail.com telling me how long it
;;;    took, what implementation you have, and what kind of computer
;;;    you have (CPU, clock speed, OS).
