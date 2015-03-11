(defclass c1 () ((%x :initarg :x :reader x)))
(defclass c2 (c1) ())
(defclass c3 (c1) ())
(defclass c4 (c1) ())
(defclass c5 (c1) ())

(defparameter *i1* (make-instance 'c1 :x 1))
(defparameter *i2* (make-instance 'c2 :x 1))
(defparameter *i3* (make-instance 'c3 :x 1))
(defparameter *i4* (make-instance 'c4 :x 1))
(defparameter *i5* (make-instance 'c5 :x 1))

(defun ff ()
  (declare (optimize (safety 0) (speed 3) (debug 0)))
  (loop with i1 = *i1*
	with i2 = *i2*
	with i3 = *i3*
	with i4 = *i4*
	with i5 = *i5*
        repeat 200000000
	do (x i1)
	   (x i2)
	   (x i3)
	   (x i4)
	   (x i5)))

(time (f)) ; 13s

(defstruct s class rack)

(defparameter *j* 
  (let ((rack (make-array 2 :initial-contents '(10 1))))
    (make-s :class nil :rack rack)))

(defun y (instance)
  (declare (optimize (safety 0) (speed 3) (debug 0)))
  (let* ((rack (s-rack instance))
         (stamp (svref rack 0)))
    (declare (type fixnum stamp))
    (if (= stamp 10)
        (svref rack 1)
        (error "1"))))

(proclaim '(notinline y))

(defun g ()
  (declare (optimize (safety 0) (speed 3) (debug 0)))
  (loop with j = *j*
        repeat 1000000000
	do (y j)))

(time (g)) ; < 3s

(defun yy (instance)
  (declare (optimize (safety 0) (speed 3) (debug 0)))
  (let* ((rack (s-rack instance))
         (stamp (svref rack 0)))
    (declare (type fixnum stamp))
    (cond ((> stamp 1280)
           (error "1"))
          ((> stamp 640)
           (error "2"))
          ((> stamp 320)
           (error "3"))
          ((> stamp 160)
           (error "4"))
          ((> stamp 80)
           (error "5"))
          ((> stamp 40)
           (error "6"))
          ((> stamp 20)
           (error "7"))
          ((> stamp 10)
           (error "8"))
          (t (svref rack 1)))))

(proclaim '(notinline yy))

(defun gg ()
  (declare (optimize (safety 0) (speed 3) (debug 0)))
  (loop with j = *j*
        repeat 1000000000
	do (yy j)))

(time (gg)) ; < 4.5s

(defun yyy (instance)
  (declare (optimize (safety 0) (speed 3) (debug 0)))
  (let* ((rack (s-rack instance))
         (stamp (svref rack 0)))
    (declare (type fixnum stamp))
    (cond ((> stamp 12800000)
           (error "1"))
          ((> stamp 6400000)
           (error "2"))
          ((> stamp 3200000)
           (error "3"))
          ((> stamp 1600000)
           (error "4"))
          ((> stamp 800000)
           (error "5"))
          ((> stamp 400000)
           (error "6"))
          ((> stamp 200000)
           (error "7"))
          ((> stamp 100000)
           (error "8"))
          (t (svref rack 1)))))

(proclaim '(notinline yyy))

(defun ggg ()
  (declare (optimize (safety 0) (speed 3) (debug 0)))
  (loop with j = *j*
        repeat 1000000000
        do (yyy j)))

(time (ggg)) ; < 4.5s
