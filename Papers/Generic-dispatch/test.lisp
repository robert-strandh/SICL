(defclass c () ((%x :initarg :x :reader x)))

(defparameter *l* (list (make-instance 'c :x 1)))
               
(setf (cdr *l*) *l*)

(defun f ()
  (declare (optimize (safety 0) (speed 3) (debug 0)))
  (loop for l = *l* then (cdr l)
        repeat 1000000000
        maximize (x (car l))))

(time (f))

(defstruct s class rack)

(defparameter *l2* 
  (let ((rack (make-array 2 :initial-contents '(10 1))))
    (list (make-s :class nil :rack rack))))

(setf (cdr *l2*) *l2*)

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
  (loop for l = *l2* then (cdr l)
        repeat 1000000000
        maximize (y (car l))))

(time (g))

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
  (loop for l = *l2* then (cdr l)
        repeat 1000000000
        maximize (yy (car l))))

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
  (loop for l = *l2* then (cdr l)
        repeat 1000000000
        maximize (yyy (car l))))
