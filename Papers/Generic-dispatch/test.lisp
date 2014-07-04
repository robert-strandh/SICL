(defclass c () ((%x :initarg :x :reader x)))

(defparameter *i* (make-instance 'c :x 1))

(defun f ()
  (declare (optimize (safety 0) (speed 3) (debug 0)))
  (loop with i = *i*
        repeat 1000000000
	do (x i)))

(time (f)) ; 13s

(defstruct s class rack)

(defparameter *j* 
  (let ((rack (make-array 2 :initial-contents '(10 1))))
    (make-s :class nil :rack rack)))

(defun y1 (instance)
  (declare (optimize (safety 0) (speed 3) (debug 0)))
  (let* ((rack (s-rack instance))
         (stamp (svref rack 0)))
    (declare (type fixnum stamp))
    (if (= stamp 10)
        (svref rack 1)
        (error "1"))))

(proclaim '(notinline y1))

(defun g1 ()
  (declare (optimize (safety 0) (speed 3) (debug 0)))
  (loop with j = *j*
        repeat 1000000000
	do (y1 j)))

(time (g1)) ; < 3s

(defun y2 (instance)
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

(proclaim '(notinline y2))

(defun g2 ()
  (declare (optimize (safety 0) (speed 3) (debug 0)))
  (loop with j = *j*
        repeat 1000000000
	do (y2 j)))

(time (g2)) ; < 4.5s

(defun y3 (instance)
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

(proclaim '(notinline y3))

(defun g3 ()
  (declare (optimize (safety 0) (speed 3) (debug 0)))
  (loop with j = *j*
        repeat 1000000000
        do (y3 j)))

(time (g3)) ; < 4.5s

(defun y4 (instance)
  (declare (optimize (safety 0) (speed 3) (debug 0)))
  (let* ((rack (s-rack instance))
         (stamp (svref rack 0)))
    (declare (type fixnum stamp))
    (if (< stamp 2)
        (if (< stamp 1)
            (svref rack 1)
            (svref rack 2))
        (if (< stamp 3)
            (svref rack 3)
            (svref rack 4)))))

(proclaim '(notinline y4))

(defun make (stamp)
  (let ((rack (make-array 5
                          :initial-contents
                          (cons stamp '(1 1 1 1)))))
    (make-s :class nil :rack rack)))

(defun g4 ()
  (declare (optimize (safety 0) (speed 3) (debug 0)))
  (loop with j0 = (make 0)
        with j1 = (make 1)
        with j2 = (make 2)
        with j3 = (make 3)
        repeat 250000000
        do (y4 j0) (y4 j2) (y4 j1) (y4 j3)))

(time (g4)) ; 3.3s
