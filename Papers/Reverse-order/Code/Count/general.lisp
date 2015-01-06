(defun time-in-sec (tstart tend)
  (/ (- tend tstart) internal-time-units-per-second))

(defmacro get-time (call)
 `(let ((tstart (get-internal-run-time)))
    (values
     ,call
     (time-in-sec tstart (get-internal-run-time)))))

(defmacro get-just-time (call)
 `(let ((tstart (get-internal-run-time)))
    ,call
    (time-in-sec tstart (get-internal-run-time))))

(defun print-time (time &optional (stream t))
  (multiple-value-bind (min sec) (floor time 60)
    (if (zerop min)
	(format stream " in ~Asec~%" (coerce sec 'float))
	(format stream " in ~Amin ~Asec~%" min (coerce sec 'float)))))

(defmacro with-time (call &optional (stream t))
  `(multiple-value-bind (res time)
       (get-time ,call)
       (print-time time ,stream)
     res))

(defmacro evaluate-time (call &optional (times 1))
  (let ((fois (gensym)))
    `(let ((,fois ,times))
       (coerce
	(/
	 (loop
	   repeat ,fois
	   sum (get-just-time ,call))
	 ,fois)
	'float))))

(defun time-string (&optional (seconds t))
   (multiple-value-bind (s m h day month year) (get-decoded-time)
     (declare (ignore s))
     (if seconds
	 (format nil "~A-~2,'0D-~2,'0D-~A:~A" year month day h m)
	 (format nil "~A-~A-~A-~A" year month day h))))

(defun date-string (&optional (string ""))
  (concatenate 'string
	       (time-string nil)
	       "-"
	       string))

(defun zeros (n)
  (make-list n :initial-element 0))

(defun more-zeros (n l)
  (nconc (make-zeros n) l))

(defun mouline-stream (stream reverse-count-fun n &key (step 1) (start 1) (times 1))
  (loop
    repeat n
    for k from start by step
    for list = (make-zeros start) then (more-zeros step list)
    do (format stream "~3D ~A~%" k (evaluate-time (funcall reverse-count-fun 0 list) times))))

(defun mouline-file (file reverse-count-fun n &key (step 1) (start 1) (times 1))
  (with-open-file (stream (date-string file)
			  :direction :output :if-does-not-exist :create :if-exists :supersede)
    (mouline-next-stream stream reverse-count-fun n :step step :start start :times times)))

(defun mouline-truc-versus-machin-one-stream
    (stream reverse-count-fun1 reverse-count-fun2 start end &key (step 1) (times 1))
  (loop
    for k from start to end by step
    for list = (make-zeros start) then (more-zeros step list)
    do (let ((truc-time (evaluate-time (funcall reverse-count-fun1 0 list) times))
	     (machin-time (evaluate-time (funcall reverse-count-fun2 0 list) times)))
	 (format t "~3D ~10,5F ~10,5F~%" k truc-time machin-time)
	 (format stream "~3D ~10,5F ~10,5F~%" k truc-time machin-time))))

(defun mouline-truc-versus-machin-one-file
    (file reverse-count-fun1 reverse-count-fun2 start end &key (step 1) (times 1))
  (with-open-file (stream file :direction :output :if-does-not-exist :create :if-exists :supersede)
      (mouline-truc-versus-machin-one-stream
       stream
       reverse-count-fun1 reverse-count-fun2 
       start end :step step :times times)))
