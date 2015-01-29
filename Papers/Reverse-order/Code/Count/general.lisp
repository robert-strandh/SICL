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

(defvar *the-symbol* 'a)

(defun symbols (n)
  (make-list n :initial-element *the-symbol*))

(defun more-symbols (n l)
  (nconc (symbols n) l))

(defun test-call (reverse-count-fun list)
  (funcall reverse-count-fun *the-symbol* list))

(defun evaluate-test-call (reverse-count-fun list times)
  (evaluate-time (test-call reverse-count-fun list)))

(defun mouline-stream (stream reverse-count-fun n &key (step 1) (start 1) (times 1))
  (loop
    repeat n
    for k from start by step
    for list = (symbols start) then (more-symbols step list)
    do (let ((time (evaluate-test-call reverse-count-fun list times)))
		 (format t "~3D ~A~%" k time)
		 (format stream "~3D ~A~%" k time))))

(defun mouline-file (file reverse-count-fun n &key (step 1) (start 1) (times 1))
  (with-open-file (stream (date-string file)
			  :direction :output :if-does-not-exist :create :if-exists :supersede)
    (mouline-stream stream reverse-count-fun n :step step :start start :times times)))

(defun mouline-truc-versus-machin-one-stream
    (stream reverse-count-fun1 reverse-count-fun2 start end &key (step 1) (times 1))
  (loop
    for k from start to end by step
    for list = (symbols start) then (more-symbols step list)
    do (let ((truc-time (evaluate-test-call reverse-count-fun1 list times))
			 (machin-time (evaluate-test-call reverse-count-fun2 list times)))
	 (format t "~3D ~5,10F ~5,10F~%" k truc-time machin-time)
	 (finish-output t)
	 (format stream "~3D ~5,10F ~5,10F~%" k truc-time machin-time))))

(defun mouline-truc-versus-machin-one-file
    (file reverse-count-fun1 reverse-count-fun2 start end &key (step 1) (times 1))
  (with-open-file (stream file :direction :output :if-does-not-exist :create :if-exists :supersede)
      (mouline-truc-versus-machin-one-stream
       stream
       reverse-count-fun1 reverse-count-fun2 
       start end :step step :times times)))

;; CL-USER> (mouline-truc-versus-machin-one-file "v1-vs-v4" #'reverse-count-1 #'reverse-count-4 1000 1000000 :step 1000 :times 30)
;; 1000    0.00000    0.03333
;; 2000    0.03333    0.06667
;; 3000    0.06667    0.10000
;; 4000    0.06667    0.13333
