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

(defun mouline-next-stream (stream reverse-count-fun n &key (step 1) (start 1) (times 1))
  (loop
    repeat n
    for k from start by step
    for list = (make-zeros start) then (more-zeros step list)
    do (format stream "~3D ~A~%" k (evaluate-time (funcall reverse-count-fun 0 list) times))))

(defun mouline-next-file (reverse-count-fun n &key (step 1) (start 1) (times 1))
  (with-open-file (stream (date-string file)
			  :direction :output :if-does-not-exist :create :if-exists :supersede)
    (mouline-next-stream stream reverse-count-fun n :step step :start start :times times)))

(defun mouline-stream
    (automaton n stream graph-fun &key (start 1) (step 1) (fois 1) (enum t))
  (loop
    for i from start
    for k from start by step
    repeat n
    do (let ((time (evaluate-time (funcall graph-fun k) automaton :fois fois :enum enum)))
	 (format stream "~3D ~10,5F~%" k time)
	 (unless (eq stream t)
	   (format *trace-output* "~3D ~A~%" k time)))))

(defun mouline-file (automaton n file graph-fun
		     &key (start 1) (step 1) (fois 1) (enum t))
  (with-open-file (stream file :direction :output :if-does-not-exist :create :if-exists :supersede)
    (mouline-stream automaton n stream graph-fun
		    :start start :step step :fois fois :enum enum)))

(defun mouline-truc-versus-machin-stream
    (truc-automaton machin-automaton
     i j truc-stream machin-stream graph-fun &key (step 1) (fois 1) (enum t))
  (loop
    for k from i to j by step
    do (let* ((term (funcall graph-fun k))
	      (truc-time (evaluate-time (funcall  :fois fois :enum enum))
	      (machin-time (eval-time-aut-term term machin-automaton :fois fois :enum enum)))
	 (format truc-stream "~3D ~10,5F~%" k truc-time)
	 (format machin-stream "~3D ~10,5F~%" k machin-time))))

(defun mouline-truc-versus-machin-file
    (truc-automaton machin-automaton i j truc-file machin-file graph-fun
     &key (step 1) (fois 1) (enum t))
  (with-open-file (truc-stream truc-file :direction :output :if-does-not-exist :create :if-exists :supersede)
    (with-open-file (machin-stream machin-file :direction :output :if-does-not-exist :create :if-exists :supersede)
      (mouline-truc-versus-machin-stream
       truc-automaton machin-automaton
       i j truc-stream machin-stream graph-fun :step step :fois fois :enum enum))))

(defun mouline-truc-versus-machin-one-stream
    (truc-automaton machin-automaton
     i j stream graph-fun &key (step 1) (fois 1) (enum t))
  (loop
    for k from i to j by step
    do (let* ((term (funcall graph-fun k))
	      (truc-time (eval-time-aut-term
			  term truc-automaton :fois fois :enum enum))
	      (machin-time (eval-time-aut-term
			    term machin-automaton :fois fois :enum enum)))
	 (format t "~3D ~10,5F ~10,5F~%" k truc-time machin-time)
	 (format stream "~3D ~10,5F ~10,5F~%" k truc-time machin-time))))

(defun mouline-truc-versus-machin-one-file
    (truc-automaton machin-automaton i j file graph-fun
     &key (step 1) (fois 1) (enum t))
  (with-open-file (stream file :direction :output :if-does-not-exist :create :if-exists :supersede)
      (mouline-truc-versus-machin-one-stream
       truc-automaton machin-automaton
       i j stream graph-fun :step step :fois fois :enum enum)))

(defun truc-versus-machin-stream
    (truc-stream machin-stream
     truc-automaton machin-automaton
     term-fun next-term-fun
     n ;; n computations
     &key (start 1) (step 1) (fois 1))
  (loop
     with enum = (not (deterministic-p truc-automaton))
     with next-term-fun = (apply-k-next step next-term-fun)
     repeat n
     for i from start by step
     for term = (funcall term-fun start) then (funcall next-term-fun term)
     do (let ((truc-time (eval-time-aut-term term truc-automaton :fois fois :enum enum))
	      (machin-time (eval-time-aut-term term machin-automaton :fois fois :enum enum)))
	 (format t "~3D ~10,5F ~10,5F~%" i truc-time machin-time)
	 (unless (eq truc-stream t)
	   (if (eq truc-stream machin-stream)
	       (format truc-stream "~3D ~10,5F ~10,5F~%" i truc-time machin-time)
	       (progn
		 (format truc-stream "~3D ~10,5F~%" i truc-time)
		 (format machin-stream "~3D ~10,5F~%" i machin-time)))))))

(defun truc-versus-machin-file
    (truc-file machin-file
     truc-automaton machin-automaton
     term-fun next-term-fun
     n
     &key  (start 1) (step 1) (fois 1))
  (with-open-file (truc-stream truc-file :direction :output :if-does-not-exist :create :if-exists :supersede)
    (with-open-file (machin-stream machin-file :direction :output :if-does-not-exist :create :if-exists :supersede)
      (truc-versus-machin-stream
       truc-stream machin-stream
       truc-automaton machin-automaton
       term-fun next-term-fun
       n
       :start start :step step :fois fois))))

