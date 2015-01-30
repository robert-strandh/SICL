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

(defun date-string ()
  (time-string nil))

(defvar *the-symbol* 'a)

(defun symbols (n)
  (make-list n :initial-element *the-symbol*))

(defun more-symbols (n l)
  (nconc (symbols n) l))

(defun test-call (reverse-count-fun list)
  (funcall reverse-count-fun *the-symbol* list))

(defun evaluate-test-call (reverse-count-fun list times)
  (evaluate-time (test-call reverse-count-fun list) times))

(defun compare-versions-stream (stream versions start end &key (step 1) (times 1))
  (loop for version in versions
		do (format stream " ~10T~A~10T" (symbol-name version))
		do (format t " ~10T~A~10T" (symbol-name version))
		finally (progn (terpri stream) (terpri t)))
  (setq versions (mapcar #'symbol-function versions))
  (loop
    for k from start to end by step
    for list = (symbols start) then (more-symbols step list)
	do (format stream "~D~10T" k)
	do (format t "~D~10T" k)
    do (loop for version in versions
			 do (let ((version-time (evaluate-test-call version list times)))
				  (format stream "~10,5F ~10T" version-time)
				  (format t "~10,5F ~10T" version-time))
			 finally (progn (format stream "~%") (format t "~%") (finish-output t) (finish-output stream)))))

(defun compare-versions-file (file version-numbers start end &key (step 1) (times 1))
  (with-open-file (stream file :direction :output :if-does-not-exist :create :if-exists :supersede)
	(compare-versions-stream
       stream
	   (mapcar (lambda (n)
				 (find-symbol (format nil "REVERSE-COUNT-~A" n)))
			   version-numbers)
       start end :step step :times times)))

(defvar *local*)
(setq *local* t)
(defun make-test-filename (&optional (prefix ""))
  (if *local* 
	  prefix
	  (format nil "~A:~A-~A-~A.res"
			  prefix
			  (cl:lisp-implementation-type)
			  (cl:lisp-implementation-version) 
			  (machine-version))))
		  
(defun compare-versions (version-numbers start end &key (step 1) (times 1))
  (let ((name (format nil "v~A" (first version-numbers))))
	(loop
	  for n in (rest version-numbers)
	  do (setq name (format nil "~A-v~A" name n)))
	(compare-versions-file (make-test-filename name) version-numbers start end :step step :times times)))

(defun test-versions (version-numbers)
  (compare-versions version-numbers 10000 10000000 :step 100000 :times 2))


;; CL-USER> (mouline-truc-versus-machin-one-file "v1-vs-v4" #'reverse-count-1 #'reverse-count-4 1000 1000000 :step 1000 :times 30)
;; 1000    0.00000    0.03333
;; 2000    0.03333    0.06667
;; 3000    0.06667    0.10000
;; 4000    0.06667    0.13333
