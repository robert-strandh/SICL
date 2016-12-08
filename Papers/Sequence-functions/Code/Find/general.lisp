(in-package :find)

(defvar *trace*)
(setq *trace* nil)

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
(defvar *the-other-symbol* 'b)

(defun symbols (n)
  (let ((vector
	  (make-array n :initial-element *the-symbol* :adjustable t)))
    (setf (aref vector (1- n)) *the-other-symbol*)
    vector))

(defun more-symbols (n vector)
  (let ((len (length vector)))
    (setf (aref vector (1- len)) *the-symbol*)
    (let ((newlen (+ len n)))
      (adjust-array vector newlen :initial-element *the-symbol*)
      (setf (aref vector (1- newlen)) *the-other-symbol*))
    vector))

(defun test-call (find-fun vector)
  (funcall find-fun *the-other-symbol* vector))

(defun evaluate-test-call (find-fun vector times)
  (evaluate-time (test-call find-fun vector) times))
