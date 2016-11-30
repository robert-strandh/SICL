(defun find-from-end-5 (x list n nn)
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (declare (type list list))
  (labels ((recursive (x list n)
	     (declare (type fixnum n))
	     (if (zerop n)
		 nil
		 (progn (recursive x (cdr list) (1- n))
			(when (eq x (car list))
			  (return-from find-from-end-5 x))))))
    (labels ((aux (x list n)
	       (declare (type fixnum n))
	       (cond ((<= n nn)
		      (recursive x list n))
		     ((<= n (* nn nn))
		      (aux x (nthcdr nn list) (- n nn))
		      (aux x list nn))
		     (t
		      (let* ((n/2 (ash n -1))
			     (half (nthcdr n/2 list)))
			(aux x half (- n n/2))
			(aux x list n/2))))))
      (aux x list n))))

(defparameter *c*
  (let ((c (list 'a 'b)))
    (setf (cddr c) (cdr c))
    c))

(defun test ()
  ;; The cost is 6ns per element. 
  (find-from-end-5 'a *c* 1000 6000))
  ;; The cost is 10ns per element. 
  (find-from-end-5 'a *c* 1000000 6000))
  ;; The cost is 15ns per element. 
  (find-from-end-5 'a *c* 10000000000 6000))

