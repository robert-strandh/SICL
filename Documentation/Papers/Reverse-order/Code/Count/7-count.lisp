(defun count-from-end-with-length-7 (x list length)
  (declare (optimize (speed 3) (safety 0) (debug 0) 
           (compilation-speed 0)))
  (declare (type fixnum length))
  (labels (;; AUX1 is the recursive traversal 
	   ;; by CDR.
	   (aux1 (x list length)
	     (declare (type fixnum length))
	     (if (zerop length)
		 0
		 (+ (aux1 x (cdr list) (1- length))
		    (if (eq x (car list))
			1
			0))))
	   ;; AUX2 recursive traversal 
	   ;; by (NTHCDR 10000 ...).
	   ;; used when the length of the list is 
	   ;; less than 100000000.
	   (aux2 (x list length)
	     (declare (type fixnum length))
	     (if (<= length 10000)
		 (aux1 x list length)
		 (+ (aux2 x
			  (nthcdr 10000 list)
			  (- length 10000))
		    (aux1 x list 10000))))
	   ;; AUX3 recursive traversal 
	   ;; by half the size of the list.  
	   ;; used for lists that have more than
	   ;; 100000000 elements.
	   (aux3 (x list length)
	     (declare (type fixnum length))
	     (if (< length 100000000)
		 (aux2 x list length)
		 (let* ((n (ash length -1))
			(middle (nthcdr n list)))
		   (+ (aux3 x middle (- length n))
		      (aux3 x list n))))))
    (aux3 x list length)))

(defun reverse-count-7 (x list)
  (count-from-end-with-length-7
   x list (length list)))
