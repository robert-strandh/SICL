;;; This version divides the list in 2 parts if it has more than one
;;; hundred million elements.  Otherwise, if it has more than 10000
;;; elements, it divides it into chunks that have 10000 elements each.
;;; Finally, if it has no more than 10000 elements, then it uses the
;;; standard recursive method.
;;;
;;; I think this method is faster than the others, at least for
;;; lengths no more than one hundred million elements, because then it
;;; is guaranteed to traverse the list at most 3 times + 1 time for
;;; computing the length.  It could be improved for lengths greater
;;; than one hundred million by using a better division than 2 then.

(defun count-from-end-with-length-7 (x list length)
  (declare (type fixnum length))
  (declare (optimize (speed 3) (safety 0) (debug 0) (compilation-speed 0)))
  (labels (;; AUX1 is the recursive traversal by CDR.
	   (aux1 (x list length)
	     (declare (type fixnum length))
	     (if (zerop length)
		 0
		 (+ (aux1 x (cdr list) (1- length))
		    (if (eql x (car list))
			1
			0))))
	   ;; AUX2 is the recursive traversal by (NTHCDR 10000 ...).
	   ;; It is used when the length of the list is less than
	   ;; 100000000.
	   (aux2 (x list length)
	     (declare (type fixnum length))
	     (if (<= length 10000)
		 (aux1 x list length)
		 (+ (aux2 x (nthcdr 10000 list) (- length 10000))
		    (aux1 x list 10000))))
	   ;; AUX3 is the recursive traversal by half the size of the
	   ;; list.  It is used for lists that have more than
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
  (count-from-end-with-length-7 x list (length list)))
