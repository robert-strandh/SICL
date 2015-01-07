<<<<<<< HEAD
;;; divide by a constant *k* = 2^*log* (but 2^4 = 16 works best)

(defvar *log*)
(defvar *k*)
(setq *log* 4 *k* (ash 2 *log*))

(defun count-from-end-with-length-5 (x list length)
  (declare (type fixnum length) (optimize (speed 3) (compilation-speed 0)))
  (let ((count 0))
    (declare (type fixnum count))
    (flet ((process (elem)
	     (when (eql elem x)
	       (incf count))))
      (labels ((recursive-traverse (rest length)
		 (declare (type fixnum length))
		 (when (> length 0)
		     (recursive-traverse (cdr rest) (1- length))
		     (process (car rest))))
	       (traverse (rest length)
		 (declare (type fixnum length))
		 (if (< length 16384)
		     (recursive-traverse rest length)
		     ;; divide
		     (let* ((f (ash length (- *log*)))
			    (r0 rest) (r1 (nthcdr f r0)) (r2 (nthcdr f r1)) (r3 (nthcdr f r2)) 
			    (r4 (nthcdr f r3)) (r5 (nthcdr f r4)) (r6 (nthcdr f r5)) (r7 (nthcdr f r6))
			    (r8 (nthcdr f r7)) (r9 (nthcdr f r8)) (r10 (nthcdr f r9)) (r11 (nthcdr f r10))
			    (r12 (nthcdr f r11)) (r13 (nthcdr f r12)) (r14 (nthcdr f r13)) (r15 (nthcdr f r14)))
		       (traverse (nthcdr f r15) (- length (ash f *k*)))
		       (traverse r15 f) (traverse r14 f) (traverse r13 f) (traverse r12 f) (traverse r11 f)
		       (traverse r10 f) (traverse r9 f) (traverse r8 f) (traverse r7 f) (traverse r6 f)
		       (traverse r5 f) (traverse r4 f) (traverse r3 f) (traverse r2 f) (traverse r1 f)
		       (traverse rest f)))))
	       (traverse list length)
	       count))))

(defun reverse-count-5 (x list)
  (count-from-end-with-length-5 x list (length list)))

=======
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

(defun count-from-end-with-length-5 (x list length)
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

(defun reverse-count-5 (x list)
  (count-from-end-with-length-5 x list (length list)))
>>>>>>> 63ef22e508400cfeeabf7f1a7fe7e86028e721a7
