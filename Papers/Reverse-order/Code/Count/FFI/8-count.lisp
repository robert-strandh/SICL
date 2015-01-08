(defun load-so ()
  (sb-alien:load-shared-object "/home/strandh/Lisp/My-Projects/GIT-ified/SICL/Papers/Reverse-order/Code/Count/FFI/libcount.so"))

(defun recursive-count (item list length)
  (sb-alien:alien-funcall
   (sb-alien:extern-alien 
    "tt2" 
    (function (unsigned 64) (unsigned 64) (unsigned 64) (unsigned 64)))
   (sb-kernel:get-lisp-obj-address item)
   (sb-kernel:get-lisp-obj-address list)
   length))

(defun count-from-end-with-length-8 (x list length)
  (declare (type fixnum length))
  (declare (optimize (speed 3) (safety 0) (debug 0) (compilation-speed 0)))
  (labels (;; AUX2 is the recursive traversal by (NTHCDR 10000 ...).
	   ;; It is used when the length of the list is less than
	   ;; 100000000.
	   (aux2 (x list length)
	     (declare (type fixnum length))
	     (if (<= length 100000)
		 (recursive-count x list length)
		 (+ (aux2 x (nthcdr 100000 list) (- length 100000))
		    (recursive-count x list 100000))))
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

(defun reverse-count-8 (x list)
  (count-from-end-with-length-8 x list (length list)))
