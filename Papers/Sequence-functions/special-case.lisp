(defparameter *v*
  (let ((result (make-array 100000000
			    :element-type '(unsigned-byte 8)
			    :initial-element 0)))
    (setf (aref result (1- (length result))) 1)
    result))

;;; This is the general version.  It takes 650ms on my machine.
(defun find-vector-1 (item vector)
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (loop for index from 0 below (length vector)
	for element = (aref vector index)
	when (eql item element)
	  return element))

;;; In this version, we specialize each access so that the case for an
;;; element type of (UNSIGNED-BYTE 8) is handled specially.  This
;;; version takes 400ms on my computer.
(defun find-vector-2 (item vector)
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (loop for index from 0 below (length vector)
	for element = (if (typep vector '(simple-array (unsigned-byte 8)))
			  (locally (declare (type (simple-array (unsigned-byte 8))
						  vector))
			    (aref vector index))
			  (aref vector index))
	when (eql item element)
	  return element))

;;; In this version, we also specialize for the case of an element
;;; type of (UNSIGNED-BYTE 8), but this time the entire loop is
;;; specialized.  This version takes less than 100ms on my computer.
;;; But it is also less maintainable because of the duplication of the
;;; loop.  Testing all special cases can become very difficult.
;;; Furthermore, although SBCL propagates the type of the vector so
;;; that the AREF can be specialized, not all implementations would do
;;; that.
(defun find-vector-3 (item vector)
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (if (typep vector '(simple-array (unsigned-byte 8)))
      (loop for index from 0 below (length vector)
	    for element = (aref vector index)
	    when (eql item element)
	      return element)
      (loop for index from 0 below (length vector)
	    for element = (aref vector index)
	    when (eql item element)
	      return element)))

;;; This macro duplicates the body in two different contexts according
;;; to the type of the vector.
(defmacro with-vector-type (vector-var &body body)
  `(if (typep ,vector-var '(simple-array (unsigned-byte 8)))
       (locally (declare (type (simple-array (unsigned-byte 8)) ,vector-var))
	 ,@body)
       (progn
	 ,@body)))

;;; This version is only slightly more complicated than the general
;;; version, and therefore more maintainable and easier to test.
(defun find-vector-4 (item vector)
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (with-vector-type vector
    (loop for index from 0 below (length vector)
	  for element = (aref vector index)
	  when (eql item element)
	    return element)))

(defun general-find (item list extra)
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (loop for element in list
	do (cond ((and (eq extra 'a) (eq item element))
		  (return-from general-find 1))
		 ((and (eq extra 'b) (eq item element))
		  (return-from general-find 2))
		 ((and (eq extra 'c) (eq item element))
		  (return-from general-find 3))
		 ((and (eq extra 'd) (eq item element))
		  (return-from general-find 4))
		 ((and (eq extra 'e) (eq item element))
		  (return-from general-find 5))
		 ((and (eq extra 'f) (eq item element))
		  (return-from general-find 6))
		 ((and (eq extra 'g) (eq item element))
		  (return-from general-find 7))
		 ((and (eq extra 'h) (eq item element))
		  (return-from general-find 8))
		 ((and (eq extra 'i) (eq item element))
		  (return-from general-find 9))
		 ((and (eq extra 'j) (eq item element))
		  (return-from general-find 10))
		 ((and (eq extra 'k) (eq item element))
		  (return-from general-find 11))
		 ((and (eq extra 'l) (eq item element))
		  (return-from general-find 12))
		 ((and (eq extra 'm) (eq item element))
		  (return-from general-find 13))
		 ((and (eq extra 'n) (eq item element))
		  (return-from general-find 14))
		 ((eq item element)
		  (return-from general-find element))
		 (t nil))))

(defun find-1 (item list)
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (loop for element in list
	do (when (eq item element)
	     (return-from find-1 element))))
  
(defun find-2 (item list end)
  (declare (type (or null fixnum) end))
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (loop for element in list
	for index of-type fixnum from 0
	do (if (and end (>= index end))
	       (return-from find-2 nil)
	       (when (eq item element)
		 (return-from find-2 element)))))
  
(defun find-3 (item vector)
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (loop for index from 0 below (length vector)
	for element = (aref vector index)
	when (eql item element)
	  return element))
  
(defun find-4 (item vector)
  (declare (type (simple-array character) vector))
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (loop for index from 0 below (length vector)
	for element = (aref vector index)
	when (eql item element)
	  return element))
  
(defun find-5 (item vector)
  (declare (type (simple-array character) vector))
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (loop for index from 0 below (length vector)
	for element = (if (simple-string-p vector)
			  (aref vector index)
			  (aref vector index))
	when (eql item element)
	  return element))

(defun find-6 (item vector)
  (declare (type (simple-array (unsigned-byte 8)) vector))
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (loop for index from 0 below (length vector)
	for element = (aref vector index)
	when (eql item element)
	  return element))
  
(defun find-7 (item vector)
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (loop for index from 0 below (length vector)
	for element = (if (typep vector '(simple-array (unsigned-byte 8)))
			  (locally (declare (type (simple-array (unsigned-byte 8))
						  vector))
			    (aref vector index))
			  (aref vector index))
	when (eql item element)
	  return element))
