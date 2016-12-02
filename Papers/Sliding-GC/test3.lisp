(defparameter *size* (ash 1 21))

(defparameter *heap*
  (make-array *size*))

(loop for i from 0 below *size*
      do (setf (aref *heap* i) i))

(defparameter *bitmap*
  (make-array *size* :element-type 'bit :initial-element 0))

(loop for i from 2 below *size* by 4
      do (setf (sbit *bitmap* i) 0)
	 (setf (sbit *bitmap* (1+ i)) 0))

(loop for i from 0 by (ash 1 10) by (ash 1 9) below *size*
      do (loop for j from 0 below (ash 1 8)
	       do (setf (sbit *bitmap* (+ i j)) 1)))

(setf (sbit *bitmap* (ash 1 20)) 1)

(progn (fill *bitmap* 0)
       (loop for i from 2 below *size* by (ash 1 (- 19 15))
	     do (setf (sbit *bitmap* i) 1)))

(defparameter *start* (count 1 *bitmap*))

(defun build-table (heap bitmap start)
  (let ((acc 0)
	(end start))
    (declare (type fixnum acc start end)
	     (type (simple-vector #.*size*) heap)
	     (type (simple-array bit (#.*size*)) bitmap)
	     (optimize (speed 3) (safety 0) (debug 0)))
    (loop for address of-type fixnum from 0 
	  for prev of-type bit = 1 then bit
	  for bit of-type bit across bitmap
	  do (when (= bit 0)
	       (when (= prev 1)
		 (setf (svref heap end) address)
		 (setf (svref heap (1+ end)) acc)
		 (incf end 2))
	       (incf acc))
	  finally (when (= prev 1)
		    (setf (svref heap end) address)
		    (setf (svref heap (1+ end)) acc)
		    (incf end 2))
		  (return end))))
