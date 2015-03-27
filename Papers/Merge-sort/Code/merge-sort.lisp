(defpackage #:merge-sort
  (:use #:common-lisp)
  (:shadow #:merge #:sort))

(in-package #:merge-sort)

(defparameter *buffer* (make-array 100000))

(defun merge (vector start middle end less-fun buffer buffer-length)
  (let ((buffer-start 0)
	(buffer-end 0)
	(first-start start)
	(second-start middle))
    (flet ((empty-buffer ()
	     ;; Compact the vector
	     (replace vector vector
		      :start2 first-start
		      :end2 middle
		      :end1 second-start)
	     ;; Move the buffer to the vector
	     (if (> buffer-end buffer-start)
		 (replace vector buffer
			  :start1 first-start
			  :start2 buffer-start
			  :end2 buffer-end)
		 (progn
		   (replace vector buffer
			    :start1 first-start
			    :start2 buffer-start
			    :end2 buffer-length)
		   (replace vector buffer
			    :start1 (+ first-start (- buffer-length buffer-start))
			    :start2 0
			    :end2 buffer-end)))
	     (psetq buffer-start 0
		    buffer-end 0
		    middle second-start
		    first-start (+ first-start (- second-start middle)))))
      (loop until (or (= first-start middle)
		      (= second-start end))
	    do (if (funcall less-fun
			    (aref vector second-start)
			    (aref vector first-start))
		   (progn
		     (setf (aref buffer buffer-end)
			   (aref vector second-start))
		     (incf second-start)
		     (incf buffer-end)
		     (when (= buffer-end buffer-length)
		       (setf buffer-end 0))
		     (when (= buffer-start buffer-end)
		       (empty-buffer)))
		   (let ((temp (aref buffer buffer-start)))
		     (incf buffer-start)
		     (when (= buffer-start buffer-length)
		       (setf buffer-start 0))
		     (setf (aref buffer buffer-end)
			   (aref vector first-start))
		     (incf buffer-end)
		     (when (= buffer-end buffer-length)
		       (setf buffer-end 0))
		     (setf (aref vector first-start) temp)
		     (incf first-start))))
      (unless (= buffer-start buffer-end)
	(empty-buffer)))))

(defun sort-part (vector start end less-fun buffer buffer-length)
  (assert (plusp (- end start)))
  (cond ((= (- end start) 1)
	 nil)
	((= (- end start) 2)
	 (when (funcall less-fun
			(aref vector (1+ start))
			(aref vector start))
	   (let ((temp (aref vector (1+ start))))
	     (setf (aref vector (1+ start)) (aref vector start))
	     (setf (aref vector start) temp))))
	(t
	 (let ((middle (floor (+ start end) 2)))
	   (sort-part vector start middle less-fun buffer buffer-length)
	   (sort-part vector middle end less-fun buffer buffer-length)
	   (merge vector start middle end less-fun buffer buffer-length)))))

(defun sort (vector less-fun)
  (sort-part vector 0 (length vector) less-fun *buffer* (length *buffer*)))
