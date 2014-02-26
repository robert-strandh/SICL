(cl:in-package #:sicl-clos)

;;;; This file contains some utilities that are useful in
;;;; discriminating functions, and perhaps elsewhere as well.
;;;; 
;;;; The utilities defined here are special versions of functions that
;;;; exist in standard Common Lisp.  The reason for not using the more
;;;; general existing version is that those versions might be defined
;;;; as GENERIC functions in some implementations, in particular in
;;;; SICL, and we these functions in the general machinery for making
;;;; generic functions work.  So to avoid circular dependencies and
;;;; metastability issues, we define these special versions that only
;;;; work on list. 

;;; Non-destructive list reversal. 
(defun reverse-list (list)
  (loop with result = '()
	for element in list
	do (push element result)
	finally (return result)))

;;; Destructive list merging.
(defun merge-list (list1 list2 predicate &key key)
  (when (null key)
    (setf key #'identity))
  (cond ((null list1)
	 list2)
	((null list2)
	 list1)
	(t
	 (let ((result '()))
	   ;; Figure out a first element to put on the resulting list.
	   (if (funcall predicate
			(funcall key (car list2))
			(funcall key (car list1)))
	       (let ((temp (cdr list2)))
		 (setf result list2)
		 (setf list2 temp))
	       (let ((temp (cdr list1)))
		 (setf result list1)
		 (setf list1 temp)))
	   ;; Do the remaining elements
	   (let ((rest result))
	     (loop until (or (null list1) (null list2))
		   do (if (funcall predicate
				   (funcall key (car list2))
				   (funcall key (car list1)))
			  (let ((temp (cdr list2)))
			    (setf (cdr rest) list2)
			    (setf list2 temp))
			  (let ((temp (cdr list1)))
			    (setf (cdr rest) list1)
			    (setf list1 temp)))
		      (setf rest (cdr rest)))
	     (setf (cdr rest)
		   (if (null list1) list2 list1)))
	   result))))
		      
;;; Destructive list sorting.
(defun sort-list (list predicate &key key)
  (if (or (null list) (null (cdr list)))
      list
      (let* ((temp (nthcdr (1- (floor (length list) 2)) list))
	     (suffix (cdr temp)))
	(setf (cdr temp) nil)
	(merge-list (sort-list list predicate :key key)
		    (sort-list suffix predicate :key key)
		    predicate :key key))))

;;; List version of COUNT
(defun count-list (item list &key (key #'identity) (test #'eql))
  (loop for element in list
	count (funcall test item (funcall key element))))

;;; List version of LENGTH
(defun length-list (list)
  (loop for element in list
	count t))

