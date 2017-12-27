(cl:in-package #:sicl-reader)

(defun convert-according-to-readtable-case (token token-escapes)
  (ecase (sicl-readtable:readtable-case *readtable*)
    (:upcase
     (loop for escape across token-escapes
	   for i from 0
	   do (when (null escape)
		(setf (aref token i)
		      (char-upcase (aref token i))))))
    (:downcase
     (loop for escape across token-escapes
	   for i from 0
	   do (when (null escape)
		(setf (aref token i)
		      (char-downcase (aref token i))))))
    (:preserve
     nil)
    (:invert
     (let ((count-upper-case 0)
	   (count-lower-case 0))
       (loop for escape across token-escapes
	     for char across token
	     do (when (null escape)
		  (cond ((upper-case-p char)
			 (incf count-upper-case))
			((lower-case-p char)
			 (incf count-lower-case))
			(t
			 nil))))
       (cond ((zerop count-upper-case)
	      (loop for escape across token-escapes
		    for i from 0
		    do (when (null escape)
			 (setf (aref token i)
			       (char-upcase (aref token i))))))
	     ((zerop count-lower-case)
	      (loop for escape across token-escapes
		    for i from 0
		    do (when (null escape)
			 (setf (aref token i)
			       (char-downcase (aref token i))))))
	     (t
	      nil))))))
