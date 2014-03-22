(cl:in-package #:sicl-string)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Test NSTRING-UPCASE

(defun nlist-upcase (list start end)
  (loop for rest on list
	for i from 0
	when (and (<= start i) (< i end))
	  do (setf (car rest) (char-upcase (car rest))))
  list)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Test NSTRING-DOWNCASE

(defun nlist-downcase (list start end)
  (loop for rest on list
	for i from 0
	when (and (<= start i) (< i end))
	  do (setf (car rest) (char-downcase (car rest))))
  list)
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Test NSTRING-CAPITALIZE

(defun nlist-capitalize (list start end)
  (loop with prev = #\Space
	for rest on list
	for i from 0
	when (and (<= start i) (< i end))
	  do (if (alphanumericp prev)
		 (setf (car rest) (char-downcase (car rest)))
		 (setf (car rest) (char-upcase (car rest))))
	     (setf prev (car rest)))
  list)

