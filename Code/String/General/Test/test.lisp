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

(defun test-one-nstring-upcase (string &key (start 0) end)
  (let ((list1 (coerce string 'list))
	(result (nstring-upcase string :start start :end end)))
    (let ((list2 (coerce result 'list)))
      (assert (eq result string))
      (let ((real-end (if (null end) (length list2) end)))
	(assert (equal (nlist-upcase list1 start real-end) list2))))))

(defun test-nstring-upcase (n)
  (loop repeat n
	do (let* ((length (random 10))
		  (string (make-string length))
		  (start (random (1+ length)))
		  (end (+ start (random (1+ (- length start))))))
	     ;; Fill the string with some random characters.
	     (loop for i from 0 below length
		   do (setf (char string i)
			    (code-char (random 500))))
	     (test-one-nstring-upcase string :start start :end end))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Test NSTRING-DOWNCASE

(defun nlist-downcase (list start end)
  (loop for rest on list
	for i from 0
	when (and (<= start i) (< i end))
	  do (setf (car rest) (char-downcase (car rest))))
  list)
  
(defun test-one-nstring-downcase (string &key (start 0) end)
  (let ((list1 (coerce string 'list))
	(result (nstring-downcase string :start start :end end)))
    (let ((list2 (coerce result 'list)))
      (assert (eq result string))
      (let ((real-end (if (null end) (length list2) end)))
	(assert (equal (nlist-downcase list1 start real-end) list2))))))

(defun test-nstring-downcase (n)
  (loop repeat n
	do (let* ((length (random 10))
		  (string (make-string length))
		  (start (random (1+ length)))
		  (end (+ start (random (1+ (- length start))))))
	     ;; Fill the string with some random characters.
	     (loop for i from 0 below length
		   do (setf (char string i)
			    (code-char (random 500))))
	     (test-one-nstring-downcase string :start start :end end))))

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

