(cl:in-package #:sicl-string)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Utilities.

(defun first-mismatch-simple-simple-char=
    (string1 string2 start1 end1 start2 end2)
  (assert (simple-string-p string1))
  (assert (>= start1 0))
  (assert (<= end1 (length string1)))
  (assert (<= start1 end1))
  (assert (simple-string-p string1))
  (assert (>= start2 0))
  (assert (<= end2 (length string2)))
  (assert (<= start2 end2))
  (locally (declare (type simple-base-string string1 string2)
		    (type fixnum start1 end1 start2 end2)
		    (optimize (speed 3) (safety 0) (debug 0)))
    (loop for i of-type fixnum from start1 below end1
	  for j of-type fixnum from start2 below end2
	  unless (char= (schar string1 i) (schar string2 j))
	    return i
	  finally (return i))))

(defun first-mismatch-simple-general-char=
    (string1 string2 start1 end1 start2 end2)
  (assert (simple-string-p string1))
  (assert (>= start1 0))
  (assert (<= end1 (length string1)))
  (assert (<= start1 end1))
  (assert (simple-string-p string1))
  (assert (>= start2 0))
  (assert (<= end2 (length string2)))
  (assert (<= start2 end2))
  (locally (declare (type simple-base-string string1 string2)
		    (type fixnum start1 end1 start2 end2)
		    (optimize (speed 3) (safety 0) (debug 0)))
    (loop for i of-type fixnum from start1 below end1
	  for j of-type fixnum from start2 below end2
	  unless (char= (schar string1 i) (char string2 j))
	    return i
	  finally (return i))))

(defun first-mismatch-general-simple-char=
    (string1 string2 start1 end1 start2 end2)
  (assert (simple-string-p string1))
  (assert (>= start1 0))
  (assert (<= end1 (length string1)))
  (assert (<= start1 end1))
  (assert (simple-string-p string1))
  (assert (>= start2 0))
  (assert (<= end2 (length string2)))
  (assert (<= start2 end2))
  (locally (declare (type simple-base-string string1 string2)
		    (type fixnum start1 end1 start2 end2)
		    (optimize (speed 3) (safety 0) (debug 0)))
    (loop for i of-type fixnum from start1 below end1
	  for j of-type fixnum from start2 below end2
	  unless (char= (char string1 i) (schar string2 j))
	    return i
	  finally (return i))))

(defun first-mismatch-general-general-char=
    (string1 string2 start1 end1 start2 end2)
  (assert (simple-string-p string1))
  (assert (>= start1 0))
  (assert (<= end1 (length string1)))
  (assert (<= start1 end1))
  (assert (simple-string-p string1))
  (assert (>= start2 0))
  (assert (<= end2 (length string2)))
  (assert (<= start2 end2))
  (locally (declare (type simple-base-string string1 string2)
		    (type fixnum start1 end1 start2 end2)
		    (optimize (speed 3) (safety 0) (debug 0)))
    (loop for i of-type fixnum from start1 below end1
	  for j of-type fixnum from start2 below end2
	  unless (char= (char string1 i) (char string2 j))
	    return i
	  finally (return i))))

(defun first-mismatch-simple-simple-char-equal
    (string1 string2 start1 end1 start2 end2)
  (assert (simple-string-p string1))
  (assert (>= start1 0))
  (assert (<= end1 (length string1)))
  (assert (<= start1 end1))
  (assert (simple-string-p string1))
  (assert (>= start2 0))
  (assert (<= end2 (length string2)))
  (assert (<= start2 end2))
  (locally (declare (type simple-base-string string1 string2)
		    (type fixnum start1 end1 start2 end2)
		    (optimize (speed 3) (safety 0) (debug 0)))
    (loop for i of-type fixnum from start1 below end1
	  for j of-type fixnum from start2 below end2
	  unless (char-equal (schar string1 i) (schar string2 j))
	    return i
	  finally (return i))))

(defun first-mismatch-simple-general-char-equal
    (string1 string2 start1 end1 start2 end2)
  (assert (simple-string-p string1))
  (assert (>= start1 0))
  (assert (<= end1 (length string1)))
  (assert (<= start1 end1))
  (assert (simple-string-p string1))
  (assert (>= start2 0))
  (assert (<= end2 (length string2)))
  (assert (<= start2 end2))
  (locally (declare (type simple-base-string string1 string2)
		    (type fixnum start1 end1 start2 end2)
		    (optimize (speed 3) (safety 0) (debug 0)))
    (loop for i of-type fixnum from start1 below end1
	  for j of-type fixnum from start2 below end2
	  unless (char-equal (schar string1 i) (char string2 j))
	    return i
	  finally (return i))))

(defun first-mismatch-general-simple-char-equal
    (string1 string2 start1 end1 start2 end2)
  (assert (simple-string-p string1))
  (assert (>= start1 0))
  (assert (<= end1 (length string1)))
  (assert (<= start1 end1))
  (assert (simple-string-p string1))
  (assert (>= start2 0))
  (assert (<= end2 (length string2)))
  (assert (<= start2 end2))
  (locally (declare (type simple-base-string string1 string2)
		    (type fixnum start1 end1 start2 end2)
		    (optimize (speed 3) (safety 0) (debug 0)))
    (loop for i of-type fixnum from start1 below end1
	  for j of-type fixnum from start2 below end2
	  unless (char-equal (char string1 i) (schar string2 j))
	    return i
	  finally (return i))))

(defun first-mismatch-general-general-char-equal
    (string1 string2 start1 end1 start2 end2)
  (assert (simple-string-p string1))
  (assert (>= start1 0))
  (assert (<= end1 (length string1)))
  (assert (<= start1 end1))
  (assert (simple-string-p string1))
  (assert (>= start2 0))
  (assert (<= end2 (length string2)))
  (assert (<= start2 end2))
  (locally (declare (type simple-base-string string1 string2)
		    (type fixnum start1 end1 start2 end2)
		    (optimize (speed 3) (safety 0) (debug 0)))
    (loop for i of-type fixnum from start1 below end1
	  for j of-type fixnum from start2 below end2
	  unless (char-equal (char string1 i) (char string2 j))
	    return i
	  finally (return i))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function STRING=.

(defun string= (string1 string2 &key (start1 0) end1 (start2 0) end2)
  (let ((string1 (string string1))
	(string2 (string string2)))
    (when (null end1) (setf end1 (length string1)))
    (when (null end2) (setf end2 (length string2)))
    (check-bounding-indices string1 start1 end1)
    (check-bounding-indices string2 start2 end2)
    (and (= (- end1 start1) (- end2 start2))
	 (loop for i1 from start1 below end1
	       for i2 from start2 below end2
	       unless (char= (char string1 i1) (char string2 i2))
		 return nil
	       finally (return t)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function STRING-EQUAL.

(defun string-equal (string1 string2 &key (start1 0) end1 (start2 0) end2)
  (let ((string1 (string string1))
	(string2 (string string2)))
    (when (null end1) (setf end1 (length string1)))
    (when (null end2) (setf end2 (length string2)))
    (check-bounding-indices string1 start1 end1)
    (check-bounding-indices string2 start2 end2)
    (and (= (- end1 start1) (- end2 start2))
	 (loop for i1 from start1 below end1
	       for i2 from start2 below end2
	       unless (char-equal (char string1 i1) (char string2 i2))
		 return nil
	       finally (return t)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function STRING<.

(defun string< (string1 string2 &key (start1 0) end1 (start2 0) end2)
  (let ((string1 (string string1))
	(string2 (string string2)))
    (when (null end1) (setf end1 (length string1)))
    (when (null end2) (setf end2 (length string2)))
    (check-bounding-indices string1 start1 end1)
    (check-bounding-indices string2 start2 end2)
    (loop for i1 from start1 below end1
	  for c1 = (char string1 i1)
	  for i2 from start2 below end2
	  for c2 = (char string2 i2)
	  do (cond ((char< c1 c2) (return i1))
		   ((char< c2 c1) (return nil))
		   (t nil))
	  finally (return (if (< (- end1 start1) (- end2 start2))
			      end1
			      nil)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function STRING-LESSP.

(defun string-lessp (string1 string2 &key (start1 0) end1 (start2 0) end2)
  (let ((string1 (string string1))
	(string2 (string string2)))
    (when (null end1) (setf end1 (length string1)))
    (when (null end2) (setf end2 (length string2)))
    (check-bounding-indices string1 start1 end1)
    (check-bounding-indices string2 start2 end2)
    (loop for i1 from start1 below end1
	  for c1 = (char string1 i1)
	  for i2 from start2 below end2
	  for c2 = (char string2 i2)
	  do (cond ((char-lessp c1 c2) (return i1))
		   ((char-lessp c2 c1) (return nil))
		   (t nil))
	  finally (return (if (< (- end1 start1) (- end2 start2))
			      end1
			      nil)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function STRING>.

(defun string> (string1 string2 &key (start1 0) end1 (start2 0) end2)
  (string< string2 string1
	   :start1 start2 :end1 end2
	   :start2 start1 :end2 end1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function STRING-GREATERP.

(defun string-greaterp (string1 string2 &key (start1 0) end1 (start2 0) end2)
  (string-lessp string2 string1
		:start1 start2 :end1 end2
		:start2 start1 :end2 end1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function STRING<=.

(defun string<= (string1 string2 &key (start1 0) end1 (start2 0) end2)
  (let ((string1 (string string1))
	(string2 (string string2)))
    (when (null end1) (setf end1 (length string1)))
    (when (null end2) (setf end2 (length string2)))
    (check-bounding-indices string1 start1 end1)
    (check-bounding-indices string2 start2 end2)
    (loop for i1 from start1 below end1
	  for c1 = (char string1 i1)
	  for i2 from start2 below end2
	  for c2 = (char string2 i2)
	  do (cond ((char< c1 c2) (return i1))
		   ((char< c2 c1) (return nil))
		   (t nil))
	  finally (return (if (<= (- end1 start1) (- end2 start2))
			      end1
			      nil)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function STRING-NOT-GREATERP.

(defun string-not-greaterp (string1 string2 &key (start1 0) end1 (start2 0) end2)
  (let ((string1 (string string1))
	(string2 (string string2)))
    (when (null end1) (setf end1 (length string1)))
    (when (null end2) (setf end2 (length string2)))
    (check-bounding-indices string1 start1 end1)
    (check-bounding-indices string2 start2 end2)
    (loop for i1 from start1 below end1
	  for c1 = (char string1 i1)
	  for i2 from start2 below end2
	  for c2 = (char string2 i2)
	  do (cond ((char-lessp c1 c2) (return i1))
		   ((char-lessp c2 c1) (return nil))
		   (t nil))
	  finally (return (if (<= (- end1 start1) (- end2 start2))
			      end1
			      nil)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function STRING>=.

(defun string>= (string1 string2 &key (start1 0) end1 (start2 0) end2)
  (string<= string2 string1
	    :start1 start2 :end1 end2
	    :start2 start1 :end2 end1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function STRING-NOT-LESSP.

(defun string-not-lessp (string1 string2 &key (start1 0) end1 (start2 0) end2)
  (string-not-greaterp string2 string1
		       :start1 start2 :end1 end2
		       :start2 start1 :end2 end1))
