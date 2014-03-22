(cl:in-package #:sicl-string)

(deftype string-designator ()
  '(or string symbol character))

(defun string (designator)
  (declare (type string-designator designator))
  (etypecase designator
    (string designator)
    (character (make-string 1 :initial-element designator))
    (symbol (symbol-name designator))))

(defun string-trim (character-bag string-designator)
  (flet ((in-bag-p (char) (find char character-bag)))
    (let* ((string (string string-designator))
	   (first (position-if-not #'in-bag-p string)))
      (if (null first)
	  string
	  (let ((last (position-if-not #'in-bag-p string :from-end t)))
	    (subseq string first (1+ last)))))))

(defun string-left-trim (character-bag string-designator)
  (flet ((in-bag-p (char) (find char character-bag)))
    (let* ((string (string string-designator))
	   (first (position-if-not #'in-bag-p string)))
      (if (null first)
	  string
	  (subseq string first)))))

(defun string-right-trim (character-bag string-designator)
  (flet ((in-bag-p (char) (find char character-bag)))
    (let* ((string (string string-designator))
	   (last (position-if-not #'in-bag-p string :from-end t)))
      (if (null last)
	  string
	  (subseq string 0 (1+ last))))))

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

(defun string> (string1 string2 &key (start1 0) end1 (start2 0) end2)
  (string< string2 string1
	   :start1 start2 :end1 end2
	   :start2 start1 :end2 end1))

(defun string-greaterp (string1 string2 &key (start1 0) end1 (start2 0) end2)
  (string-lessp string2 string1
		:start1 start2 :end1 end2
		:start2 start1 :end2 end1))

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

(defun string>= (string1 string2 &key (start1 0) end1 (start2 0) end2)
  (string<= string2 string1
	    :start1 start2 :end1 end2
	    :start2 start1 :end2 end1))

(defun string-not-lessp (string1 string2 &key (start1 0) end1 (start2 0) end2)
  (string-not-greaterp string2 string1
		       :start1 start2 :end1 end2
		       :start2 start1 :end2 end1))
