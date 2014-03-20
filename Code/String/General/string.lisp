(cl:in-package #:sicl-string)

(deftype string-designator ()
  (or string symbol character))

(defun string (designator)
  (declare (type string-designator designator))
  (etypecase designator
    (string designator)
    (character (make-string 1 :initial-element designator))
    (symbol (symbol-name designator))))

(defun copy-string (string)
  (let ((result (make-string (length string))))
    (loop for i from 0 below (length string)
	  do (setf (char result i) (char string i)))
    result))

(defun check-bounding-indices (string start end)
  (let ((length (length string)))
    (unless (typep start `(integer 0 ,length))
      (error 'type-error
	     :datum start
	     :expected-type `(integer 0 ,length)))
    (unless (typep end `(integer 0 ,length))
      (error 'type-error
	     :datum end
	     :expected-type `(integer 0 ,length)))
    (unless (<= start end)
      ;; FIXME: signal a more appropriate condition.
      (error "START must be less than or equal to END."))))

(defun nstring-upcase (string &key (start 0) end)
  (declare (type string string))
  (let ((length (length string)))
    (when (null end) (setf end length))
    (check-bounding-indices string start end)
    (loop for i from start below end
	  do (setf (char string i) (char-upcase (char string i)))))
  string)

(defun string-upcase (string-designator &key (start 0) end)
  (let* ((string (string string-designator))
	 (copy (copy-string string)))
    (nstring-upcase copy start end)))

(defun nstring-downcase (string &key (start 0) end)
  (declare (type string string))
  (let ((length (length string)))
    (when (null end) (setf end length))
    (check-bounding-indices string start end)
    (loop for i from start below end
	  do (setf (char string i) (char-downcase (char string i)))))
  string)

(defun string-downcase (string-designator &key (start 0) end)
  (let* ((string (string string-designator))
	 (copy (copy-string string)))
    (nstring-downcase copy start end)))

(defun nstring-capitalize (string &key (start 0) end)
  (declare (type string string))
  (let ((length (length string)))
    (when (null end) (setf end length))
    (check-bounding-indices string start end)
    (loop with state = nil
	  for i from start below end
	  for char = (char string i)
	  do (if state
		 (if (alphanumericp char)
		     (setf (char string i) (char-downcase char))
		     (setf state nil))
		 (when (alphanumericp char)
		   (setf (char string i) (char-upcase char))
		   (setf state t)))))
  string)

(defun string-capitalize (string-designator &key (start 0) end)
  (let* ((string (string string-designator))
	 (copy (copy-string string)))
    (nstring-capitalize copy start end)))

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
