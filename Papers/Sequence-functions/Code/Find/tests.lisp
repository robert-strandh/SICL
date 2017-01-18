(in-package :find)

(defun the-values (element-type)
  (case element-type
    (symbol  (values 'b 'a))
    (char (values #\b #\a))
    (t (values 1 0))))

(defun make-vector (n element-type)
  (multiple-value-bind (the-value the-other-value) (the-values element-type)
    (let ((vector (make-array n :element-type element-type :initial-element the-other-value)))
      (setf (aref vector (1- n)) the-value)
      vector)))

(defun adjust-vector (n vector)
  (make-vector (+ (length vector) n) (array-element-type vector)))

(defun test-call (find-fun the-value vector test)
  (funcall find-fun the-value vector test))

(defun evaluate-test-call (find-fun the-value vector test times)
  (evaluate-time (test-call find-fun the-value vector test) times))

(defun version-symbol (version-number)
  (find-symbol (format nil "FIND-VECTOR-~A" version-number)
	       (find-package :find)))

(defun version-symbols (version-numbers)
  (mapcar #'version-symbol version-numbers))

(defun header (stream versions element-type test-symbol)
  (format stream "Element-type: ~A~%" element-type)
  (format stream "Test: ~A~%" test-symbol)
  (format stream " N~15T")
  (format stream "~{~14,A~}~%" versions)
  )

(defun compare-versions-to-stream (stream versions element-type test-symbol start end &key (step 1) (times 1))
  (header stream versions element-type test-symbol)
  (loop
    with test = (symbol-function test-symbol)
    with the-value = (the-value element-type)
    for k from start to end by step
    for vector = (make-vector start element-type) 
      then (adjust-vector step vector)
    do (format stream "~8D" k)
    do (when *trace* (format t "~D~14T" k))
    do (loop for version in (mapcar #'symbol-function versions)
	     do (let ((version-time (evaluate-test-call version the-value vector test times)))
		  (format stream "~14,5F" version-time)
		  (when *trace*
		    (format t "~14,5F" version-time))
		  )
	     finally (progn (format stream "~%")
			    (when *trace* (format t "~%") (finish-output t))
			    (finish-output stream)))))

(defun compare-version-numbers-to-stream (stream version-numbers element-type test-symbol start end &key (step 1) (times 1))
  (compare-versions-to-stream 
   stream
   (version-symbols version-numbers)
   element-type
   test-symbol
   start end :step step :times times))

(defun compare-version-numbers-to-file (file version-numbers element-type test-symbol start end &key (step 1) (times 1))
  (with-open-file (stream file :direction :output :if-does-not-exist :create :if-exists :supersede)
    (compare-version-numbers-to-stream
     stream
     version-numbers
     element-type test-symbol start end :step step :times times)))

(defun make-test-filename (element-type test-symbol &optional (prefix ""))
  (format nil "~A:~A:~A:~A-~A-~A.res"
	  prefix
	  element-type
	  test-symbol
	  (cl:lisp-implementation-type)
	  (cl:lisp-implementation-version) 
	  (machine-version)))

(defun compare-version-numbers (version-numbers element-type test-symbol start end &key (step 1) (times 1))
  (let ((name (format nil "v~A" (first version-numbers))))
    (loop
      for n in (rest version-numbers)
      do (setq name (format nil "~A-v~A" name n)))
    (let ((filename (make-test-filename element-type test-symbol name)))
      (format t "Ouput written to ~A~%" filename)
      (finish-output t)
      (compare-version-numbers-to-file filename version-numbers element-type test-symbol start end :step step :times times))))

(defun test-version-numbers (element-type test-symbol &rest version-numbers)
  (compare-version-numbers version-numbers element-type test-symbol 10000 1000000 :step 100000 :times 2))

(defun the-test ()
  (test-version-numbers 'bit '= 0 1)
  (test-version-numbers 'symbol 'eq 0 1)
  (test-version-numbers 'char 'eql 0 1)
  (test-version-numbers '(unsigned-byte 8) '= 0 1)
  )
