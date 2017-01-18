(in-package :find)

(defvar *bignum* 16892338847315625991)

(defun the-vector-values (element-type)
  (case element-type
    (char (values #\b #\a))
    (t (values 1 0))))

(defun random-bignum ()
  (loop
    for n = (random (ash 1 64))
    when (and (typep n 'bignum) (/= *bignum* n))
      do (return n)))

(defun the-list-values (test-symbol)
  (let ((v0 'a) (v1 'b))
    (when (eq test-symbol '=)
      (setq v0 0 v1 1))
    (when (eq test-symbol 'eql)
      (setq v0 (random-bignum) v1 *bignum*))
    (values v1 v0)))

(defun make-vector-data (n element-type)
  (multiple-value-bind (the-value the-other-value) (the-values element-type)
    (let ((vector (make-array n :element-type element-type :initial-element the-other-value)))
      (setf (aref vector (1- n)) the-value)
      vector)))

(defun list-value (v key-symbol)
  (if key-symbol
      (cons v v)
      v))

(defun make-list-data (n test-symbol key-symbol)
  (multiple-value-bind (the-value the-other-value) (the-list-values test-symbol)
    (let ((l (make-list (1- n) :initial-element (list-value the-other-value key-symbol))))
      (setf (cdr (last l)) (list (list-value the-value key-symbol)))
      l)))

(defun adjust-vector-data (n vector)
  (make-vector-data (+ (length vector) n) (array-element-type vector)))

(defun make-data (n data-type &key element-type end key-symbol)
  (if (eq data-type 'vector)
      (make-vector-data n element-type)
      (make-list-data n end key-symbol)))

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

(defun compare-versions-to-stream (stream versions data-type test-symbol &key element-type end key-symbol (step 1) (times 1))
  (header stream versions element-type test-symbol)
  (loop
    with test = (symbol-function test-symbol)
    with the-value = (the-value element-type)
    for k from start to end by step
    for vector = (make-vector-data start element-type) 
      then (adjust-vector-data step vector)
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

(defun compare-versions-to-stream (stream data-type test-symbol &key element-type end key-symbol (step 1) (times 1))
  (compare-versions-to-stream 
   stream
   (version-symbols '(0 1))
     data-type
     test-symbol
     :element-type element-type
     :end end :key-symbol key-symbol :step step :times times))

(defun compare-versions-to-file (file data-type test-symbol &key element-type end key-symbol (step 1) (times 1))
  (with-open-file (stream file :direction :output :if-does-not-exist :create :if-exists :supersede)
    (compare-versions-to-stream
     stream
     data-type
     test-symbol
     :element-type element-type
     :end end :key-symbol key-symbol :step step :times times)))

(defun make-test-filename (data-type test-symbol &key element-type end key-symbol)
  (let ((name (format nil "~A-~A" data-type test-symbol)))
    (if (eq 'vector data-type)
	(setq name (concatenate 'string name (format nil "-~A" element-type)))
	(progn
	  (when end
	    (setq name (concatenate 'string name "-END")))
	  (when key-symbol
	    (setq name (concatenate 'string name "-" (symbol-name key-symbol))))))))

;; (cl:lisp-implementation-type)
	  ;; (cl:lisp-implementation-version) 
	  ;; (machine-version)))

(defun compare-versions (data-type test-symbol &key element-type end key-symbol (step 1) (times 1))
  (let ((filename (make-test-filename data-type test-symbol :element-type element-type :end end :key-symbol key-symbol)))
    (format t "Ouput written to ~A~%" filename)
    (finish-output t)
    (compare-versions-to-file
     filename
     data-type
     test-symbol :element-type element-type :end end :key-symbol key-symbol :step step :times times))))

(defun test-versions (data-type test-symbol &key element-type end key-symbol)
  (compare-versions data-type test-symbol key-symbol etype/end key-symbol 10000 1000000 :step 100000 :times 2))

(defun the-test ()
  (test-versions 'vector '= :element-type 'bit)
  (test-versions 'vector 'eql :element-type 'char)
  (test-versions 'vector '= :element-type '(unsigned-byte 8))
  (test-versions 'list '=)  ;; 0 1
  (test-versions 'list 'eq) ;; symboles
  (test-versions 'list 'eql) ;; random bignums ??
  (test-versions 'list '= :end t)
  (test-versions 'list '= :key #'car)
  )

;; pour les tableaux tester: test, element-type
;; pour les listes tester:  test, end, key
