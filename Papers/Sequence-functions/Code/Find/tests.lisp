(in-package :find)

(defvar *version-numbers* '(0 1))

(defvar *bignum* 16892338847315625991)
(defvar *start*)
(setq *start* 100000)
(defvar *end*)
(setq *end* 10000000)
(defvar *step*)
(setq *step* 100000)

(defun random-bignum ()
  (loop
    for n = (random (ash 1 64))
    when (and (typep n 'bignum) (/= *bignum* n))
      do (return n)))

(defun the-vector-values (element-type)
  (case element-type
    (char (values #\b #\a))
    (t (values 1 0))))

(defun the-list-values (test-symbol)
  (let ((v0 'a) (v1 'b))
    (when (eq test-symbol '=)
      (setq v0 0 v1 1))
    (when (eq test-symbol 'eql)
      (setq v0 (random-bignum) v1 *bignum*))
    (values v1 v0)))

(defun the-values (data-type test-symbol &key element-type)
  (if (eq 'vector data-type)
      (the-vector-values element-type)
      (the-list-values test-symbol)))

(defun make-the-vector (n element-type)
  (multiple-value-bind (the-value the-other-value) (the-vector-values element-type)
    (let ((vector (make-array n :element-type element-type :initial-element the-other-value)))
      (setf (aref vector (1- n)) the-value)
      vector)))

(defun make-the-list (n the-value the-other-value)
  (let ((l (make-list (1- n) :initial-element the-other-value)))
    (setf (cdr (last l)) (list the-value))
    l))

(defun adjust-the-vector (n vector)
  (make-the-vector (+ (length vector) n) (array-element-type vector)))

(defun adjust-the-list (n list the-other-value)
  (nconc
   (make-list n :initial-element the-other-value)
   list))

(defun evaluate-test-call (times test-fun data)
  (evaluate-time (funcall test-fun data) times))

(defun version-symbol (data-type version-number)
  (find-symbol (format nil "FIND-~A-~A" data-type version-number)
	       (find-package :find)))

(defun version-symbols (data-type version-numbers)
  (mapcar (lambda (version-number) (version-symbol data-type version-number)) version-numbers))

(defun header (stream versions element-type test-symbol)
  (format stream "Element-type: ~A~%" element-type)
  (format stream "Test: ~A~%" test-symbol)
  (format stream " N~15T")
  (format stream "~{~14,A~}~%" versions))

(defclass test ()
  ((test-name :initarg :test-name :reader test-name)
   (test-funs :initarg :test-funs :reader test-funs)
   (make-fun :initarg :make-fun :reader make-fun)
   (adjust-fun :initarg :adjust-fun :reader adjust-fun)))

(defun make-test (name test-funs make-fun adjust-fun)
  (make-instance 'test :test-name name :test-funs test-funs :make-fun make-fun :adjust-fun adjust-fun))

(defun make-vector-fun (number test-symbol element-type)
  (let ((the-value (the-vector-values element-type))
	(test-fun (symbol-function test-symbol)))
    (if (zerop number)
	(lambda (vector)
	  (find-vector-0 the-value vector test-fun))
	(lambda (vector)
	  (find-vector-1 the-value vector test-fun)))))

(defun make-list-fun (number test-symbol endp key-symbol)
  (let ((the-value (the-list-values test-symbol))
	(test-fun (and test-symbol (symbol-function test-symbol)))
	(key-fun  (and key-symbol (symbol-function key-symbol))))
    (if (zerop number)
	(lambda (list)
	  (find-list-0 the-value list test-fun endp key-fun))
	(lambda (list)
	  (find-list-1 the-value list test-fun endp key-fun)))))

(defun make-vector-funs (numbers test-symbol element-type)
  (mapcar
   (lambda (number)
     (make-vector-fun number test-symbol element-type))
   numbers))

(defun make-list-funs (numbers test-symbol endp key-symbol)
  (mapcar
   (lambda (number)
     (make-list-fun number test-symbol endp key-symbol))
   numbers))

(defun list-value (v key-symbol)
  (if key-symbol
      (cons v v)
      v))

(defun alpha-test (test-symbol)
  (if (eq test-symbol '=)
      'egal
      test-symbol))

(defun make-vector-test (name test-symbol element-type)
  (make-test
   ;;   (format nil "VECTOR-~A-~A" (alpha-test test-symbol) element-type)
   name
   (make-vector-funs *version-numbers* test-symbol element-type)
   (lambda (n) (make-the-vector n element-type))
   (lambda (n vector) (adjust-the-vector n vector))))

(defun make-list-test (name test-symbol &key endp key-symbol)
  (multiple-value-bind (the-value the-other-value) (the-list-values test-symbol)
    (let ((v (list-value the-value key-symbol))
	  (ov (list-value the-other-value key-symbol)))
      (make-test
       ;;       (format nil "LIST-~A-~A-~A" (alpha-test test-symbol) endp key-symbol)
       name
       (make-list-funs *version-numbers*  test-symbol endp key-symbol)
       (lambda (n) (make-the-list n v ov))
       (lambda (n list) (adjust-the-list n list ov))))))

(defgeneric compare-versions-to-stream (stream test start end &key step times))
(defmethod compare-versions-to-stream 
    (stream (test test) (start integer) (end integer) &key (step 1) (times 1))
  (format stream "~A~%" (test-name test))
    (loop
      for k from start to end by step
      for data = (funcall (make-fun test) start) then (funcall (adjust-fun test) step data)
      do (format stream "~8D" k)
      do (when *trace* (format t "~D~14T" k))
      do (loop 
	   for test-fun in (test-funs test)
	   do (let ((version-time 
		      (evaluate-test-call times test-fun data)))
		    (format stream "~14,5F" version-time)
		    (when *trace*
		      (format t "~14,5F" version-time)))
	       finally (progn (format stream "~%")
			      (when *trace* (format t "~%") (finish-output t))
			      (finish-output stream)))))
(defun filepath (name)
  (concatenate 'string "../../" name ".res"))

(defgeneric compare-versions-to-file (test start end &key step times))
(defmethod compare-versions-to-file ((test test) start end &key (step 1) (times 1))
  (let ((filename (filepath (test-name test))))
    (format t "Ouput written to ~A~%" filename)
    (finish-output t)
    (with-open-file (stream filename :direction :output :if-does-not-exist :create :if-exists :supersede)
      (compare-versions-to-stream
       stream
       test
       start end
       :step step :times times))))

;; (cl:lisp-implementation-type)
	  ;; (cl:lisp-implementation-version) 
	  ;; (machine-version)))

(defgeneric compare-versions (test start end &key step times))
(defmethod compare-versions ((test test) (start integer) (end integer) &key (step 1) (times 1))
  (compare-versions-to-file test start end :step step :times times))

(defgeneric run-test (test))
(defmethod run-test ((test test))
  (compare-versions
   test
   *start* *end*
   :step *step* :times 2))

  ;; (test-versions 'vector '= :element-type 'bit)
  ;; (test-versions 'vector 'eql :element-type 'char)
  ;; (test-versions 'vector '= :element-type '(unsigned-byte 8))
  ;; (test-versions 'list '=)  ;; 0 1
  ;; (test-versions 'list 'eq) ;; symboles
  ;; (test-versions 'list 'eql) ;; random bignums ??
  ;; (test-versions 'list '= :endp t)
  ;; (test-versions 'list '= :key-symbol 'car)


(defparameter *tests* 
  (list
   (make-vector-test "VECTOR-EGAL-BIT" '= 'bit)
   (make-vector-test "VECTOR-EQ-SYMBOL" 'eq)
   (make-vector-test "VECTOR-EQL-CHAR" 'eql 'char)
   (make-vector-test "VECTOR-EGAL-UB8" '=  '(unsigned-byte 8))
   
   (make-list-test "LIST-EQ-SYMBOL" 'eq)
   (make-list-test "LIST-EGAL-BIGNUM" '=)
   (make-list-test "LIST-EGAL-CAR" '= :key-symbol 'car)
   (make-list-test "LIST-EGAL-END" '= :endp 'car)))

   

(defun run-tests (tests)
  (loop for test in tests
	do (run-test test)))

(defun the-tests ()
  (run-tests *tests*))

;; pour les tableaux tester: test, element-type
;; pour les listes tester:  test, endp, key

