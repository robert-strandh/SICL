(in-package :find)

(defvar *the-value* 1)
(defvar *the-other-value* 0)

(setq *the-value* 1)

(defun make-sbv (n &key (element-type '(unsigned-byte 8)))
  (let ((vector
	  (make-array n :element-type element-type)))
    (setf (aref vector (1- n)) *the-value*)
    vector))

(defun adjust-sbv (n vector)
  (make-sbv (+ (length vector) n) :element-type (array-element-type vector)))

(defun test-call (find-fun vector)
  (funcall find-fun *the-value* vector))

(defun evaluate-test-call (find-fun vector times)
  (evaluate-time (test-call find-fun vector) times))

(defun version-symbol (version-number)
  (find-symbol (format nil "FIND-VECTOR-~A" version-number)
	       (find-package :find)))

(defun version-symbols (version-numbers)
  (mapcar #'version-symbol version-numbers))

(defun header (stream versions element-type)
  (format stream "Element-type: ~A~%" element-type)
  (format stream "N~15T")
  (format t "~{~14,A~}~%" versions)
  )
  
(defun compare-versions-to-stream (stream versions element-type start end &key (step 1) (times 1))
  (header stream versions element-type)
  (loop
    for k from start to end by step
    for vector = (make-sbv start :element-type element-type) 
      then (adjust-sbv step vector)
    do (format stream "~8D" k)
    do (when *trace* (format t "~D~14T" k))
    do (loop for version in (mapcar #'symbol-function versions)
			 do (let ((version-time (evaluate-test-call version vector times)))
			      (format stream "~14,5F" version-time)
			      (when *trace*
				(format t "~14,5F" version-time))
			      )
	     finally (progn (format stream "~%")
			    (when *trace* (format t "~%") (finish-output t))
			    (finish-output stream)))))

(defun compare-version-numbers-to-stream (stream version-numbers element-type start end &key (step 1) (times 1))
  (compare-versions-to-stream 
   stream (version-symbols version-numbers)
   element-type
   start end :step step :times times))

(defun compare-version-numbers-to-file (file version-numbers element-type start end &key (step 1) (times 1))
  (with-open-file (stream file :direction :output :if-does-not-exist :create :if-exists :supersede)
    (compare-version-numbers-to-stream
     stream
     version-numbers
     element-type start end :step step :times times)))

(defvar *local*)
(setq *local* nil)
(defun make-test-filename (&optional (prefix ""))
  (if *local* 
	  prefix
	  (format nil "~A:~A-~A-~A.res"
			  prefix
			  (cl:lisp-implementation-type)
			  (cl:lisp-implementation-version) 
			  (machine-version))))
		  
(defun compare-version-numbers (version-numbers element-type start end &key (step 1) (times 1))
  (let ((name (format nil "v~A" (first version-numbers))))
	(loop
	  for n in (rest version-numbers)
	  do (setq name (format nil "~A-v~A" name n)))
	(let ((filename (make-test-filename name)))
	  (format t "Ouput written to ~A~%" filename)
	  (finish-output t)
	  (compare-version-numbers-to-file filename version-numbers element-type start end :step step :times times))))

(defun test-version-numbers (element-type &rest version-numbers)
  (compare-version-numbers version-numbers element-type 10000 10000000 :step 100000 :times 2))

(defun the-test ()
  (test-version-numbers 0 1 7))

(defun the-test2 ()
  (test-version-numbers 7 11))

;; CL-USER> (mouline-truc-versus-machin-one-file "v1-vs-v4" #'find-vector-1 #'reverse-count-4 1000 1000000 :step 1000 :times 30)
;; 1000    0.00000    0.03333
;; 2000    0.03333    0.06667
;; 3000    0.06667    0.10000
;; 4000    0.06667    0.13333
