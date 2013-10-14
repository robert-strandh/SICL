(cl:in-package #:sicl-compiler)

(defvar *worklist*)

(defvar *type-info*)

(defun initialize-type-info (initial-instruction)
  (let ((table (make-hash-table :test #'eq)))
    (labels ((traverse (instruction)
	       (unless (gethash instruction table)
		 (setf (gethash instruction table) t)
		 (loop for succ in (sicl-mir:successors instruction)
		       do (setf (gethash (list instruction succ) *type-info*)
				(sicl-compiler-types:make-t-type-map)))
		 (loop for succ in (sicl-mir:successors instruction)
		       do (traverse succ)))))
      (traverse initial-instruction))))
			  
(defun initialize-worklist (initial-instruction)
  (let ((table (make-hash-table :test #'eq)))
    (labels ((traverse (instruction)
	       (unless (gethash instruction table)
		 (setf (gethash instruction table) t)
		 (push instruction *worklist*)
		 (loop for succ in (sicl-mir:successors instruction)
		       do (traverse succ)))))
      (traverse initial-instruction))))

(defgeneric process-instruction (instruction))

(defmethod process-instruction :around (instruction)
  (let ((old-maps (loop for succ in (sicl-mir:successors instruction)
			collect (gethash (list instruction succ) *type-info*))))
    (call-next-method)
    (loop for succ in (sicl-mir:successors instruction)
	  for old-map in old-maps
	  do (unless (sicl-compiler-types:type-map-equal
		      old-map (gethash (list instruction succ) *type-info*))
	       (push succ *worklist*)))))

(defun type-inference (initial-instruction)
  (let ((*type-info* (make-hash-table :test #'equal))
	(*worklist* '()))
    (initialize-type-info initial-instruction)
    (initialize-worklist initial-instruction)
    (loop until (null *worklist*)
	  do (process-instruction (pop *worklist*)))))
    
;;; By default, we take the OR of the type maps of the incoming arcs,
;;; and propagate the result to every outgoing arc.
(defmethod process-instruction (instruction)
  (let* ((in-maps (loop for prec in (sicl-mir:predecessors instruction)
			collect (gethash (list prec instruction) *type-info*)))
	 (result (reduce #'sicl-compiler-types:type-map-or in-maps)))
    (loop for succ in (sicl-mir:successors instruction)
	  do (setf (gethash (list instruction succ) *type-info*) result))))

(defmethod process-instruction ((instruction sicl-mir:get-argcount-instruction))
  (setf (gethash (list instruction (car (sicl-mir:successors instruction)))
		 *type-info*)
	(sicl-compiler-types:type-map-and
	 (reduce #'sicl-compiler-types:type-map-or
		 (loop for pred in (sicl-mir:predecessors instruction)
		       collect (gethash (list pred instruction)
					*type-info*)))
	 (sicl-compiler-types:make-single-variable-type-map
	  (car (sicl-mir:outputs instruction))
	  '(integer 0 #.(expt 2 30))))))

