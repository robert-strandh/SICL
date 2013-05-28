(in-package #:externals-elimination)

;;;; This operation consists of taking an abstract syntax tree, and
;;;; removing all references to "externals", i.e.:
;;;;
;;;;   * constants that can not be expressed as immediates,
;;;;
;;;;   * special variables,
;;;;
;;;;   * global functions.
;;;;
;;;; The resulting abstract syntax tree is in some ways independent of
;;;; the backend, and in other ways highly dependent.  It is
;;;; independent in that it contains only references to backend
;;;; independent objects such as lexical locations, and operators.  It
;;;; is higly dependent on the backend in that it has been decided
;;;; what is to be passed in registers to a function, and memory
;;;; layout for some objects has been decided as well.
;;;;
;;;; At the moment, we are targeting a simplified x86 backend, and for
;;;; that backend, the following information is passed as arguments to
;;;; a function (either in registers or on the stack):
;;;;
;;;;   * The explicit arguments to the function.  We assume for now
;;;;     that only required arguments are possible.
;;;;
;;;;   * The argument count as a raw machine integer.
;;;;
;;;;   * The Lisp vector of "externals". 
;;;; 
;;;;   * The static environment of the enclosing function. 
;;;;
;;;; The fact that the resulting abstract syntax tree only contains
;;;; references to backend independent objects means that the
;;;; conversion to MIR and subsequent optimizations are also backend
;;;; independent.

(defun immediate-from-constant (value)
  (if (typep value 'character)
      (+ (ash (char-code value) 2) #b10)
      (ash value 2)))

(defun replace-by-immediate-p (value)
  (or (typep value 'character)
      (and (typep value 'integer)
	   (<= (- #.(expt 2 29)) value #.(1- (expt 2 29))))))

(defun collect-constants (ast)
  (let ((result '()))
    (labels ((traverse (ast)
	       (when (typep ast 'sicl-ast:load-time-value-ast)
		 (let ((value (sicl-ast:value ast)))
		   (unless (replace-by-immediate-p value)
		     (push value result))))
	       (mapc #'traverse (sicl-ast:children ast))))
      (traverse ast))
    result))

(defun build-constants (ast)
  (let ((result (collect-constants ast)))
    (setf result (remove-duplicates result :test #'equal))
    (setf result (remove t result))
    (setf result (remove nil result))
    (cons nil (cons t result))))

(defun collect-globals (ast)
  (let ((result '()))
    (labels ((traverse (ast)
	       (when (typep ast 'sicl-env:global-location-info)
		 (push (sicl-env:location ast) result))
	       (mapc #'traverse (sicl-ast:children ast))))
      (traverse ast))
    result))

(defun build-globals (ast)
  (let ((result (collect-globals ast)))
    (remove-duplicates result :test #'eq)))
    
(defun replace-constant (ast table)
  (let ((value (sicl-ast:value ast)))
    (if (replace-by-immediate-p value)
	(change-class ast 'sicl-ast:word-ast
		      :value (immediate-from-constant value))
	(let ((location (cdr (assoc value table :test #'equal))))
	  (change-class ast 'sicl-env:lexical-location-info
			:dynamic-extent-p nil
			:ignore-info nil
			:inline-info nil
			:type t
			:location location)))))

(defun replace-constants (ast table)
  (labels ((traverse (ast)
	     (when (typep ast 'sicl-ast:load-time-value-ast)
	       (replace-constant ast table))
	     (mapc #'traverse (sicl-ast:children ast))))
    (traverse ast)))

(defun make-lexical-location-info (location)
  (make-instance 'sicl-env:lexical-location-info
    :dynamic-extent-p nil
    :ignore-info nil
    :inline-info nil
    :type t
    :location location))

(defun replace-setq (ast table)
  (let* ((lhs-ast (sicl-ast:lhs-ast ast))
	 (location (cdr (assoc (sicl-env:location lhs-ast) table))))
    (change-class
     ast 'sicl-ast:memset-ast
     :argument-asts
     (list (sicl-ast:make-u--ast
	    (list (make-lexical-location-info location)
		  (sicl-ast:make-word-ast 1)))
	   (sicl-ast:value ast)))))

(defun replace-global (ast table)
  (let ((location (cdr (assoc (sicl-env:location ast) table :test #'eq))))
    (change-class
     ast 'sicl-ast:memref-ast
     :argument-asts
     (list (sicl-ast:make-u--ast
	    (list (make-lexical-location-info location)
		  (sicl-ast:make-word-ast 1)))))))

(defun replace-globals (ast table)
  (labels ((traverse (ast)
	     (cond ((and (typep ast 'sicl-ast:setq-ast)
			 (typep (sicl-ast:lhs-ast ast)
				'sicl-env:global-location-info))
		    (replace-setq ast table))
		   ((typep ast 'sicl-env:global-location-info)
		    (replace-global ast table))
		   ((typep ast 'sicl-env:special-location-info)
		    (error "can't handle specials yet"))
		   (t nil))
	     (mapc #'traverse (sicl-ast:children ast))))
    (traverse ast)))

(defun make-assignment (in-loc offset out-loc)
  (sicl-ast:make-setq-ast
   (make-lexical-location-info out-loc)
   (sicl-ast:make-memref-ast
    (sicl-ast:make-u+-ast
     (list (make-lexical-location-info in-loc)
	   (sicl-ast:make-word-ast offset))))))

(defun eliminate-externals (ast)
  (check-type ast sicl-ast:function-ast)
  (let* ((constants (build-constants ast))
	 (constants-table
	   (loop for constant in constants
		 collect (cons constant
			       (sicl-env:make-lexical-location (gensym)))))
	 (globals (build-globals ast))
	 (globals-table
	   (loop for global in globals
		 collect (cons global
			       (sicl-env:make-lexical-location (gensym))))))
    (replace-constants ast constants-table)
    (replace-globals ast globals-table)
    (let* ((externals-location (sicl-env:make-lexical-location (gensym)))
	   (externals-in (make-lexical-location-info externals-location)))
      (push externals-in (sicl-ast:required ast))
      (setf (sicl-ast:body-ast ast)
	    (sicl-ast:make-progn-ast
	     (append (loop for entry in (append constants-table
						globals-table)
			   for loc = (cdr entry)
			   for offset from 0 by 4
			   collect (make-assignment
				    externals-location offset loc))
		     (list (sicl-ast:body-ast ast))))))))
