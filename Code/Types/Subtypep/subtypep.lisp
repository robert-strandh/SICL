(defparameter *permanent-atomic-types* (make-hash-table :test #'eq))
(defparameter *temporary-atomic-types* nil)
(defparameter *temporary-objects* nil)

(defparameter *type-counter* 0)

(defun class-direct-superclasses (class)
  (declare (ignore class))
  nil)

(declaim (notinline class-direct-superclasses))
;;; Find the MOP function class-direct-superclasses
(eval-when (:load-toplevel)
  (do-all-symbols (symbol)
    (when (and (string= (symbol-name symbol)
			'#:class-direct-superclasses)
	       (fboundp symbol))
      (setf (symbol-function 'class-direct-superclasses)
	    (symbol-function symbol)))))

(defun find-type (name)
  (or (and *temporary-atomic-types* (gethash name *temporary-atomic-types*))
      (gethash name *permanent-atomic-types*)))

(defun add-mask (name mask)
  (if (and *temporary-atomic-types* (gethash name *temporary-atomic-types*))
      (setf (gethash name *temporary-atomic-types*)
	    (logior mask (gethash name *temporary-atomic-types*)))
      (setf (gethash name *permanent-atomic-types*)
	    (logior mask (gethash name *permanent-atomic-types*)))))

(defun register-type (name table)
  (cond ((symbolp name)
	 (when (null (find-type name))
	   (mapc (lambda (superclass)
		   (register-type superclass table))
		 (class-direct-superclasses (find-class name)))
	   (mapc (lambda (superclass)
		   (let ((super-name (class-name superclass))
			 (mask (ash 1 *type-counter*)))
		     (add-mask super-name mask)))
		 (class-direct-superclasses (find-class name)))
	   (setf (gethash name table) (ash 1 *type-counter*))
	   (incf *type-counter*)))
	((typep name 'class)
	 (register-permanent-type (class-name name)))
	(t (error "invalid type to register: ~s" name))))

(defun register-permanent-type (name)
  (register-type name *permanent-atomic-types*))

(defun register-temporary-type (name)
  (register-type name *temporary-atomic-types*))

(defun register-types (type-specifier)
  (cond ((atom type-specifier)
	 (unless (member type-specifier
			 '(integer bignum bit single-float long-float number
			   complex double-float ratio rational real float
			   fixnum unsigned-byte signed-byte short-float
			   base-char standard-char character extended-char
			   string array simple-string base-string
			   simple-vector bit-vector vector simple-array
			   simple-base-string simple-bit-vector))
	   (register-temporary-type type-specifier)))
	((member (car type-specifier) '(and or not))
	 (mapc #'register-types (cdr type-specifier)))
	(t nil)))

(defun find-object (object)
  (gethash object *temporary-objects*))

(defun register-object (object)
  (unless (or (typep object 'number)
	      (typep object 'character)
	      (typep object 'array))
    (when (null (gethash object *temporary-objects*))
      (let ((mask (ash 1 (shiftf *type-counter* (1+ *type-counter*)))))
	(maphash (lambda (key value)
		   (when (typep object key)
		     (setf mask (logior mask value))))
		 *temporary-atomic-types*)
	(maphash (lambda (key value)
		   (when (typep object key)
		     (setf mask (logior mask value))))
		 *permanent-atomic-types*)
	(setf (gethash object *temporary-objects*) mask)))))

(defun register-objects (type-specifier)
  (cond ((atom type-specifier) nil)
	((or (eq (car type-specifier) 'eql) (eq (car type-specifier) 'member))
	 (mapc #'register-object (cdr type-specifier)))
	((member (car type-specifier) '(and or not))
	 (mapc #'register-objects (cdr type-specifier)))
	(t nil)))
	 

;;; Do the nil type
(setf (gethash 'nil *permanent-atomic-types*) 0)
(setf (gethash 't *permanent-atomic-types*) -1)

;;; Condition types
(register-permanent-type 'condition)
(register-permanent-type 'simple-condition)
(register-permanent-type 'warning)
(register-permanent-type 'simple-warning)
(register-permanent-type 'style-warning)
(register-permanent-type 'serious-condition)
(register-permanent-type 'storage-condition)
(register-permanent-type 'error)
(register-permanent-type 'simple-error)
(register-permanent-type 'file-error)
(register-permanent-type 'package-error)
(register-permanent-type 'stream-error)
(register-permanent-type 'end-of-file)
(register-permanent-type 'arithmetic-error)
(register-permanent-type 'division-by-zero)
(register-permanent-type 'floating-point-inexact)
(register-permanent-type 'floating-point-invalid-operation)
(register-permanent-type 'floating-point-overflow)
(register-permanent-type 'floating-point-underflow)
(register-permanent-type 'cell-error)
(register-permanent-type 'unbound-slot)
(register-permanent-type 'unbound-variable)
(register-permanent-type 'undefined-function)
(register-permanent-type 'type-error)
(register-permanent-type 'simple-type-error)
(register-permanent-type 'parse-error)
(register-permanent-type 'reader-error)
(register-permanent-type 'control-error)
(register-permanent-type 'program-error)
(register-permanent-type 'print-not-readable)

;;; Function types
(register-permanent-type 'function)
(register-permanent-type 'generic-function)
(register-permanent-type 'standard-generic-function)
;;; compiled-function is not a system class apparently
;;; (register-permanent-type 'compiled-function)

;;; Symbol types
(register-permanent-type 'symbol)
;;; keyword is not a system class apparently
;;; (register-permanent-type 'keyword)

;;; Stream types
(register-permanent-type 'stream)
(register-permanent-type 'broadcast-stream)
(register-permanent-type 'concatenated-stream)
(register-permanent-type 'string-stream)
(register-permanent-type 'echo-stream)
(register-permanent-type 'synonym-stream)
(register-permanent-type 'two-way-stream)
(register-permanent-type 'file-stream)

;;; Pathname types
(register-permanent-type 'pathname)
(register-permanent-type 'logical-pathname)

;;; Class and method-related types
(register-permanent-type 'standard-object)
(register-permanent-type 'class)
(register-permanent-type 'standard-class)
(register-permanent-type 'structure-class)
(register-permanent-type 'built-in-class)
(register-permanent-type 'structure-object)
(register-permanent-type 'method)
(register-permanent-type 'standard-method)
(register-permanent-type 'method-combination)

;;; Other atomic types
(register-permanent-type 'hash-table)
(register-permanent-type 'package)
(register-permanent-type 'random-state)
(register-permanent-type 'readtable)
(register-permanent-type 'restart)
(register-permanent-type 'cons)

(defclass type-descriptor ()
  ((%atomic-types :initarg :atomic-types :reader atomic-types)))

(defun make-type-descriptor (type-specifier)
  ;; FIXME: need to expand types defined by deftype
  (cond ((symbolp type-specifier)
	 (unless (member type-specifier
			 '(integer bignum bit single-float long-float number
			   complex double-float ratio rational real float
			   fixnum unsigned-byte signed-byte short-float
			   base-char standard-char character extended-char
			   string array simple-string base-string
			   simple-vector bit-vector vector simple-array
			   simple-base-string simple-bit-vector))
	   (let ((mask (find-type type-specifier)))
	     (make-instance 'type-descriptor
			    :atomic-types mask))))
	((eq (car type-specifier) 'and)
	 (loop with mask = -1
	       for descriptor in (mapcar #'make-type-descriptor
					   (cdr type-specifier))
	       do (setf mask (logand mask (atomic-types descriptor)))
	       finally (return (make-instance 'type-descriptor
					      :atomic-types mask))))
	((eq (car type-specifier) 'or)
	 (loop with mask = 0
	       for descriptor in (mapcar #'make-type-descriptor
					   (cdr type-specifier))
	       do (setf mask (logior mask (atomic-types descriptor)))
	       finally (return (make-instance 'type-descriptor
					      :atomic-types mask))))
	((eq (car type-specifier) 'not)
	 (let ((descriptor (make-type-descriptor (cadr type-specifier))))
	   (make-instance 'type-descriptor
			  :atomic-types (lognot (atomic-types descriptor)))))
	((eq (car type-specifier) 'eql)
	 (make-instance 'type-descriptor
			:atomic-types (find-object (cadr type-specifier))))
	((eq (car type-specifier) 'member)
	 (let ((mask 0))
	   (loop for object in (cdr type-specifier)
		 do (setf mask (logior mask (find-object object))))
	   (make-instance 'type-descriptor
			  :atomic-types mask)))
	(t nil))) ; FIXME: check for errors
	  
  (defun stp (type-1 type-2)
  (let ((*temporary-objects* (make-hash-table))
	(*temporary-atomic-types* (make-hash-table :test #'eq))
	(*type-counter* *type-counter*))
    (register-types type-1)
    (register-types type-2)
    (register-objects type-1)
    (register-objects type-2)
    (zerop (logand (atomic-types (make-type-descriptor type-1))
		   (lognot (atomic-types (make-type-descriptor type-2)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Numbers

;;; Given a floating-point number x, compute its successor.
;;; The number x must be strictly smaller than the maximum 
;;; floating-point number, and strictly greater than 0. 
(defun successor (x)
  (multiple-value-bind (m e)
      (decode-float x)
    (declare (ignore m))
  (+ x (scale-float (float 1 x) (- e (float-precision x))))))

;;; Given a floating-point number x, compute its predecessor The
;;; number x must be smaller than or equal to the maximum
;;; floating-point number, and strictly greater than 0.
(defun predecessor (x)
  (let ((p (float-precision x)))
    (multiple-value-bind (m e)
	(decode-float x)
      (declare (ignore m))
      (if (and (= (decode-float x) 0.5)
	       (/= (- x (scale-float (float 1 x) (- e 1 p))) x))
	  (- x (scale-float (float 1 x) (- e 1 p)))
	  (- x (scale-float (float 1 x) (- e p)))))))

(defun not-float (intervals)
  (if (null intervals)
      (list (cons '* '*))
      (let ((result '()))
	(unless (eq (car (first intervals)) '*)
	  (push (cons '* (predecessor (car (first intervals)))) result))
	(loop until (null (cdr intervals))
	      do (push (cons (successor (cdr (first intervals)))
			     (predecessor (car (second intervals))))
		       result)
		 (pop intervals))
	(unless (eq (cdr (first intervals)) '*)
	  (push (cons (successor (cdr (first intervals))) '*) result))
	(nreverse result))))

(defun entirely-before (interval-1 interval-2)
  (and (not (eq (cdr interval-1) '*))
       (not (eq (car interval-2) '*))
       (or (and (eql (coerce (cdr interval-1) 'single-float) -0s0)
		(eql (coerce (car interval-2) 'single-float) 0s0))
	   (< (cdr interval-1) (car interval-2)))))

(defun and-float (intervals-1 intervals-2)
  (let ((result '()))
    (loop until (or (null intervals-1) (null intervals-2))
	  do (cond ((entirely-before (first intervals-1) (first intervals-2))
		    (pop intervals-1))
		   ((entirely-before (first intervals-2) (first intervals-1))
		    (pop intervals-2))
		   (t
		    (let ((start (cond ((eq (car (first intervals-1)) '*)
					(car (first intervals-2)))
				       ((eq (car (first intervals-2)) '*)
					(car (first intervals-1)))
				       (t
					(max (car (first intervals-1))
					     (car (first intervals-2)))))))
		      (cond ((and (eq (cdr (first intervals-1)) '*)
				  (eq (cdr (first intervals-2)) '*))
			     (push (cons start '*) result)
			     (pop intervals-1)
			     (pop intervals-2))
			    ((or (eq (cdr (first intervals-1)) '*)
				 (and (not (eq (cdr (first intervals-2)) '*))
				      (<= (cdr (first intervals-2))
					  (cdr (first intervals-1)))))
			     (push (cons start (cdr (first intervals-2))) result)
			     (pop intervals-2))
			    (t
			     (push (cons start (cdr (first intervals-1))) result)
			     (pop intervals-1)))))))
    (nreverse result)))

(defun or-float (intervals-1 intervals-2)
  (not-float (and-float (not-float intervals-1) (not-float intervals-2))))

;;; Array types
;; string
;; array
;; simple-string
;; base-string
;; simple-vector
;; bit-vector
;; vector
;; simple-array
;; simple-base-string
;; simple-bit-vector

;;; Number types
;; integer
;; bignum
;; bit
;; single-float
;; long-float
;; number
;; complex
;; double-float
;; ratio
;; rational
;; real
;; float
;; fixnum
;; unsigned-byte
;; signed-byte
;; short-float

;;; Character types
;; base-char
;; standard-char
;; character
;; extended-char


;; atom                          defined to be (not cons)
;; list                          defined to be (or cons null)
;; null			         defined to be (member nil)
;; sequence                      defined to be (or list vector)

