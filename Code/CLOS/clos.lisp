(in-package :sicl-clos)

;;; The DEFCLASS macro.  The AMOP is inconsistent with respect to the
;;; CLHS.  For instance, it requires the arguments to ENSURE-CLASS to
;;; appear in the same order as they appear in the DEFCLASS form, but
;;; that should not matter since they are not evaluated.  Furthermore,
;;; the AMOP talks about additional calss options, but no such
;;; additional options are permitted according to the CLHS.  We follow
;;; the CLHS.

;;; Define error messages as constants to avoid cluttering the code.  

(defconstant +malformed-slots-list+
  "The DIRECT-SLOTS must be a proper list of slot specs, 
but ~s was found.")

(defmacro defclass (name direct-superclasses direct-slots &rest options)
  (assert (and (listp direct-slots)
	       (null (last direct-slots 0)))
	  (direct-slots)
	  #.+malformed-slots-list+
	  direct-slots)
  `(ensure-class ,name
		 :direct-superclasses ,direct-superclasses
		 :direct-slots ,(mapcar #'canonicalize-slot-spec direct-slots)
		 ,@(process-class-options options)))
		 
(defconstant +malformed-class-option+
  "An option must be either
(:default-initargs <name> <value> <name> <value>...),
(:documentation <string>), or
(:metaclass <name>).  However ~s was found.")

(defconstant +malformed-documentation-option+
  "The DOCUMENTATION option takes the form
(:documentation <string>)
but ~s was found.")

(defconstant +default-initargs-once+
  "The DEFAULT-INITARGS option can appear only once
in the list of class options, but a second such
option was found: ~s."

(defconstant +documentation-once+
  "The DOCUMENTATION option can appear only once
in the list of class options, but a second such
option was found: ~s."

(defconstant +metaclass-once+
  "The METACLASS option can appear only once
in the list of class options, but a second such
option was found: ~s."

(defconstant +malformed-metaclass-option+
  "The METACLASS option takes the form
(:metaclass <class-name>)
but ~s was found.")

(defconstant +unknown-class-option+
  "A class option is either :default-initargs, :documentation,
or :metaclass, but ~s was found.") 

;;; Make sure each class options is well formed, and check that a
;;; class option appears at most once.  Return a list of class
;;; options, including the corresponding keyword argument, to be
;;; spliced into the call to ENSURE-CLASS.
(defun process-class-options (options)
  (let (default-initargs-option documentation-option metaclass-option)
    (loop for option in options
	  do (assert (consp option)
		     (option)
		     #.+malformed-class-option+
		     option)
	  do (assert (member (car option) '(:default-initargs :documentation :metaclass))
		     (car option)
		     #.+unknown-class-option+
		     (car option))
	  do (ecase (car option)
	       (:default-initargs
		   (assert (null default-initargs-option)
			   (option)
			   #.+default-initargs-once+
			   option)
		   (setf default-initargs-option option))
	       (:documentation
		(assert (and (consp (cdr option))
			     (null (cddr option)))
			(option)
			#.+malformed-documentation-option+
			option)
		(assert (null documentation-option)
			(option)
			#.+documentation-once+
			option)
		(setf documentation-option option))
	       (:metaclass
		(assert (and (consp (cdr option))
			     (null (cddr option)))
			(option)
			#.+malformed-metaclass-option+)
		(assert (null metaclass-option)
			(option)
			#.+metaclass-once+
			option)
		(setf metaclass-option option))))
    `(,@(unless (null default-initargs-option)
	  `(:default-initargs ,(cdr default-initargs-option)))
      ,@documentation-option
      ,@metaclass-option)))

(defconstant +malformed-default-initargs-option+
  "The DEFAULT-INITARG option takes the form
(:default-initargs <name> <value> <name> <value>...)
but ~s was found.")

(defun canonicalize-slot-spec (slot-spec)


