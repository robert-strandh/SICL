(cl:in-package #:sicl-global-environment)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Querying the environment.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function ENSURE-FUNCTION-ENTRY.
;;;
;;; Given a name and an optional lambda expression, make sure there is
;;; a global function entry in the global environment with that name.
;;; If there is no such entry then create one and return it.  If there
;;; already is an entry with that name, then if a lambda list is
;;; given, then make that lambda list the new lambda list of the
;;; existing entry.  If the function to be defined has an INLINE
;;; proclamation in effect, then create the abstract syntax tree for
;;; the function and store it in the function entry.  Return either
;;; the newly created entry or the existing entry.

(defun ensure-function-entry
    (name lambda-list ast parameters)
  (declare (cl:type function-name name))
  (let ((entry (find name (functions *global-environment*)
		     :test #'equal :key #'name)))
    (if (null entry)
	(let ((new-entry (make-function-entry
			  name lambda-list ast parameters)))
	  (push new-entry (functions *global-environment*))
	  new-entry)
	(progn (setf (lambda-list entry) lambda-list)
	       (setf (ast entry) ast)
	       (setf (parameters entry) parameters)
	       entry))))

(defun find-variable (symbol environment)
  (find symbol
	(append
	 ;; The order here doesn't matter because, there can only be
	 ;; one entry for a particular name in the global environment.
	 (constant-variables environment)
	 (symbol-macros environment)
	 (special-variables environment))
	:test #'eq
	:key #'name))

(defun find-function (function-name environment)
  (find function-name
	(append
	 ;; We want to search global macros first,
	 ;; because if such an entry exists, it
	 ;; takes precedence over other entries.
	 (macros environment)
	 (functions environment)
	 (special-operators environment))
	:test #'equal
	:key #'name))

(defun find-type (entry env)
  `(and ,@(loop for e in (append env (proclamations *global-environment*))
		when (and (type-declaration-entry-p e)
			  (eq (base-entry e) entry))
		  collect (type e))))

(defun find-inline-info (entry env)
  (loop for e in (append env (proclamations *global-environment*))
	do (when (and (inline-or-notinline-declaration-entry-p e)
		      (eq (base-entry e) entry))
	     (return (if (inline-declaration-entry-p e)
			 :inline
			 :notinline)))))

(defun find-ignore-info (entry env)
  (cond ((loop for e in (append env (proclamations *global-environment*))
	       when (and (ignore-declaration-entry-p e)
			 (eq (base-entry e) entry))
		 return t)
	 :ignore)
	((loop for e in (append env (proclamations *global-environment*))
	       when (and (ignorable-declaration-entry-p e)
			 (eq (base-entry e) entry))
		 return t)
	 :ignorable)
	(t nil)))

(defun find-dynamic-extent-info (entry env)
  (loop for e in (append env (proclamations *global-environment*))
	when (and (dynamic-extent-declaration-entry-p e)
		  (eq (base-entry e) entry))
	  return t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Method on CLEAVIR-ENV:VARIABLE-INFO.
;;;
;;; Cleavir requires that the implementation define a method on this
;;; generic function, specialized to the implementation-specific
;;; global environment type.  

(defmethod cleavir-env:variable-info ((env global-environment) symbol)
  (let ((entry (find-variable symbol env)))
    (cond ((null entry)
	   nil)
	  ((constant-variable-entry-p entry)
	   (make-instance 'cleavir-env:constant-variable-info
	     :name (name entry)
	     :value (definition entry)))
	  ((symbol-macro-entry-p entry)
	   (make-instance 'cleavir-env:symbol-macro-info 
	     :name (name entry)
	     :expansion (definition entry)
	     :type (find-type entry env)))
	  (t
	   (let ((type (find-type entry env))
		 (ignore-info (find-ignore-info entry env)))
	     (make-instance 'cleavir-env:special-variable-info
	       :name symbol
	       :type type
	       :ignore ignore-info))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Method on CLEAVIR-ENV:FUNCTION-INFO.
;;;
;;; Cleavir requires that the implementation define a method on this
;;; generic function, specialized to the implementation-specific
;;; global environment type.  

(defmethod cleavir-env:function-info ((env global-environment) function-name)
  (let ((entry (find-function function-name env)))
    (cond ((null entry)
	   nil)
	  ((and (symbolp function-name) (special-operator-p function-name))
	   (make-instance 'cleavir-env:special-operator-info
	     :name function-name))
	  ((macro-entry-p entry)
	   (make-instance 'cleavir-env:global-macro-info 
	     :name (name entry)
	     ;; FIXME: add compiler-macro infomation
	     :compiler-macro nil
	     :expander (definition entry)))
	  (t
	   (let ((type (find-type entry env))
		 (inline-info (find-inline-info entry env))
		 (ignore-info (find-ignore-info entry env))
		 (dynamic-extent-p (find-dynamic-extent-info entry env)))
	     (make-instance 'cleavir-env:global-function-info
	       :name function-name
	       :type type
	       :inline inline-info
	       :ignore ignore-info
	       ;; FIXME: add compiler-macro infomation
	       :compiler-macro nil
	       :dynamic-extent dynamic-extent-p))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Create an environment entry from a canonicalized declaration
;;; specifier.

(defun make-entry-from-declaration
    (canonicalized-declaration-specifier environment)
  (destructuring-bind (head . rest) canonicalized-declaration-specifier
    (case head
      (declaration
       (make-declaration-declaration-entry (car rest)))
      (dynamic-extent
       (let ((entry (if (consp (car rest))
			(find-function (cadr (car rest)) environment)
			(find-variable (car rest) environment))))
	 (make-dynamic-extent-declaration-entry entry)))
      (ftype
       (let ((entry (find-function (cadr rest) environment)))
	 (make-type-declaration-entry entry (car rest))))
      (ignorable
       (let ((entry (if (consp (car rest))
			  (find-function (cadr (car rest)) environment)
			  (find-variable (car rest) environment))))
	 (make-ignorable-declaration-entry entry)))
      (ignore
       (let ((entry (if (consp (car rest))
			(find-function (cadr (car rest)) environment)
			(find-variable (car rest) environment))))
	 (make-ignore-declaration-entry entry)))
      (inline
       (let ((entry (find-function (car rest) environment)))
	 (make-inline-declaration-entry entry)))
      (notinline
       (let ((entry (find-function (car rest) environment)))
	 (make-notinline-declaration-entry entry)))
      (optimize
       (make-optimize-declaration-entry
	(car (car rest)) (cadr (car rest))))
      (special
       ;; FIXME: is this right?
       (make-special-variable-entry (car rest)))
      (type
       (let ((entry (find-variable (cadr rest) environment)))
	 (make-type-declaration-entry entry (car rest)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function FIND-FUNCTION-CELL.
;;;
;;; This function is used when a a FASL file is loaded.  When the
;;; source file was compiled, there was a reference to a named global
;;; function in it.  As part of loading the FASL file, a CODE OBJECT
;;; must be built, and that code object contains a LINKAGE VECTOR,
;;; which contains all the external references of the file that was
;;; compiled.  External references to named global functions result in
;;; an entry in the linkage vector containg the CONS cell that is the
;;; value of the STORAGE slot of the global function entry.  
;;;
;;; This function finds that CONS cell and returns it.  Normally, the
;;; FASL file should be loaded into an environment that contains the
;;; global function entry, because if it did not already exist, it was
;;; created as part of the compilation.  However, we must put
;;; SOMETHING in the linkage vector even if the file happens to be
;;; loaded into an environment that does not have the global function
;;; entry that we want.  Otherwise the system will crash when an
;;; attempt is made to execute the code we loaded.  For that reason,
;;; we create the global function entry if it so happens that it does
;;; not exist.

(defun find-function-cell (function-name)
  (let ((function-entry
	  (find function-name (functions *global-environment*)
		:key #'name :test #'equal)))
    (when (null function-entry)
      ;; No function entry found.  Create one.
      (setf function-entry (make-function-entry function-name))
      (push function-entry (functions *global-environment*)))
    (storage (location function-entry))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function FIND-VALUE-CELL.
;;;
;;; This function is used when a a FASL file is loaded.  This function
;;; is used when the source file that was compiled contained a
;;; reference to a special variable or to a free undefined variable
;;; (which is taken to be a special variable that will ultimately be
;;; defined).  As part of loading the FASL file, a CODE OBJECT must be
;;; built, and that code object contains a LINKAGE VECTOR, which
;;; contains all the external references of the file that was
;;; compiled.  External references to special variables result in an
;;; entry in the linkage vector containg the CONS cell that is the
;;; value of the STORAGE slot of the special variable entry.  This
;;; value cell contains the global value of the variable, or +unbound+
;;; if the variable does not have a global value.
;;;
;;; This function finds that CONS cell and returns it.  Normally, the
;;; FASL file should be loaded into an environment that contains the
;;; special variable entry, because if it did not already exist, it
;;; was created as part of the compilation.  However, we must put
;;; SOMETHING in the linkage vector even if the file happens to be
;;; loaded into an environment that does not have the special variable
;;; entry that we want.  Otherwise the system will crash when an
;;; attempt is made to execute the code we loaded.  For that reason,
;;; we create the special variable entry if it so happens that it does
;;; not exist.

(defun find-value-cell (name)
  (let ((special-variable-entry
	  (find name (special-variables *global-environment*)
		:key #'name :test #'eq)))
    (when (null special-variable-entry)
      ;; No function entry found.  Create one.
      (setf special-variable-entry
	    (make-special-variable-entry name))
      (push special-variable-entry
	    (special-variables *global-environment*)))
    (storage (location special-variable-entry))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; The expansion of the macro IN-PACKAGE results in a call to this
;;; function.

(defun in-package-function (string-designator)
  (declare ((or character symbol string) string-designator))
  (setq *package* (find-package string-designator)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; The expansion of the macro DEFCONSTANT results in a call to this
;;; function.

(defun defconstant-function (name initial-value)
  (unless (null (find name (special-variables *global-environment*)
		      :key #'name :test #'eq))
    (error "attempt to redefine a special variable as a constant variable."))
  (unless (null (find name (symbol-macros *global-environment*)
		      :key #'name :test #'eq))
    (error "attempt to redefine a global symbol macro as a constant variable."))
  (let ((existing-entry (find name (constant-variables *global-environment*)
			      :key #'name :test #'eq)))
    (cond ((null existing-entry)
	   (push (make-constant-variable-entry name initial-value)
		 (constant-variables *global-environment*)))
	  ((not (eql initial-value (definition existing-entry)))
	   (error "attempt to redefine a constant variable"))
	  (t
	   nil)))
  ;; Return the name as the HyperSpec requires
  name)
