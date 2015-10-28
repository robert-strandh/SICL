(cl:in-package #:sicl-global-environment)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function PROCLAIM.

(defun proclaim-declaration (name)
  (unless (find-if (lambda (entry)
		     (and (typep entry 'declaration-declaration-entry)
			  (eq (name entry) name)))
		   (proclamations *global-environment*))
    (push (make-instance 'declaration-declaration-entry
	    :name name)
	  (proclamations *global-environment*))))

(defun proclaim-ftype (name type)
  (setf (sicl-genv:function-type name (global-environment))
	type))

(defun proclaim-special (name)
  (pushnew (make-special-variable-entry name)
	   (special-variables *global-environment*)
	   :key #'name
	   :test #'eq))

(defun proclaim-inline (name)
  (let ((base-entry (find name (functions *global-environment*)
			  :key #'name)))
    ;; If there is not an base entry for the function, then create
    ;; one.
    (when (null base-entry)
      (setf base-entry (make-function-entry name))
      (push base-entry (functions *global-environment*)))
    ;; Next, check whether there is already an INLINE or a NOTINLINE
    ;; proclamation.
    (let ((aux-entry (find-if
		      (lambda (entry)
			(and (or (typep entry 'inline-declaration-entry)
				 (typep entry 'notinline-declaration-entry))
			     (eq (base-entry entry) base-entry)))
		      (proclamations *global-environment*))))
      (cond ((null aux-entry)
	     (push (make-inline-declaration-entry base-entry)
		   (proclamations *global-environment*)))
	    ((typep aux-entry 'notinline-declaration-entry)
	     (change-class aux-entry 'inline-declaration-entry))
	    (t
	     nil)))))
  
(defun proclaim-notinline (name)
  (let ((base-entry (find name (functions *global-environment*)
			  :key #'name)))
    ;; If there is not an base entry for the function, then create
    ;; one.
    (when (null base-entry)
      (setf base-entry (make-function-entry name))
      (push base-entry (functions *global-environment*)))
    ;; Next, check whether there is already an INLINE or a NOTINLINE
    ;; proclamation.
    (let ((aux-entry (find-if
		      (lambda (entry)
			(and (or (typep entry 'inline-declaration-entry)
				 (typep entry 'notinline-declaration-entry))
			     (eq (base-entry entry) base-entry)))
		      (proclamations *global-environment*))))
      (cond ((null aux-entry)
	     (push (make-notinline-declaration-entry base-entry)
		   (proclamations *global-environment*)))
	    ((typep aux-entry 'inline-declaration-entry)
	     (change-class aux-entry 'notinline-declaration-entry))
	    (t
	     nil)))))
  
(defun proclaim (declaration-specifier)
  (case (car declaration-specifier)
    (declaration
     (mapc #'proclaim-declaration
	   (cdr declaration-specifier)))
    (ftype
     (mapc (lambda (name) (proclaim-ftype name (cadr declaration-specifier)))
	   (cddr declaration-specifier)))
    (special
     (mapc #'proclaim-special
	   (cdr declaration-specifier)))
    (inline
     (mapc #'proclaim-inline
	   (cdr declaration-specifier)))
    (notinline
     (mapc #'proclaim-notinline
	   (cdr declaration-specifier)))
    ;; FIXME: handle more proclamations
    ))
