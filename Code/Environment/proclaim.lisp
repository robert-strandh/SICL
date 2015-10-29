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
  (setf (sicl-genv:special-variable
	 name
	 (load-time-value (sicl-genv:global-environment))
	 nil)
	nil))

(defun proclaim-inline (name)
  (setf (sicl-genv:function-inline name) 'inline))
  
(defun proclaim-notinline (name)
  (setf (sicl-genv:function-inline name) 'notinline))
  
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
