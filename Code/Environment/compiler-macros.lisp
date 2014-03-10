(cl:in-package #:sicl-global-environment)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compiler macros.

;;; Since a compiler macro entry exists only if the name is fbound,
;;; this means that if the compiler macro entry exists, it refers
;;; either to a global macro entry, or to a global function entry
;;; which does not have +funbound+ in its storage cell. 
;;;
;;; I am not sure what the optional environment argument could be.  It
;;; seems to me that it must either be a global environment, i.e. an
;;; instance of GLOBAL-ENVIRONMENT, a local environment, or NIL.  In
;;; the last two cases, unless I am wrong, the value of
;;; *global-envrionment* must be used. 
(defun compiler-macro-function (name &optional environment)
  (unless (typep environment 'global-environment)
    (setf environment *global-environment*))
  (let ((entry (find-if (lambda (entry)
			  (eq (name (base-entry entry)) name))
			(compiler-macros environment))))
    (if (null entry)
	nil
	(definition entry))))

;;; If there is a compiler macro entry, it must refer to a base entry
;;; which is either a global macro entry or to a global function entry
;;; that is bound.  This function searches for such a base entry.
;;;
;;; FIXME: for now, don't require the function entry to be bound.
(defun find-base-entry (name environment)
  (or (find-if (lambda (entry)
		 (eq (name entry) name))
	       (macros environment))
      (find-if (lambda (entry)
		 (eq (name entry) name))
;;		 (and (eq (name entry) name)
;;		      (not (eq (car (storage (location entry))) +funbound+))))
	       (functions environment))))

(defun (setf compiler-macro-function) (new-function name &optional environment)
  (unless (null environment)
    (error "Environment object must be nil."))
  (let ((base-entry (find-base-entry name *global-environment*)))
    (when (null base-entry)
      (error "A global macro or a global function must already exist."))
    (let ((c-m-entry (find-if (lambda (entry)
				(eq (base-entry entry) base-entry))
			      (compiler-macros *global-environment*))))
      ;; Remove the old entry if there was one.
      (unless (null c-m-entry)
	(setf (compiler-macros *global-environment*)
	      (delete c-m-entry (compiler-macros *global-environment*)
		      :test #'eq)))
      ;; Add a new entry unless the new function is NIL.
      (unless (null new-function)
	(push (make-compiler-macro-entry base-entry new-function)
	      (compiler-macros *global-environment*)))))
  ;; Return the new value as required by the HyperSpec.
  new-function)

;;; FIXME: we probably won't need this function, so maybe remove it.
(defun compiler-macroexpand-1 (form &optional env)
  (if (symbolp (car form))
      (let ((entry (find-if (lambda (entry)
			      (eq (name (base-entry entry)) (car form)))
			    (compiler-macros env))))
	(if (null entry)
	    form
	    (funcall (coerce *macroexpand-hook* 'function)
		     (definition entry)
		     form
		     env)))
      form))

;;; FIXME: we probably won't need this function, so maybe remove it.
(defun compiler-macroexpand (form &optional env)
  (loop for expanded-form = (compiler-macroexpand-1 form env)
	until (eq expanded-form form)
	do (setf form expanded-form))
  form)

