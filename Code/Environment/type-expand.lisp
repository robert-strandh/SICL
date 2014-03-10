(cl:in-package #:sicl-global-environment)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Type expansion.

(defun type-function (symbol)
  (let ((entry (find symbol (types *global-environment*)
		     :key #'name :test #'eq)))
    (if (null entry)
	nil
	(definition entry))))

;;; FIXME: check if the type is already there, maybe?
(defun (setf type-function) (new-function symbol)
  (push (make-type-entry symbol new-function)
	(types *global-environment*))
  ;; Respect the general rule that setters should return the new
  ;; value.
  new-function)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function TYPEEXPAND-1.

(defun typeexpand-1 (type)
  (let* ((name (if (symbolp type) type (car type)))
	 (expander (type-function name)))
    (if (null expander)
	type
	(funcall expander
		 (if (symbolp type) (list type) type)
		 *global-environment*))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function TYPEEXPAND.

(defun typeexpand (type)
  (let ((expansion (typeexpand-1 type)))
    (if (eq expansion type)
	type
	(typeexpand expansion))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function TYPEEXPAND-ALL.

;;; FIXME: figure out what to do with the FUNCTION type specifier.
(defun typeexpand-all (type)
  (let ((expansion (typeexpand type)))
    (if (or (symbolp expansion) (null (cdr expansion)))
	(let ((name (if (symbolp expansion) expansion (car expansion))))
	  (case name
	    ((array cons integer rational
	      float short-float single-float double-float long-float)
	     `(,name * *))
	    (complex '(complex *))
	    (t
	     name)))
	(case (car expansion)
	  ((array cons integer rational
	    float short-float single-float double-float long-float)
	   (if (null (cddr expansion))
	       (append expansion '(*))
	       expansion))
	  ((and or values)
	   (cons (car expansion) (mapcar #'typeexpand-all (cdr expansion))))
	  (not
	   (cons 'not (typeexpand-all (cadr expansion))))
	  (t
	   expansion)))))

