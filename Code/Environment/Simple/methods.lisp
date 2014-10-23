(cl:in-package #:sicl-simple-environment)

;;; Recall that this function should return true if FUNCTION-NAME has
;;; a definition in ENVIRONMENT as an ordinary function, a generic
;;; function, a macro, or a special operator.
(defmethod sicl-env:fboundp (function-name (env simple-environment))
  (let ((entry (find-function-entry env function-name)))
    (and (not (null entry))
	 (or (not (eq (car (function-cell entry))
		      (unbound entry)))
	     (not (null (macro-function entry)))
	     (not (null (special-operator entry)))))))

(defmethod sicl-env:fmakunbound (function-name (env simple-environment))
  (let ((entry (find-function-entry env function-name)))
    (unless (null entry)
      (setf (car (function-cell entry))
	    (unbound entry))
      (setf (macro-function entry) nil)
      (setf (special-operator entry) nil)
      (setf (type entry) t)
      (setf (compiler-macro-function entry) nil)
      (setf (inline entry) nil))))

(defmethod sicl-env:special-operator (function-name (env simple-environment))
  (let ((entry (find-function-entry env function-name)))
    (if (null entry)
	nil
	(special-operator entry))))

(defmethod (setf sicl-env:special-operator)
    (new-definition function-name (env simple-environment))
  (let ((entry (find-function-entry env function-name)))
    (cond ((and (null entry) (null new-definition))
	   nil)
	  ((null entry)
	   (setf entry (ensure-function-entry env function-name))
	   (setf (special-operator entry) new-definition))
	  ((or (not (eq (car (function-cell entry))
			(unbound entry)))
	       (not (null (macro-function entry))))
	   (error "The name ~s already has a definition." function-name))
	  (t
	   (setf (special-operator entry) new-definition)))))

(defmethod sicl-env:fdefinition (function-name (env simple-environment))
  (let ((entry (find-function-entry env function-name)))
    (cond ((null entry)
	   (error 'undefined-function :name function-name))
	  ((not (eq (car (function-cell entry))
		    (unbound entry)))
	   (car (function-cell entry)))
	  ((not (null (macro-function entry)))
	   `(cl:macro-function ,(macro-function entry)))
	  ((not (null (special-operator entry)))
	   `(cl:special (special-operator entry)))
	  (t
	   (error 'undefined-function :name function-name)))))

(defmethod (setf sicl-env:fdefinition)
    (new-definition function-name (env simple-environment))
  (assert (functionp new-definition))
  (let ((entry (ensure-function-entry env function-name)))
    (if (not (null (special-operator entry)))
	(error "The name ~s has a definition as a special operator"
	       function-name)
	(progn (setf (car (function-cell entry)) new-definition)
	       (setf (macro-function entry) nil)
	       (setf (type entry) t)
	       (setf (inline entry) nil)))))

(defmethod sicl-env:macro-function (symbol (env simple-environment))
  (let ((entry (find-function-entry env symbol)))
    (if (null entry)
	nil
	(macro-function entry))))

(defmethod (setf sicl-env:macro-function)
    (new-definition function-name (env simple-environment))
  (assert (functionp new-definition))
  (let ((entry (ensure-function-entry env function-name)))
    (progn (setf (car (function-cell entry)) (unbound entry))
	   (setf (macro-function entry) new-definition)
	   (setf (type entry) t)
	   (setf (inline entry) nil))))

