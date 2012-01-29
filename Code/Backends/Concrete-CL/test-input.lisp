;; (defmacro when (test &body body)
;;   `(if ,test (progn ,@body) nil))

;; (defmacro unless (test &body body)
;;   (list 'if test nil (cons 'progn body)))

;; (defmacro lambda (parameters &body body)
;;   `(function (lambda ,parameters ,@body)))

;; (defmacro setf (place value)
;;   (if (symbolp place)
;;       `(setq ,place ,value)
;;       `(funcall (function (setf ,(car place))) ,value ,(cadr place))))

(defmacro defun (name lambda-list &body body)
  `(setf (fdefinition ',name)
	 (lambda ,lambda-list
	   (block ,name
	     ,@body))))

(defun identity (x) x)

	 