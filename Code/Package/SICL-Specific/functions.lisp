(cl:in-package #:sicl-package)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function PACKAGEP.

(defgeneric packagep (object)
  (:method (object)
    (declare (ignore object))
    nil)
  (:method ((object package))
    (declare (ignorable object))
    t))

(deftype string-designator ()
  '(or character string symbol))

(defun package-designator-to-package (package-designator)
  (cond ((packagep package-designator)
	 package-designator)
	((typep package-designator 'string-designator)
	 (find-package package-designator))
	(t
	 (error 'not-a-package-designator :datum package-designator))))

;;; FIXME: signal correctable errors.
;;; FIXME: check that nicknames is a proper list of string designators
;;; FIXME: check that use is a proper list of package designators.
(defun make-package (name &key nicknames use)
  (declare (type string-designator name))
  (let ((existing-package (find-package name)))
    (loop until (null existing-package)
          do (restart-case (error 'package-already-exists
                                  :name existing-package)
               (force (stuff)
                 :report (lambda (stream)
                           (format stream
                                   "Replace the existing package."))
                 (setf existing-package nil))
               (change-name (new-package-name)
                 :report (lambda (stream)
                           (format stream
                                   "Make a package with a different name."))
                 :interactive (lambda ()
                                (format *query-io* "Enter new name: ")
                                (list (read *query-io*)))
                 (setf existing-package (find-package new-package-name))))))
  (loop for rest = nicknames then (cdr rest)
	while (consp rest)
	do (assert (null (find-package (car rest)))))
  (let ((package (make-in-instance 'package)))
    (setf (name package) (string name))
    (setf (nicknames package) (copy-list nicknames))
    (use-package use package)
    package))

(defun package-name (package-designator)
  (let ((package (package-designator-to-package package-designator)))
    (name package)))

(defun package-shadowing-symbols (package-designator)
  (let ((package (package-designator-to-package package-designator)))
    (copy-list (shadowing-symbols package))))

(defun package-use-list (package-designator)
  (let ((package (package-designator-to-package package-designator)))
    (copy-list (use-list package))))

(defun package-used-by-list (package-designator)
  (let ((package (package-designator-to-package package-designator)))
    (copy-list (used-by-list package))))

;;; FIXME: it should be the CL-USER package.
(defparameter *package* nil)

(defun find-symbol (symbol-name &optional (package-designator *package*))
  (let ((package (package-designator-to-package package-designator)))
    (loop for symbols = (external-symbols package)
	    then (cdr symbols)
	  while (consp symbols)
	  when (string= (symbol-name (car symbols)) symbol-name)
	    do (return-from find-symbol
		 (values (car symbols) :external)))
    (loop for symbols = (internal-symbols package)
	    then (cdr symbols)
	  while (consp symbols)
	  when (string= (symbol-name (car symbols)) symbol-name)
	    do (return-from find-symbol
		 (values (car symbols) :internal)))
    (loop for packages = (use-list package)
	    then (cdr packages)
	  while (consp packages)
	  do (loop for symbols = (external-symbols (car packages))
		     then (cdr symbols)
		   while (consp symbols)
		   when (string= (symbol-name (car symbols)) symbol-name)
		     do (return-from find-symbol
			  (values (car symbols) :inherited))))
    (values nil nil)))

(defun intern (symbol-name &optional (package-designator *package*))
  (multiple-value-bind (symbol-or-nil status)
      (find-symbol symbol-name package)
    (if (null symbol-or-nil)
	(let ((new-symbol (make-symbol symbol-name))
	      (package (package-designator-to-package package-designator)))
	  (setf (sicl-symbol:package new-symbol) package)
	  (push new-symbol (internal-symbols package))
	  (values new-symbol nil))
	(values symbol-or-nil status))))

;;; FIXME: check syntax better
(defmacro do-external-symbols ((symbol-variable
				&optional
				  (package-form '*package*)
				  (result-form 'nil))
			       &body body)
  (let ((function-name (gensym))
	(package-var (gensym))
	(remaining-body body)
	(declarations '()))
    (loop while (eq (caar remaining-body) 'declare)
	  do (push (pop remaining-body) declarations))
    `(block nil
       (let ((,function-name (lambda (,symbol-variable)
			       (locally ,@(reverse declarations)
				 (tagbody
				    ,@remaining-body))))
	     (,package-var (package-designator-to-package ,package-form)))
	 (mapc ,function-name
	       (external-symbols ,package-var)))
       ,result-form)))

;;; FIXME: check syntax better
(defmacro do-symbols ((symbol-variable
		       &optional
			 (package-form '*package*)
			 (result-form 'NIL))
		      &body body)
  (let ((function-name (gensym))
	(package-var (gensym))
	(remaining-body body)
	(declarations '()))
    (loop while (eq (caar remaining-body) 'declare)
	  do (push (pop remaining-body) declarations))
    `(block nil
       (let ((,function-name (lambda (,symbol-variable)
			       (locally ,@(reverse declarations)
				 (tagbody
				    ,@remaining-body))))
	     (,package-var ,package-form))
	 (mapc ,function-name
	       (external-symbols ,package-var))
	 (mapc ,function-name
	       (internal-symbols ,package-var))
	 (mapc (lambda (used-package)
		 (mapc
		  (lambda (symbol)
		    (unless (symbol-member
			     symbol
			     (shadowing-symbols ,package-var))
		      (mapc ,function-name symbol)))
		  (external-symbols used-package)))
	       (use-list ,package-var)))
       ,result-form)))

;;; FIXME: check for conflicts
;;; FIXME: return T
;;; FIXME: check definition of a designator for a list of symbols
;;;        with respect to NIL.
(defun export (symbols &optional package *package*)
  (unless (packagep package)
    (setf package (find-package (string package))))
  (flet ((make-external (sym)
	   (setf (package-external-symbols package)
		 (cons sym (package-external-symbols package)))))
    (flet ((aux (symbol)
	     (cond ((symbol-in-list-p symbol (package-external-symbols package))
		    ;; do nothing
		    (return-from export cl:t))
		   ((symbol-in-list-p symbol (internal-symbols package))
		    ;; change it to be external
		    (setf (internal-symbols package)
			  (remove-symbol-from-list
			   symbol (internal-symbols package)))
		    (make-external symbol))
		   (t
		    (loop for used = (package-use-list package)
			    then (cdr used)
			  while (consp used)
			  do (loop for syms = (package-use-list (car used))
				     then (cdr syms)
				   do (when (eq (car syms) symbol)
					(make-external symbol)
					(return-from export t))))
		    ;; come here if the symbol is not accessible
		    (error "symbol ~s not accessible in package ~s"
			   (symbol-name symbol)
			   ;; FIXME: This won't work for symbols
			   ;; without a home package.
			   (package-name (symbol-package symbol)))))))
      (cond ((symbolp symbols)
	     (aux symbols))
	    ((or (stringp symbols) (characterp symbols))
	     (aux (find-symbol (string symbols))))
	    ((consp symbols)
	     (loop for rest = symbols
		     then (cdr rest)
		   while (consp rest)
		   do (let ((sym (car rest)))
			(cond ((symbolp sym)
			       (aux sym))
			      ((or (stringp sym) (characterp sym))
			       (aux (find-symbol (string sym))))
			      (t
			       (error "~s is not a symbol designator"
				      (host-object-from-object sym)))))))
	    (t 
	     (error "~s is not a designator for a list of symbols"
		    (host-object-from-object symbols)))))))



