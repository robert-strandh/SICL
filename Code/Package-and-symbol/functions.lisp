(cl:in-package #:sicl-package)

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
  (let ((package (package-designator-to-package package-designator)))
    (multiple-value-bind (symbol-or-nil status)
        (find-symbol symbol-name package)
      (if (null symbol-or-nil)
          (let ((new-symbol (make-symbol symbol-name))
                (package (package-designator-to-package package-designator)))
            (setf (sicl-symbol:package new-symbol) package)
            (push new-symbol (internal-symbols package))
            (values new-symbol nil))
          (values symbol-or-nil status)))))

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

(defun export-one-symbol (symbol package)
  (flet ((make-external (sym)
	   (setf (external-symbols package)
		 (cons sym (external-symbols package)))))
    (cond ((member symbol (external-symbols package))
           ;; do nothing
           (return-from export-one-symbol cl:t))
          ((member symbol (internal-symbols package))
           ;; change it to be external
           (setf (internal-symbols package)
                 (remove symbol (internal-symbols package)
                         :test #'eq))
           (make-external symbol))
          (t
           (loop for used = (package-use-list package)
                   then (cdr used)
                 while (consp used)
                 do (loop for syms = (package-use-list (car used))
                            then (cdr syms)
                          do (when (eq (car syms) symbol)
                               (make-external symbol)
                               (return-from export-one-symbol t))))
           ;; come here if the symbol is not accessible
           (error "symbol ~s not accessible in package ~s"
                  (symbol-name symbol)
                  ;; FIXME: This won't work for symbols
                  ;; without a home package.
                  (package-name (symbol-package symbol)))))))

(defun proper-list-p (list)
  (integerp (ignore-errors (list-length list))))

;;; Recall that a designator for a list of symbols is either a non-nil
;;; symbol (denoting a singleton list whose element is is that non-nil
;;; symbol, or a proper list of symbols, denoting itself.  Thus the
;;; symbol NIL denotes the empty list of symbols.
;;; FIXME: check for conflicts
(defun designated-list-of-symbols (designator)
  (cond ((null designator) '())
        ((symbolp designator) (list designator))
        ((and (proper-list-p designator)
              (every #'symbolp designator))
         designator)
        (t
         (error 'not-a-valid-designator-for-list-of-symbols
                :exptected-type 'designator-for-list-of-symbols
                :datum designator))))

(defun export (symbols-designator &optional package-designator *package*)
  (let ((package (package-designator-to-package package-designator))
        (symbols (designated-list-of-symbols symbols-designator)))
    (loop for symbol in symbols
          do (export-one-symbol symbol package)))
  t)
