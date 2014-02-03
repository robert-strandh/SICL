(cl:in-package #:sicl-clos)

(defun parse-defmethod (args)
  (let ((name (pop args))
	(qualifiers (loop while (and (consp args) (not (listp (car args))))
			  collect (pop args)))
	(lambda-list (pop args)))
    ;; FIXME: handle declarations and documentation
    (let* ((parsed-lambda-list (parse-specialized-lambda-list lambda-list))
	   (required (required parsed-lambda-list)))
      (values name
	      qualifiers
	      (append (mapcar #'car required)
		      (subseq lambda-list (length required)))
	      (mapcar #'cadr required)
	      args))))

(defun canonicalize-specializer (specializer)
  (cond ((symbolp specializer)
	 `',specializer)
	((and (consp specializer)
	      (consp (cdr specializer))
	      (null (cddr specializer)))
	 `(make-instance 'eql-specializer :object ,(cadr specializer)))
	(t
	 (error "malformed specializer: ~s" specializer))))

(defun canonicalize-specializers (specializers)
  `(list ,@(mapcar #'canonicalize-specializer specializers)))
