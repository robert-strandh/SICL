(cl:in-package #:sicl-clos)

(defun parse-defmethod (all-but-name)
  (let* ((lambda-list-position (position-if #'listp all-but-name))
	 (qualifiers (subseq all-but-name 0 lambda-list-position))
	 (lambda-list (elt all-but-name lambda-list-position))
	 (body (subseq all-but-name (1+ lambda-list-position)))
	 (parsed-lambda-list (parse-specialized-lambda-list lambda-list))
	 (required (required parsed-lambda-list)))
    (multiple-value-bind (declarations documentation forms)
	(cleavir-code-utilities:separate-function-body body)
      (values qualifiers
	      (append (mapcar #'car required)
		      (subseq lambda-list (length required)))
	      (mapcar #'cadr required)
	      declarations
	      documentation
	      forms))))

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
