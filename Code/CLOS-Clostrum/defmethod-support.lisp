(cl:in-package #:sicl-clos)

(defun parse-defmethod (all-but-name)
  (let* ((lambda-list-position (position-if #'listp all-but-name))
         (qualifiers (subseq all-but-name 0 lambda-list-position))
         (lambda-list (elt all-but-name lambda-list-position))
         (body (subseq all-but-name (1+ lambda-list-position)))
         (parsed-lambda-list
           (cleavir-code-utilities:parse-specialized-lambda-list lambda-list))
         (required (cleavir-code-utilities:required parsed-lambda-list)))
    (multiple-value-bind (declarations documentation forms)
        (cleavir-code-utilities:separate-function-body body)
      (values qualifiers
              (mapcar #'car required)
              (subseq lambda-list (length required))
              (mapcar #'cadr required)
              declarations
              documentation
              forms))))

(defun canonicalize-specializer (specializer)
  (cond ((symbolp specializer)
         `(find-class ',specializer))
        ((and (consp specializer)
              (consp (cdr specializer))
              (null (cddr specializer)))
         `(make-instance 'eql-specializer :object ,(second specializer)))
        (t
         (error 'malformed-specializer :specializer specializer))))

(defun canonicalize-specializers (specializers)
  (mapcar #'canonicalize-specializer specializers))
