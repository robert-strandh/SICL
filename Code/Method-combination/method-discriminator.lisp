(cl:in-package #:sicl-method-combination)

(defun make-collection-clause (parsed-method-group-specifier)
  (destructuring-bind (name test options)
      (parse-method-group-specifier parsed-method-group-specifier)
    (let ((order-form (getf options :order)))
      `(,test
        (let ((order ,(if (null order-form) :most-specific-first order-form)))
          (unless (member order '(:most-specific-first :most-specific-last))
            (error "invalid order"))
          (if (eq order :most-specific-first)
              (setf ,name (append ,name (list method)))
              (push method ,name)))))))
      
(defun make-method-discriminator-code (parsed-method-group-specifiers)
  `(cond ,@(mapcar #'make-collection-clause parsed-method-group-specifiers)
         (t (error "Method qualifiers do not match any method group specifier"))))

(defun wrap-body (method-group-specifiers body)
  (unless (cleavir-code-utilities:proper-list-p method-group-specifiers)
    (error "method group specifiers must be a proper list"))
  (let ((parsed-method-group-specifiers (mapcar #'parse-method-group-specifier
                                                method-group-specifiers)))
    `(let ,@(loop for specifier in parsed-method-group-specifiers
                  collect `(,(first specifier) '()))
       (loop for method in methods
             do ,(make-method-discriminator-code parsed-method-group-specifiers)
             ,@(loop for specifier in parsed-method-group-specifiers
                     for name = (first specifier)
                     for options = (third specifier)
                     for required = (getf options :required)
                     unless (null required)
                       collect `(when (and ,required (null ,name))
                                  (error "Empty list of requred methods"))))
       ,body)))
