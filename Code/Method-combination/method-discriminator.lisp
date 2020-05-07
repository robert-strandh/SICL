(cl:in-package #:sicl-method-combination)

(defun make-collection-clause
    (parsed-method-group-specifier method-var)
  (destructuring-bind (name test options)
      parsed-method-group-specifier
    (let ((order-form (getf options :order)))
      `(,test
        (let ((order ,(if (null order-form) :most-specific-first order-form)))
          (unless (member order '(:most-specific-first :most-specific-last))
            (error "invalid order"))
          (if (eq order :most-specific-first)
              (setf ,name (append ,name (list ,method-var)))
              (push ,method-var ,name)))))))
      
(defun make-method-discriminator-code
    (parsed-method-group-specifiers method-var)
  `(cond ,@(loop for specifier in parsed-method-group-specifiers
                 collect (make-collection-clause specifier method-var))
         (t (error "Method qualifiers do not match any method group specifier"))))

(defun parse-method-group-specifiers (method-group-specifiers qualifiers-var)
  (loop for specifier in method-group-specifiers
        collect (parse-method-group-specifier specifier qualifiers-var)))

(defun wrap-body (method-list-var method-group-specifiers body)
  (unless (cleavir-code-utilities:proper-list-p method-group-specifiers)
    (error "method group specifiers must be a proper list"))
  (let* ((qualifiers-var (gensym "qualifiers"))
         (parsed-method-group-specifiers
           (parse-method-group-specifiers method-group-specifiers qualifiers-var))
         (method-var (gensym "method")))
    `(let ,(loop for specifier in parsed-method-group-specifiers
                 collect `(,(first specifier) '()))
       (loop for (,method-var . ,qualifiers-var) in ,method-list-var
             do ,(make-method-discriminator-code
                  parsed-method-group-specifiers method-var))
       ,@(loop for specifier in parsed-method-group-specifiers
               for name = (first specifier)
               for options = (third specifier)
               for required = (getf options :required)
               unless (null required)
                 collect `(when (and ,required (null ,name))
                            (error "Empty list of required methods")))
       ,@body)))
