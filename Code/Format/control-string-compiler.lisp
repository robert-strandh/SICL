(cl:in-package #:sicl-format)

(defun compile-parameter-value (directive parameter-spec)
  (let* ((parameter-name (car parameter-spec))
         (compile-time-value (funcall parameter-name directive)))
    (cond ((null compile-time-value)
           ;; The parameter was not given at all, in the format control
           ;; string, neither as a constant value, nor as a value to
           ;; be acquired at runtime (# or V).  We must use a default
           ;; value of it has any.
           (getf (cdr parameter-spec) :default-value))
          ((eq compile-time-value 'V)
           ;; The parameter was given the explicit value V in the
           ;; format control string, meaning we use the next argument
           ;; to acquire the value of the parameter.  We must generate
           ;; code to test that there are more arguments, to consume
           ;; the next one, and to check that the type of the argument
           ;; acquired is correct.
           `(progn (when (>= *next-argument-pointer*
                             (length *arguments*))
                     (error 'no-more-arguments))
                   (let ((argument (aref *arguments*
                                         *next-argument-pointer*)))
                     (incf *next-argument-pointer*)
                     (unless (typep argument ',(getf (cdr parameter-spec) :type))
                       (error 'argument-type-error
                              :expected-type
                              ',(getf (cdr parameter-spec) :type)
                              :datum
                              argument))
                     argument)))
          ((eq compile-time-value '|#|)
           ;; The parameter was given the explicit value # in the
           ;; format control string, meaning we use the number of
           ;; remaining arguments as the value of the parameter.
           `(let ((number-of-remaining-arguments
                   (- (length *arguments*) *next-argument-pointer*)))
              (unless (typep number-of-remaining-arguments
                             ',(getf (cdr parameter-spec) :type))
                (error 'argument-type-error
                       :expected-type
                       ',(getf (cdr parameter-spec) :type)
                       :datum
                       number-of-remaining-arguments))
              number-of-remaining-arguments))
          (t
           ;; The parameter was given an explicit value (number or
           ;; character) in the format control string, and this is the
           ;; value we want.
           compile-time-value))))

(defun compile-directive (directive)
  (let ((parameter-specs (parameter-specs (class-name (class-of directive)))))
    `(let ,(loop for parameter-spec in parameter-specs
                 collect `(,(car parameter-spec)
                            ,(getf (cdr parameter-spec) :default-value)))
       (declare (ignorable ,@(mapcar #'car parameter-specs)))
       (let ,(loop for parameter-spec in parameter-specs
                   and given-parameter in (given-parameters directive)
                   collect `(,(car parameter-spec)
                              ,(compile-parameter-value directive parameter-spec)))
         ;; this is not quite right, I think.
         (declare (ignorable ,@(loop for parameter-spec in parameter-specs
                                     and given-parameter in (given-parameters directive)
                                     collect (car parameter-spec))))
         ,(compile-format-directive directive)))))

(defun compile-item (item)
  (if (stringp item)
      `(write-string ,item *destination*)
      (compile-directive item)))

(defun compile-items (items)
  (map 'list #'compile-item items))

(defun compile-control-string (control-string)
  (let ((items (structure-items (split-control-string control-string) nil)))
    `(progn ,@(loop for item across items
                    collect (compile-item item)))))
