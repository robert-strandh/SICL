(cl:in-package #:sicl-sequence)

(defmacro with-key-function ((name key) &body body)
  (let ((e (gensym))
        (f (gensym)))
    (once-only (key)
      `(if (null ,key)
          (flet ((,name (,e) ,e)) ,@body)
          (let ((,f (if (functionp ,key) ,key (fdefinition ,key))))
            (flet ((,name (,e) (funcall ,f ,e)))
              ,@body))))))

(defun canonicalize-test-and-test-not (test test-not)
  (cond ((and (not (null test))
              (not (null test-not)))
         (error 'both-test-and-test-not-given
                :test test
                :test-not test-not))
        ((null test)
         (cond ((null test-not)
                (values #'eql nil))
               ((functionp test-not)
                (values test-not t))
               (t
                (values (fdefinition test-not) t))))
        ((functionp test)
         (values test nil))
        (t
         (values (fdefinition test) nil))))

(defmacro with-test-function ((name test test-not) &body body)
  (let ((e1 (gensym))
        (e2 (gensym))
        (f (gensym))
        (complementp (gensym)))
    (once-only (test test-not)
      `(multiple-value-bind (,f ,complementp)
           (canonicalize-test-and-test-not ,test ,test-not)
         (when ,complementp (setf ,f (complement ,f)))
         (flet ((,name (,e1 ,e2)
                  (funcall ,f ,e1 ,e2)))
           ,@body)))))

(defun class-subclasses (class)
  (list* class
         (mapcan #'class-subclasses
                 (closer-mop:class-direct-subclasses class))))

(defmacro replicate-for-each-relevant-vector-subclass (symbol &body body)
  `(progn
     ,@(loop for class in (class-subclasses (find-class 'vector))
             unless (subtypep (class-name class) '(array nil))
               collect (subst class symbol `(progn ,@body)))))
