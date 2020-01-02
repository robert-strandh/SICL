(cl:in-package #:sicl-sequence)

(declaim (inline function-designator-function))
(defun function-designator-function (function-designator)
  (typecase function-designator
    (function function-designator)
    (symbol (coerce function-designator 'function))
    (t (error 'must-be-function-designator
              :datum function-designator))))

(defmacro with-predicate ((name predicate) &body body)
  (let ((f (gensym)))
    `(let ((,f (function-designator-function ,predicate)))
       (flet ((,name (x) (funcall ,f x)))
         ,@body))))

(defmacro with-key-function ((name key) &body body)
  (let ((f (gensym)))
    (sicl-utilities:once-only (key)
      `(if (null ,key)
           (flet ((,name (x) x))
             ,@body)
          (let ((,f (function-designator-function ,key)))
            (flet ((,name (x) (funcall ,f x)))
              ,@body))))))

(declaim (inline canonicalize-test-and-test-not))
(defun canonicalize-test-and-test-not (test test-not)
  (if (null test)
      (if (null test-not)
          (values #'eql nil)
          (values (function-designator-function test-not) t))
      (if (null test-not)
          (values (function-designator-function test) nil)
          (error 'both-test-and-test-not-given
                    :test test
                    :test-not test-not))))

(defmacro with-test-function ((name test test-not) &body body)
  (let ((f (gensym))
        (complementp (gensym)))
    (sicl-utilities:once-only (test test-not)
      `(multiple-value-bind (,f ,complementp)
           (canonicalize-test-and-test-not ,test ,test-not)
         (if ,complementp
             (flet ((,name (a b) (not (funcall ,f a b))))
               ,@body)
             (flet ((,name (a b) (funcall ,f a b)))
               ,@body))))))

(defun class-subclasses (class)
  (list* class
         (mapcan #'class-subclasses
                 (closer-mop:class-direct-subclasses class))))

(defmacro replicate-for-each-relevant-vectoroid (symbol &body body)
  `(progn
     ,@(loop for class in (list* (find-class 'sequence)
                                 (class-subclasses (find-class 'vector)))
             unless (subtypep (class-name class) '(array nil))
               append (subst class symbol body))))
