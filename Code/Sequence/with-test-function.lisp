(cl:in-package #:sicl-sequence)

(defmacro with-test-function ((name test test-not) &body body)
  (sicl-utilities:with-gensyms (f complementp)
    (sicl-utilities:once-only (test test-not)
      `(multiple-value-bind (,f ,complementp)
           (canonicalize-test-and-test-not ,test ,test-not)
         (declare (function ,f))
         (if ,complementp
             (flet ((,name (a b) (not (funcall ,f a b))))
               (declare (dynamic-extent #',name))
               ,@body)
             (flet ((,name (a b) (funcall ,f a b)))
               (declare (inline ,name))
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
