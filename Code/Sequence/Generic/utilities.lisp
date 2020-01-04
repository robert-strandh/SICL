(cl:in-package #:sicl-sequence)

(declaim (inline function-designator-function))
(defun function-designator-function (function-designator)
  (typecase function-designator
    (function function-designator)
    (symbol (coerce function-designator 'function))
    (t (error 'must-be-function-designator
              :datum function-designator))))

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

(declaim (inline canonicalize-count))
(defun canonicalize-count (count)
  (cond ((null count)
         most-positive-fixnum)
        ((not (integerp count))
         (error 'type-error
                :datum count
                :expected-type '(or null integer)))
        ((not (plusp count))
         0)
        (t
         (min count most-positive-fixnum))))

(declaim (inline canonicalize-start-and-end))
(defun canonicalize-start-and-end (sequence length start end)
  (declare (type (integer 0 (#.array-total-size-limit)) length))
  (let ((start (typecase start
                 (null 0)
                 ((integer 0) start)
                 (otherwise (error 'invalid-start-index-type
                                   :expected-type '(or null (integer 0))
                                   :datum start
                                   :sequence sequence))))
        (end (typecase end
               (null length)
               (integer end)
               (otherwise (error 'invalid-end-index-type
                                 :expected-type '(or null integer)
                                 :datum end
                                 :sequence sequence)))))
    (unless (<= end length)
      (error 'invalid-end-index
             :datum end
             :expected-type `(integer 0 ,length)
             :in-sequence sequence))
    (unless (<= start end)
      (error 'end-less-than-start
             :datum start
             :expected-type `(integer 0 ,end)
             :end-index end
             :in-sequence sequence))
    (values start end)))

;;; Skip a prefix of a list and signal an error if the list is too short,
;;; or if it is not a proper list.  Also check that start is a nonnegative
;;; integer.
(defun skip-to-start (list start)
  (declare (list list) (integer start))
  (unless (plusp start)
    (error 'invalid-start-index
           :datum start
           :expected-type unsigned-byte
           :in-sequence list))
  (do ((countdown start (1- countdown))
       (remaining list (cdr remaining)))
      ((or (zerop countdown)
           (atom remaining))
       (when (and (atom remaining)
                  (not (null remaining)))
         (error 'must-be-proper-list
                :datum list))
       (when (plusp countdown)
         (error 'invalid-start-index
                :datum start
                :expected-type `(integer 0 ,(- start countdown))
                :in-sequence list)))))

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

(declaim (inline shrink-vector))
(defun shrink-vector (vector new-length)
  (declare (vector vector))
  (cond ((= (length vector) new-length)
         vector)
        ((array-has-fill-pointer-p vector)
         (setf (fill-pointer vector) new-length))
        (t
         (subseq vector 0 new-length))))
