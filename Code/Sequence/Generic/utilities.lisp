(cl:in-package #:sicl-sequence)

(deftype array-index ()
  '(integer 0 (#.(1- array-total-size-limit))))

(deftype array-length ()
  '(integer 0 (#.array-total-size-limit)))

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
(declaim (ftype (function (sequence array-length t t)
                          (values array-index array-index &optional))
                canonicalize-start-and-end))
(defun canonicalize-start-and-end (sequence length start end)
  (declare (sequence sequence) (array-length length))
  (let ((start (typecase start
                 (unsigned-byte start)
                 (otherwise (error 'invalid-start-index-type
                                   :expected-type '(or null unsigned-byte)
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

;;; Returns the first cons of the interval.  Perform all relevant type
;;; check on the supplied list and bounding index designators, except that
;;; no check is made whether the end is too large.
(declaim (ftype (function (list t t)
                          (values t unsigned-byte (or null unsigned-byte) &optional))
                canonicalize-list-interval))
(defun canonicalize-list-interval (list start end)
  (declare (list list))
  ;; Check START.
  (unless (typep start 'unsigned-byte)
    (error 'invalid-start-index
           :datum start
           :expected-type 'unsigned-byte
           :in-sequence list))
  ;; Check END.
  (typecase end
    (null)
    (integer
     (unless (<= start end)
       (error 'end-less-than-start
              :datum end
              :start-index start
              :in-sequence list
              :expected-type `(integer ,start))))
    (otherwise
     (error 'invalid-end-index
            :datum end
            :in-sequence list
            :expected-type '(or null unsigned-byte))))
  ;; Cautiously skip the first START conses.
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
                :in-sequence list))
       (values remaining start end))))

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

(defun sequence-class-element-type (sequence-class)
  (if (subtypep sequence-class '(not vector))
      't
      (let ((direct-subclasses (closer-mop:class-direct-subclasses sequence-class)))
        (if (null direct-subclasses)
            (array-element-type
             (coerce nil sequence-class))
            `(or ,@(mapcar #'sequence-class-element-type direct-subclasses))))))

(defmacro replicate-for-each-relevant-vectoroid (symbol &body body)
  `(progn
     ,@(loop for class in (list* (find-class 'sequence)
                                 (class-subclasses (find-class 'vector)))
             unless (subtypep class '(array nil))
               append (subst class symbol body))))

;;; A vectoroid is compatible with another vectoroid, if elements of the
;;; former can be stored in the latter, i.e., when the intersection of both
;;; element types is non-empty.
(defmacro replicate-for-all-compatible-vectoroids (symbol-1 symbol-2 &body body)
  (let ((vectoroid-classes (list* (find-class 'sequence) (class-subclasses (find-class 'vector))))
        (forms '()))
    (loop for class-1 in vectoroid-classes do
      (loop for class-2 in vectoroid-classes do
        (let ((element-type-1 (sequence-class-element-type class-1))
              (element-type-2 (sequence-class-element-type class-2)))
          (unless (subtypep element-type-1 nil)
            (unless (subtypep element-type-2 nil)
              (unless (subtypep `(and ,element-type-1 ,element-type-2) nil)
                (push (subst class-2 symbol-2 (subst class-1 symbol-1 `(progn ,@body)))
                      forms)))))))
    `(progn ,@(cl:reverse forms))))

(declaim (inline shrink-vector))
(defun shrink-vector (vector new-length)
  (declare (vector vector))
  (cond ((= (length vector) new-length)
         vector)
        ((array-has-fill-pointer-p vector)
         (setf (fill-pointer vector) new-length))
        (t
         (subseq vector 0 new-length))))
