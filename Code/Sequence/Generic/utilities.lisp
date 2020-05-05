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

(declaim (ftype (function (sequence array-length t t)
                          (values array-index array-length &optional))
                canonicalize-start-and-end))
(declaim (inline canonicalize-start-and-end))
(defun canonicalize-start-and-end (sequence length start end)
  (declare (sequence sequence) (array-length length))
  (let ((start (typecase start
                 (unsigned-byte start)
                 (otherwise (error 'invalid-start-index-type
                                   :expected-type 'unsigned-byte
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

(defmacro with-predicate ((name predicate) &body body)
  (sicl-utilities:with-gensyms (f)
    `(let ((,f (function-designator-function ,predicate)))
       (flet ((,name (x) (funcall ,f x)))
         ,@body))))

(defmacro with-key-function ((name key) &body body)
  (sicl-utilities:with-gensyms (f)
    (sicl-utilities:once-only (key)
      `(if (null ,key)
           (flet ((,name (x) x))
             ,@body)
           (let ((,f (function-designator-function ,key)))
             (flet ((,name (x) (funcall ,f x)))
               ,@body))))))

(defmacro with-test-function ((name test test-not) &body body)
  (sicl-utilities:with-gensyms (f complementp)
    (sicl-utilities:once-only (test test-not)
      `(multiple-value-bind (,f ,complementp)
           (canonicalize-test-and-test-not ,test ,test-not)
         (if ,complementp
             (flet ((,name (a b) (not (funcall ,f a b))))
               ,@body)
             (flet ((,name (a b) (funcall ,f a b)))
               ,@body))))))

;; Note: Some macros rely on the fact that CLASS-SUBCLASSES sorts its
;; entries most-specific-first.
(defun class-subclasses (class)
  (let ((table (make-hash-table))
        (subclasses '()))
    (labels
        ((push-subclasses (class)
           (unless (gethash class table)
             (setf (gethash class table) t)
             (push class subclasses)
             (mapc #'push-subclasses (closer-mop:class-direct-subclasses class)))))
      (push-subclasses class)
      subclasses)))

(defparameter *vector-classes* (class-subclasses (find-class 'vector)))

(defmacro replicate-for-each-relevant-vectoroid (symbol &body body)
  `(progn
     ,@(loop for class in *vector-classes*
             unless (subtypep class '(array nil))
               append (subst class symbol body))))

;;; A vectoroid is compatible with another vectoroid, if elements of the
;;; former can be stored in the latter, i.e., when the intersection of both
;;; element types is non-empty.
(defmacro replicate-for-all-compatible-vectoroids (symbol-1 symbol-2 &body body)
  (sicl-utilities:with-collectors ((forms collect-form))
    (loop for class-1 in *vector-classes* do
      (loop for class-2 in *vector-classes* do
        (let ((element-type-1 (sequence-class-element-type class-1))
              (element-type-2 (sequence-class-element-type class-2)))
          (unless (subtypep element-type-1 nil)
            (unless (subtypep element-type-2 nil)
              (unless (subtypep `(and ,element-type-1 ,element-type-2) nil)
                (collect-form
                 (subst class-2 symbol-2 (subst class-1 symbol-1 `(progn ,@body))))))))))
    `(progn ,@(forms))))

(defun sequence-class-element-type (sequence-class)
  (if (subtypep sequence-class '(not vector))
      't
      (let ((direct-subclasses (closer-mop:class-direct-subclasses sequence-class)))
        (if (null direct-subclasses)
            (array-element-type (coerce nil sequence-class))
            `(or ,@(mapcar #'sequence-class-element-type direct-subclasses))))))

;;; This function is used to simplify a constant first argument of MAP or
;;; MAKE-SEQUENCE at compile time.
(defun simplify-sequence-type-specifier (type-specifier)
  (if (subtypep type-specifier '(not sequence))
      type-specifier
      (dolist (class (class-subclasses (find-class 'sequence)) type-specifier)
        (when (and (subtypep type-specifier class)
                   (subtypep class type-specifier))
          (return-from simplify-sequence-type-specifier (class-name class))))))

(declaim (inline shrink-vector))
(defun shrink-vector (vector new-length)
  (declare (vector vector))
  (cond ((= (length vector) new-length)
         vector)
        ((array-has-fill-pointer-p vector)
         (setf (fill-pointer vector) new-length))
        (t
         (subseq vector 0 new-length))))

(declaim (inline nth-cons))
(defun nth-cons (list index)
  (declare (list list)
           (array-index index))
  (labels ((nth-cons-aux (rest counter)
             (declare (array-index counter))
             (if (atom rest)
                 (if (null rest)
                     (error 'invalid-sequence-index
                            :datum index
                            :in-sequence list
                            :expected-type `(integer 0 ,(1- (length list))))
                     (error 'must-be-proper-list
                            :datum list))
                 (if (zerop counter)
                     rest
                     (nth-cons-aux (cdr rest) (1- counter))))))
    (nth-cons-aux list index)))

(declaim (inline skip-to-start))
(defun skip-to-start (list start)
  (declare (array-index start))
  (do ((index 0 (1+ index))
       (rest list (cdr rest)))
      ((or (= index start)
           (atom rest))
       (unless (or (not (atom rest))
                   (null rest))
         (error 'must-be-proper-list
                :datum list))
       (unless (= index start)
         (error 'invalid-start-index
                :datum start
                :expected-type `(integer 0 ,(1- index))
                :in-sequence list))
       rest)
    (declare (fixnum index))))

(defun enumerate-symbol (symbol n)
  (intern
   (concatenate
    'string
    (symbol-name symbol)
    "-"
    (format nil "~D" n))
   (symbol-package symbol)))
