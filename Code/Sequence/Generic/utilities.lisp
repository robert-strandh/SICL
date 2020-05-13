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
  (cond ((null count) (- array-total-size-limit 2))
        ((not (integerp count))
         (error 'type-error
                :datum count
                :expected-type '(or null integer)))
        ((minusp count) 0)
        (t (min count (- array-total-size-limit 2)))))

(declaim (ftype (function (sequence t t)
                          (values array-index array-length array-length &optional))
                canonicalize-start-and-end))
(declaim (inline canonicalize-start-and-end))
(defun canonicalize-start-and-end (sequence start end)
  (declare (sequence sequence))
  (let* ((length (length sequence))
         (start
           (typecase start
             (unsigned-byte start)
             (otherwise
              (error 'invalid-start-index
                     :expected-type 'unsigned-byte
                     :datum start
                     :in-sequence sequence))))
         (end (typecase end
                (null length)
                (integer
                 (unless (<= end length)
                   (error 'invalid-end-index
                          :datum end
                          :expected-type `(integer 0 ,length)
                          :in-sequence sequence))
                 end)
                (otherwise
                 (error 'invalid-end-index
                        :expected-type '(or null integer)
                        :datum end
                        :in-sequence sequence)))))
    (unless (<= start end)
      (error 'end-less-than-start
             :datum start
             :expected-type `(integer 0 ,end)
             :end-index end
             :in-sequence sequence))
    (values start end length)))

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

(defmacro replicate-for-each-vector-class (symbol &body body)
  `(progn
     ,@(loop for class in *vector-classes*
             unless (subtypep class '(array nil))
               append (subst class symbol body))))

;;; A vector class is compatible with another vector class, if elements of
;;; the former can be stored in the latter, i.e., when the intersection of
;;; both element types is non-empty.
(defmacro replicate-for-all-compatible-vector-classes (symbol-1 symbol-2 &body body)
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

(defun enumerate-symbol (symbol n)
  (intern
   (concatenate
    'string
    (symbol-name symbol)
    "-"
    (format nil "~D" n))
   (symbol-package symbol)))

(defvar *vector-prototype-table* (make-hash-table :test #'equal))

(defun vector-prototype (element-type)
  (if (eql element-type '*)
      (load-time-value (vector))
      (let ((uaet (upgraded-array-element-type element-type)))
        (multiple-value-bind (prototype present-p)
            (gethash uaet *vector-prototype-table*)
          (if present-p
              prototype
              (setf (gethash uaet *vector-prototype-table*)
                    (make-array 0 :element-type uaet)))))))

(define-compiler-macro vector-prototype (&whole form element-type)
  (if (constantp element-type)
      `',(vector-prototype (eval element-type))
      form))
