(cl:in-package #:sicl-sequence)

;;; This function is called by MAP, MERGE, CONCATENATE, and MAKE-SEQUENCE
;;; to handle their respective first argument.  It returns two values:
;;;
;;; 1. A sequence prototype, i.e., a sequence that is almost of the
;;;    supplied type, except that its may have a different length.
;;;
;;; 2. NIL, T, or an integer.  A value of NIL means the type specifier does
;;;    not contain any length information.  A value of T means that the
;;;    only way to ensure that an object is of the specified type is by
;;;    calling TYPEP.  An integer value means that the type specifier
;;;    prescribes exactly that value as the sequence's length.

(defun reify-sequence-type-specifier (type-specifier &optional environment)
  (labels ((fail ()
             (error 'must-be-sequence-type-specifier
                    :datum type-specifier))
           (try-slow-path ()
             (reify-sequence-type-specifier/slow type-specifier environment))
           (parse-size (size)
             (typecase size
               ((eql *) nil)
               (array-length size)
               (otherwise (fail)))))
    (typecase type-specifier
      (symbol
       (case type-specifier
         ((nil) (fail))
         ((null)
          (values '() 0))
         ((list cons)
          '(()))
         ((vector simple-vector)
          (vector-prototype '*))
         ((bit-vector simple-bit-vector)
          (vector-prototype 'bit))
         ((string simple-string)
          (vector-prototype 'character))
         ((base-string simple-base-string)
          (vector-prototype 'base-char))
         ((sequence) (fail)) ; Sequence is an abstract class.
         (otherwise
          (let ((class (find-class type-specifier nil environment)))
            (if (not class)
                (try-slow-path)
                (class-prototype class))))))
      (cons
       (case (car type-specifier)
         ;; Cons Type Specifiers.
         ((cons)
          (labels ((reify-cons-type-specifier (type-specifier length)
                     (cond ((eql type-specifier '*)
                            (values '(()) nil))
                           ((eql type-specifier 'null)
                            (values '(()) length))
                           ((and (consp type-specifier)
                                 (eql (car type-specifier) 'cons))
                            (unless (<= (length type-specifier) 3)
                              (fail))
                            (destructuring-bind (&optional (car '*) (cdr '*))
                                (rest type-specifier)
                              (declare (ignore car))
                              (reify-cons-type-specifier cdr (1+ length))))
                           (t (values '(()) t)))))
            (reify-cons-type-specifier type-specifier 0)))
         ((array simple-array)
          (unless (<= (length type-specifier) 3)
            (fail))
          (destructuring-bind (&optional (element-type '*) (dimension-spec '*))
              (rest type-specifier)
            (unless (and (consp dimension-spec)
                         (null (cdr dimension-spec)))
              (fail))
            (values
             (vector-prototype element-type)
             (parse-size (car dimension-spec)))))
         ((vector)
          (unless (<= (length type-specifier) 3)
            (fail))
          (destructuring-bind (&optional (element-type '*) (size '*))
              (rest type-specifier)
            (values
             (vector-prototype element-type)
             (parse-size size))))
         ((simple-vector)
          (unless (<= (length type-specifier) 2)
            (fail))
          (destructuring-bind (&optional (size '*))
              (rest type-specifier)
            (values
             (vector-prototype '*)
             (parse-size size))))
         ((bit-vector simple-bit-vector)
          (unless (<= (length type-specifier) 2)
            (fail))
          (destructuring-bind (&optional (size '*))
              (rest type-specifier)
            (values
             (vector-prototype 'bit)
             (parse-size size))))
         ((string simple-string)
          (unless (<= (length type-specifier) 2)
            (fail))
          (destructuring-bind (&optional (size '*))
              (rest type-specifier)
            (values
             (vector-prototype 'character)
             (parse-size size))))
         ((base-string simple-base-string)
          (unless (<= (length type-specifier) 2)
            (fail))
          (destructuring-bind (&optional (size '*))
              (rest type-specifier)
            (values
             (vector-prototype 'base-char)
             (parse-size size))))
         (otherwise (try-slow-path))))
      (otherwise (try-slow-path)))))

(defun reify-sequence-type-specifier/slow (type-specifier environment)
  (if (not (subtypep type-specifier 'sequence environment))
      (error 'must-be-sequence-type-specifier
             :datum type-specifier)
      (labels ((find-most-specific-prototype (class)
                 (loop for subclass in (class-direct-subclasses class) do
                   (when (subtypep type-specifier subclass environment)
                     (return (find-most-specific-prototype subclass)))
                       finally (return (class-prototype class)))))
        (values (find-most-specific-prototype (find-class 'sequence)) t))))

(defmacro with-reified-result-type ((prototype result-type) &body body)
  (check-type prototype symbol)
  (sicl-utilities:once-only (result-type)
    (sicl-utilities:with-gensyms (result p l)
      `(multiple-value-bind (,p ,l) (reify-sequence-type-specifier ,result-type)
         (let ((,result (let ((,prototype ,p)) ,@body)))
           (unless (or (not ,l)
                       (and (integerp ,l) (= (length ,result) ,l))
                       (typep ,result ,result-type))
             (error 'must-be-result-type
                    :datum ,result
                    :expected-type ,result-type))
           ,result)))))
