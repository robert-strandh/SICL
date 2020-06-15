(cl:in-package #:sicl-sequence)

;;; This function is called by MAP, MERGE, CONCATENATE, and MAKE-SEQUENCE
;;; to handle their respective first argument.  It returns two values:
;;;
;;; 1. A sequence prototype, i.e., a sequence that is almost of the
;;;    supplied type, except that its may have a different length.
;;;
;;; 2. One of the following objects:
;;;
;;;    - An integer, meaning that the type specifier prescribes exactly
;;;      that value as the sequence's length.
;;;
;;;    - The symbol NIL, meaning that the type specifier doesn't prescribe
;;;      a particular sequence length.
;;;
;;;    - The keyword :COMPLICATED, meaning that the type specifier is too
;;;      complicated to be represented by just a prototype and some length
;;;      information.

(defun reify-sequence-type-specifier (type-specifier &optional environment)
  (labels ((fail ()
             (error 'must-be-recognizable-subtype-of-sequence
                    :datum type-specifier))
           (try-slow-path ()
             (reify-sequence-type-specifier/slow type-specifier environment))
           (parse-size (size)
             (typecase size
               ((eql *) nil)
               (vector-length size)
               (otherwise (fail)))))
    (typecase type-specifier
      (symbol
       (case type-specifier
         ((nil) (fail))
         ((null)
          (values '() 0))
         ((list)
          '(()))
         ((cons)
          (values '(()) :complicated))
         ((vector simple-vector)
          (vector-prototype '*))
         ((bit-vector simple-bit-vector)
          (vector-prototype 'bit))
         ((string simple-string)
          (vector-prototype 'character))
         ((base-string simple-base-string)
          (vector-prototype 'base-char))
         ((sequence) (fail))            ; Sequence is an abstract class.
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
                     (destructuring-bind (&optional (car '*) (cdr '*))
                         (rest type-specifier)
                       (cond ((not (member car '(t *)))
                              (values '(()) :complicated))
                             ((eql cdr 'null)
                              (values '(()) length))
                             ((and (consp cdr)
                                   (eql (first cdr) 'cons))
                              (reify-cons-type-specifier cdr (1+ length)))
                             (t
                              (values '(()) :complicated))))))
            (reify-cons-type-specifier type-specifier 1)))
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
  (cond
    ((subtypep type-specifier 'list environment)
     (if (subtypep type-specifier 'null environment)
         (values nil 0)
         (values '(()) :complicated)))
    ((subtypep type-specifier 'vector environment)
     (if (classp type-specifier)
         (values (class-prototype type-specifier) nil)
         (let ((candidates '()))
           (loop for vector-class in (list* 'simple-vector *specialized-vector-classes*)
                 unless (subtypep `(and ,vector-class ,type-specifier) nil)
                   do (pushnew vector-class candidates
                               :key #'vector-class-element-type
                               :test #'type=))
           (unless (= 1 (length candidates))
             (error 'must-be-recognizable-subtype-of-vector
                    :datum type-specifier))
           (values
            (vector-prototype (vector-class-element-type (first candidates)))
            :complicated))))
    ((and (symbolp type-specifier)
          (find-class type-specifier nil environment))
     (class-prototype (find-class type-specifier t environment)))
    (t
     (error 'must-be-recognizable-subtype-of-sequence
            :datum type-specifier))))

(defmacro check-result-type (result type)
  (sicl-utilities:once-only (result)
    `(unless (typep ,result ',type)
       (result-type-error ,result ',type))))

(defun result-type-error (result type)
  (error 'must-be-result-type
         :datum result
         :expected-type type))

(defmacro with-reified-result-type ((prototype result-type) &body body)
  (check-type prototype symbol)
  (sicl-utilities:once-only (result-type)
    (sicl-utilities:with-gensyms (result p l)
      `(multiple-value-bind (,p ,l) (reify-sequence-type-specifier ,result-type)
         (let ((,result (let ((,prototype ,p)) ,@body)))
           (unless (or (not ,l)
                       (and (integerp ,l)
                            (= (length ,result) ,l))
                       (typep ,result ,result-type))
             (result-type-error ,result ,result-type))
           ,result)))))
