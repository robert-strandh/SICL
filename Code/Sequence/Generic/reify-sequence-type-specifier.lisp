(cl:in-package #:sicl-sequence)

;;; This function is called by MAP, CONCATENATE, and MAKE-SEQUENCE to
;;; handle their respective first argument.  It returns two values:
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
  (flet ((fail ()
           (error 'must-be-sequence-type-specifier
                  :datum type-specifier))
         (try-slow-path ()
           (reify-sequence-type-specifier/slow type-specifier environment)))
    (typecase type-specifier
      (symbol
       (case type-specifier
         ((null)
          (values '() 0))
         ((list cons)
          '(()))
         ((vector simple-vector)
          (load-time-value
           (vector)))
         ((bit-vector simple-bit-vector)
          (load-time-value
           (make-array 0 :element-type 'bit)))
         ((string simple-string)
          (load-time-value
           (make-array 0 :element-type 'character)))
         ((base-string simple-base-string)
          (load-time-value
           (make-array 0 :element-type 'base-char)))
         ((sequence)
          (fail))
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
         ;; Array Type Specifiers.
         ((array simple-array)
          (unless (<= (length type-specifier) 3)
            (fail))
          (destructuring-bind (&optional (element-type '*) (dimension-spec '*))
              (rest type-specifier)
            (unless (and (consp dimension-spec)
                         (null (cdr dimension-spec)))
              (fail))
            (let ((prototype (vector-prototype element-type)))
              (typecase (car dimension-spec)
                ((eql *)
                 (values prototype nil))
                (array-length
                 (values prototype (car dimension-spec)))
                (otherwise (fail))))))
         ;; Vector Type Specifiers.
         ((vector)
          (unless (<= (length type-specifier) 3)
            (fail))
          (destructuring-bind (&optional (element-type '*) (size '*))
              (rest type-specifier)
            (let ((prototype (vector-prototype element-type)))
              (typecase size
                ((eql *)
                 (values prototype nil))
                (array-length
                 (values prototype size))
                (otherwise (fail))))))
         ;; Simple Vector Type Specifiers.
         ((simple-vector)
          (unless (<= (length type-specifier) 2)
            (fail))
          (destructuring-bind (&optional (size '*))
              (rest type-specifier)
            (let ((prototype (vector-prototype '*)))
              (typecase size
                ((eql *)
                 (values prototype nil))
                (array-length
                 (values prototype size))
                (otherwise (fail))))))
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
