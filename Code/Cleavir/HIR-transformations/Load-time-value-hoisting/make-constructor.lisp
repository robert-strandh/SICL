(cl:in-package #:cleavir-load-time-value-hoisting)

;;; Automatically compile any given creation form or initialization form.
(defmethod make-constructor :around (object system)
  (with-accessors ((creation-form creation-form)
                   (creation-thunk creation-thunk)
                   (initialization-form initialization-form)
                   (initialization-thunk initialization-thunk))
      (call-next-method)
    (when (and creation-form (not creation-thunk))
      (setf creation-thunk
            (compile-form creation-form system)))
    (when (and initialization-form (not initialization-thunk))
      (setf initialization-thunk
            (compile-form initialization-form system)))))

(defmethod make-constructor ((bignum integer) system)
  (let* ((size (load-time-value
                (loop for size = 1 then (* size 2)
                      while (and (typep (+ (expt 2 size)) 'fixnum)
                                 (typep (- (expt 2 size)) 'fixnum))
                      maximize size)))
         (sign (signum bignum))
         (blocks (ceiling (integer-length bignum) size)))
    (loop for block from blocks downto 0
          for offset = (* block size)
          for form = sign then
          `(dpb ,(ldb (byte size offset) bignum)
                (byte ,size ,offset) ,form)
          finally (return form))))

(defmethod make-constructor ((ratio ratio) system)
  (make-instance 'constructor
    :creation-form
    `(/ ',(numerator ratio)
        ',(denominator ratio))))

(defmethod make-constructor ((complex complex) system)
  (make-instance 'constructor
    :creation-form
    `(complex ',(realpart complex)
              ',(imagpart complex))))

(defmethod make-constructor ((cons cons) system)
  (make-instance 'constructor
    :creation-form `(cons nil nil)
    :initialization-form
    `(setf (car ,cons) ',(car cons)
           (cdr ,cons) ',(cdr cons))))

(defmethod make-constructor ((symbol symbol) system)
  (make-instance 'constructor
    ;; We have a problem here - the CAR of the creation form is itself a
    ;; symbol, leading to a circular creation form.  Clients must provide a
    ;; custom method for symbols to avoid this case.
    :creation-form
    `(intern ',(symbol-name symbol)
             ',(symbol-package symbol))))

(defmethod make-constructor ((package package) system)
  (make-instance 'constructor
    :creation-form
    `(or (find-package ',(package-name package))
         (error "There is no package named ~A."
                ,(package-name package)))))

(defmethod make-constructor ((array array) system)
  (make-instance 'constructor
    :creation-form
    `(make-array
      ',(array-dimensions array)
      :element-type ',(array-element-type array))
    :initialization-form
    `(progn ,@(loop for index below (array-total-size array)
                    collect
                    `(setf (row-major-aref ,array ,index)
                           ',(row-major-aref array index))))))

(defmethod make-constructor ((hash-table hash-table) system)
  (make-instance 'constructor
    :creation-form
    `(make-hash-table :test ',(hash-table-test hash-table)
                      :size ',(hash-table-size hash-table))
    :initialization-form
    `(progn ,@(maphash
               (lambda (key value)
                 `(setf (gethash ,key ,hash-table) ',value))
               hash-table))))
