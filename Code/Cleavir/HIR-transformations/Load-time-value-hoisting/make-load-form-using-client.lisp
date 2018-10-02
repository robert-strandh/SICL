(cl:in-package #:cleavir-load-time-value-hoisting)

(defmethod make-load-form-using-client ((bignum integer) system)
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

(defmethod make-load-form-using-client ((ratio ratio) system)
  `(/ ',(numerator ratio)
      ',(denominator ratio)))

(defmethod make-load-form-using-client ((complex complex) system)
  `(complex ',(realpart complex)
            ',(imagpart complex)))

(defmethod make-load-form-using-client ((cons cons) system)
  (values
   `(cons nil nil)
   `(setf (car ,cons) ',(car cons)
          (cdr ,cons) ',(cdr cons))))

(defmethod make-load-form-using-client ((symbol symbol) system)
  ;; We have a problem here - the CAR of the creation form is itself a
  ;; symbol, leading to a circular creation form.  Clients must provide a
  ;; custom method for symbols to avoid this case.
  `(intern ',(symbol-name symbol)
           ',(symbol-package symbol)))

(defmethod make-load-form-using-client ((package package) system)
  `(or (find-package ',(package-name package))
       (error "There is no package named ~A."
              ,(package-name package))))

(defmethod make-load-form-using-client ((array array) system)
  (values
   `(make-array ',(array-dimensions array)
                :element-type ',(array-element-type array))
   `(progn ,@(loop for index below (array-total-size array)
                   collect
                   `(setf (row-major-aref ,array ,index)
                          ',(row-major-aref array index))))))

(defmethod make-load-form-using-client ((hash-table hash-table) system)
  (values
   `(make-hash-table :test ',(hash-table-test hash-table)
                     :size ',(hash-table-size hash-table))
   `(progn ,@(maphash
              (lambda (key value)
                `(setf (gethash ,key ,hash-table) ',value))
              hash-table))))

(defmethod make-load-form-using-client ((random-state random-state) system)
  ;; CLHS 22.1.3.10 explicitly permits the following serialization.
  `(with-standard-io-syntax
     (values
      (read-from-string
       ,(with-standard-io-syntax
          (prin1-to-string random-state))))))
