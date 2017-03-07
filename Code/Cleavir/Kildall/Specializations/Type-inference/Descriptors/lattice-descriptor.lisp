(cl:in-package #:cleavir-type-descriptors)

;;; not even remotely accurate but we only need it to distinguish
;;; from values and function descriptors.
(deftype ltype () 'unsigned-byte)

(declaim (inline ltype-size))
(defun ltype-size (environment)
  (+ 5 ; null, symbol, cons, function, base-char
     (if (cleavir-env:has-extended-char-p environment) 1 0)
     3 ; fixnum, bignum, ratio
     (length (cleavir-env:float-types environment))
     (length (cleavir-env:upgraded-complex-part-types environment))
     (* 2 (length (cleavir-env:upgraded-array-element-types environment)))
     1)) ; other

(declaim (inline make-ltype))
(defun make-ltype (environment)
  (declare (ignore environment))
  #+(or)
  (make-array (ltype-size environment) :element-type 'bit)
  0)

(declaim (inline binary-join binary-meet subltypep))
(defun ltype-join (l1 l2) (logior l1 l2))
(defun ltype-meet (l1 l2) (logand l1 l2))
(defun subltypep (l1 l2) (zerop (logandc2 l1 l2)))

(declaim (inline top-ltype bottom-ltype))
(defun ltype-top (environment)
  (1- (ash 1 (ltype-size environment))))
(defun ltype-bottom (environment)
  (declare (ignore environment))
  0)

(declaim (inline ltype-top-p ltype-bottom-p))
(defun ltype-top-p (ltype environment)
  ;; just compare to T.
  (= ltype (ltype-top environment)))
(defun ltype-bottom-p (ltype environment)
  (declare (ignore environment))
  (zerop ltype))

(defun specifier->ltype (specifier environment)
  (let ((index 0)
        (result (make-ltype environment)))
    (labels ((overlap? (s1 s2)
               ;; do type specifiers s1 and s2 have any overlap?
               ;; this treats NIL NIL and NIL T the same, i.e. it
               ;; approximates correctly.
               (not (subtypep `(and ,s1 ,s2) nil environment)))
             (set-bit (spec)
               (setf (ldb (byte 1 index) result)
                     (if (overlap? spec specifier) 1 0))
               (incf index)))
      (declare (inline set-bit overlap?))
      (set-bit 'null)
      (set-bit '(and symbol (not null)))
      (set-bit 'cons)
      (set-bit 'function)
      (set-bit 'base-char)
      (when (cleavir-env:has-extended-char-p environment)
        (set-bit 'extended-char))
      (set-bit 'fixnum)
      (set-bit 'bignum)
      (set-bit 'ratio)
      (loop for spec in (cleavir-env:float-types environment)
            do (set-bit spec))
      (loop for ucpt in (cleavir-env:upgraded-complex-part-types environment)
            do (set-bit `(complex ,ucpt)))
      (loop for uaet in (cleavir-env:upgraded-array-element-types environment)
            do (set-bit `(and (array ,uaet) ; messy
                              (not simple-array)))
               (set-bit `(simple-array ,uaet)))
      (set-bit
       '(not (or symbol cons function character number array))))
    result))

;;; this could be the above with an eql type, but by using typep
;;; we can save a bit of time.
(defun object-ltype (object environment)
  (let ((index 0)
        (result (make-ltype environment)))
    (labels ((set-bit (spec)
               (cond
                 ((typep object spec environment)
                  (setf (ldb (byte 1 index) result) 1)
                  (return-from object-ltype result))
                 (t (incf index)))))
      (declare (inline set-bit))
      (set-bit 'null)
      (set-bit '(and symbol (not null)))
      (set-bit 'cons)
      (set-bit 'function)
      (set-bit 'base-char)
      (when (cleavir-env:has-extended-char-p environment)
        (set-bit 'extended-char))
      (set-bit 'fixnum)
      (set-bit 'bignum)
      (set-bit 'ratio)
      (loop for spec in (cleavir-env:float-types environment)
            do (set-bit spec))
      (loop for ucpt in (cleavir-env:upgraded-complex-part-types
                         environment)
            do (set-bit `(complex ,ucpt)))
      (loop for uaet in (cleavir-env:upgraded-array-element-types
                         environment)
            do (set-bit `(and (array ,uaet) ; messy
                              (not simple-array)))
               (set-bit `(simple-array ,uaet)))
      (set-bit
       '(not (or symbol cons function character number array))))
    (error "BUG: somehow there are other types?")))

(defun ltype->specifier (ltype environment)
  (cond
    ((ltype-top-p ltype environment) t)
    ((ltype-bottom-p ltype environment) nil)
    (t
     ;;; The basic idea here is we collect a list of types to
     ;;; disjoin. To make it human readable, we start with larger
     ;;; types like NUMBER, and if they're not appropriate move in.
     ;;; Except that if we have the (not ...) type, we do the
     ;;; inverse, and collect conjoinees.
     (let (sum
           (reverse (subltypep (specifier->ltype
                                '(not (or symbol cons function
                                       character number array))
                                environment)
                               ltype)))
       (flet ((check (spec)
                (if reverse
                    (if (subltypep
                         ltype
                         (specifier->ltype `(not ,spec)
                                           environment))
                        (push `(not ,spec) sum)
                        nil)
                    (if (subltypep
                         (specifier->ltype spec environment) ltype)
                        (push spec sum)
                        nil))))
         (if (check 'list)
             ;; list is in, so null is, so there's no possibility
             ;; of having to display just (and symbol (not null)).
             (check 'symbol)
             (progn
               (check 'cons)
               (unless (check 'symbol)
                 (check 'null)
                 (check '(and symbol (not null))))))
         (check 'function)
         (unless (check 'character)
           (check 'base-char)
           (when (cleavir-env:has-extended-char-p environment)
             (check 'extended-char)))
         (unless (check 'number)
           (unless (check 'complex)
             (loop for ucpt
                     in (cleavir-env:upgraded-complex-part-types
                         environment)
                   do (check `(complex ,ucpt))))
           (unless (check 'float)
             (loop for spec
                     in (cleavir-env:float-types environment)
                   do (check spec)))
           (unless (check 'rational)
             (check 'ratio)
             (unless (check 'integer)
               (check 'fixnum)
               (check 'bignum))))
         (unless (check 'array)
           (unless (check 'simple-array)
             (loop for uaet
                     in (cleavir-env:upgraded-array-element-types
                         environment)
                   do (check `(and (array ,uaet) ; still messy
                                   (not simple-array)))
                      (check `(simple-array ,uaet)))))
         (if (null (rest sum)) ; only one of the above
             (first sum)
             (if reverse
                 `(and ,@sum)
                 `(or ,@sum))))))))
