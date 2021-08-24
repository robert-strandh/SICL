(cl:in-package #:sicl-arithmetic)

(defmethod binary-divide ((x integer) (y fixnum))
  (cond ((zerop y)
         (error 'division-by-zero
                :operation '/
                :operands (list x y)))
        ((= y 1) x)
        ((= y -1) (- x))
        (t
         (multiple-value-bind (num den)
             (let* ((g (gcd x y))
                    (gx (truncate x g)) (gy (truncate y g)))
               (if (minusp gy)
                   (values (- gx) (- gy))
                   (values gx gy)))
           (if (= den 1)
               num
               (make-instance 'ratio :numerator num :denominator den))))))

(defmethod binary-divide ((x integer) (y bignum))
  ;; as a bignum, y cannot be 0, 1, or -1, so we just do the hard part.
  ;; Also, the denominator will definitely not be 1.
  (multiple-value-bind (num den)
      (let* ((g (gcd x y))
             (gx (truncate x g)) (gy (truncate y g)))
        (if (minusp gy)
            (values (- gx) (- gy))
            (values gx gy)))
    (make-instance 'ratio :numerator num :denominator den)))

(defmethod binary-divide ((dividend ratio) (divisor integer))
  ;; This is similar to the BINARY-MULTIPLY method, but the result is always a
  ;; ratio, and we need to check signs and for division by zero.
  (if (zerop divisor)
      (error 'division-by-zero
             :operation '/
             :operands (list dividend divisor))
      (let* ((num (numerator dividend)) (den (denominator dividend))
             (g (gcd num divisor))
             (nnum (truncate num g)) (ndivisor (truncate divisor g))
             (nden (* ndivisor den)))
        (if (minusp divisor) ; flip signs
            (make-instance 'ratio :numerator (- nnum) :denominator (- nden))
            (make-instance 'ratio :numerator nnum :denominator nden)))))

(defmethod binary-divide ((dividend integer) (divisor ratio))
  ;; This is similar to the BINARY-MULTIPLY method. The difference is that
  ;; since we are dividing the dividend by the numerator, and the numerator has
  ;; a sign, we need to make sure we sign the result correctly.
  (let* ((num (numerator divisor)) (den (denominator divisor))
         (g (gcd dividend num))
         (ndividend (truncate dividend g)) (nden (truncate num g))
         (nnum (* ndividend den)))
    (cond ((= nden 1) nnum)
          ((= nden -1) (- nnum))
          ((minusp nden)
           (make-instance 'ratio :numerator (- nnum) :denominator (- nden)))
          (t
           (make-instance 'ratio :numerator nnum :denominator nden)))))

;;; This is nearly identical to the BINARY-MULTIPLY method.
(defmethod binary-divide ((dividend ratio) (divisor ratio))
  (let* ((num1 (numerator dividend)) (den1 (denominator dividend))
         (num2 (numerator divisor)) (den2 (denominator divisor))
         (gcd1 (gcd num1 num2)) (gcd2 (gcd den2 den1))
         (num1 (truncate num1 gcd1)) (den1 (truncate den1 gcd2))
         (den2 (truncate den2 gcd2)) (num2 (truncate num2 gcd1))
         (num (* num1 den2)) (den (* den1 num2)))
    (if (= den 1)
        num
        (make-instance 'ratio :numerator num :denominator den))))
