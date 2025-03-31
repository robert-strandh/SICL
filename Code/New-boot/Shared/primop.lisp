(cl:in-package #:sicl-new-boot)

(defgeneric primop (operation &rest arguments))

;;; Add two fixnums.  If the result is a fixnum, then return it.  If
;;; the result is not a fixnum, then it is either positive or
;;; negative.  If it is positive, substract 2^63 from it so that it
;;; becomes a negative fixnum, thereby simulating overflow behavior in
;;; most architectures.  If it is negative, add 2^63 to it so that it
;;; becomes a positive fixnum, for the same reason.
(defmethod primop ((operation (eql :fixnum-add)) &rest arguments)
  (destructuring-bind (x y) arguments
    (let ((sum (+ x y)))
      (cond ((typep sum 'fixnum) sum)
            ((minusp sum) (+ sum (ash 1 63)))
            (t (- sum (ash 1 63)))))))

(defmethod primop ((operation (eql :fixnum-subtract)) &rest arguments)
  (destructuring-bind (x y) arguments
    (let ((difference (- x y)))
      (cond ((typep difference 'fixnum) difference)
            ((minusp difference) (+ difference (ash 1 63)))
            (t (- difference (ash 1 63)))))))

;;; Multiply two positive fixnums.  Return the result as two values.
(defmethod primop ((operation (eql :fixnum-multiply)) &rest arguments)
  (destructuring-bind (x y) arguments
    (let ((product (* x y)))
      (values (ldb (byte 31 31) product)
              (ldb (byte 31 0) product)))))

(defmethod primop ((operation (eql :fixnum-equal)) &rest arguments)
  (destructuring-bind (x y) arguments
    (= x y)))

(defmethod primop ((operation (eql :fixnum-less)) &rest arguments)
  (destructuring-bind (x y) arguments
    (< x y)))

(defmethod primop ((operation (eql :fixnum-not-greater)) &rest arguments)
  (destructuring-bind (x y) arguments
    (<= x y)))

(defmethod primop ((operation (eql :fixnum-greater)) &rest arguments)
  (destructuring-bind (x y) arguments
    (> x y)))

(defmethod primop ((operation (eql :fixnum-logand)) &rest arguments)
  (destructuring-bind (x y) arguments
    (logand x y)))

(defmethod primop ((operation (eql :fixnum-logior)) &rest arguments)
  (destructuring-bind (x y) arguments
    (logior x y)))

(defmethod primop ((operation (eql :fixnum-logxor)) &rest arguments)
  (destructuring-bind (x y) arguments
    (logxor x y)))

(defmethod primop ((operation (eql :fixnum-divide)) &rest arguments)
  (destructuring-bind (x y) arguments
    ;; Apparently, most current architectures behave like TRUNCATE, so
    ;; we will take that as the primitive operation.
    (truncate x y)))

(defmethod primop ((operation (eql :bits-to-single-float)) &rest arguments)
  (destructuring-bind (x) arguments
    (quaviver:bits-float 'single-float x)))

(defmethod primop ((operation (eql :bits-to-double-float)) &rest arguments)
  (destructuring-bind (x) arguments
    (quaviver:bits-float 'double-float x)))

(defmethod primop ((operation (eql :single-float-equal)) &rest arguments)
  (destructuring-bind (x y) arguments
    (= x y)))

(defmethod primop ((operation (eql :single-float-add)) &rest arguments)
  (destructuring-bind (x y) arguments
    (+ x y)))

(defmethod primop ((operation (eql :single-float-multiply)) &rest arguments)
  (destructuring-bind (x y) arguments
    (* x y)))

(defmethod primop ((operation (eql :single-float-divide)) &rest arguments)
  (destructuring-bind (x y) arguments
    (/ x y)))

(defmethod primop ((operation (eql :double-float-equal)) &rest arguments)
  (destructuring-bind (x y) arguments
    (= x y)))

(defmethod primop ((operation (eql :double-float-multiply)) &rest arguments)
  (destructuring-bind (x y) arguments
    (* x y)))

(defmethod primop ((operation (eql :double-float-divide)) &rest arguments)
  (destructuring-bind (x y) arguments
    (/ x y)))
