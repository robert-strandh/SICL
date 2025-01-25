(cl:in-package #:sicl-new-boot)

(defgeneric primop (operation &rest arguments))

;;; Add two positive fixnums.  If the result is not a fixnum, then
;;; substract from it so that it becomes a negative fixnum, thereby
;;; simulating overflow behavior in most architectures.
(defmethod primop ((operation (eql :fixnum-add)) &rest arguments)
  (destructuring-bind (x y) arguments
    (check-type x (integer 0))
    (check-type y (integer 0))
    (let ((sum (+ x y)))
      (if (typep sum 'fixnum)
          sum
          (- sum (ash 1 63))))))

;;; Multiply two positive fixnums.  Return the result as two values.
(defmethod primop ((operation (eql :fixnum-add)) &rest arguments)
  (destructuring-bind (x y) arguments
    (check-type x (integer 0))
    (check-type y (integer 0))
    (let ((product (* x y)))
      (values (ldb (byte 31 31) product)
              (ldb (byte 31 0) product)))))
