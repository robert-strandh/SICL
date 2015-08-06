(cl:in-package #:sicl-arithmetic)

(declaim (notinline general-binary-< general-binary-<=
		    general-binary-> general-binary->=
		    general-binary-= general-binary-/=))

(declaim (inline binary-< binary-<=
		 binary-> binary->=
		 binary-= binary-/=))

(defun binary-< (x y)
  ;; (declare (type real x y))
  (if (and (typep x 'fixnum)
	   (typep y 'fixnum))
      (cleavir-primop:fixnum-< x y)
      (general-binary-< x y)))

(defun binary-<= (x y)
  ;; (declare (type real x y))
  (if (and (typep x 'fixnum)
	   (typep y 'fixnum))
      (cleavir-primop:fixnum-<= x y)
      (general-binary-<= x y)))

(defun binary-> (x y)
  ;; (declare (type real x y))
  (if (and (typep x 'fixnum)
	   (typep y 'fixnum))
      (cleavir-primop:fixnum-< y x)
      (general-binary-> x y)))

(defun binary->= (x y)
  ;; (declare (type real x y))
  (if (and (typep x 'fixnum)
	   (typep y 'fixnum))
      (cleavir-primop:fixnum-<= y x)
      (general-binary->= x y)))

(defun binary-= (x y)
  ;; (declare (type number x y))
  (if (and (typep x 'fixnum)
	   (typep y 'fixnum))
      (cleavir-primop:fixnum-= x y)
      (general-binary-= x y)))

(defun binary-/= (x y)
  ;; (declare (type number x y))
  (if (binary-= x y) nil t))

