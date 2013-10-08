(cl:in-package #:sicl-arithmetic)

(declaim (notinline general-binary-< general-binary-<=
		    general-binary-> general-binary->=
		    general-binary-== general-binary-/=)

(declaim (inline binary-< binary-<=
		 binary-> binary->=
		 binary-== binary-/=))

(defun binary-< (x y)
  (declare (type real x y))
  (if (and (typep x 'fixnum)
	   (typep y 'fixnum))
      (sicl-word:s< x y)
      (general-binary-< x y)))

(defun binary-<= (x y)
  (declare (type real x y))
  (if (and (typep x 'fixnum)
	   (typep y 'fixnum))
      (sicl-word:s<= x y)
      (general-binary-<= x y)))

(defun binary-> (x y)
  (declare (type real x y))
  (if (and (typep x 'fixnum)
	   (typep y 'fixnum))
      (sicl-word:s> x y)
      (general-binary-> x y)))

(defun binary->= (x y)
  (declare (type real x y))
  (if (and (typep x 'fixnum)
	   (typep y 'fixnum))
      (sicl-word:s>= x y)
      (general-binary->= x y)))

(defun binary-= (x y)
  (declare (type number x y))
  (if (and (typep x 'fixnum)
	   (typep y 'fixnum))
      (sicl-word:== x y)
      (general-binary-= x y)))

(defun binary-/= (x y)
  (declare (type number x y))
  (not (binary-= x y)))

