(cl:in-package #:sicl-arithmetic)

(declaim (notinline general-binary-less general-binary-not-greater
		    general-binary-greater general-binary-not-less
		    general-binary-equal general-binary-not-equal))

(declaim (inline binary-less binary-not-greater
		 binary-greater binary-not-less
		 binary-equal binary-not-equal))

(defun binary-less (x y)
  ;; (declare (type real x y))
  (if (and (typep x 'fixnum)
	   (typep y 'fixnum))
      (cleavir-primop:fixnum-less x y)
      (general-binary-less x y)))

(defun binary-not-greater (x y)
  ;; (declare (type real x y))
  (if (and (typep x 'fixnum)
	   (typep y 'fixnum))
      (cleavir-primop:fixnum-not-greater x y)
      (general-binary-not-greater x y)))

(defun binary-greater (x y)
  ;; (declare (type real x y))
  (if (and (typep x 'fixnum)
	   (typep y 'fixnum))
      (cleavir-primop:fixnum-less y x)
      (general-binary-greater x y)))

(defun binary-not-less (x y)
  ;; (declare (type real x y))
  (if (and (typep x 'fixnum)
	   (typep y 'fixnum))
      (cleavir-primop:fixnum-not-greater y x)
      (general-binary-not-less x y)))

(defun binary-equal (x y)
  ;; (declare (type number x y))
  (if (and (typep x 'fixnum)
	   (typep y 'fixnum))
      (cleavir-primop:fixnum-equal x y)
      (general-binary-equal x y)))

(defun binary-not-equal (x y)
  ;; (declare (type number x y))
  (if (binary-equal x y) nil t))

