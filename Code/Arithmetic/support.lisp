(cl:in-package #:sicl-arithmetic)

;;; Right now, we support only fixnum arithmetic.  

(defun binary-+ (x y)
  (etypecase x
    (fixnum (etypecase y
	      (fixnum (fixnum-+ x y))))))

(defun binary-- (x y)
  (etypecase x
    (fixnum (etypecase y
	      (fixnum (fixnum-- x y))))))

(defun binary-* (x y)
  (etypecase x
    (fixnum (etypecase y
	      (fixnum (fixnum-* x y))))))

(defun binary-/ (x y)
  (etypecase x
    (fixnum (etypecase y
	      (fixnum (fixnum-/ x y))))))

(defun binary-< (x y)
  (etypecase x
    (fixnum (etypecase y
	      (fixnum (fixnum-< x y))))))

(defun binary-<= (x y)
  (etypecase x
    (fixnum (etypecase y
	      (fixnum (fixnum-<= x y))))))

(defun binary-> (x y)
  (etypecase x
    (fixnum (etypecase y
	      (fixnum (fixnum-> x y))))))

(defun binary->= (x y)
  (etypecase x
    (fixnum (etypecase y
	      (fixnum (fixnum->= x y))))))

(defun binary-= (x y)
  (etypecase x
    (fixnum (etypecase y
	      (fixnum (fixnum-= x y))))))
