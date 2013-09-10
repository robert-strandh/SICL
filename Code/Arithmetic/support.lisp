(cl:in-package #:sicl-arithmetic)

;; ;;; addition
;; (defun binary-+ (x y)
;;   (check-type x number "a number")
;;   (check-type y number "a number")
;;   (typecase x
;;     (fixnum (typecase y
;; 	      (fixnum (fixnum-fixnum-+ x y))
;; 	      (bignum (fixnum-bignum-+ x y))
;; 	      (rational (fixnum-rational-+ x y))
;; 	      (float (fixnum-float-+ x y))))
;;     (bignum (typecase y
;; 	      (fixnum (bignum-fixnum-+ x y))
;; 	      (bignum (bignum-bignum-+ x y))
;; 	      (rational (bignum-rational-+ x y))
;; 	      (float (bignum-float-+ x y))))
;;     (rational (typecase y
;; 		(fixnum (ratio-fixnum-+ x y))
;; 		(bignum (ratio-bignum-+ x y))
;; 		(rational (ratio-rational-+ x y))
;; 		(float (ratio-float-+ x y))))
;;     (float (typecase y
;; 	     (fixnum (float-fixnum-+ x y))
;; 	     (bignum (float-bignum-+ x y))
;; 	     (rational (float-rational-+ x y))
;; 	     (float (float-float-+ x y))))))

;; ;;; subtraction
;; (defun binary-- (x y)
;;   (check-type x number "a number")
;;   (check-type y number "a number")
;;   (typecase x
;;     (fixnum (typecase y
;; 	      (fixnum (fixnum-fixnum-- x y))
;; 	      (bignum (fixnum-bignum-- x y))
;; 	      (rational (fixnum-rational-- x y))
;; 	      (float (fixnum-float-- x y))))
;;     (bignum (typecase y
;; 	      (fixnum (bignum-fixnum-- x y))
;; 	      (bignum (bignum-bignum-- x y))
;; 	      (rational (bignum-rational-- x y))
;; 	      (float (bignum-float-- x y))))
;;     (rational (typecase y
;; 		(fixnum (ratio-fixnum-- x y))
;; 		(bignum (ratio-bignum-- x y))
;; 		(rational (ratio-rational-- x y))
;; 		(float (ratio-float-- x y))))
;;     (float (typecase y
;; 	     (fixnum (float-fixnum-- x y))
;; 	     (bignum (float-bignum-- x y))
;; 	     (rational (float-rational-- x y))
;; 	     (float (float-float-- x y))))))

 
;; ;;; multiplication
;; (defun binary-* (x y)
;;   (check-type x number "a number")
;;   (check-type y number "a number")
;;   (typecase x
;;     (fixnum (typecase y
;; 	      (fixnum (fixnum-fixnum-* x y))
;; 	      (bignum (fixnum-bignum-* x y))
;; 	      (rational (fixnum-rational-* x y))
;; 	      (float (fixnum-float-* x y))))
;;     (bignum (typecase y
;; 	      (fixnum (bignum-fixnum-* x y))
;; 	      (bignum (bignum-bignum-* x y))
;; 	      (rational (bignum-rational-* x y))
;; 	      (float (bignum-float-* x y))))
;;     (rational (typecase y
;; 		(fixnum (ratio-fixnum-* x y))
;; 		(bignum (ratio-bignum-* x y))
;; 		(rational (ratio-rational-* x y))
;; 		(float (ratio-float-* x y))))
;;     (float (typecase y
;; 	     (fixnum (float-fixnum-* x y))
;; 	     (bignum (float-bignum-* x y))
;; 	     (rational (float-rational-* x y))
;; 	     (float (float-float-* x y))))))

;; ;;; division
;; (defun binary-/ (x y)
;;   (check-type x number "a number")
;;   (check-type y number "a number")
;;   (typecase x
;;     (fixnum (typecase y
;; 	      (fixnum (fixnum-fixnum-/ x y))
;; 	      (bignum (fixnum-bignum-/ x y))
;; 	      (rational (fixnum-rational-/ x y))
;; 	      (float (fixnum-float-/ x y))))
;;     (bignum (typecase y
;; 	      (fixnum (bignum-fixnum-/ x y))
;; 	      (bignum (bignum-bignum-/ x y))
;; 	      (rational (bignum-rational-/ x y))
;; 	      (float (bignum-float-/ x y))))
;;     (rational (typecase y
;; 		(fixnum (ratio-fixnum-/ x y))
;; 		(bignum (ratio-bignum-/ x y))
;; 		(rational (ratio-rational-/ x y))
;; 		(float (ratio-float-/ x y))))
;;     (float (typecase y
;; 	     (fixnum (float-fixnum-/ x y))
;; 	     (bignum (float-bignum-/ x y))
;; 	     (rational (float-rational-/ x y))
;; 	     (float (float-float-/ x y))))))

