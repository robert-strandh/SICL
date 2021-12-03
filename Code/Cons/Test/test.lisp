(cl:in-package #:sicl-cons-high-test)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Helper function to defeat the type inferencing of the 
;;; compiler so there are no warnings.

;;; This function always returns true when given a proper list.
(defun twisted (list)
  (if (null list)
      t
      (twisted (cdr list))))

(declaim (notinline twisted))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Tests for the CAAR function.

(define-test caar.1
  (assert-equal 'a (caar '((a)))))

(define-test caar.error.1
  (assert-error 'type-error (caar 'a)))

(define-test caar.error.2
  (assert-error 'type-error (caar '(a))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Tests for the CADR function.

(define-test cdar.1
  (assert-equal 'b (cdar '((a . b)))))

(define-test cdar.error.1
  (assert-error 'type-error (cdar 'a)))

(define-test cdar.error.2
  (assert-error 'type-error (cdar '(a . b))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Tests for the CADR function.

(define-test cadr.1
  (assert-equal 'b (cadr '(a b))))

(define-test cadr.error.1
  (assert-error 'type-error (cadr 'a)))

(define-test cadr.error.2
  (assert-error 'type-error (cadr '(a . b))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Tests for the CDDR function.

(define-test cddr.1
  (assert-equal 'c (cddr '(a b . c))))

(define-test cddr.error.1
  (assert-error 'type-error (cddr 'a)))

(define-test cddr.error.2
  (assert-error 'type-error (cddr '(a . b))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Tests for the CAAAR function.

(define-test caaar.1
  (assert-equal 'a (caaar '(((a))))))

(define-test caaar.error.1
  (assert-error 'type-error (caaar 'a)))

(define-test caaar.error.2
  (assert-error 'type-error (caaar '(a))))

(define-test caaar.error.3
  (assert-error 'type-error (caaar '((a)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Tests for the CDAAR function.

(define-test cdaar.1
  (assert-equal 'b (cdaar '(((a . b))))))

(define-test cdaar.error.1
  (assert-error 'type-error (cdaar 'a)))

(define-test cdaar.error.2
  (assert-error 'type-error (cdaar '(a))))

(define-test cdaar.error.3
  (assert-error 'type-error (cdaar '((a . b)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Tests for the CADAR function.

(define-test cadar.1
  (assert-equal 'b (cadar (cons (cons 'a (cons 'b 'c)) 'd))))

(define-test cadar.error.1
  (assert-error 'type-error (cadar 'a)))

(define-test cadar.error.2
  (assert-error 'type-error (cadar '(a . b))))

(define-test cadar.error.3
  (assert-error 'type-error (cadar '((a . c) . b))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Tests for the CDDAR function.

(define-test cddar.1
  (assert-equal 'c (cddar (cons (cons 'a (cons 'b 'c)) 'd))))

(define-test cddar.error.1
  (assert-error 'type-error (cddar 'a)))

(define-test cddar.error.2
  (assert-error 'type-error (cddar '(a . b))))

(define-test cddar.error.3
  (assert-error 'type-error (cddar '((a . b) . b))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Tests for the CAADR function.

(define-test caadr.1
  (assert-equal 'b (caadr (cons 'a (cons (cons 'b 'c) 'd)))))

(define-test caadr.error.1
  (assert-error 'type-error (caadr 'a)))

(define-test caadr.error.2
  (assert-error 'type-error (caadr '(a . b))))

(define-test caadr.error.3
  (assert-error 'type-error (caadr '(a . (b)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Tests for the CADDR function.

(define-test caddr.1
  (assert-equal 'c (caddr (cons 'a (cons 'b (cons 'c 'd))))))

(define-test caddr.error.1
  (assert-error 'type-error (caddr 'a)))

(define-test caddr.error.2
  (assert-error 'type-error (caddr '(a . b))))

(define-test caddr.error.3
  (assert-error 'type-error (caddr '(a c . b))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Tests for the CDADR function.

(define-test cdadr.1
  (assert-equal 'c (cdadr (cons 'a (cons (cons 'b 'c) 'd)))))

(define-test cdadr.error.1
  (assert-error 'type-error (cdadr 'a)))

(define-test cdadr.error.2
  (assert-error 'type-error (cdadr '(a . b))))

(define-test cdadr.error.3
  (assert-error 'type-error (cdadr '(a b . c))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Tests for the CDDDR function.

(define-test cdddr.1
  (assert-equal 'd (cdddr (cons 'a (cons 'b (cons 'c 'd))))))

(define-test cdddr.error.1
  (assert-error 'type-error (cdddr 'a)))

(define-test cdddr.error.2
  (assert-error 'type-error (cdddr '(a . b))))

(define-test cdddr.error.3
  (assert-error 'type-error (cdddr '(a c . b))))

;;; Tree to be used for testing some c*r functions.

(defvar *cons-test-4*
  (cons (cons (cons (cons 'a 'b)
                    (cons 'c 'd))
              (cons (cons 'e 'f)
                    (cons 'g 'h)))
        (cons (cons (cons 'i 'j)
                    (cons 'k 'l))
              (cons (cons 'm 'n)
                    (cons 'o 'p)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Tests for the CAAAAR function.

(define-test caaaar.1
  (assert-equal 'a (caaaar *cons-test-4*)))

(define-test caaaar.error.1
  (assert-error 'type-error (caaaar 'a)))

(define-test caaaar.error.2
  (assert-error 'type-error (caaaar '(a))))

(define-test caaaar.error.3
  (assert-error 'type-error (caaaar '((a)))))

(define-test caaaar.error.4
  (assert-error 'type-error (caaaar '(((a))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Tests for the CAAADR function.

(define-test caaadr.1
  (assert-equal 'i (caaadr *cons-test-4*)))

(define-test caaadr.error.1
  (assert-error 'type-error (caaadr 'a)))

(define-test caaadr.error.2
  (assert-error 'type-error (caaadr '(a . b))))

(define-test caaadr.error.3
  (assert-error 'type-error (caaadr '(a . (b)))))

(define-test caaadr.error.4
  (assert-error 'type-error (caaadr '(a . ((b))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Tests for the CAADAR function.

(define-test caadar.1
  (assert-equal 'e (caadar *cons-test-4*)))

(define-test caadar.error.1
  (assert-error 'type-error (caadar 'a)))

(define-test caadar.error.2
  (assert-error 'type-error (caadar '(a . b))))

(define-test caadar.error.3
  (assert-error 'type-error (caadar '((a . c) . b))))

(define-test caadar.error.4
  (assert-error 'type-error (caadar '((a . (c)) . b))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Tests for the CAADDR function.

(define-test caaddr.1
  (assert-equal 'm (caaddr *cons-test-4*)))

(define-test caaddr.error.1
  (assert-error 'type-error (caaddr 'a)))

(define-test caaddr.error.2
  (assert-error 'type-error (caaddr '(a . b))))

(define-test caaddr.error.3
  (assert-error 'type-error (caaddr '(a c . b))))

(define-test caaddr.error.4
  (assert-error 'type-error (caaddr '(a c . (b)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Tests for the CADAAR function.

(define-test cadaar.1
  (assert-equal 'c (cadaar *cons-test-4*)))

(define-test cadaar.error.1
  (assert-error 'type-error (cadaar 'a)))

(define-test cadaar.error.2
  (assert-error 'type-error (cadaar '(a))))

(define-test cadaar.error.3
  (assert-error 'type-error (cadaar '((a . b)))))

(define-test cadaar.error.4
  (assert-error 'type-error (cadaar '((a . (b))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Tests for the CADADR function.

(define-test cadadr.1
  (assert-equal 'k (cadadr *cons-test-4*)))

(define-test cadadr.error.1
  (assert-error 'type-error (cadadr 'a)))

(define-test cadadr.error.2
  (assert-error 'type-error (cadadr '(a . b))))

(define-test cadadr.error.3
  (assert-error 'type-error (cadadr '(a b . c))))

(define-test cadadr.error.4
  (assert-error 'type-error (cadadr '(a (b . e) . c))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Tests for the CADDAR function.

(define-test caddar.1
  (assert-equal 'g (caddar *cons-test-4*)))

(define-test caddar.error.1
  (assert-error 'type-error (caddar 'a)))

(define-test caddar.error.2
  (assert-error 'type-error (caddar '(a . b))))

(define-test caddar.error.3
  (assert-error 'type-error (caddar '((a . b) . b))))

(define-test caddar.error.4
  (assert-error 'type-error (caddar '((a  b . c) . b))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Tests for the CADDDR function.

(define-test cadddr.1
  (assert-equal 'o (cadddr *cons-test-4*)))

(define-test cadddr.error.1
  (assert-error 'type-error (cadddr 'a)))

(define-test cadddr.error.2
  (assert-error 'type-error (cadddr '(a . b))))

(define-test cadddr.error.3
  (assert-error 'type-error (cadddr '(a c . b))))

(define-test cadddr.error.4
  (assert-error 'type-error (cadddr '(a c e . b))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Tests for the CDAAAR function.

(define-test cdaaar.1
  (assert-equal 'b (cdaaar *cons-test-4*)))

(define-test cdaaar.error.1
  (assert-error 'type-error (cdaaar 'a)))

(define-test cdaaar.error.2
  (assert-error 'type-error (cdaaar '(a))))

(define-test cdaaar.error.3
  (assert-error 'type-error (cdaaar '((a)))))

(define-test cdaaar.error.4
  (assert-error 'type-error (cdaaar '(((a . b))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Tests for the CDAADR function.

(define-test cdaadr.1
  (assert-equal 'j (cdaadr *cons-test-4*)))

(define-test cdaadr.error.1
  (assert-error 'type-error (cdaadr 'a)))

(define-test cdaadr.error.2
  (assert-error 'type-error (cdaadr '(a . b))))

(define-test cdaadr.error.3
  (assert-error 'type-error (cdaadr '(a . (b)))))

(define-test cdaadr.error.4
  (assert-error 'type-error (cdaadr '(a . ((b . c))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Tests for the CDADAR function.

(define-test cdadar.1
  (assert-equal 'f (cdadar *cons-test-4*)))

(define-test cdadar.error.1
  (assert-error 'type-error (cdadar 'a)))

(define-test cdadar.error.2
  (assert-error 'type-error (cdadar '(a . b))))

(define-test cdadar.error.3
  (assert-error 'type-error (cdadar '((a . c) . b))))

(define-test cdadar.error.4
  (assert-error 'type-error (cdadar '((a . (c . d)) . b))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Tests for the CDADDR function.

(define-test cdaddr.1
  (assert-equal 'n (cdaddr *cons-test-4*)))

(define-test cdaddr.error.1
  (assert-error 'type-error (cdaddr 'a)))

(define-test cdaddr.error.2
  (assert-error 'type-error (cdaddr '(a . b))))

(define-test cdaddr.error.3
  (assert-error 'type-error (cdaddr '(a c . b))))

(define-test cdaddr.error.4
  (assert-error 'type-error (cdaddr '(a c b . d))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Tests for the CDDAAR function.

(define-test cddaar.1
  (assert-equal 'd (cddaar *cons-test-4*)))

(define-test cddaar.error.1
  (assert-error 'type-error (cddaar 'a)))

(define-test cddaar.error.2
  (assert-error 'type-error (cddaar '(a))))

(define-test cddaar.error.3
  (assert-error 'type-error (cddaar '((a . b)))))

(define-test cddaar.error.4
  (assert-error 'type-error (cddaar '((a . (b))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Tests for the CDDADR function.

(define-test cddadr.1
  (assert-equal 'l (cddadr *cons-test-4*)))

(define-test cddadr.error.1
  (assert-error 'type-error (cddadr 'a)))

(define-test cddadr.error.2
  (assert-error 'type-error (cddadr '(a . b))))

(define-test cddadr.error.3
  (assert-error 'type-error (cddadr '(a b . c))))

(define-test cddadr.error.4
  (assert-error 'type-error (cddadr '(a (b . e) . c))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Tests for the CDDDAR function.

(define-test cdddar.1
  (assert-equal 'h (cdddar *cons-test-4*)))

(define-test cdddar.error.1
  (assert-error 'type-error (cdddar 'a)))

(define-test cdddar.error.2
  (assert-error 'type-error (cdddar '(a . b))))

(define-test cdddar.error.3
  (assert-error 'type-error (cdddar '((a . b) . b))))

(define-test cdddar.error.4
  (assert-error 'type-error (cdddar '((a  b . c) . b))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Tests for the CDDDDR function.

(define-test cddddr.1
  (assert-equal 'p (cddddr *cons-test-4*)))

(define-test cddddr.error.1
  (assert-error 'type-error (cddddr 'a)))

(define-test cddddr.error.2
  (assert-error 'type-error (cddddr '(a . b))))

(define-test cddddr.error.3
  (assert-error 'type-error (cddddr '(a c . b))))

(define-test cddddr.error.4
  (assert-error 'type-error (cddddr '(a c e . b))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Tests for the FIRST function

(define-test first.1
  (assert-equal nil (first nil)))

(define-test first.2
  (assert-equal 1 (first '(1))))

(define-test first.3
  (assert-equal 1 (first '(1 2))))

(define-test first.error.1
  (assert-error 'type-error (first 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Tests for the SECOND function

(define-test second.1
  (assert-equal nil (second nil)))

(define-test second.2
  (assert-equal nil (second '(1))))

(define-test second.3
  (assert-equal 1 (second '(0 1))))

(define-test second.4
  (assert-equal 1 (second '(0 1 2))))

(define-test second.error.1
  (assert-error 'type-error (second 1)))

(define-test second.error.2
  (assert-error 'type-error (second '(0 . 1))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Tests for the THIRD function.

(define-test third.1
  (assert-equal nil (third nil)))

(define-test third.2
  (assert-equal nil (third '(1))))

(define-test third.3
  (assert-equal nil (third '(1 1))))

(define-test third.4
  (assert-equal 1 (third '(0 0 1))))

(define-test third.5
  (assert-equal 1 (third '(0 0 1 2))))

(define-test third.error.1
  (assert-error 'type-error (third 1)))

(define-test third.error.2
  (assert-error 'type-error (third '(0 . 1))))

(define-test third.error.3
  (assert-error 'type-error (third '(0 0 . 1))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Tests for the FOURTH function.

(define-test fourth.1
  (assert-equal nil (fourth nil)))

(define-test fourth.2
  (assert-equal nil (fourth '(1))))

(define-test fourth.3
  (assert-equal nil (fourth '(1 1))))

(define-test fourth.4
  (assert-equal nil (fourth '(1 1 1))))

(define-test fourth.5
  (assert-equal 1 (fourth '(0 0 0 1))))

(define-test fourth.6
  (assert-equal 1 (fourth '(0 0 0 1 2))))

(define-test fourth.error.1
  (assert-error 'type-error (fourth 1)))

(define-test fourth.error.2
  (assert-error 'type-error (fourth '(0 . 1))))

(define-test fourth.error.3
  (assert-error 'type-error (fourth '(0 0 . 1))))

(define-test fourth.error.4
  (assert-error 'type-error (fourth '(0 0 0 . 1))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Tests for the FIFTH function.

(define-test fifth.1
  (assert-equal nil (fifth nil)))

(define-test fifth.2
  (assert-equal nil (fifth '(1))))

(define-test fifth.3
  (assert-equal nil (fifth '(1 1))))

(define-test fifth.4
  (assert-equal nil (fifth '(1 1 1))))

(define-test fifth.5
  (assert-equal nil (fifth '(1 1 1 1))))

(define-test fifth.6
  (assert-equal 1 (fifth '(0 0 0 0 1))))

(define-test fifth.7
  (assert-equal 1 (fifth '(0 0 0 0 1 2))))

(define-test fifth.error.1
  (assert-error 'type-error (fifth 1)))

(define-test fifth.error.2
  (assert-error 'type-error (fifth '(0 . 1))))

(define-test fifth.error.3
  (assert-error 'type-error (fifth '(0 0 . 1))))

(define-test fifth.error.4
  (assert-error 'type-error (fifth '(0 0 0 . 1))))

(define-test fifth.error.5
  (assert-error 'type-error (fifth '(0 0 0 0 . 1))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Tests for the SIXTH function.

(define-test sixth.1
  (assert-equal nil (sixth nil)))

(define-test sixth.2
  (assert-equal nil (sixth '(1))))

(define-test sixth.3
  (assert-equal nil (sixth '(1 1))))

(define-test sixth.4
  (assert-equal nil (sixth '(1 1 1))))

(define-test sixth.5
  (assert-equal nil (sixth '(1 1 1 1))))

(define-test sixth.6
  (assert-equal nil (sixth '(1 1 1 1 1))))

(define-test sixth.7
  (assert-equal 1 (sixth '(0 0 0 0 0 1))))

(define-test sixth.8
  (assert-equal 1 (sixth '(0 0 0 0 0 1 2))))

(define-test sixth.error.1
  (assert-error 'type-error (sixth 1)))

(define-test sixth.error.2
  (assert-error 'type-error (sixth '(0 . 1))))

(define-test sixth.error.3
  (assert-error 'type-error (sixth '(0 0 . 1))))

(define-test sixth.error.4
  (assert-error 'type-error (sixth '(0 0 0 . 1))))

(define-test sixth.error.5
  (assert-error 'type-error (sixth '(0 0 0 0 . 1))))

(define-test sixth.error.6
  (assert-error 'type-error (sixth '(0 0 0 0 0 . 1))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Tests for the SEVENTH function.

(define-test seventh.1
  (assert-equal nil (seventh nil)))

(define-test seventh.2
  (assert-equal nil (seventh '(1))))

(define-test seventh.3
  (assert-equal nil (seventh '(1 1))))

(define-test seventh.4
  (assert-equal nil (seventh '(1 1 1))))

(define-test seventh.5
  (assert-equal nil (seventh '(1 1 1 1))))

(define-test seventh.6
  (assert-equal nil (seventh '(1 1 1 1 1))))

(define-test seventh.7
  (assert-equal nil (seventh '(1 1 1 1 1 1))))

(define-test seventh.8
  (assert-equal 1 (seventh '(0 0 0 0 0 0 1))))

(define-test seventh.9
  (assert-equal 1 (seventh '(0 0 0 0 0 0 1 2))))

(define-test seventh.error.1
  (assert-error 'type-error (seventh 1)))

(define-test seventh.error.2
  (assert-error 'type-error (seventh '(0 . 1))))

(define-test seventh.error.3
  (assert-error 'type-error (seventh '(0 0 . 1))))

(define-test seventh.error.4
  (assert-error 'type-error (seventh '(0 0 0 . 1))))

(define-test seventh.error.5
  (assert-error 'type-error (seventh '(0 0 0 0 . 1))))

(define-test seventh.error.6
  (assert-error 'type-error (seventh '(0 0 0 0 0 . 1))))

(define-test seventh.error.7
  (assert-error 'type-error (seventh '(0 0 0 0 0 0 . 1))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Tests for the EIGHTH function.

(define-test eighth.1
  (assert-equal nil (eighth nil)))

(define-test eighth.2
  (assert-equal nil (eighth '(1))))

(define-test eighth.3
  (assert-equal nil (eighth '(1 1))))

(define-test eighth.4
  (assert-equal nil (eighth '(1 1 1))))

(define-test eighth.5
  (assert-equal nil (eighth '(1 1 1 1))))

(define-test eighth.6
  (assert-equal nil (eighth '(1 1 1 1 1))))

(define-test eighth.7
  (assert-equal nil (eighth '(1 1 1 1 1 1))))

(define-test eighth.8
  (assert-equal nil (eighth '(1 1 1 1 1 1 1))))

(define-test eighth.9
  (assert-equal 1 (eighth '(0 0 0 0 0 0 0 1))))

(define-test eighth.10
  (assert-equal 1 (eighth '(0 0 0 0 0 0 0 1 2))))

(define-test eighth.error.1
  (assert-error 'type-error (eighth 1)))

(define-test eighth.error.2
  (assert-error 'type-error (eighth '(0 . 1))))

(define-test eighth.error.3
  (assert-error 'type-error (eighth '(0 0 . 1))))

(define-test eighth.error.4
  (assert-error 'type-error (eighth '(0 0 0 . 1))))

(define-test eighth.error.5
  (assert-error 'type-error (eighth '(0 0 0 0 . 1))))

(define-test eighth.error.6
  (assert-error 'type-error (eighth '(0 0 0 0 0 . 1))))

(define-test eighth.error.7
  (assert-error 'type-error (eighth '(0 0 0 0 0 0 . 1))))

(define-test eighth.error.8
  (assert-error 'type-error (eighth '(0 0 0 0 0 0 . 1))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Tests for the NINTH function.

(define-test ninth.1
  (assert-equal nil (ninth nil)))

(define-test ninth.2
  (assert-equal nil (ninth '(1))))

(define-test ninth.3
  (assert-equal nil (ninth '(1 1))))

(define-test ninth.4
  (assert-equal nil (ninth '(1 1 1))))

(define-test ninth.5
  (assert-equal nil (ninth '(1 1 1 1))))

(define-test ninth.6
  (assert-equal nil (ninth '(1 1 1 1 1))))

(define-test ninth.7
  (assert-equal nil (ninth '(1 1 1 1 1 1))))

(define-test ninth.8
  (assert-equal nil (ninth '(1 1 1 1 1 1 1))))

(define-test ninth.9
  (assert-equal nil (ninth '(1 1 1 1 1 1 1 1))))

(define-test ninth.10
  (assert-equal 1 (ninth '(0 0 0 0 0 0 0 0 1))))

(define-test ninth.11
  (assert-equal 1 (ninth '(0 0 0 0 0 0 0 0 1 2))))

(define-test ninth.error.1
  (assert-error 'type-error (ninth 1)))

(define-test ninth.error.2
  (assert-error 'type-error (ninth '(0 . 1))))

(define-test ninth.error.3
  (assert-error 'type-error (ninth '(0 0 . 1))))

(define-test ninth.error.4
  (assert-error 'type-error (ninth '(0 0 0 . 1))))

(define-test ninth.error.5
  (assert-error 'type-error (ninth '(0 0 0 0 . 1))))

(define-test ninth.error.6
  (assert-error 'type-error (ninth '(0 0 0 0 0 . 1))))

(define-test ninth.error.7
  (assert-error 'type-error (ninth '(0 0 0 0 0 0 . 1))))

(define-test ninth.error.8
  (assert-error 'type-error (ninth '(0 0 0 0 0 0 . 1))))

(define-test ninth.error.9
  (assert-error 'type-error (ninth '(0 0 0 0 0 0 0 . 1))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Tests for the TENTH function.

(define-test tenth.1
  (assert-equal nil (tenth nil)))

(define-test tenth.2
  (assert-equal nil (tenth '(1))))

(define-test tenth.3
  (assert-equal nil (tenth '(1 1))))

(define-test tenth.4
  (assert-equal nil (tenth '(1 1 1))))

(define-test tenth.5
  (assert-equal nil (tenth '(1 1 1 1))))

(define-test tenth.6
  (assert-equal nil (tenth '(1 1 1 1 1))))

(define-test tenth.7
  (assert-equal nil (tenth '(1 1 1 1 1 1))))

(define-test tenth.8
  (assert-equal nil (tenth '(1 1 1 1 1 1 1))))

(define-test tenth.9
  (assert-equal nil (tenth '(1 1 1 1 1 1 1 1))))

(define-test tenth.10
  (assert-equal nil (tenth '(1 1 1 1 1 1 1 1 1))))

(define-test tenth.11
  (assert-equal 1 (tenth '(0 0 0 0 0 0 0 0 0 1))))

(define-test tenth.12
  (assert-equal 1 (tenth '(0 0 0 0 0 0 0 0 0 1 2))))

(define-test tenth.error.1
  (assert-error 'type-error (tenth 1)))

(define-test tenth.error.2
  (assert-error 'type-error (tenth '(0 . 1))))

(define-test tenth.error.3
  (assert-error 'type-error (tenth '(0 0 . 1))))

(define-test tenth.error.4
  (assert-error 'type-error (tenth '(0 0 0 . 1))))

(define-test tenth.error.5
  (assert-error 'type-error (tenth '(0 0 0 0 . 1))))

(define-test tenth.error.6
  (assert-error 'type-error (tenth '(0 0 0 0 0 . 1))))

(define-test tenth.error.7
  (assert-error 'type-error (tenth '(0 0 0 0 0 0 . 1))))

(define-test tenth.error.8
  (assert-error 'type-error (tenth '(0 0 0 0 0 0 . 1))))

(define-test tenth.error.9
  (assert-error 'type-error (tenth '(0 0 0 0 0 0 0 . 1))))

(define-test tenth.error.10
  (assert-error 'type-error (tenth '(0 0 0 0 0 0 0 0 . 1))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Tests for the (SETF CAAR) function and SETF expander.

(define-test |setf caar 1|
  (let ((list (copy-tree '((((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0))) .
                           (((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0)))))))
    (assert-equal 1 (setf (caar list) 1))
    (assert-equal '((1                   . ((0 . 0) . (0 . 0))) .
                    (((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0))))
                  list)))

(define-test |setf caar error 1|
  (let ((list '()))
    (assert-error 'type-error
                  (setf (caar list) 1))))

(define-test |setf caar error 2|
  (let ((list 0))
    (assert-error 'type-error
                  (setf (caar list) 1))))

(define-test |setf caar error 3|
  (let ((list (copy-tree '(0 0))))
    (assert-error 'type-error
                  (setf (caar list) 1))))

(define-test |setf caar error 4|
  (let ((list (copy-tree '(() 0))))
    (assert-error 'type-error
                  (setf (caar list) 1))))

(define-test |setf caar apply 1|
  (let ((list (copy-tree '((((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0))) .
                           (((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0)))))))
    (assert-equal 1 (apply #'(setf caar) (list 1 list)))
    (assert-equal '((1                   . ((0 . 0) . (0 . 0))) .
                    (((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0))))
                  list)))

(define-test |setf caar apply error 1|
  (let ((list '()))
    (assert-error 'type-error
                  (apply #'(setf caar) (list 1 list)))))

(define-test |setf caar apply error 2|
  (let ((list 0))
    (assert-error 'type-error
                  (apply #'(setf caar) (list 1 list)))))

(define-test |setf caar apply error 3|
  (let ((list (copy-tree '(0 0))))
    (assert-error 'type-error
                  (apply #'(setf caar) (list 1 list)))))

(define-test |setf caar apply error 4|
  (let ((list (copy-tree '(() 0))))
    (assert-error 'type-error
                  (apply #'(setf caar) (list 1 list)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Tests for the (SETF CADR) function and SETF expander.

(define-test |setf cadr 1|
  (let ((list (copy-tree '(0 0))))
    (assert-equal 1 (setf (cadr list) 1))
    (assert-equal '(0 1) list)))

(define-test |setf cadr error 1|
  (let ((list '()))
    (assert-error 'type-error
                  (setf (cadr list) 1))))

(define-test |setf cadr error 2|
  (let ((list 0))
    (assert-error 'type-error
                  (setf (cadr list) 1))))

(define-test |setf cadr error 3|
  (let ((list (copy-tree '(0))))
    (assert-error 'type-error
                  (setf (cadr list) 1))))

(define-test |setf cadr error 4|
  (let ((list (copy-tree '(0 . 0))))
    (assert-error 'type-error
                  (setf (cadr list) 1))))

(define-test |setf cadr apply 1|
  (let ((list (copy-tree '(0 0))))
    (assert-equal 1 (apply #'(setf cadr) (list 1 list)))
    (assert-equal '(0 1) list)))

(define-test |setf cadr apply error 1|
  (let ((list '()))
    (assert-error 'type-error
                  (apply #'(setf cadr) (list 1 list)))))

(define-test |setf cadr apply error 2|
  (let ((list 0))
    (assert-error 'type-error
                  (apply #'(setf cadr) (list 1 list)))))

(define-test |setf cadr apply error 3|
  (let ((list (copy-tree '(0))))
    (assert-error 'type-error
                  (apply #'(setf cadr) (list 1 list)))))

(define-test |setf cadr apply error 4|
  (let ((list (copy-tree '(0 . 0))))
    (assert-error 'type-error
                  (apply #'(setf cadr) (list 1 list)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Tests for the (SETF CDAR) function and SETF expander.

(define-test |setf cdar 1|
  (let ((list (copy-tree '((0) 0))))
    (assert-equal 1 (setf (cdar list) 1))
    (assert-equal '((0 . 1) 0) list)))

(define-test |setf cdar error 1|
  (let ((list '()))
    (assert-error 'type-error
                  (setf (cdar list) 1))))

(define-test |setf cdar error 2|
  (let ((list 0))
    (assert-error 'type-error
                  (setf (cdar list) 1))))

(define-test |setf cdar error 3|
  (let ((list (copy-tree '(0))))
    (assert-error 'type-error
                  (setf (cdar list) 1))))

(define-test |setf cdar error 4|
  (let ((list (copy-tree '(()))))
    (assert-error 'type-error
                  (setf (cdar list) 1))))

(define-test |setf cdar apply 1|
  (let ((list (copy-tree '((0) 0))))
    (assert-equal 1 (apply #'(setf cdar) (list 1 list)))
    (assert-equal '((0 . 1) 0) list)))

(define-test |setf cdar apply error 1|
  (let ((list '()))
    (assert-error 'type-error
                  (apply #'(setf cdar) (list 1 list)))))

(define-test |setf cdar apply error 2|
  (let ((list 0))
    (assert-error 'type-error
                  (apply #'(setf cdar) (list 1 list)))))

(define-test |setf cdar apply error 3|
  (let ((list (copy-tree '(0))))
    (assert-error 'type-error
                  (apply #'(setf cdar) (list 1 list)))))

(define-test |setf cdar apply error 4|
  (let ((list (copy-tree '(()))))
    (assert-error 'type-error
                  (apply #'(setf cdar) (list 1 list)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Tests for the (SETF CDDR) function and SETF expander.

(define-test |setf cddr 1|
  (let ((list (copy-tree '(0 0))))
    (assert-equal 1 (setf (cddr list) 1))
    (assert-equal '(0 0 . 1) list)))

(define-test |setf cddr error 1|
  (let ((list '()))
    (assert-error 'type-error
                  (setf (cddr list) 1))))

(define-test |setf cddr error 2|
  (let ((list 0))
    (assert-error 'type-error
                  (setf (cddr list) 1))))

(define-test |setf cddr error 3|
  (let ((list (copy-tree '(0))))
    (assert-error 'type-error
                  (setf (cddr list) 1))))

(define-test |setf cddr error 4|
  (let ((list (copy-tree '(0 . 1))))
    (assert-error 'type-error
                  (setf (cddr list) 1))))

(define-test |setf cddr apply 1|
  (let ((list (copy-tree '(0 0))))
    (assert-equal 1 (apply #'(setf cddr) (list 1 list)))
    (assert-equal '(0 0 . 1) list)))

(define-test |setf cddr apply error 1|
  (let ((list '()))
    (assert-error 'type-error
                  (apply #'(setf cddr) (list 1 list)))))

(define-test |setf cddr apply error 2|
  (let ((list 0))
    (assert-error 'type-error
                  (apply #'(setf cddr) (list 1 list)))))

(define-test |setf cddr apply error 3|
  (let ((list (copy-tree '(0))))
    (assert-error 'type-error
                  (apply #'(setf cddr) (list 1 list)))))

(define-test |setf cddr apply error 4|
  (let ((list (copy-tree '(0 . 1))))
    (assert-error 'type-error
                  (apply #'(setf cddr) (list 1 list)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Tests for the (SETF CAAAR) function and SETF expander.

(define-test |setf caaar 1|
  (let ((list (copy-tree '(((0 0) 0) 0))))
    (assert-equal 1 (setf (caaar list) 1))
    (assert-equal '(((1 0) 0) 0) list)))

(define-test |setf caaar error 1|
  (let ((list '()))
    (assert-error 'type-error
                  (setf (caaar list) 1))))

(define-test |setf caaar error 2|
  (let ((list 0))
    (assert-error 'type-error
                  (setf (caaar list) 1))))

(define-test |setf caaar error 3|
  (let ((list (copy-tree '(0 0))))
    (assert-error 'type-error
                  (setf (caaar list) 1))))

(define-test |setf caaar error 4|
  (let ((list (copy-tree '((0 0) 0))))
    (assert-error 'type-error
                  (setf (caaar list) 1))))

(define-test |setf caaar error 5|
  (let ((list (copy-tree '((() 0) 0))))
    (assert-error 'type-error
                  (setf (caaar list) 1))))

(define-test |setf caaar apply 1|
  (let ((list (copy-tree '(((0 0) 0) 0))))
    (assert-equal 1 (apply #'(setf caaar) (list 1 list)))
    (assert-equal '(((1 0) 0) 0) list)))

(define-test |setf caaar apply error 1|
  (let ((list '()))
    (assert-error 'type-error
                  (apply #'(setf caaar) (list 1 list)))))

(define-test |setf caaar apply error 2|
  (let ((list 0))
    (assert-error 'type-error
                  (apply #'(setf caaar) (list 1 list)))))

(define-test |setf caaar apply error 3|
  (let ((list (copy-tree '(0 0))))
    (assert-error 'type-error
                  (apply #'(setf caaar) (list 1 list)))))

(define-test |setf caaar apply error 4|
  (let ((list (copy-tree '((0 0) 0))))
    (assert-error 'type-error
                  (apply #'(setf caaar) (list 1 list)))))

(define-test |setf caaar apply error 5|
  (let ((list (copy-tree '((() 0) 0))))
    (assert-error 'type-error
                  (apply #'(setf caaar) (list 1 list)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Tests for the (SETF CAADR) function and SETF expander.

(define-test |setf caadr 1|
  (let ((list (copy-tree '((((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0))) .
                           (((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0)))))))
    (assert-equal 1 (setf (caadr list) 1))
    (assert-equal '((((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0))) .
                    ((1       . (0 . 0)) . ((0 . 0) . (0 . 0))))
                  list)))

(define-test |setf caadr error 1|
  (let ((list 0))
    (assert-error 'type-error
                  (setf (caadr list) 1))))

(define-test |setf caadr error 2|
  (let ((list (copy-tree '((((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0))) .
                           0                                         ))))
    (assert-error 'type-error
                  (setf (caadr list) 1))))

(define-test |setf caadr error 3|
  (let ((list (copy-tree '((((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0))) .
                           (0                   . ((0 . 0) . (0 . 0)))))))
    (assert-error 'type-error
                  (setf (caadr list) 1))))

(define-test |setf caadr apply 1|
  (let ((list (copy-tree '((((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0))) .
                           (((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0)))))))
    (assert-equal 1 (apply #'(setf caadr) (list 1 list)))
    (assert-equal '((((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0))) .
                    ((1       . (0 . 0)) . ((0 . 0) . (0 . 0))))
                  list)))

(define-test |setf caadr apply error 1|
  (let ((list 0))
    (assert-error 'type-error
                  (apply #'(setf caadr) (list 1 list)))))

(define-test |setf caadr apply error 2|
  (let ((list (copy-tree '((((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0))) .
                           0                                         ))))
    (assert-error 'type-error
                  (apply #'(setf caadr) (list 1 list)))))

(define-test |setf caadr apply error 3|
  (let ((list (copy-tree '((((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0))) .
                           (0                   . ((0 . 0) . (0 . 0)))))))
    (assert-error 'type-error
                  (apply #'(setf caadr) (list 1 list)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Tests for the (SETF CADAR) function and SETF expander.

(define-test |setf cadar 1|
  (let ((list (copy-tree '((((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0))) .
                           (((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0)))))))
    (assert-equal 1 (setf (cadar list) 1))
    (assert-equal '((((0 . 0) . (0 . 0)) . (1       . (0 . 0))) .
                    (((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0))))
                  list)))

(define-test |setf cadar error 1|
  (let ((list 0))
    (assert-error 'type-error
                  (setf (cadar list) 1))))

(define-test |setf cadar error 2|
  (let ((list (copy-tree '(0 .
                           (((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0)))))))
    (assert-error 'type-error
                  (setf (cadar list) 1))))

(define-test |setf cadar error 3|
  (let ((list (copy-tree '((((0 . 0) . (0 . 0)) .                   0) .
                           (((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0)))))))
    (assert-error 'type-error
                  (setf (cadar list) 1))))

(define-test |setf cadar apply 1|
  (let ((list (copy-tree '((((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0))) .
                           (((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0)))))))
    (assert-equal 1 (apply #'(setf cadar) (list 1 list)))
    (assert-equal '((((0 . 0) . (0 . 0)) . (1       . (0 . 0))) .
                    (((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0))))
                  list)))

(define-test |setf cadar apply error 1|
  (let ((list 0))
    (assert-error 'type-error
                  (apply #'(setf cadar) (list 1 list)))))

(define-test |setf cadar apply error 2|
  (let ((list (copy-tree '(0 .
                           (((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0)))))))
    (assert-error 'type-error
                  (apply #'(setf cadar) (list 1 list)))))

(define-test |setf cadar apply error 3|
  (let ((list (copy-tree '((((0 . 0) . (0 . 0)) .                   0) .
                           (((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0)))))))
    (assert-error 'type-error
                  (apply #'(setf cadar) (list 1 list)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Tests for the (SETF CADDR) function and SETF expander.

(define-test |setf caddr 1|
  (let ((list (copy-tree '((((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0))) .
                           (((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0)))))))
    (assert-equal 1 (setf (caddr list) 1))
    (assert-equal '((((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0))) .
                    (((0 . 0) . (0 . 0)) . (1       . (0 . 0))))
                  list)))

(define-test |setf caddr error 1|
  (let ((list 0))
    (assert-error 'type-error
                  (setf (caddr list) 1))))

(define-test |setf caddr error 2|
  (let ((list (copy-tree '((((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0))) .
                           0                                          ))))
    (assert-error 'type-error
                  (setf (caddr list) 1))))

(define-test |setf caddr error 3|
  (let ((list (copy-tree '((((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0))) .
                           (((0 . 0) . (0 . 0)) . 0                   )))))
    (assert-error 'type-error
                  (setf (caddr list) 1))))

(define-test |setf caddr apply 1|
  (let ((list (copy-tree '((((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0))) .
                           (((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0)))))))
    (assert-equal 1 (apply #'(setf caddr) (list 1 list)))
    (assert-equal '((((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0))) .
                    (((0 . 0) . (0 . 0)) . (1       . (0 . 0))))
                  list)))

(define-test |setf caddr apply error 1|
  (let ((list 0))
    (assert-error 'type-error
                  (apply #'(setf caddr) (list 1 list)))))

(define-test |setf caddr apply error 2|
  (let ((list (copy-tree '((((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0))) .
                           0                                          ))))
    (assert-error 'type-error
                  (apply #'(setf caddr) (list 1 list)))))

(define-test |setf caddr apply error 3|
  (let ((list (copy-tree '((((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0))) .
                           (((0 . 0) . (0 . 0)) . 0                   )))))
    (assert-error 'type-error
                  (apply #'(setf caddr) (list 1 list)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Tests for the (SETF CDAAR) function and SETF expander.

(define-test |setf cdaar 1|
  (let ((list (copy-tree '((((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0))) .
                           (((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0)))))))
    (assert-equal 1 (setf (cdaar list) 1))
    (assert-equal '((((0 . 0) .       1) . ((0 . 0) . (0 . 0))) .
                    (((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0))))
                  list)))

(define-test |setf cdaar error 1|
  (let ((list 0))
    (assert-error 'type-error
                  (setf (cdaar list) 1))))

(define-test |setf cdaar error 2|
  (let ((list (copy-tree '(0 .
                           (((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0)))))))
    (assert-error 'type-error
                  (setf (cdaar list) 1))))

(define-test |setf cdaar error 3|
  (let ((list (copy-tree '((0                   . ((0 . 0) . (0 . 0))) .
                           (((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0)))))))
    (assert-error 'type-error
                  (setf (cdaar list) 1))))

(define-test |setf cdaar apply 1|
  (let ((list (copy-tree '((((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0))) .
                           (((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0)))))))
    (assert-equal 1 (apply #'(setf cdaar) (list 1 list)))
    (assert-equal '((((0 . 0) .       1) . ((0 . 0) . (0 . 0))) .
                    (((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0))))
                  list)))

(define-test |setf cdaar apply error 1|
  (let ((list 0))
    (assert-error 'type-error
                  (apply #'(setf cdaar) (list 1 list)))))

(define-test |setf cdaar apply error 2|
  (let ((list (copy-tree '(0 .
                           (((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0)))))))
    (assert-error 'type-error
                  (apply #'(setf cdaar) (list 1 list)))))

(define-test |setf cdaar apply error 3|
  (let ((list (copy-tree '((0                   . ((0 . 0) . (0 . 0))) .
                           (((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0)))))))
    (assert-error 'type-error
                  (apply #'(setf cdaar) (list 1 list)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Tests for the (SETF CDADR) function and SETF expander.

(define-test |setf cdadr 1|
  (let ((list (copy-tree '((((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0))) . 
                           (((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0)))))))
    (assert-equal 1 (setf (cdadr list) 1))
    (assert-equal '((((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0))) .
                    (((0 . 0) .       1) . ((0 . 0) . (0 . 0))))
                  list)))

(define-test |setf cdadr error 1|
  (let ((list 0))
    (assert-error 'type-error
                  (setf (cdadr list) 1))))

(define-test |setf cdadr error 2|
  (let ((list (copy-tree '((((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0))) .
                           0                                          ))))
    (assert-error 'type-error
                  (setf (cdadr list) 1))))

(define-test |setf cdadr error 3|
  (let ((list (copy-tree '((((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0))) .
                           (0                   . ((0 . 0) . (0 . 0)))))))
    (assert-error 'type-error
                  (setf (cdadr list) 1))))

(define-test |setf cdadr apply 1|
  (let ((list (copy-tree '((((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0))) . 
                           (((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0)))))))
    (assert-equal 1 (apply #'(setf cdadr) (list 1 list)))
    (assert-equal '((((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0))) .
                    (((0 . 0) .       1) . ((0 . 0) . (0 . 0))))
                  list)))

(define-test |setf cdadr apply error 1|
  (let ((list 0))
    (assert-error 'type-error
                  (apply #'(setf cdadr) (list 1 list)))))

(define-test |setf cdadr apply error 2|
  (let ((list (copy-tree '((((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0))) .
                           0                                          ))))
    (assert-error 'type-error
                  (apply #'(setf cdadr) (list 1 list)))))

(define-test |setf cdadr apply error 3|
  (let ((list (copy-tree '((((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0))) .
                           (0                   . ((0 . 0) . (0 . 0)))))))
    (assert-error 'type-error
                  (apply #'(setf cdadr) (list 1 list)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Tests for the (SETF CDDAR) function and SETF expander.

(define-test |setf cddar 1|
  (let ((list (copy-tree '((((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0))) . 
                           (((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0)))))))
    (assert-equal 1 (setf (cddar list) 1))
    (assert-equal '((((0 . 0) . (0 . 0)) . ((0 . 0) .        1)) .
                    (((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0))))
                  list)))

(define-test |setf cddar error 1|
  (let ((list 0))
    (assert-error 'type-error
                  (setf (cddar list) 1))))

(define-test |setf cddar error 2|
  (let ((list (copy-tree '(0                                           .
                           (((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0)))))))
    (assert-error 'type-error
                  (setf (cddar list) 1))))

(define-test |setf cddar error 3|
  (let ((list (copy-tree '((((0 . 0) . (0 . 0)) .                   0) .
                           (((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0)))))))
    (assert-error 'type-error
                  (setf (cddar list) 1))))

(define-test |setf cddar apply 1|
  (let ((list (copy-tree '((((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0))) . 
                           (((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0)))))))
    (assert-equal 1 (apply #'(setf cddar) (list 1 list)))
    (assert-equal '((((0 . 0) . (0 . 0)) . ((0 . 0) .        1)) .
                    (((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0))))
                  list)))

(define-test |setf cddar apply error 1|
  (let ((list 0))
    (assert-error 'type-error
                  (apply #'(setf cddar) (list 1 list)))))

(define-test |setf cddar apply error 2|
  (let ((list (copy-tree '(0                                           .
                           (((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0)))))))
    (assert-error 'type-error
                  (apply #'(setf cddar) (list 1 list)))))

(define-test |setf cddar apply error 3|
  (let ((list (copy-tree '((((0 . 0) . (0 . 0)) .                   0) .
                           (((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0)))))))
    (assert-error 'type-error
                  (apply #'(setf cddar) (list 1 list)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Tests for the (SETF CDDDR) function and SETF expander.

(define-test |setf cdddr 1|
  (let ((list (copy-tree '((((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0))) . 
                           (((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0)))))))
    (assert-equal 1 (setf (cdddr list) 1))
    (assert-equal '((((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0))) .
                    (((0 . 0) . (0 . 0)) . ((0 . 0) .        1)))
                  list)))

(define-test |setf cdddr error 1|
  (let ((list 0))
    (assert-error 'type-error
                  (setf (cdddr list) 1))))

(define-test |setf cdddr error 2|
  (let ((list (copy-tree '((((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0))) . 
                           (((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0)))))))
    (setf (cdr list) 0)
    (assert-error 'type-error
                  (setf (cdddr list) 1))))

(define-test |setf cdddr error 3|
  (let ((list (copy-tree '((((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0))) . 
                           (((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0)))))))
    (setf (cddr list) 0)
    (assert-error 'type-error
                  (setf (cdddr list) 1))))

(define-test |setf cdddr apply 1|
  (let ((list (copy-tree '((((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0))) . 
                           (((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0)))))))
    (assert-equal 1 (apply #'(setf cdddr) (list 1 list)))
    (assert-equal '((((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0))) .
                    (((0 . 0) . (0 . 0)) . ((0 . 0) .        1)))
                  list)))

(define-test |setf cdddr apply error 1|
  (let ((list 0))
    (assert-error 'type-error
                  (apply #'(setf cdddr) (list 1 list)))))

(define-test |setf cdddr apply error 2|
  (let ((list (copy-tree '((((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0))) . 
                           (((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0)))))))
    (setf (cdr list) 0)
    (assert-error 'type-error
                  (apply #'(setf cdddr) (list 1 list)))))

(define-test |setf cdddr apply error 3|
  (let ((list (copy-tree '((((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0))) . 
                           (((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0)))))))
    (setf (cddr list) 0)
    (assert-error 'type-error
                  (apply #'(setf cdddr) (list 1 list)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Tests for the (SETF CAAAAR) function and SETF expander.

(define-test |setf caaaar 1|
  (let ((list (copy-tree '((((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0))) . 
                           (((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0)))))))
    (assert-equal 1 (setf (caaaar list) 1))
    (assert-equal '((((1 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0))) .
                    (((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0))))
                  list)))

(define-test |setf caaaar error 1|
  (let ((list 0))
    (assert-error 'type-error
                  (setf (caaaar list) 1))))

(define-test |setf caaaar error 2|
  (let ((list (copy-tree '((((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0))) . 
                           (((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0)))))))
    (setf (car list) 0)
    (assert-error 'type-error
                  (setf (caaaar list) 1))))

(define-test |setf caaaar error 3|
  (let ((list (copy-tree '((((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0))) . 
                           (((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0)))))))
    (setf (caar list) 0)
    (assert-error 'type-error
                  (setf (caaaar list) 1))))

(define-test |setf caaaar error 4|
  (let ((list (copy-tree '((((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0))) . 
                           (((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0)))))))
    (setf (caaar list) 0)
    (assert-error 'type-error
                  (setf (caaaar list) 1))))

(define-test |setf caaaar apply 1|
  (let ((list (copy-tree '((((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0))) . 
                           (((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0)))))))
    (assert-equal 1 (apply #'(setf caaaar) (list 1 list)))
    (assert-equal '((((1 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0))) .
                    (((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0))))
                  list)))

(define-test |setf caaaar apply error 1|
  (let ((list 0))
    (assert-error 'type-error
                  (apply #'(setf caaaar) (list 1 list)))))

(define-test |setf caaaar apply error 2|
  (let ((list (copy-tree '((((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0))) . 
                           (((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0)))))))
    (setf (car list) 0)
    (assert-error 'type-error
                  (apply #'(setf caaaar) (list 1 list)))))

(define-test |setf caaaar apply error 3|
  (let ((list (copy-tree '((((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0))) . 
                           (((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0)))))))
    (setf (caar list) 0)
    (assert-error 'type-error
                  (apply #'(setf caaaar) (list 1 list)))))

(define-test |setf caaaar apply error 4|
  (let ((list (copy-tree '((((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0))) . 
                           (((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0)))))))
    (setf (caaar list) 0)
    (assert-error 'type-error
                  (apply #'(setf caaaar) (list 1 list)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Tests for the (SETF CAAADR) function and SETF expander.

(define-test |setf caaadr 1|
  (let ((list (copy-tree '((((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0))) . 
                           (((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0)))))))
    (assert-equal 1 (setf (caaadr list) 1))
    (assert-equal '((((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0))) .
                    (((1 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0))))
                  list)))

(define-test |setf caaadr error 1|
  (let ((list 0))
    (assert-error 'type-error
                  (setf (caaadr list) 1))))

(define-test |setf caaadr error 2|
  (let ((list (copy-tree '((((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0))) . 
                           (((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0)))))))
    (setf (cdr list) 0)
    (assert-error 'type-error
                  (setf (caaadr list) 1))))

(define-test |setf caaadr error 3|
  (let ((list (copy-tree '((((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0))) . 
                           (((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0)))))))
    (setf (cadr list) 0)
    (assert-error 'type-error
                  (setf (caaadr list) 1))))

(define-test |setf caaadr error 4|
  (let ((list (copy-tree '((((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0))) . 
                           (((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0)))))))
    (setf (caadr list) 0)
    (assert-error 'type-error
                  (setf (caaadr list) 1))))

(define-test |setf caaadr apply 1|
  (let ((list (copy-tree '((((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0))) . 
                           (((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0)))))))
    (assert-equal 1 (apply #'(setf caaadr) (list 1 list)))
    (assert-equal '((((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0))) .
                    (((1 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0))))
                  list)))

(define-test |setf caaadr apply error 1|
  (let ((list 0))
    (assert-error 'type-error
                  (apply #'(setf caaadr) (list 1 list)))))

(define-test |setf caaadr apply error 2|
  (let ((list (copy-tree '((((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0))) . 
                           (((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0)))))))
    (setf (cdr list) 0)
    (assert-error 'type-error
                  (apply #'(setf caaadr) (list 1 list)))))

(define-test |setf caaadr apply error 3|
  (let ((list (copy-tree '((((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0))) . 
                           (((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0)))))))
    (setf (cadr list) 0)
    (assert-error 'type-error
                  (apply #'(setf caaadr) (list 1 list)))))

(define-test |setf caaadr apply error 4|
  (let ((list (copy-tree '((((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0))) . 
                           (((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0)))))))
    (setf (caadr list) 0)
    (assert-error 'type-error
                  (apply #'(setf caaadr) (list 1 list)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Tests for the (SETF CAADAR) function and SETF expander.

(define-test |setf caadar 1|
  (let ((list (copy-tree '((((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0))) . 
                           (((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0)))))))
    (assert-equal 1 (setf (caadar list) 1))
    (assert-equal '((((0 . 0) . (0 . 0)) . ((1 . 0) . (0 . 0))) .
                    (((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0))))
                  list)))

(define-test |setf caadar error 1|
  (let ((list 0))
    (assert-error 'type-error
                  (setf (caadar list) 1))))

(define-test |setf caadar error 2|
  (let ((list (copy-tree '((((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0))) . 
                           (((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0)))))))
    (setf (car list) 0)
    (assert-error 'type-error
                  (setf (caadar list) 1))))

(define-test |setf caadar error 3|
  (let ((list (copy-tree '((((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0))) . 
                           (((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0)))))))
    (setf (cdar list) 0)
    (assert-error 'type-error
                  (setf (caadar list) 1))))

(define-test |setf caadar error 4|
  (let ((list (copy-tree '((((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0))) . 
                           (((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0)))))))
    (setf (cadar list) 0)
    (assert-error 'type-error
                  (setf (caadar list) 1))))

(define-test |setf caadar apply 1|
  (let ((list (copy-tree '((((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0))) . 
                           (((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0)))))))
    (assert-equal 1 (apply #'(setf caadar) (list 1 list)))
    (assert-equal '((((0 . 0) . (0 . 0)) . ((1 . 0) . (0 . 0))) .
                    (((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0))))
                  list)))

(define-test |setf caadar apply error 1|
  (let ((list 0))
    (assert-error 'type-error
                  (apply #'(setf caadar) (list 1 list)))))

(define-test |setf caadar apply error 2|
  (let ((list (copy-tree '((((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0))) . 
                           (((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0)))))))
    (setf (car list) 0)
    (assert-error 'type-error
                  (apply #'(setf caadar) (list 1 list)))))

(define-test |setf caadar apply error 3|
  (let ((list (copy-tree '((((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0))) . 
                           (((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0)))))))
    (setf (cdar list) 0)
    (assert-error 'type-error
                  (apply #'(setf caadar) (list 1 list)))))

(define-test |setf caadar apply error 4|
  (let ((list (copy-tree '((((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0))) . 
                           (((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0)))))))
    (setf (cadar list) 0)
    (assert-error 'type-error
                  (apply #'(setf caadar) (list 1 list)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Tests for the (SETF CAADDR) function and SETF expander.

(define-test |setf caaddr 1|
  (let ((list (copy-tree '((((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0))) . 
                           (((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0)))))))
    (assert-equal 1 (setf (caaddr list) 1))
    (assert-equal '((((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0))) .
                    (((0 . 0) . (0 . 0)) . ((1 . 0) . (0 . 0))))
                  list)))

(define-test |setf caaddr error 1|
  (let ((list 0))
    (assert-error 'type-error
                  (setf (caaddr list) 1))))

(define-test |setf caaddr error 2|
  (let ((list (copy-tree '((((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0))) . 
                           (((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0)))))))
    (setf (cdr list) 0)
    (assert-error 'type-error
                  (setf (caaddr list) 1))))

(define-test |setf caaddr error 3|
  (let ((list (copy-tree '((((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0))) . 
                           (((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0)))))))
    (setf (cddr list) 0)
    (assert-error 'type-error
                  (setf (caaddr list) 1))))

(define-test |setf caaddr error 4|
  (let ((list (copy-tree '((((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0))) . 
                           (((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0)))))))
    (setf (caddr list) 0)
    (assert-error 'type-error
                  (setf (caaddr list) 1))))

(define-test |setf caaddr apply 1|
  (let ((list (copy-tree '((((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0))) . 
                           (((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0)))))))
    (assert-equal 1 (apply #'(setf caaddr) (list 1 list)))
    (assert-equal '((((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0))) .
                    (((0 . 0) . (0 . 0)) . ((1 . 0) . (0 . 0))))
                  list)))

(define-test |setf caaddr apply error 1|
  (let ((list 0))
    (assert-error 'type-error
                  (apply #'(setf caaddr) (list 1 list)))))

(define-test |setf caaddr apply error 2|
  (let ((list (copy-tree '((((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0))) . 
                           (((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0)))))))
    (setf (cdr list) 0)
    (assert-error 'type-error
                  (apply #'(setf caaddr) (list 1 list)))))

(define-test |setf caaddr apply error 3|
  (let ((list (copy-tree '((((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0))) . 
                           (((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0)))))))
    (setf (cddr list) 0)
    (assert-error 'type-error
                  (apply #'(setf caaddr) (list 1 list)))))

(define-test |setf caaddr apply error 4|
  (let ((list (copy-tree '((((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0))) . 
                           (((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0)))))))
    (setf (caddr list) 0)
    (assert-error 'type-error
                  (apply #'(setf caaddr) (list 1 list)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Tests for the (SETF CADAAR) function and SETF expander.

(define-test |setf cadaar 1|
  (let ((list (copy-tree '((((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0))) . 
                           (((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0)))))))
    (assert-equal 1 (setf (cadaar list) 1))
    (assert-equal '((((0 . 0) . (1 . 0)) . ((0 . 0) . (0 . 0))) .
                    (((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0))))
                  list)))

(define-test |setf cadaar error 1|
  (let ((list 0))
    (assert-error 'type-error
                  (setf (cadaar list) 1))))

(define-test |setf cadaar error 2|
  (let ((list (copy-tree '((((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0))) . 
                           (((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0)))))))
    (setf (car list) 0)
    (assert-error 'type-error
                  (setf (cadaar list) 1))))

(define-test |setf cadaar error 3|
  (let ((list (copy-tree '((((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0))) . 
                           (((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0)))))))
    (setf (caar list) 0)
    (assert-error 'type-error
                  (setf (cadaar list) 1))))

(define-test |setf cadaar error 4|
  (let ((list (copy-tree '((((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0))) . 
                           (((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0)))))))
    (setf (cdaar list) 0)
    (assert-error 'type-error
                  (setf (cadaar list) 1))))

(define-test |setf cadaar apply 1|
  (let ((list (copy-tree '((((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0))) . 
                           (((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0)))))))
    (assert-equal 1 (apply #'(setf cadaar) (list 1 list)))
    (assert-equal '((((0 . 0) . (1 . 0)) . ((0 . 0) . (0 . 0))) .
                    (((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0))))
                  list)))

(define-test |setf cadaar apply error 1|
  (let ((list 0))
    (assert-error 'type-error
                  (apply #'(setf cadaar) (list 1 list)))))

(define-test |setf cadaar apply error 2|
  (let ((list (copy-tree '((((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0))) . 
                           (((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0)))))))
    (setf (car list) 0)
    (assert-error 'type-error
                  (apply #'(setf cadaar) (list 1 list)))))

(define-test |setf cadaar apply error 3|
  (let ((list (copy-tree '((((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0))) . 
                           (((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0)))))))
    (setf (caar list) 0)
    (assert-error 'type-error
                  (apply #'(setf cadaar) (list 1 list)))))

(define-test |setf cadaar apply error 4|
  (let ((list (copy-tree '((((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0))) . 
                           (((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0)))))))
    (setf (cdaar list) 0)
    (assert-error 'type-error
                  (apply #'(setf cadaar) (list 1 list)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Tests for the (SETF CADADR) function and SETF expander.

(define-test |setf cadadr 1|
  (let ((list (copy-tree '((((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0))) . 
                           (((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0)))))))
    (assert-equal 1 (setf (cadadr list) 1))
    (assert-equal '((((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0))) .
                    (((0 . 0) . (1 . 0)) . ((0 . 0) . (0 . 0))))
                  list)))

(define-test |setf cadadr error 1|
  (let ((list 0))
    (assert-error 'type-error
                  (setf (cadadr list) 1))))

(define-test |setf cadadr error 2|
  (let ((list (copy-tree '((((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0))) . 
                           (((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0)))))))
    (setf (cdr list) 0)
    (assert-error 'type-error
                  (setf (cadadr list) 1))))

(define-test |setf cadadr error 3|
  (let ((list (copy-tree '((((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0))) . 
                           (((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0)))))))
    (setf (cadr list) 0)
    (assert-error 'type-error
                  (setf (cadadr list) 1))))

(define-test |setf cadadr error 4|
  (let ((list (copy-tree '((((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0))) . 
                           (((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0)))))))
    (setf (cdadr list) 0)
    (assert-error 'type-error
                  (setf (cadadr list) 1))))

(define-test |setf cadadr apply 1|
  (let ((list (copy-tree '((((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0))) . 
                           (((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0)))))))
    (assert-equal 1 (apply #'(setf cadadr) (list 1 list)))
    (assert-equal '((((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0))) .
                    (((0 . 0) . (1 . 0)) . ((0 . 0) . (0 . 0))))
                  list)))

(define-test |setf cadadr apply error 1|
  (let ((list 0))
    (assert-error 'type-error
                  (apply #'(setf cadadr) (list 1 list)))))

(define-test |setf cadadr apply error 2|
  (let ((list (copy-tree '((((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0))) . 
                           (((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0)))))))
    (setf (cdr list) 0)
    (assert-error 'type-error
                  (apply #'(setf cadadr) (list 1 list)))))

(define-test |setf cadadr apply error 3|
  (let ((list (copy-tree '((((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0))) . 
                           (((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0)))))))
    (setf (cadr list) 0)
    (assert-error 'type-error
                  (apply #'(setf cadadr) (list 1 list)))))

(define-test |setf cadadr apply error 4|
  (let ((list (copy-tree '((((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0))) . 
                           (((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0)))))))
    (setf (cdadr list) 0)
    (assert-error 'type-error
                  (apply #'(setf cadadr) (list 1 list)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Tests for the (SETF CADDAR) function and SETF expander.

(define-test |setf caddar 1|
  (let ((list (copy-tree '((((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0))) . 
                           (((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0)))))))
    (assert-equal 1 (setf (caddar list) 1))
    (assert-equal '((((0 . 0) . (0 . 0)) . ((0 . 0) . (1 . 0))) .
                    (((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0))))
                  list)))

(define-test |setf caddar error 1|
  (let ((list 0))
    (assert-error 'type-error
                  (setf (caddar list) 1))))

(define-test |setf caddar error 2|
  (let ((list (copy-tree '((((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0))) . 
                           (((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0)))))))
    (setf (car list) 0)
    (assert-error 'type-error
                  (setf (caddar list) 1))))

(define-test |setf caddar error 3|
  (let ((list (copy-tree '((((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0))) . 
                           (((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0)))))))
    (setf (cdar list) 0)
    (assert-error 'type-error
                  (setf (caddar list) 1))))

(define-test |setf caddar error 4|
  (let ((list (copy-tree '((((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0))) . 
                           (((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0)))))))
    (setf (cddar list) 0)
    (assert-error 'type-error
                  (setf (caddar list) 1))))

(define-test |setf caddar apply 1|
  (let ((list (copy-tree '((((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0))) . 
                           (((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0)))))))
    (assert-equal 1 (apply #'(setf caddar) (list 1 list)))
    (assert-equal '((((0 . 0) . (0 . 0)) . ((0 . 0) . (1 . 0))) .
                    (((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0))))
                  list)))

(define-test |setf caddar apply error 1|
  (let ((list 0))
    (assert-error 'type-error
                  (apply #'(setf caddar) (list 1 list)))))

(define-test |setf caddar apply error 2|
  (let ((list (copy-tree '((((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0))) . 
                           (((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0)))))))
    (setf (car list) 0)
    (assert-error 'type-error
                  (apply #'(setf caddar) (list 1 list)))))

(define-test |setf caddar apply error 3|
  (let ((list (copy-tree '((((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0))) . 
                           (((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0)))))))
    (setf (cdar list) 0)
    (assert-error 'type-error
                  (apply #'(setf caddar) (list 1 list)))))

(define-test |setf caddar apply error 4|
  (let ((list (copy-tree '((((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0))) . 
                           (((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0)))))))
    (setf (cddar list) 0)
    (assert-error 'type-error
                  (apply #'(setf caddar) (list 1 list)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Tests for the (SETF CADDDR) function and SETF expander.

(define-test |setf cadddr 1|
  (let ((list (copy-tree '((((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0))) . 
                           (((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0)))))))
    (assert-equal 1 (setf (cadddr list) 1))
    (assert-equal '((((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0))) .
                    (((0 . 0) . (0 . 0)) . ((0 . 0) . (1 . 0))))
                  list)))

(define-test |setf cadddr error 1|
  (let ((list 0))
    (assert-error 'type-error
                  (setf (cadddr list) 1))))

(define-test |setf cadddr error 2|
  (let ((list (copy-tree '((((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0))) . 
                           (((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0)))))))
    (setf (cdr list) 0)
    (assert-error 'type-error
                  (setf (cadddr list) 1))))

(define-test |setf cadddr error 3|
  (let ((list (copy-tree '((((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0))) . 
                           (((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0)))))))
    (setf (cddr list) 0)
    (assert-error 'type-error
                  (setf (cadddr list) 1))))

(define-test |setf cadddr error 4|
  (let ((list (copy-tree '((((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0))) . 
                           (((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0)))))))
    (setf (cdddr list) 0)
    (assert-error 'type-error
                  (setf (cadddr list) 1))))

(define-test |setf cadddr apply 1|
  (let ((list (copy-tree '((((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0))) . 
                           (((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0)))))))
    (assert-equal 1 (apply #'(setf cadddr) (list 1 list)))
    (assert-equal '((((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0))) .
                    (((0 . 0) . (0 . 0)) . ((0 . 0) . (1 . 0))))
                  list)))

(define-test |setf cadddr apply error 1|
  (let ((list 0))
    (assert-error 'type-error
                  (apply #'(setf cadddr) (list 1 list)))))

(define-test |setf cadddr apply error 2|
  (let ((list (copy-tree '((((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0))) . 
                           (((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0)))))))
    (setf (cdr list) 0)
    (assert-error 'type-error
                  (apply #'(setf cadddr) (list 1 list)))))

(define-test |setf cadddr apply error 3|
  (let ((list (copy-tree '((((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0))) . 
                           (((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0)))))))
    (setf (cddr list) 0)
    (assert-error 'type-error
                  (apply #'(setf cadddr) (list 1 list)))))

(define-test |setf cadddr apply error 4|
  (let ((list (copy-tree '((((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0))) . 
                           (((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0)))))))
    (setf (cdddr list) 0)
    (assert-error 'type-error
                  (apply #'(setf cadddr) (list 1 list)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Tests for the (SETF CDAAAR) function and SETF expander.

(define-test |setf cdaaar 1|
  (let ((list (copy-tree '((((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0))) . 
                           (((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0)))))))
    (assert-equal 1 (setf (cdaaar list) 1))
    (assert-equal '((((0 . 1) . (0 . 0)) . ((0 . 0) . (0 . 0))) .
                    (((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0))))
                  list)))

(define-test |setf cdaaar error 1|
  (let ((list 0))
    (assert-error 'type-error
                  (setf (cdaaar list) 1))))

(define-test |setf cdaaar error 2|
  (let ((list (copy-tree '((((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0))) . 
                           (((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0)))))))
    (setf (car list) 0)
    (assert-error 'type-error
                  (setf (cdaaar list) 1))))

(define-test |setf cdaaar error 3|
  (let ((list (copy-tree '((((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0))) . 
                           (((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0)))))))
    (setf (caar list) 0)
    (assert-error 'type-error
                  (setf (cdaaar list) 1))))

(define-test |setf cdaaar error 4|
  (let ((list (copy-tree '((((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0))) . 
                           (((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0)))))))
    (setf (caaar list) 0)
    (assert-error 'type-error
                  (setf (cdaaar list) 1))))

(define-test |setf cdaaar apply 1|
  (let ((list (copy-tree '((((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0))) . 
                           (((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0)))))))
    (assert-equal 1 (apply #'(setf cdaaar) (list 1 list)))
    (assert-equal '((((0 . 1) . (0 . 0)) . ((0 . 0) . (0 . 0))) .
                    (((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0))))
                  list)))

(define-test |setf cdaaar apply error 1|
  (let ((list 0))
    (assert-error 'type-error
                  (apply #'(setf cdaaar) (list 1 list)))))

(define-test |setf cdaaar apply error 2|
  (let ((list (copy-tree '((((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0))) . 
                           (((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0)))))))
    (setf (car list) 0)
    (assert-error 'type-error
                  (apply #'(setf cdaaar) (list 1 list)))))

(define-test |setf cdaaar apply error 3|
  (let ((list (copy-tree '((((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0))) . 
                           (((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0)))))))
    (setf (caar list) 0)
    (assert-error 'type-error
                  (apply #'(setf cdaaar) (list 1 list)))))

(define-test |setf cdaaar apply error 4|
  (let ((list (copy-tree '((((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0))) . 
                           (((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0)))))))
    (setf (caaar list) 0)
    (assert-error 'type-error
                  (apply #'(setf cdaaar) (list 1 list)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Tests for the (SETF CDAADR) function and SETF expander.

(define-test |setf cdaadr 1|
  (let ((list (copy-tree '((((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0))) . 
                           (((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0)))))))
    (assert-equal 1 (setf (cdaadr list) 1))
    (assert-equal '((((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0))) .
                    (((0 . 1) . (0 . 0)) . ((0 . 0) . (0 . 0))))
                  list)))

(define-test |setf cdaadr error 1|
  (let ((list 0))
    (assert-error 'type-error
                  (setf (cdaadr list) 1))))

(define-test |setf cdaadr error 2|
  (let ((list (copy-tree '((((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0))) . 
                           (((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0)))))))
    (setf (cdr list) 0)
    (assert-error 'type-error
                  (setf (cdaadr list) 1))))

(define-test |setf cdaadr error 3|
  (let ((list (copy-tree '((((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0))) . 
                           (((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0)))))))
    (setf (cadr list) 0)
    (assert-error 'type-error
                  (setf (cdaadr list) 1))))

(define-test |setf cdaadr error 4|
  (let ((list (copy-tree '((((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0))) . 
                           (((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0)))))))
    (setf (caadr list) 0)
    (assert-error 'type-error
                  (setf (cdaadr list) 1))))

(define-test |setf cdaadr apply 1|
  (let ((list (copy-tree '((((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0))) . 
                           (((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0)))))))
    (assert-equal 1 (apply #'(setf cdaadr) (list 1 list)))
    (assert-equal '((((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0))) .
                    (((0 . 1) . (0 . 0)) . ((0 . 0) . (0 . 0))))
                  list)))

(define-test |setf cdaadr apply error 1|
  (let ((list 0))
    (assert-error 'type-error
                  (apply #'(setf cdaadr) (list 1 list)))))

(define-test |setf cdaadr apply error 2|
  (let ((list (copy-tree '((((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0))) . 
                           (((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0)))))))
    (setf (cdr list) 0)
    (assert-error 'type-error
                  (apply #'(setf cdaadr) (list 1 list)))))

(define-test |setf cdaadr apply error 3|
  (let ((list (copy-tree '((((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0))) . 
                           (((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0)))))))
    (setf (cadr list) 0)
    (assert-error 'type-error
                  (apply #'(setf cdaadr) (list 1 list)))))

(define-test |setf cdaadr apply error 4|
  (let ((list (copy-tree '((((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0))) . 
                           (((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0)))))))
    (setf (caadr list) 0)
    (assert-error 'type-error
                  (apply #'(setf cdaadr) (list 1 list)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Tests for the (SETF CDADAR) function and SETF expander.

(define-test |setf cdadar 1|
  (let ((list (copy-tree '((((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0))) . 
                           (((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0)))))))
    (assert-equal 1 (setf (cdadar list) 1))
    (assert-equal '((((0 . 0) . (0 . 0)) . ((0 . 1) . (0 . 0))) .
                    (((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0))))
                  list)))

(define-test |setf cdadar error 1|
  (let ((list 0))
    (assert-error 'type-error
                  (setf (cdadar list) 1))))

(define-test |setf cdadar error 2|
  (let ((list (copy-tree '((((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0))) . 
                           (((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0)))))))
    (setf (car list) 0)
    (assert-error 'type-error
                  (setf (cdadar list) 1))))

(define-test |setf cdadar error 3|
  (let ((list (copy-tree '((((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0))) . 
                           (((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0)))))))
    (setf (cdar list) 0)
    (assert-error 'type-error
                  (setf (cdadar list) 1))))

(define-test |setf cdadar error 4|
  (let ((list (copy-tree '((((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0))) . 
                           (((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0)))))))
    (setf (cadar list) 0)
    (assert-error 'type-error
                  (setf (cdadar list) 1))))

(define-test |setf cdadar apply 1|
  (let ((list (copy-tree '((((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0))) . 
                           (((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0)))))))
    (assert-equal 1 (apply #'(setf cdadar) (list 1 list)))
    (assert-equal '((((0 . 0) . (0 . 0)) . ((0 . 1) . (0 . 0))) .
                    (((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0))))
                  list)))

(define-test |setf cdadar apply error 1|
  (let ((list 0))
    (assert-error 'type-error
                  (apply #'(setf cdadar) (list 1 list)))))

(define-test |setf cdadar apply error 2|
  (let ((list (copy-tree '((((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0))) . 
                           (((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0)))))))
    (setf (car list) 0)
    (assert-error 'type-error
                  (apply #'(setf cdadar) (list 1 list)))))

(define-test |setf cdadar apply error 3|
  (let ((list (copy-tree '((((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0))) . 
                           (((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0)))))))
    (setf (cdar list) 0)
    (assert-error 'type-error
                  (apply #'(setf cdadar) (list 1 list)))))

(define-test |setf cdadar apply error 4|
  (let ((list (copy-tree '((((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0))) . 
                           (((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0)))))))
    (setf (cadar list) 0)
    (assert-error 'type-error
                  (apply #'(setf cdadar) (list 1 list)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Tests for the (SETF CDADDR) function and SETF expander.

(define-test |setf cdaddr 1|
  (let ((list (copy-tree '((((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0))) . 
                           (((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0)))))))
    (assert-equal 1 (setf (cdaddr list) 1))
    (assert-equal '((((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0))) .
                    (((0 . 0) . (0 . 0)) . ((0 . 1) . (0 . 0))))
                  list)))

(define-test |setf cdaddr error 1|
  (let ((list 0))
    (assert-error 'type-error
                  (setf (cdaddr list) 1))))

(define-test |setf cdaddr error 2|
  (let ((list (copy-tree '((((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0))) . 
                           (((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0)))))))
    (setf (cdr list) 0)
    (assert-error 'type-error
                  (setf (cdaddr list) 1))))

(define-test |setf cdaddr error 3|
  (let ((list (copy-tree '((((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0))) . 
                           (((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0)))))))
    (setf (cddr list) 0)
    (assert-error 'type-error
                  (setf (cdaddr list) 1))))

(define-test |setf cdaddr error 4|
  (let ((list (copy-tree '((((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0))) . 
                           (((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0)))))))
    (setf (caddr list) 0)
    (assert-error 'type-error
                  (setf (cdaddr list) 1))))

(define-test |setf cdaddr apply 1|
  (let ((list (copy-tree '((((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0))) . 
                           (((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0)))))))
    (assert-equal 1 (apply #'(setf cdaddr) (list 1 list)))
    (assert-equal '((((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0))) .
                    (((0 . 0) . (0 . 0)) . ((0 . 1) . (0 . 0))))
                  list)))

(define-test |setf cdaddr apply error 1|
  (let ((list 0))
    (assert-error 'type-error
                  (apply #'(setf cdaddr) (list 1 list)))))

(define-test |setf cdaddr apply error 2|
  (let ((list (copy-tree '((((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0))) . 
                           (((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0)))))))
    (setf (cdr list) 0)
    (assert-error 'type-error
                  (apply #'(setf cdaddr) (list 1 list)))))

(define-test |setf cdaddr apply error 3|
  (let ((list (copy-tree '((((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0))) . 
                           (((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0)))))))
    (setf (cddr list) 0)
    (assert-error 'type-error
                  (apply #'(setf cdaddr) (list 1 list)))))

(define-test |setf cdaddr apply error 4|
  (let ((list (copy-tree '((((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0))) . 
                           (((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0)))))))
    (setf (caddr list) 0)
    (assert-error 'type-error
                  (apply #'(setf cdaddr) (list 1 list)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Tests for the (SETF CDDAAR) function and SETF expander.

(define-test |setf cddaar 1|
  (let ((list (copy-tree '((((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0))) . 
                           (((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0)))))))
    (assert-equal 1 (setf (cddaar list) 1))
    (assert-equal '((((0 . 0) . (0 . 1)) . ((0 . 0) . (0 . 0))) .
                    (((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0))))
                  list)))

(define-test |setf cddaar error 1|
  (let ((list 0))
    (assert-error 'type-error
                  (setf (cddaar list) 1))))

(define-test |setf cddaar error 2|
  (let ((list (copy-tree '((((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0))) . 
                           (((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0)))))))
    (setf (car list) 0)
    (assert-error 'type-error
                  (setf (cddaar list) 1))))

(define-test |setf cddaar error 3|
  (let ((list (copy-tree '((((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0))) . 
                           (((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0)))))))
    (setf (caar list) 0)
    (assert-error 'type-error
                  (setf (cddaar list) 1))))

(define-test |setf cddaar error 4|
  (let ((list (copy-tree '((((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0))) . 
                           (((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0)))))))
    (setf (cdaar list) 0)
    (assert-error 'type-error
                  (setf (cddaar list) 1))))

(define-test |setf cddaar apply 1|
  (let ((list (copy-tree '((((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0))) . 
                           (((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0)))))))
    (assert-equal 1 (apply #'(setf cddaar) (list 1 list)))
    (assert-equal '((((0 . 0) . (0 . 1)) . ((0 . 0) . (0 . 0))) .
                    (((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0))))
                  list)))

(define-test |setf cddaar apply error 1|
  (let ((list 0))
    (assert-error 'type-error
                  (apply #'(setf cddaar) (list 1 list)))))

(define-test |setf cddaar apply error 2|
  (let ((list (copy-tree '((((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0))) . 
                           (((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0)))))))
    (setf (car list) 0)
    (assert-error 'type-error
                  (apply #'(setf cddaar) (list 1 list)))))

(define-test |setf cddaar apply error 3|
  (let ((list (copy-tree '((((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0))) . 
                           (((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0)))))))
    (setf (caar list) 0)
    (assert-error 'type-error
                  (apply #'(setf cddaar) (list 1 list)))))

(define-test |setf cddaar apply error 4|
  (let ((list (copy-tree '((((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0))) . 
                           (((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0)))))))
    (setf (cdaar list) 0)
    (assert-error 'type-error
                  (apply #'(setf cddaar) (list 1 list)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Tests for the (SETF CDDADR) function and SETF expander.

(define-test |setf cddadr 1|
  (let ((list (copy-tree '((((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0))) . 
                           (((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0)))))))
    (assert-equal 1 (setf (cddadr list) 1))
    (assert-equal '((((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0))) .
                    (((0 . 0) . (0 . 1)) . ((0 . 0) . (0 . 0))))
                  list)))

(define-test |setf cddadr error 1|
  (let ((list 0))
    (assert-error 'type-error
                  (setf (cddadr list) 1))))

(define-test |setf cddadr error 2|
  (let ((list (copy-tree '((((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0))) . 
                           (((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0)))))))
    (setf (cdr list) 0)
    (assert-error 'type-error
                  (setf (cddadr list) 1))))

(define-test |setf cddadr error 3|
  (let ((list (copy-tree '((((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0))) . 
                           (((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0)))))))
    (setf (cadr list) 0)
    (assert-error 'type-error
                  (setf (cddadr list) 1))))

(define-test |setf cddadr error 4|
  (let ((list (copy-tree '((((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0))) . 
                           (((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0)))))))
    (setf (cdadr list) 0)
    (assert-error 'type-error
                  (setf (cddadr list) 1))))

(define-test |setf cddadr apply 1|
  (let ((list (copy-tree '((((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0))) . 
                           (((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0)))))))
    (assert-equal 1 (apply #'(setf cddadr) (list 1 list)))
    (assert-equal '((((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0))) .
                    (((0 . 0) . (0 . 1)) . ((0 . 0) . (0 . 0))))
                  list)))

(define-test |setf cddadr apply error 1|
  (let ((list 0))
    (assert-error 'type-error
                  (apply #'(setf cddadr) (list 1 list)))))

(define-test |setf cddadr apply error 2|
  (let ((list (copy-tree '((((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0))) . 
                           (((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0)))))))
    (setf (cdr list) 0)
    (assert-error 'type-error
                  (apply #'(setf cddadr) (list 1 list)))))

(define-test |setf cddadr apply error 3|
  (let ((list (copy-tree '((((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0))) . 
                           (((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0)))))))
    (setf (cadr list) 0)
    (assert-error 'type-error
                  (apply #'(setf cddadr) (list 1 list)))))

(define-test |setf cddadr apply error 4|
  (let ((list (copy-tree '((((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0))) . 
                           (((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0)))))))
    (setf (cdadr list) 0)
    (assert-error 'type-error
                  (apply #'(setf cddadr) (list 1 list)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Tests for the (SETF CDDDAR) function and SETF expander.

(define-test |setf cdddar 1|
  (let ((list (copy-tree '((((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0))) . 
                           (((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0)))))))
    (assert-equal 1 (setf (cdddar list) 1))
    (assert-equal '((((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 1))) .
                    (((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0))))
                  list)))

(define-test |setf cdddar error 1|
  (let ((list 0))
    (assert-error 'type-error
                  (setf (cdddar list) 1))))

(define-test |setf cdddar error 2|
  (let ((list (copy-tree '((((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0))) . 
                           (((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0)))))))
    (setf (car list) 0)
    (assert-error 'type-error
                  (setf (cdddar list) 1))))

(define-test |setf cdddar error 3|
  (let ((list (copy-tree '((((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0))) . 
                           (((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0)))))))
    (setf (cdar list) 0)
    (assert-error 'type-error
                  (setf (cdddar list) 1))))

(define-test |setf cdddar error 4|
  (let ((list (copy-tree '((((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0))) . 
                           (((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0)))))))
    (setf (cddar list) 0)
    (assert-error 'type-error
                  (setf (cdddar list) 1))))

(define-test |setf cdddar apply 1|
  (let ((list (copy-tree '((((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0))) . 
                           (((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0)))))))
    (assert-equal 1 (apply #'(setf cdddar) (list 1 list)))
    (assert-equal '((((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 1))) .
                    (((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0))))
                  list)))

(define-test |setf cdddar apply error 1|
  (let ((list 0))
    (assert-error 'type-error
                  (apply #'(setf cdddar) (list 1 list)))))

(define-test |setf cdddar apply error 2|
  (let ((list (copy-tree '((((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0))) . 
                           (((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0)))))))
    (setf (car list) 0)
    (assert-error 'type-error
                  (apply #'(setf cdddar) (list 1 list)))))

(define-test |setf cdddar apply error 3|
  (let ((list (copy-tree '((((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0))) . 
                           (((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0)))))))
    (setf (cdar list) 0)
    (assert-error 'type-error
                  (apply #'(setf cdddar) (list 1 list)))))

(define-test |setf cdddar apply error 4|
  (let ((list (copy-tree '((((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0))) . 
                           (((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0)))))))
    (setf (cddar list) 0)
    (assert-error 'type-error
                  (apply #'(setf cdddar) (list 1 list)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Tests for the (SETF CDDDDR) function and SETF expander.

(define-test |setf cddddr 1|
  (let ((list (copy-tree '((((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0))) . 
                           (((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0)))))))
    (assert-equal 1 (setf (cddddr list) 1))
    (assert-equal '((((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0))) .
                    (((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 1))))
                  list)))

(define-test |setf cddddr error 1|
  (let ((list 0))
    (assert-error 'type-error
                  (setf (cddddr list) 1))))

(define-test |setf cddddr error 2|
  (let ((list (copy-tree '((((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0))) . 
                           (((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0)))))))
    (setf (cdr list) 0)
    (assert-error 'type-error
                  (setf (cddddr list) 1))))

(define-test |setf cddddr error 3|
  (let ((list (copy-tree '((((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0))) . 
                           (((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0)))))))
    (setf (cddr list) 0)
    (assert-error 'type-error
                  (setf (cddddr list) 1))))

(define-test |setf cddddr error 4|
  (let ((list (copy-tree '((((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0))) . 
                           (((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0)))))))
    (setf (cdddr list) 0)
    (assert-error 'type-error
                  (setf (cddddr list) 1))))

(define-test |setf cddddr apply 1|
  (let ((list (copy-tree '((((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0))) . 
                           (((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0)))))))
    (assert-equal 1 (apply #'(setf cddddr) (list 1 list)))
    (assert-equal '((((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0))) .
                    (((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 1))))
                  list)))

(define-test |setf cddddr apply error 1|
  (let ((list 0))
    (assert-error 'type-error
                  (apply #'(setf cddddr) (list 1 list)))))

(define-test |setf cddddr apply error 2|
  (let ((list (copy-tree '((((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0))) . 
                           (((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0)))))))
    (setf (cdr list) 0)
    (assert-error 'type-error
                  (apply #'(setf cddddr) (list 1 list)))))

(define-test |setf cddddr apply error 3|
  (let ((list (copy-tree '((((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0))) . 
                           (((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0)))))))
    (setf (cddr list) 0)
    (assert-error 'type-error
                  (apply #'(setf cddddr) (list 1 list)))))

(define-test |setf cddddr apply error 4|
  (let ((list (copy-tree '((((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0))) . 
                           (((0 . 0) . (0 . 0)) . ((0 . 0) . (0 . 0)))))))
    (setf (cdddr list) 0)
    (assert-error 'type-error
                  (apply #'(setf cddddr) (list 1 list)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Tests for the (SETF FIRST) function and SETF expander.

(define-test |setf first 1|
  (let ((list (copy-tree '(0 0 0 0 0 0 0 0 0 0))))
    (assert-equal 1 (setf (first list) 1))
    (assert-equal '(1 0 0 0 0 0 0 0 0 0) list)))

(define-test |setf first error 1|
  (let ((list '()))
    (assert-error 'type-error
                  (setf (first list) 1))))
                  
(define-test |setf first 1|
  (let ((list (copy-tree '(0 0 0 0 0 0 0 0 0 0))))
    (assert-equal 1 (setf (first list) 1))
    (assert-equal '(1 0 0 0 0 0 0 0 0 0) list)))

(define-test |setf first error 1|
  (let ((list '()))
    (assert-error 'type-error
                  (setf (first list) 1))))
                  
(define-test |setf first apply 1|
  (let ((list (copy-tree '(0 0 0 0 0 0 0 0 0 0))))
    (assert-equal 1 (apply #'(setf first) (list 1 list)))
    (assert-equal '(1 0 0 0 0 0 0 0 0 0) list)))

(define-test |setf first apply error 1|
  (let ((list '()))
    (assert-error 'type-error
                  (apply #'(setf first) (list 1 list)))))
                  
(define-test |setf first apply 1|
  (let ((list (copy-tree '(0 0 0 0 0 0 0 0 0 0))))
    (assert-equal 1 (apply #'(setf first) (list 1 list)))
    (assert-equal '(1 0 0 0 0 0 0 0 0 0) list)))

(define-test |setf first apply error 1|
  (let ((list '()))
    (assert-error 'type-error
                  (apply #'(setf first) (list 1 list)))))
                  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Tests for the (SETF SECOND) function and SETF expander.

(define-test |setf second 1|
  (let ((list (copy-tree '(0 0 0 0 0 0 0 0 0 0))))
    (assert-equal 1 (setf (second list) 1))
    (assert-equal '(0 1 0 0 0 0 0 0 0 0) list)))

(define-test |setf second error 1|
  (let ((list (copy-tree '(0 0 0 0 0 0 0 0 0 0))))
    (setf (cdr (nthcdr 0 list)) 1)
    (assert-error 'type-error
                  (setf (second list) 1))))
                  
(define-test |setf second 1|
  (let ((list (copy-tree '(0 0 0 0 0 0 0 0 0 0))))
    (assert-equal 1 (setf (second list) 1))
    (assert-equal '(0 1 0 0 0 0 0 0 0 0) list)))

(define-test |setf second error 1|
  (let ((list (copy-tree '(0 0 0 0 0 0 0 0 0 0))))
    (setf (cdr (nthcdr 0 list)) 1)
    (assert-error 'type-error
                  (setf (second list) 1))))
                  
(define-test |setf second apply 1|
  (let ((list (copy-tree '(0 0 0 0 0 0 0 0 0 0))))
    (assert-equal 1 (apply #'(setf second) (list 1 list)))
    (assert-equal '(0 1 0 0 0 0 0 0 0 0) list)))

(define-test |setf second apply error 1|
  (let ((list (copy-tree '(0 0 0 0 0 0 0 0 0 0))))
    (setf (cdr (nthcdr 0 list)) 1)
    (assert-error 'type-error
                  (apply #'(setf second) (list 1 list)))))
                  
(define-test |setf second apply 1|
  (let ((list (copy-tree '(0 0 0 0 0 0 0 0 0 0))))
    (assert-equal 1 (apply #'(setf second) (list 1 list)))
    (assert-equal '(0 1 0 0 0 0 0 0 0 0) list)))

(define-test |setf second apply error 1|
  (let ((list (copy-tree '(0 0 0 0 0 0 0 0 0 0))))
    (setf (cdr (nthcdr 0 list)) 1)
    (assert-error 'type-error
                  (apply #'(setf second) (list 1 list)))))
                  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Tests for the (SETF THIRD) function and SETF expander.

(define-test |setf third 1|
  (let ((list (copy-tree '(0 0 0 0 0 0 0 0 0 0))))
    (assert-equal 1 (setf (third list) 1))
    (assert-equal '(0 0 1 0 0 0 0 0 0 0) list)))

(define-test |setf third error 1|
  (let ((list (copy-tree '(0 0 0 0 0 0 0 0 0 0))))
    (setf (cdr (nthcdr 1 list)) 1)
    (assert-error 'type-error
                  (setf (third list) 1))))

(define-test |setf third 1|
  (let ((list (copy-tree '(0 0 0 0 0 0 0 0 0 0))))
    (assert-equal 1 (setf (third list) 1))
    (assert-equal '(0 0 1 0 0 0 0 0 0 0) list)))

(define-test |setf third error 1|
  (let ((list (copy-tree '(0 0 0 0 0 0 0 0 0 0))))
    (setf (cdr (nthcdr 1 list)) 1)
    (assert-error 'type-error
                  (setf (third list) 1))))

(define-test |setf third apply 1|
  (let ((list (copy-tree '(0 0 0 0 0 0 0 0 0 0))))
    (assert-equal 1 (apply #'(setf third) (list 1 list)))
    (assert-equal '(0 0 1 0 0 0 0 0 0 0) list)))

(define-test |setf third apply error 1|
  (let ((list (copy-tree '(0 0 0 0 0 0 0 0 0 0))))
    (setf (cdr (nthcdr 1 list)) 1)
    (assert-error 'type-error
                  (apply #'(setf third) (list 1 list)))))

(define-test |setf third apply 1|
  (let ((list (copy-tree '(0 0 0 0 0 0 0 0 0 0))))
    (assert-equal 1 (apply #'(setf third) (list 1 list)))
    (assert-equal '(0 0 1 0 0 0 0 0 0 0) list)))

(define-test |setf third apply error 1|
  (let ((list (copy-tree '(0 0 0 0 0 0 0 0 0 0))))
    (setf (cdr (nthcdr 1 list)) 1)
    (assert-error 'type-error
                  (apply #'(setf third) (list 1 list)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Tests for the (SETF FOURTH) function and SETF expander.

(define-test |setf fourth 1|
  (let ((list (copy-tree '(0 0 0 0 0 0 0 0 0 0))))
    (assert-equal 1 (setf (fourth list) 1))
    (assert-equal '(0 0 0 1 0 0 0 0 0 0) list)))

(define-test |setf fourth error 1|
  (let ((list (copy-tree '(0 0 0 0 0 0 0 0 0 0))))
    (setf (cdr (nthcdr 2 list)) 1)
    (assert-error 'type-error
                  (setf (fourth list) 1))))

(define-test |setf fourth 1|
  (let ((list (copy-tree '(0 0 0 0 0 0 0 0 0 0))))
    (assert-equal 1 (setf (fourth list) 1))
    (assert-equal '(0 0 0 1 0 0 0 0 0 0) list)))

(define-test |setf fourth error 1|
  (let ((list (copy-tree '(0 0 0 0 0 0 0 0 0 0))))
    (setf (cdr (nthcdr 2 list)) 1)
    (assert-error 'type-error
                  (setf (fourth list) 1))))

(define-test |setf fourth apply 1|
  (let ((list (copy-tree '(0 0 0 0 0 0 0 0 0 0))))
    (assert-equal 1 (apply #'(setf fourth) (list 1 list)))
    (assert-equal '(0 0 0 1 0 0 0 0 0 0) list)))

(define-test |setf fourth apply error 1|
  (let ((list (copy-tree '(0 0 0 0 0 0 0 0 0 0))))
    (setf (cdr (nthcdr 2 list)) 1)
    (assert-error 'type-error
                  (apply #'(setf fourth) (list 1 list)))))

(define-test |setf fourth apply 1|
  (let ((list (copy-tree '(0 0 0 0 0 0 0 0 0 0))))
    (assert-equal 1 (apply #'(setf fourth) (list 1 list)))
    (assert-equal '(0 0 0 1 0 0 0 0 0 0) list)))

(define-test |setf fourth apply error 1|
  (let ((list (copy-tree '(0 0 0 0 0 0 0 0 0 0))))
    (setf (cdr (nthcdr 2 list)) 1)
    (assert-error 'type-error
                  (apply #'(setf fourth) (list 1 list)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Tests for the (SETF FIFTH) function and SETF expander.

(define-test |setf fifth 1|
  (let ((list (copy-tree '(0 0 0 0 0 0 0 0 0 0))))
    (assert-equal 1 (setf (fifth list) 1))
    (assert-equal '(0 0 0 0 1 0 0 0 0 0) list)))

(define-test |setf fifth error 1|
  (let ((list (copy-tree '(0 0 0 0 0 0 0 0 0 0))))
    (setf (cdr (nthcdr 3 list)) 1)
    (assert-error 'type-error
                  (setf (fifth list) 1))))

(define-test |setf fifth 1|
  (let ((list (copy-tree '(0 0 0 0 0 0 0 0 0 0))))
    (assert-equal 1 (setf (fifth list) 1))
    (assert-equal '(0 0 0 0 1 0 0 0 0 0) list)))

(define-test |setf fifth error 1|
  (let ((list (copy-tree '(0 0 0 0 0 0 0 0 0 0))))
    (setf (cdr (nthcdr 3 list)) 1)
    (assert-error 'type-error
                  (setf (fifth list) 1))))

(define-test |setf fifth apply 1|
  (let ((list (copy-tree '(0 0 0 0 0 0 0 0 0 0))))
    (assert-equal 1 (apply #'(setf fifth) (list 1 list)))
    (assert-equal '(0 0 0 0 1 0 0 0 0 0) list)))

(define-test |setf fifth apply error 1|
  (let ((list (copy-tree '(0 0 0 0 0 0 0 0 0 0))))
    (setf (cdr (nthcdr 3 list)) 1)
    (assert-error 'type-error
                  (apply #'(setf fifth) (list 1 list)))))

(define-test |setf fifth apply 1|
  (let ((list (copy-tree '(0 0 0 0 0 0 0 0 0 0))))
    (assert-equal 1 (apply #'(setf fifth) (list 1 list)))
    (assert-equal '(0 0 0 0 1 0 0 0 0 0) list)))

(define-test |setf fifth apply error 1|
  (let ((list (copy-tree '(0 0 0 0 0 0 0 0 0 0))))
    (setf (cdr (nthcdr 3 list)) 1)
    (assert-error 'type-error
                  (apply #'(setf fifth) (list 1 list)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Tests for the (SETF SIXTH) function and SETF expander.

(define-test |setf sixth 1|
  (let ((list (copy-tree '(0 0 0 0 0 0 0 0 0 0))))
    (assert-equal 1 (setf (sixth list) 1))
    (assert-equal '(0 0 0 0 0 1 0 0 0 0) list)))

(define-test |setf sixth error 1|
  (let ((list (copy-tree '(0 0 0 0 0 0 0 0 0 0))))
    (setf (cdr (nthcdr 4 list)) 1)
    (assert-error 'type-error
                  (setf (sixth list) 1))))

(define-test |setf sixth 1|
  (let ((list (copy-tree '(0 0 0 0 0 0 0 0 0 0))))
    (assert-equal 1 (setf (sixth list) 1))
    (assert-equal '(0 0 0 0 0 1 0 0 0 0) list)))

(define-test |setf sixth error 1|
  (let ((list (copy-tree '(0 0 0 0 0 0 0 0 0 0))))
    (setf (cdr (nthcdr 4 list)) 1)
    (assert-error 'type-error
                  (setf (sixth list) 1))))

(define-test |setf sixth apply 1|
  (let ((list (copy-tree '(0 0 0 0 0 0 0 0 0 0))))
    (assert-equal 1 (apply #'(setf sixth) (list 1 list)))
    (assert-equal '(0 0 0 0 0 1 0 0 0 0) list)))

(define-test |setf sixth apply error 1|
  (let ((list (copy-tree '(0 0 0 0 0 0 0 0 0 0))))
    (setf (cdr (nthcdr 4 list)) 1)
    (assert-error 'type-error
                  (apply #'(setf sixth) (list 1 list)))))

(define-test |setf sixth apply 1|
  (let ((list (copy-tree '(0 0 0 0 0 0 0 0 0 0))))
    (assert-equal 1 (apply #'(setf sixth) (list 1 list)))
    (assert-equal '(0 0 0 0 0 1 0 0 0 0) list)))

(define-test |setf sixth apply error 1|
  (let ((list (copy-tree '(0 0 0 0 0 0 0 0 0 0))))
    (setf (cdr (nthcdr 4 list)) 1)
    (assert-error 'type-error
                  (apply #'(setf sixth) (list 1 list)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Tests for the (SETF SEVENTH) function and SETF expander.

(define-test |setf seventh 1|
  (let ((list (copy-tree '(0 0 0 0 0 0 0 0 0 0))))
    (assert-equal 1 (setf (seventh list) 1))
    (assert-equal '(0 0 0 0 0 0 1 0 0 0) list)))

(define-test |setf seventh error 1|
  (let ((list (copy-tree '(0 0 0 0 0 0 0 0 0 0))))
    (setf (cdr (nthcdr 5 list)) 1)
    (assert-error 'type-error
                  (setf (seventh list) 1))))
                  
(define-test |setf seventh 1|
  (let ((list (copy-tree '(0 0 0 0 0 0 0 0 0 0))))
    (assert-equal 1 (setf (seventh list) 1))
    (assert-equal '(0 0 0 0 0 0 1 0 0 0) list)))

(define-test |setf seventh error 1|
  (let ((list (copy-tree '(0 0 0 0 0 0 0 0 0 0))))
    (setf (cdr (nthcdr 5 list)) 1)
    (assert-error 'type-error
                  (setf (seventh list) 1))))
                  
(define-test |setf seventh apply 1|
  (let ((list (copy-tree '(0 0 0 0 0 0 0 0 0 0))))
    (assert-equal 1 (apply #'(setf seventh) (list 1 list)))
    (assert-equal '(0 0 0 0 0 0 1 0 0 0) list)))

(define-test |setf seventh apply error 1|
  (let ((list (copy-tree '(0 0 0 0 0 0 0 0 0 0))))
    (setf (cdr (nthcdr 5 list)) 1)
    (assert-error 'type-error
                  (apply #'(setf seventh) (list 1 list)))))
                  
(define-test |setf seventh apply 1|
  (let ((list (copy-tree '(0 0 0 0 0 0 0 0 0 0))))
    (assert-equal 1 (apply #'(setf seventh) (list 1 list)))
    (assert-equal '(0 0 0 0 0 0 1 0 0 0) list)))

(define-test |setf seventh apply error 1|
  (let ((list (copy-tree '(0 0 0 0 0 0 0 0 0 0))))
    (setf (cdr (nthcdr 5 list)) 1)
    (assert-error 'type-error
                  (apply #'(setf seventh) (list 1 list)))))
                  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Tests for the (SETF EIGHTH) function and SETF expander.

(define-test |setf eighth 1|
  (let ((list (copy-tree '(0 0 0 0 0 0 0 0 0 0))))
    (assert-equal 1 (setf (eighth list) 1))
    (assert-equal '(0 0 0 0 0 0 0 1 0 0) list)))

(define-test |setf eighth error 1|
  (let ((list (copy-tree '(0 0 0 0 0 0 0 0 0 0))))
    (setf (cdr (nthcdr 6 list)) 1)
    (assert-error 'type-error
                  (setf (eighth list) 1))))
                  
(define-test |setf eighth 1|
  (let ((list (copy-tree '(0 0 0 0 0 0 0 0 0 0))))
    (assert-equal 1 (setf (eighth list) 1))
    (assert-equal '(0 0 0 0 0 0 0 1 0 0) list)))

(define-test |setf eighth error 1|
  (let ((list (copy-tree '(0 0 0 0 0 0 0 0 0 0))))
    (setf (cdr (nthcdr 6 list)) 1)
    (assert-error 'type-error
                  (setf (eighth list) 1))))
                  
(define-test |setf eighth apply 1|
  (let ((list (copy-tree '(0 0 0 0 0 0 0 0 0 0))))
    (assert-equal 1 (apply #'(setf eighth) (list 1 list)))
    (assert-equal '(0 0 0 0 0 0 0 1 0 0) list)))

(define-test |setf eighth apply error 1|
  (let ((list (copy-tree '(0 0 0 0 0 0 0 0 0 0))))
    (setf (cdr (nthcdr 6 list)) 1)
    (assert-error 'type-error
                  (apply #'(setf eighth) (list 1 list)))))
                  
(define-test |setf eighth apply 1|
  (let ((list (copy-tree '(0 0 0 0 0 0 0 0 0 0))))
    (assert-equal 1 (apply #'(setf eighth) (list 1 list)))
    (assert-equal '(0 0 0 0 0 0 0 1 0 0) list)))

(define-test |setf eighth apply error 1|
  (let ((list (copy-tree '(0 0 0 0 0 0 0 0 0 0))))
    (setf (cdr (nthcdr 6 list)) 1)
    (assert-error 'type-error
                  (apply #'(setf eighth) (list 1 list)))))
                  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Tests for the (SETF NINTH) function and SETF expander.

(define-test |setf ninth 1|
  (let ((list (copy-tree '(0 0 0 0 0 0 0 0 0 0))))
    (assert-equal 1 (setf (ninth list) 1))
    (assert-equal '(0 0 0 0 0 0 0 0 1 0) list)))

(define-test |setf ninth error 1|
  (let ((list (copy-tree '(0 0 0 0 0 0 0 0 0 0))))
    (setf (cdr (nthcdr 7 list)) 1)
    (assert-error 'type-error
                  (setf (ninth list) 1))))
                  
(define-test |setf ninth 1|
  (let ((list (copy-tree '(0 0 0 0 0 0 0 0 0 0))))
    (assert-equal 1 (setf (ninth list) 1))
    (assert-equal '(0 0 0 0 0 0 0 0 1 0) list)))

(define-test |setf ninth error 1|
  (let ((list (copy-tree '(0 0 0 0 0 0 0 0 0 0))))
    (setf (cdr (nthcdr 7 list)) 1)
    (assert-error 'type-error
                  (setf (ninth list) 1))))
                  
(define-test |setf ninth apply 1|
  (let ((list (copy-tree '(0 0 0 0 0 0 0 0 0 0))))
    (assert-equal 1 (apply #'(setf ninth) (list 1 list)))
    (assert-equal '(0 0 0 0 0 0 0 0 1 0) list)))

(define-test |setf ninth apply error 1|
  (let ((list (copy-tree '(0 0 0 0 0 0 0 0 0 0))))
    (setf (cdr (nthcdr 7 list)) 1)
    (assert-error 'type-error
                  (apply #'(setf ninth) (list 1 list)))))
                  
(define-test |setf ninth apply 1|
  (let ((list (copy-tree '(0 0 0 0 0 0 0 0 0 0))))
    (assert-equal 1 (apply #'(setf ninth) (list 1 list)))
    (assert-equal '(0 0 0 0 0 0 0 0 1 0) list)))

(define-test |setf ninth apply error 1|
  (let ((list (copy-tree '(0 0 0 0 0 0 0 0 0 0))))
    (setf (cdr (nthcdr 7 list)) 1)
    (assert-error 'type-error
                  (apply #'(setf ninth) (list 1 list)))))
                  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Tests for the (SETF TENTH) function and SETF expander.

(define-test |setf tenth 1|
  (let ((list (copy-tree '(0 0 0 0 0 0 0 0 0 0))))
    (assert-equal 1 (setf (tenth list) 1))
    (assert-equal '(0 0 0 0 0 0 0 0 0 1) list)))

(define-test |setf tenth error 1|
  (let ((list (copy-tree '(0 0 0 0 0 0 0 0 0 0))))
    (setf (cdr (nthcdr 8 list)) 1)
    (assert-error 'type-error
                  (setf (tenth list) 1))))
                  
(define-test |setf tenth 1|
  (let ((list (copy-tree '(0 0 0 0 0 0 0 0 0 0))))
    (assert-equal 1 (setf (tenth list) 1))
    (assert-equal '(0 0 0 0 0 0 0 0 0 1) list)))

(define-test |setf tenth error 1|
  (let ((list (copy-tree '(0 0 0 0 0 0 0 0 0 0))))
    (setf (cdr (nthcdr 8 list)) 1)
    (assert-error 'type-error
                  (setf (tenth list) 1))))
                  
(define-test |setf tenth apply 1|
  (let ((list (copy-tree '(0 0 0 0 0 0 0 0 0 0))))
    (assert-equal 1 (apply #'(setf tenth) (list 1 list)))
    (assert-equal '(0 0 0 0 0 0 0 0 0 1) list)))

(define-test |setf tenth apply error 1|
  (let ((list (copy-tree '(0 0 0 0 0 0 0 0 0 0))))
    (setf (cdr (nthcdr 8 list)) 1)
    (assert-error 'type-error
                  (apply #'(setf tenth) (list 1 list)))))
                  
(define-test |setf tenth apply 1|
  (let ((list (copy-tree '(0 0 0 0 0 0 0 0 0 0))))
    (assert-equal 1 (apply #'(setf tenth) (list 1 list)))
    (assert-equal '(0 0 0 0 0 0 0 0 0 1) list)))

(define-test |setf tenth apply error 1|
  (let ((list (copy-tree '(0 0 0 0 0 0 0 0 0 0))))
    (setf (cdr (nthcdr 8 list)) 1)
    (assert-error 'type-error
                  (apply #'(setf tenth) (list 1 list)))))
                  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Tests for the REST function.

(define-test rest.1
  (assert-equal '() (rest '())))

(define-test rest.2
  (assert-equal '() (rest '(0))))

(define-test rest.3
  (assert-equal '(1) (rest '(0 1))))

(define-test rest.error.1
  (assert-error 'type-error
                (rest 1)))

(define-test rest.apply.1
  (assert-equal '(1) (apply (second (list 'a #'rest)) '((0 1)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Tests for the SETF expander and SETF function for REST.

(define-test |setf-rest 1|
  (let ((list (copy-list '(0))))
    (assert-equal 1 (setf (rest list) 1))
    (assert-equal '(0 . 1) list)))

(define-test |setf-rest error 1|
  (let ((list 1))
    (assert-error 'type-error
                  (setf (rest list) 1))))

(define-test |setf-rest apply 1|
  (let ((list (copy-list '(0))))
    (assert-equal 1 (apply #'(setf rest) (list 1 list)))
    (assert-equal '(0 . 1) list)))

(define-test |setf-rest apply error 1|
  (let ((list 1))
    (assert-error 'type-error
                  (apply #'(setf rest) (list 1 list)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Tests for the LIST function.

(define-test list.1
  (assert-equal '() (list)))

(define-test list.2
  (assert-equal '(1) (list 1)))

(define-test list.3
  (assert-equal '(1 2) (list 1 2)))

(define-test list.4
  (assert-equal '(1 2 3) (list 1 2 3)))

(define-test list.5
  (assert-equal '((a) (b) 1 2) (list '(a) '(b) 1 2)))

(define-test list.apply.1a
  (assert-equal '() (apply (cadr (list 1 #'list)) '())))

(define-test list.apply.2a
  (assert-equal '(1) (apply (cadr (list 1 #'list)) (list 1))))

(define-test list.apply.3a
  (assert-equal '(1 2) (apply (cadr (list 1 #'list)) (list 1 2))))

(define-test list.apply.4a
  (assert-equal '(1 2 3) (apply (cadr (list 1 #'list)) (list 1 2 3))))

(define-test list.apply.5a
  (assert-equal '((a) (b) 1 2)
                (apply (cadr (list 1 #'list)) (list '(a) '(b) 1 2))))

(define-test list.apply.1b
  (assert-equal '() (apply (cadr (list 1 'list)) '())))

(define-test list.apply.2b
  (assert-equal '(1) (apply (cadr (list 1 'list)) (list 1))))

(define-test list.apply.3b
  (assert-equal '(1 2) (apply (cadr (list 1 'list)) (list 1 2))))

(define-test list.apply.4b
  (assert-equal '(1 2 3) (apply (cadr (list 1 'list)) (list 1 2 3))))

(define-test list.apply.5b
  (assert-equal '((a) (b) 1 2)
                (apply (cadr (list 1 'list)) (list '(a) '(b) 1 2))))

(define-test list.order.1
  (let ((i 0))
    (assert-equal '(1 2 3) (list (incf i) (incf i) (incf i)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Tests for the LIST* function.

(define-test list*.1
  (assert-equal '(1 2 3) (list* 1 '(2 3))))

(define-test list*.2
  (assert-equal '(1 2 3) (list* 1 2 '(3))))

(define-test list*.3
  (assert-equal '(1 2 3) (list* 1 2 3 '())))

(define-test list*.4
  (assert-equal 'a (list* 'a)))

(define-test list*.5
  (assert-equal '(1 2 . 3) (list* 1 2 3)))

(define-test list*.apply.1
  (assert-equal '(1 2 3)
                (apply (cadr (list 1 #'list*)) (list 1 '(2 3)))))

(define-test list*.apply.2
  (assert-equal '(1 2 3)
                (apply (cadr (list 1 #'list*)) (list 1 2 '(3)))))

(define-test list*.apply.3
  (assert-equal '(1 2 3)
                (apply (cadr (list 1 #'list*)) (list 1 2 3 '()))))

(define-test list*.apply.4
  (assert-equal 'a
                (apply (cadr (list 1 #'list*)) (list 'a))))

(define-test list*.apply.5
  (assert-equal '(1 2 . 3)
                (apply (cadr (list 1 #'list*)) (list 1 2 3))))

(define-test list*.error.1
  (assert-error 'error
                (list*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Tests for the LAST function.

(define-test |last 1|
  (assert-equal '() (last '())))

(define-test |last 2|
  (assert-equal '(1) (last '(1))))

(define-test |last 3|
  (assert-equal '(2) (last '(1 2))))

(define-test |last 4|
  (assert-equal '(1 . 2) (last '(1 . 2))))

(define-test |last 5|
  (assert-equal '(2 3) (last '(1 2 3) 2)))

(define-test |last 6|
  (assert-equal '(1 2 . 3) (last '(1 2 . 3) 2)))

(define-test |last 7|
  (assert-equal '(2 3 . 4) (last '(1 2 3 . 4) 2)))

(define-test |last 8|
  (assert-equal '(1 2 3) (last '(1 2 3) 3)))

(define-test |last 9|
  (assert-equal '(1 2 3) (last '(1 2 3) 4)))

(define-test |last error 1|
  (assert-error 'type-error
                (last 1)))

(define-test |last error 2|
  (assert-error 'type-error
                (last '(1) 'a)))

(define-test |last apply error 1|
  (assert-error 'type-error
                (apply #'last (list 1))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Tests for the COPY-LIST function.

(define-test copy-list.1
  (assert-equal '() (copy-list '())))

(define-test copy-list.2
  (assert-equal '(1) (copy-list '(1))))

(define-test copy-list.3
  (let ((thing '(a b)))
    (assert-equal thing (car (copy-list (list thing 1 2))))))

(define-test copy-list.4
  (assert-equal '(1 . 2) (copy-list '(1 . 2))))

(define-test copy-list.error
  (assert-error 'type-error (copy-list 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Tests for the LIST-LENGTH function.

(define-test |list-length proper-list 1|
  (assert-equal 0
                (list-length '())))
                
(define-test |list-length proper-list 2|
  (assert-equal 1
                (list-length '(0))))

(define-test |list-length proper-list 3|
  (assert-equal 2
                (list-length '(0 0))))

(define-test |list-length proper-list 4|
  (assert-equal 3
                (list-length '(0 0 0))))

(define-test |list-length proper-list 5|
  (assert-equal 4
                (list-length '(0 0 0 0))))

(define-test |list-length proper-list 6|
  (assert-equal 5
                (list-length '(0 0 0 0 0))))

(define-test |list-length circular-list 1|
  (assert-equal nil
                (list-length '#1=(0 . #1#))))

(define-test |list-length circular-list 2|
  (assert-equal nil
                (list-length '#1=(0 0 . #1#))))

(define-test |list-length circular-list 3|
  (assert-equal nil
                (list-length '#1=(0 0 0 . #1#))))

(define-test |list-length circular-list 4|
  (assert-equal nil
                (list-length '#1=(0 0 0 0 . #1#))))

(define-test |list-length dotted-list 1|
  (assert-error 'type-error
                (list-length '(0 . 0))))

(define-test |list-length dotted-list 2|
  (assert-error 'type-error
                (list-length '(0 0 . 0))))

(define-test |list-length dotted-list 3|
  (assert-error 'type-error
                (list-length '(0 0 0 . 0))))

(define-test |list-length dotted-list 4|
  (assert-error 'type-error
                (list-length '(0 0 0 0 . 0))))

(define-test |list-length not-a-list 1|
  (assert-error 'type-error
                (list-length 0)))

(define-test |list-length not-a-list 2|
  (assert-error 'type-error
                (list-length 'a)))

(define-test |list-length not-a-list 3|
  (assert-error 'type-error
                (list-length #(a))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Tests for the MAKE-LIST function.

(define-test make-list.1
  (assert-equal '() (make-list 0)))

(define-test make-list.2
  (assert-equal 1 (length (make-list 1))))

(define-test make-list.3
  (assert-equal '(3 3) (make-list 2 :initial-element 3)))

(define-test make-list.error.1
  (assert-error 'type-error (make-list 'a)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Tests for the NTHCDR function.

(define-test |nthcdr 1|
  (assert-equal '()
                (nthcdr 0 '())))

(define-test |nthcdr 2|
  (assert-equal '()
                (nthcdr 1 '())))

(define-test |nthcdr 3|
  (assert-equal '(0 0)
                (nthcdr 1 '(0 0 0))))

(define-test |nthcdr error 1|
  (assert-error 'type-error
                (nthcdr 'a '(0 0))))

(define-test |nthcdr error 2|
  (assert-error 'type-error
                (nthcdr '-1 '(0 0))))

(define-test |nthcdr error 3|
  (assert-error 'type-error
                (nthcdr 2 '(0 . 0))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Tests for the NTH function.

(define-test |nth 1|
  (assert-equal '()
                (nth 0 '())))

(define-test |nth 2|
  (assert-equal '()
                (nth 1 '())))

(define-test |nth 3|
  (assert-equal 1
                (nth 0 '(1 2))))

(define-test |nth 4|
  (assert-equal 2
                (nth 1 '(1 2))))

(define-test |nth 5|
  (assert-equal '()
                (nth 3 '(1 2))))

(define-test |nth error 1|
  (assert-error 'type-error
                (nth 3 '(1 2 . 0))))

(define-test |nth error 2|
  (assert-error 'type-error
                (nth -1 '(1))))

(define-test |nth error 2|
  (assert-error 'type-error
                (nth 'a '(1))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Tests for the (SETF NTH) function and SETF expander.

(define-test |setf-nth 1|
  (let ((list (copy-list '(0 0 0))))
    (assert-equal 1
                  (setf (nth 0 list) 1))
    (assert-equal '(1 0 0)
                  list)))

(define-test |setf-nth 1|
  (let ((list (copy-list '(0 0 0))))
    (assert-equal 1
                  (setf (nth 2 list) 1))
    (assert-equal '(0 0 1)
                  list)))

(define-test |setf-nth error 1|
  (assert-error 'error
                (setf (nth 0 '()) 1)))

(define-test |setf-nth error 2|
  (assert-error 'error
                (setf (nth 1 (copy-list '(0))) 1)))

(define-test |setf-nth error 3|
  (assert-error 'error
                (setf (nth 2 (copy-list '(0))) 1)))

(define-test |setf-nth error 4|
  (assert-error 'error
                (setf (nth -1 (copy-list '(0))) 1)))

(define-test |setf-nth error 5|
  (assert-error 'error
                (setf (nth 'a (copy-list '(0))) 1)))

(define-test |setf-nth apply 1|
  (let ((list (copy-list '(0 0 0))))
    (assert-equal 1
                  (apply #'(setf nth) (list 1 0 list)))
    (assert-equal '(1 0 0)
                  list)))

(define-test |setf-nth apply 1|
  (let ((list (copy-list '(0 0 0))))
    (assert-equal 1
                  (apply #'(setf nth) (list 1 2 list)))
    (assert-equal '(0 0 1)
                  list)))

(define-test |setf-nth apply error 1|
  (assert-error 'error
                (apply #'(setf nth) (list 1 0 '()))))

(define-test |setf-nth apply error 2|
  (assert-error 'error
                (apply #'(setf nth) (list 1 1 (copy-list '(0))))))

(define-test |setf-nth apply error 3|
  (assert-error 'error
                (apply #'(setf nth) (list 1 2 (copy-list '(0))))))

(define-test |setf-nth apply error 4|
  (assert-error 'error
                (apply #'(setf nth) (list 1 -1 (copy-list '(0))))))

(define-test |setf-nth apply error 5|
  (assert-error 'error
                (apply #'(setf nth) (list 1 'a (copy-list '(0))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Tests for the copy-tree function

(define-test copy-tree.1
  (assert-equal 'x (copy-tree 'x)))

(define-test copy-tree.2
  (assert-equal '(a . b) (copy-tree '(a . b))))

(define-test copy-tree.3
  (let ((tree '(((((a b . c) d (e (f g)) . h))) (i j (k)))))
    (assert-equal tree (copy-tree tree))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Tests for the tree-equal function

(define-test |tree-equal test=eql 1a|
  (assert-true (tree-equal nil nil)))

(define-test |tree-equal test=eql 1b|
  (assert-true (tree-equal nil nil :test #'eql)))

(define-test |tree-equal test=eql 1c|
  (assert-true (tree-equal nil nil :test 'eql)))

(define-test |tree-equal test=eql 2a|
  (assert-true (tree-equal 1 1)))

(define-test |tree-equal test=eql 2b|
  (assert-true (tree-equal 1 1 :test #'eql)))

(define-test |tree-equal test=eql 2c|
  (assert-true (tree-equal 1 1 :test 'eql)))

(define-test |tree-equal test=eql 3a|
  (assert-true (tree-equal '(1) '(1))))

(define-test |tree-equal test=eql 3b|
  (assert-true (tree-equal '(1) '(1) :test #'eql)))

(define-test |tree-equal test=eql 3c|
  (assert-true (tree-equal '(1) '(1) :test 'eql)))

(define-test |tree-equal test=eql 4a|
  (assert-true (tree-equal '(1 . 2) '(1 . 2))))

(define-test |tree-equal test=eql 4b|
  (assert-true (tree-equal '(1 . 2) '(1 . 2) :test #'eql)))

(define-test |tree-equal test=eql 4c|
  (assert-true (tree-equal '(1 . 2) '(1 . 2) :test 'eql)))

(define-test |tree-equal test=eql 5a|
  (assert-false (tree-equal 1 2)))

(define-test |tree-equal test=eql 5b|
  (assert-false (tree-equal 1 2 :test #'eql)))

(define-test |tree-equal test=eql 5c|
  (assert-false (tree-equal 1 2 :test 'eql)))

(define-test |tree-equal test=eql 6a|
  (assert-false (tree-equal '(1) '(2))))

(define-test |tree-equal test=eql 6b|
  (assert-false (tree-equal '(1) '(2) :test #'eql)))

(define-test |tree-equal test=eql 6c|
  (assert-false (tree-equal '(1) '(2) :test 'eql)))

(define-test |tree-equal test=eql 7a|
  (assert-false (tree-equal '(1) '((1)))))

(define-test |tree-equal test=eql 7b|
  (assert-false (tree-equal '(1) '((1)) :test #'eql)))

(define-test |tree-equal test=eql 7c|
  (assert-false (tree-equal '(1) '((1)) :test 'eql)))

(define-test |tree-equal test=eql 8a|
  (assert-false (tree-equal '((1)) '(1))))

(define-test |tree-equal test=eql 8b|
  (assert-false (tree-equal '((1)) '(1) :test #'eql)))

(define-test |tree-equal test=eql 8c|
  (assert-false (tree-equal '((1)) '(1) :test 'eql)))

(define-test |tree-equal test=eq 1a|
  (assert-true (tree-equal 'a 'a :test #'eq)))

(define-test |tree-equal test=eq 1b|
  (assert-true (tree-equal 'a 'a :test 'eq)))

(define-test |tree-equal test=eq 2a|
  (assert-false (tree-equal 'a 'b :test #'eq)))

(define-test |tree-equal test=eq 2b|
  (assert-false (tree-equal 'a 'b :test 'eq)))

(define-test |tree-equal test=eq 3a|
  (assert-true (tree-equal '(a) '(a) :test #'eq)))

(define-test |tree-equal test=eq 3b|
  (assert-true (tree-equal '(a) '(a) :test 'eq)))

(define-test |tree-equal test=eq 4a|
  (assert-false (tree-equal '(a) '((a)) :test #'eq)))

(define-test |tree-equal test=eq 4b|
  (assert-false (tree-equal '(a) '((a)) :test 'eq)))

(define-test |tree-equal test=eq 5a|
  (assert-false (tree-equal '((a)) '(a) :test #'eq)))

(define-test |tree-equal test=eq 5b|
  (assert-false (tree-equal '((a)) '(a) :test 'eq)))

(define-test |tree-equal test-not=eql 1a|
  (assert-false (tree-equal '(1) '(2) :test-not #'eql)))

(define-test |tree-equal test-not=eql 1b|
  (assert-false (tree-equal '(1) '(2) :test-not 'eql)))

(define-test |tree-equal test-not=eql 2a|
  (assert-false (tree-equal '(1) '((1)) :test-not #'eql)))

(define-test |tree-equal test-not=eql 2b|
  (assert-false (tree-equal '(1) '((1)) :test-not 'eql)))

(define-test |tree-equal test-not=eql 3a|
  (assert-false (tree-equal '((1)) '(1) :test-not #'eql)))

(define-test |tree-equal test-not=eql 3b|
  (assert-false (tree-equal '((1)) '(1) :test-not 'eql)))

(define-test |tree-equal test-not=eq 1a|
  (assert-true (tree-equal 'a 'b :test-not #'eq)))

(define-test |tree-equal test-not=eq 1b|
  (assert-true (tree-equal 'a 'b :test-not 'eq)))

(define-test |tree-equal test-not=eq 2a|
  (assert-false (tree-equal 'a 'a :test-not #'eq)))

(define-test |tree-equal test-not=eq 2b|
  (assert-false (tree-equal 'a 'a :test-not 'eq)))

(define-test |tree-equal test-not=eq 3a|
  (assert-false (tree-equal '(a) '(a) :test-not #'eq)))

(define-test |tree-equal test-not=eq 3b|
  (assert-false (tree-equal '(a) '(a) :test-not 'eq)))

(define-test |tree-equal test-not=eq 4a|
  (assert-false (tree-equal '(a) '((a)) :test-not #'eq)))

(define-test |tree-equal test-not=eq 4b|
  (assert-false (tree-equal '(a) '((a)) :test-not 'eq)))

(define-test |tree-equal test-not=eq 5a|
  (assert-false (tree-equal '((a)) '(a) :test-not #'eq)))

(define-test |tree-equal test-not=eq 5b|
  (assert-false (tree-equal '((a)) '(a) :test-not 'eq)))

(define-test |tree-equal test-not=eq 6a|
  (assert-false (tree-equal '(a) '(b) :test-not #'eq)))

(define-test |tree-equal test-not=eq 6b|
  (assert-false (tree-equal '(a) '(b) :test-not 'eq)))

(define-test |tree-equal test=other 1|
  (assert-true (tree-equal '(1) '(2)
                           :test (lambda (x y)
                                   (or (eql x y)
                                       (and (numberp x)
                                            (numberp y)
                                            (<= (abs (- x y)) 1)))))))

(define-test |tree-equal test=other 2|
  (assert-false (tree-equal '(1) '(3)
                            :test (lambda (x y)
                                    (or (eql x y)
                                        (and (numberp x)
                                             (numberp y)
                                             (<= (abs (- x y)) 1)))))))

(define-test |tree-equal test=other 3|
  (assert-false (tree-equal '(1) '((3))
                            :test (lambda (x y)
                                    (or (eql x y)
                                        (and (numberp x)
                                             (numberp y)
                                             (<= (abs (- x y)) 1)))))))

(define-test |tree-equal test=other 4|
  (assert-false (tree-equal '((1)) '(3)
                            :test (lambda (x y)
                                    (or (eql x y)
                                        (and (numberp x)
                                             (numberp y)
                                             (<= (abs (- x y)) 1)))))))

(define-test |tree-equal test-not=other 1|
  (assert-true (tree-equal '(1) '(2)
                           :test-not (lambda (x y)
                                       (not (or (eql x y)
                                                (and (numberp x)
                                                     (numberp y)
                                                     (<= (abs (- x y)) 1))))))))

(define-test |tree-equal test-not=other 2|
  (assert-false (tree-equal '(1) '(3)
                            :test-not (lambda (x y)
                                        (not (or (eql x y)
                                                 (and (numberp x)
                                                      (numberp y)
                                                      (<= (abs (- x y)) 1))))))))

(define-test |tree-equal test-not=other 3|
  (assert-false (tree-equal '(1) '((3))
                            :test-not (lambda (x y)
                                        (not (or (eql x y)
                                                 (and (numberp x)
                                                      (numberp y)
                                                      (<= (abs (- x y)) 1))))))))

(define-test |tree-equal test-not=other 4|
  (assert-false (tree-equal '((1)) '(3)
                            :test-not (lambda (x y)
                                        (not (or (eql x y)
                                                 (and (numberp x)
                                                      (numberp y)
                                                      (<= (abs (- x y)) 1))))))))

(define-test |tree-equal test=other test-not=other 1|
  (assert-error 'error
                (tree-equal '() '() :test #'eql :test-not #'eql)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Tests for the endp function

(define-test endp.1
  (assert-true (endp '())))

(define-test endp.2
  (assert-false (endp '(1 . 2))))

(define-test endp.3
  (assert-error 'type-error (endp 1)))

(define-test endp.4
  (assert-error 'type-error (endp #\a)))

(define-test endp.5
  (assert-error 'type-error (endp "a")))

(define-test endp.6
  (assert-error 'type-error (endp 'a)))

(define-test endp.7
  (assert-error 'type-error (endp 1.0)))

(define-test endp.8
  (assert-error 'type-error (endp #(a))))

(define-test endp.9
  (assert-error 'type-error (endp *standard-input*)))

(define-test endp.10
  (assert-error 'type-error (endp *package*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Tests for the mapcar function

(define-test |mapcar 1|
  (assert-equal '() (mapcar #'1+ '())))

(define-test |mapcar 2|
  (assert-equal '(1) (mapcar #'1+ '(0))))

(define-test |mapcar 3|
  (assert-equal '(-1 -2 -3) (mapcar #'- '(1 2 3))))

(define-test |mapcar 4|
  (assert-equal '(2 4 6) (mapcar #'+ '(1 2 3) '(1 2 3))))

(define-test |mapcar 5|
  (assert-equal '(3 6 9) (mapcar #'+ '(1 2 3) '(1 2 3) '(1 2 3))))

(define-test |mapcar 6|
  (assert-equal '(2 4) (mapcar #'+ '(1 2 3 4 5) '(1 2))))

(define-test |mapcar 7|
  (assert-equal '(2 4) (mapcar #'+ '(1 2) '(1 2 3 4 5))))

(define-test |mapcar error 1|
  (assert-error 'type-error (mapcar #'1+ (if (twisted '(a b c)) 1 '(a b c)))))

(define-test |mapcar error 2|
  (assert-error 'type-error (mapcar #'1+ (if (twisted '(a b c)) #(1 2 3) '(a b c)))))

(define-test |mapcar error 3|
  (assert-error 'type-error (mapcar #'1+ '(1 2 . 3))))

(define-test |mapcar error 4|
  (assert-error 'type-error (mapcar #'1+ (if (twisted '(a b c)) "1" '(a b c)))))

(define-test |mapcar error 5|
  (assert-error 'error (mapcar #'1+)))

(define-test |mapcar error 6|
  (assert-error 'type-error
                (mapcar #'list '(a b c) '(d e . f))))

(define-test |mapcar order 1|
  (let ((i 0)
        (funs (vector #'1+ #'1-))
        (lists '((1 2) (3 4) (5 6))))
    (assert-equal '(4 5) (mapcar (aref funs (incf i)) (nth (incf i) lists)))))

(define-test |mapcar apply error 1|
  (assert-error 'error
                (apply (cadr (list 'a #'mapcar)) #'car '())))

(define-test |mapcar apply error 2|
  (assert-error 'type-error
                (apply (cadr (list 'a #'mapcar)) #'list '((a b c) (d e . f)))))

(define-test |mapcar apply 1|
  (assert-equal '(a b c)
                (apply (cadr (list 'a #'mapcar)) #'car '(((a) (b) (c))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Tests for the mapc function

(define-test mapc.1
  (assert-equal '() (mapc #'1+ '())))

(define-test mapc.2
  (let ((i 0))
    (assert-equal '(1 2 3 6)
                  (append (mapc (lambda (x) (incf i x)) '(1 2 3))
                          (list i)))))

(define-test mapc.3
  (let ((i 0))
    (assert-equal '(1 2 3 12)
                  (append (mapc (lambda (x y) (incf i (+ x y)))
                                '(1 2 3)
                                '(1 2 3))
                          (list i)))))

(define-test mapc.apply.1
  (let ((i 0))
    (assert-equal '(1 2 3 12)
                  (append (apply (cadr (list 'a #'mapc))
                                 (list (lambda (x y) (incf i (+ x y)))
                                       '(1 2 3)
                                       '(1 2 3)))
                          (list i)))))

(define-test mapc.error.1
  (assert-error 'type-error (mapc #'1+ (if (twisted '(a b c)) 1 '(1 2 3)))))

(define-test mapc.error.2
  (assert-error 'type-error (mapc #'1+ (if (twisted '(a b c)) #(1 2 3) '(1 2 3)))))

(define-test mapc.error.3
  (assert-error 'type-error (mapc #'1+ '(1 2 . 3))))

(define-test mapc.error.4
  (assert-error 'type-error (mapc #'1+ (if (twisted '(a b c)) "1" '(1 2 3)))))

(define-test mapc.error.5
  (assert-error 'error (mapc #'1+)))

(define-test mapc.apply.error.1
  (assert-error 'type-error
                (apply #'mapc (list #'list '(1 2) '(3 . 4)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Tests for the maplist function

(define-test maplist.1
  (assert-equal '() (maplist #'car '())))

(define-test maplist.2
  (assert-equal '(1 2 3) (maplist #'car '(1 2 3))))

(define-test maplist.apply.1
  (assert-equal '(1 2 3)
                (apply (cadr (list 'a #'maplist))
                       (list #'car '(1 2 3)))))

(define-test maplist.error.1
  (assert-error 'type-error (maplist #'car (if (twisted '(a b c)) 1 '(a b c)))))

(define-test maplist.error.2
  (assert-error 'type-error (maplist #'car (if (twisted '(a b c)) #(1 2 3) '(a b c)))))

(define-test maplist.error.3
  (assert-error 'type-error (maplist #'car '(1 2 . 3))))

(define-test maplist.error.4
  (assert-error 'type-error (maplist #'car (if (twisted '(a b c)) "1" '(a b c)))))

(define-test maplist.error.5
  (assert-error 'error (maplist #'car)))

(define-test maplist.apply.error.1
  (assert-error 'type-error
                (apply #'maplist (list #'list '(1 2) '(3 . 4)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Tests for the mapl function

(define-test mapl.1
  (assert-equal '() (mapl #'car '())))

(define-test mapl.2
  (assert-equal '(1) (mapl #'car '(1))))

(define-test mapl.3
  (let ((i 0))
    (assert-equal 6
                  (progn (mapl (lambda (sublist) (incf i (car sublist)))
                               '(1 2 3))
                         i))))

(define-test mapl.apply.1
  (let ((i 0))
    (assert-equal 6
                  (progn (apply (cadr (list 'a #'mapl))
                                (list (lambda (sublist) (incf i (car sublist)))
                                      '(1 2 3)))
                         i))))

(define-test mapl.error.1
  (assert-error 'type-error (mapl #'car (if (twisted '(a b c)) 1 '(a b c)))))

(define-test mapl.error.2
  (assert-error 'type-error (mapl #'car (if (twisted '(a b c)) #(1 2 3) '(a b c)))))

(define-test mapl.error.3
  (assert-error 'type-error (mapl #'car '(1 2 . 3))))

(define-test mapl.error.4
  (assert-error 'type-error (mapl #'car (if (twisted '(a b c)) "1" '(a b c)))))

(define-test mapl.error.5
  (assert-error 'error (mapl #'car)))

(define-test mapl.apply.error.1
  (assert-error 'type-error
                (apply #'mapl (list #'list '(1 2) '(3 . 4)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Tests for the mapcan function

(define-test mapcan.1
  (assert-equal '(a b c)
                (mapcan #'list '(a b c))))

(define-test mapcan.2
  (assert-equal '(a d b e c f)
                (mapcan #'list '(a b c) '(d e f))))

(define-test mapcan.3
  (assert-equal '(a b c . f)
                (mapcan #'cons '(a b c) '(d e f))))

(define-test mapcan.4
  (let ((a (list 'a))
        (b (list 'b))
        (c (list 'c)))
    (assert-eq c
               (cddr (mapcan #'identity (list a b c))))))

(define-test mapcan.5
  (assert-equal '()
                (mapcan #'identity '())))

(define-test mapcan.apply.1
  (let ((a (list 'a))
        (b (list 'b))
        (c (list 'c)))
    (assert-eq c
               (cddr (apply (cadr (list 'a #'mapcan))
                            (list #'identity (list a b c)))))))

(define-test mapcan.error.1
  (assert-error 'type-error
                (mapcan #'car (if (twisted '(a b c)) 1 '((a) (b) (c))))))

(define-test mapcan.error.2
  (assert-error 'type-error
                (mapcan #'car (if (twisted '(a b c)) #(1 2 3) '((a) (b) (c))))))

(define-test mapcan.error.3
  (assert-error 'type-error (mapcan #'car '(1 2 . 3))))

(define-test mapcan.error.4
  (assert-error 'type-error
                (mapcan #'car (if (twisted '(a b c)) "1" '((a) (b) (c))))))

(define-test mapcan.error.5
  (assert-error 'error (mapcan #'car)))

(define-test mapcan.apply.error.1
  (assert-error 'type-error
                (apply #'mapcan (list #'list '(1 2) '(3 . 4)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Tests for the mapcon function

(define-test |macon 1|
  (assert-equal nil
                (mapcon #'identity '())))

(define-test |mapcon 2|
  (assert-equal '(1 2 2)
                (mapcon #'copy-list '(1 2))))

(define-test |mapcon 3|
  (assert-equal '((1 2) (3 4) (2) (4))
                (mapcon #'list '(1 2) '(3 4))))

(define-test |mapcon apply 1|
  (assert-equal '((1 2) (3 4) (2) (4))
                (apply #'mapcon #'list '(1 2) '(3 4) '())))

(define-test |mapcon error 1|
  (assert-error 'type-error
                (mapcon #'copy-list '(1 . 2))))

(define-test |mapcon error 2|
  (assert-error 'type-error
                (mapcon #'car (if (twisted '(a b c)) "1" '((a) (b) (c))))))

(define-test |mapcon error 3|
  (assert-error 'error
                (mapcon #'car)))

(define-test |mapcon apply error 1|
  (assert-error 'type-error
                (apply #'mapcon (list #'list '(1 2) '(3 . 4)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Tests for the append function

(define-test |append 1|
  (assert-equal nil (append)))

(define-test |append 2|
  (assert-equal 'x (append 'x)))

(define-test |append 4|
  (assert-equal
   '(a b c d e f g . h)
   (append (list 'a) (list 'b) (list 'c)
           (list 'd) (list 'e) (list 'f)
           (list 'g) 'h)))

(define-test |append 5|
  (assert-equal 'a (append nil nil nil nil nil nil nil nil 'a)))

;;; Test suggested by Peter Graves
(define-test |append 7|
  (let ((x (list 'a 'b 'c 'd)))
    (assert-false (eq (append x nil) x))))

;;; Order of evaluation tests

(define-test |append order 1|
  (assert-equal
   '((a b c d e f g h i) 3 1 2 3)
   (let ((i 0) x y z)
     (list
       (append (progn (setf x (incf i)) (copy-list '(a b c)))
               (progn (setf y (incf i)) (copy-list '(d e f)))
               (progn (setf z (incf i)) (copy-list '(g h i))))
       i x y z))))

(define-test |append order 2|
  (assert-equal '(1 1) (let ((i 0)) (append (list (incf i)) (list i)))))

;;; Error tests

(define-test |append error 1|
  (assert-error 'type-error (append '(a . b) '(z))))

(define-test |append error 2|
  (assert-error 'type-error (append '(x y z) '(a . b) '(z))))

;;; This test verifies that append preserves the structure of
;;; the last list.
(define-test |append sharing 1|
  (let ((list1 '(1 2))
        (list2 '(3 4)))
    (assert-eq (cddr (append list1 list2)) list2)))

(define-test |append apply 1|
  (let ((list1 '(1 2))
        (list2 '(3 4)))
    (assert-equal '(1 2 3 4)
                  (apply (cadr (list 'a #'append)) (list list1 list2)))
    (assert-equal '(1 2 3 4)
                  (apply (cadr (list 'a 'append)) (list list1 list2)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Tests for the nconc function

(define-test |nconc 1|
  (assert-eq '() (nconc)))

(define-test |nconc 2|
  (let ((list (copy-tree '(a b c))))
    (assert-eq list (nconc list))))

(define-test |nconc 3|
  (let ((list (copy-tree '(a b c))))
    (assert-eq list (nconc '() list))))

(define-test |nconc 4|
  (let ((list1 (copy-tree '(a b c)))
        (list2 (copy-tree '(d e f))))
    (assert-eq list1 (nconc list1 list2))
    (assert-eq list2 (cdddr list1))))

(define-test |nconc 5|
  (assert-equal '(a z)
                (nconc (copy-tree '(a . b))
                       (copy-tree '(z)))))

(define-test |nconc 6|
  (assert-equal '(x y z a z)
                (nconc (copy-tree '(x y z))
                       (copy-tree '(a . b))
                       (copy-tree '(z)))))

(define-test |nconc order 1|
  (assert-equal
   '((a b c d e f g h i) 3 1 2 3)
   (let ((i 0) x y z)
     (list
       (nconc (progn (setf x (incf i)) (copy-list '(a b c)))
              (progn (setf y (incf i)) (copy-list '(d e f)))
              (progn (setf z (incf i)) (copy-list '(g h i))))
       i x y z))))

(define-test |nconc apply 1|
  (let ((list1 '(1 2))
        (list2 '(3 4)))
    (assert-equal '(1 2 3 4)
                  (apply (cadr (list 'a #'nconc))
                         (copy-tree (list list1 list2))))
    (assert-equal '(1 2 3 4)
                  (apply (cadr (list 'a 'nconc))
                         (copy-tree (list list1 list2))))))

(define-test |nconc error 1|
  (assert-error 'type-error
                (nconc (copy-tree '(a b c)) 'd (copy-tree '(e f)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Tests for the revappend function

(define-test revappend.1
  (assert-equal '()
                (revappend '() '())))

(define-test revappend.2
  (assert-equal '(a)
                (revappend '(a) '())))

(define-test revappend.3
  (assert-equal '(b a)
                (revappend '(a b) '())))

(define-test revappend.4
  (assert-equal '(b a c)
                (revappend '(a b) '(c))))

(define-test revappend.5
  (assert-equal '(b a . c)
                (revappend '(a b) 'c)))

(define-test revappend.6
  (assert-equal 'c
                (revappend '() 'c)))

(define-test revappend.7
  (let ((l '(x)))
    (assert-equal l
                  (cdr (revappend '(a) l)))))

(define-test revappend.error.1
  (assert-error 'type-error
                (revappend 'a '())))

(define-test revappend.error.2
  (assert-error 'type-error
                (revappend '(a . b) '())))

(define-test revappend.error.3
  (assert-error 'type-error
                (revappend 1 '())))

(define-test revappend.error.4
  (assert-error 'type-error
                (revappend #(a b) '())))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Tests for the nreconc function

(define-test nreconc.1
  (assert-equal '()
                (nreconc '() '())))

(define-test nreconc.2
  (assert-equal '(a)
                (nreconc (copy-list '(a)) '())))

(define-test nreconc.3
  (assert-equal '(b a)
                (nreconc (copy-list '(a b)) '())))

(define-test nreconc.4
  (assert-equal '(b a c)
                (nreconc (copy-list '(a b)) '(c))))

(define-test nreconc.5
  (assert-equal '(b a . c)
                (nreconc (copy-list '(a b)) 'c)))

(define-test nreconc.6
  (assert-equal 'c
                (nreconc '() 'c)))

(define-test nreconc.7
  (let ((l '(x)))
    (assert-equal l
                  (cdr (nreconc (copy-list '(a)) l)))))

(define-test nreconc.error.1
  (assert-error 'type-error
                (nreconc 'a '())))

(define-test nreconc.error.2
  (assert-error 'type-error
                (nreconc (copy-tree '(a . b)) '())))

(define-test nreconc.error.3
  (assert-error 'type-error
                (nreconc 1 '())))

(define-test nreconc.error.4
  (assert-error 'type-error
                (nreconc #(a b) '())))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Tests for the butlast function

(define-test butlast.1
  (assert-equal '() (butlast '())))

(define-test butlast.2
  (assert-equal '() (butlast '(0))))

(define-test butlast.3
  (assert-equal '(0) (butlast '(0 1))))

(define-test butlast.4
  (assert-equal '() (butlast '(0 . 1))))

(define-test butlast.5
  (assert-equal '(0) (butlast '(0 1 . 2))))

(define-test butlast.6
  (assert-equal '() (butlast '() 0)))

(define-test butlast.7
  (assert-equal '() (butlast '() 1)))

(define-test butlast.8
  (assert-equal '() (butlast '() 2)))

(define-test butlast.9
  (assert-equal '(0) (butlast '(0) 0)))

(define-test butlast.10
  (assert-equal '(0 1) (butlast '(0 1) 0)))

(define-test butlast.11
  (assert-equal '(0) (butlast '(0 . 1) 0)))

(define-test butlast.12
  (assert-equal '(0) (butlast '(0 1 2) 2)))

(define-test butlast.13
  (assert-equal '() (butlast '(0 1 2) 3)))

(define-test butlast.13
  (assert-equal '() (butlast '(0 1 2) 4)))

(define-test butlast.error.1
  (assert-error 'type-error
                (butlast '(0 1 2) (if (twisted '(1 2 3)) -1 1))))

(define-test butlast.error.2
  (assert-error 'type-error
                (butlast '(0 1 2) (if (twisted '(1 2 3)) 'a 1))))

(define-test butlast.error.3
  (assert-error 'type-error
                (butlast '#1=(0 . #1#))))

(define-test butlast.error.4
  (assert-error 'type-error
                (butlast 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Tests for the nbutlast function

(define-test |nbutlast n=1 1a|
  (assert-equal '()
                (nbutlast '())))

(define-test |nbutlast n=1 1b|
  (assert-equal '()
                (nbutlast '() 1)))

(define-test |nbutlast n=1 2a|
  (let ((list (copy-list '(1))))
    (assert-equal '()
                  (nbutlast list))))

(define-test |nbutlast n=1 2b|
  (let ((list (copy-list '(1))))
    (assert-equal '()
                  (nbutlast list 1))))

(define-test |nbutlast n=1 3a|
  (let ((list (copy-list '(1 2))))
    (assert-equal '(1)
                  (nbutlast list))
    (assert-equal '(1)
                  list)))

(define-test |nbutlast n=1 3b|
  (let ((list (copy-list '(1 2))))
    (assert-equal '(1)
                  (nbutlast list 1))
    (assert-equal '(1)
                  list)))

(define-test |nbutlast n=1 4a|
  (let ((list (copy-list '(1 . 2))))
    (assert-equal '()
                  (nbutlast list))))

(define-test |nbutlast n=1 4b|
  (let ((list (copy-list '(1 . 2))))
    (assert-equal '()
                  (nbutlast list 1))))

(define-test |nbutlast n=1 5a|
  (let ((list (copy-list '(1 2 . 3))))
    (assert-equal '(1)
                  (nbutlast list))
    (assert-equal '(1)
                  list)))

(define-test |nbutlast n=1 5b|
  (let ((list (copy-list '(1 2 . 3))))
    (assert-equal '(1)
                  (nbutlast list 1))
    (assert-equal '(1)
                  list)))

(define-test |nbutlast n=1 6a|
  (let ((list (copy-list '(1 2 3 . 4))))
    (assert-equal '(1 2)
                  (nbutlast list))
    (assert-equal '(1 2)
                  list)))

(define-test |nbutlast n=1 6b|
  (let ((list (copy-list '(1 2 3 . 4))))
    (assert-equal '(1 2)
                  (nbutlast list 1))
    (assert-equal '(1 2)
                  list)))

(define-test |nbutlast n=1 7a|
  (let ((list (copy-list '(1 2 3 4))))
    (assert-equal '(1 2 3)
                  (nbutlast list))
    (assert-equal '(1 2 3)
                  list)))

(define-test |nbutlast n=1 7b|
  (let ((list (copy-list '(1 2 3 4))))
    (assert-equal '(1 2 3)
                  (nbutlast list 1))
    (assert-equal '(1 2 3)
                  list)))

(define-test |nbutlast n=other 1|
  (assert-equal '()
                (nbutlast '() 2)))

(define-test |nbutlast n=other 2|
  (let ((list (copy-list '(1))))
    (assert-equal '()
                  (nbutlast list 2))))

(define-test |nbutlast n=other 3|
  (let ((list (copy-list '(1 2))))
    (assert-equal '()
                  (nbutlast list 2))))

(define-test |nbutlast n=other 4|
  (let ((list (copy-list '(1 2 3))))
    (assert-equal '(1)
                  (nbutlast list 2))
    (assert-equal '(1)
                  list)))

(define-test |nbutlast n=other 5|
  (let ((list (copy-list '(1 2 . 3))))
    (assert-equal '()
                  (nbutlast list 2))))

(define-test |nbutlast n=other 6|
  (let ((list (copy-list '(1 2 3 . 4))))
    (assert-equal '(1)
                  (nbutlast list 2))
    (assert-equal '(1)
                  list)))

(define-test |nbutlast error 1|
  (assert-error 'type-error
                (nbutlast 1)))

(define-test |nbutlast error 2|
  (assert-error 'type-error
                (nbutlast '() 'a)))

(define-test |nbutlast error 3|
  (let ((list (copy-list '(0))))
    (setf (cdr list) list)
    (assert-error 'type-error
                  (nbutlast list 1))))

(define-test |nbutlast error 4|
  (let ((list (copy-list '(0))))
    (setf (cdr list) list)
    (assert-error 'type-error
                  (nbutlast list 2))))

(define-test |nbutlast error 5|
  (let ((list (copy-list '(0))))
    (setf (cdr list) list)
    (assert-error 'type-error
                  (nbutlast list 3))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Tests for the subst function

(define-test |subst test=eql key=identity 1a|
  (assert-equal '()
                (subst 'a 0 '())))

(define-test |subst test=eql key=identity 1b|
  (assert-equal '()
                (subst 'a 0 '() :test #'eql)))

(define-test |subst test=eql key=identity 1c|
  (assert-equal '()
                (subst 'a 0 '() :test 'eql)))

(define-test |subst test=eql key=identity 2a|
  (assert-equal '(((a . 1) . (2 . a)) . ((a . 3) . (4 . a)))
                (subst 'a 0 '(((0 . 1) . (2 . 0)) . ((0 . 3) . (4 . 0))))))

(define-test |subst test=eql key=identity 2b|
  (assert-equal '(((a . 1) . (2 . a)) . ((a . 3) . (4 . a)))
                (subst 'a 0 '(((0 . 1) . (2 . 0)) . ((0 . 3) . (4 . 0)))
                       :test #'eql)))

(define-test |subst test=eql key=identity 2c|
  (assert-equal '(((a . 1) . (2 . a)) . ((a . 3) . (4 . a)))
                (subst 'a 0 '(((0 . 1) . (2 . 0)) . ((0 . 3) . (4 . 0)))
                       :test 'eql)))

(define-test |subst test=eql key=nil 1|
  (assert-equal '(((a . 1) . (2 . a)) . ((a . 3) . (4 . a)))
                (subst 'a
                       0
                       '(((0 . 1) . (2 . 0)) . ((0 . 3) . (4 . 0)))
                       :key nil)))

(define-test |subst test=eq key=identity 1|
  (assert-equal '()
                (subst 'a 'b '()
                       :test #'eq)))

(define-test |subst test=eq key=identity 2|
  (assert-equal '(((a . c) . (f . a)) . ((a . d) . (e . a)))
                (subst 'a 'b '(((b . c) . (f . b)) . ((b . d) . (e . b)))
                       :test #'eq)))

(define-test |subst test=other key=identity 1|
  (assert-equal '()
                (subst 'a 0 '()
                       :test (lambda (x y) (and (numberp y) (= x (1- y)))))))

(define-test |subst test=other key=identity 2|
  (assert-equal '(((a . 1) . (2 . a)) . ((a . 3) . (4 . a)))
                (subst 'a 0 '(((0 . 1) . (2 . 0)) . ((0 . 3) . (4 . 0)))
                        :test (lambda (x y) (and (numberp y) (= x y))))))

(define-test |subst test=eql key=other 1a|
  (assert-equal '()
                (subst 'a 0 '()
                       :key (lambda (x) (if (numberp x) (1+ x) x)))))

(define-test |subst test=eql key=other 1b|
  (assert-equal '()
                (subst 'a 0 '()
                       :key (lambda (x) (if (numberp x) (1+ x) x))
                       :test #'eql)))

(define-test |subst test=eql key=other 1c|
  (assert-equal '()
                (subst 'a 0 '()
                       :key (lambda (x) (if (numberp x) (1+ x) x))
                       :test 'eql)))

(define-test |subst test=eql key=other 2a|
  (assert-equal '(((a . 1) . (2 . a)) . ((a . 3) . (4 . a)))
                (subst 'a 1 '(((0 . 1) . (2 . 0)) . ((0 . 3) . (4 . 0)))
                       :key (lambda (x) (if (numberp x) (1+ x) x)))))

(define-test |subst test=eql key=other 2b|
  (assert-equal '(((a . 1) . (2 . a)) . ((a . 3) . (4 . a)))
                (subst 'a 1 '(((0 . 1) . (2 . 0)) . ((0 . 3) . (4 . 0)))
                       :key (lambda (x) (if (numberp x) (1+ x) x))
                       :test #'eql)))

(define-test |subst test=eql key=other 2c|
  (assert-equal '(((a . 1) . (2 . a)) . ((a . 3) . (4 . a)))
                (subst 'a 1 '(((0 . 1) . (2 . 0)) . ((0 . 3) . (4 . 0)))
                       :key (lambda (x) (if (numberp x) (1+ x) x))
                       :test 'eql)))

(define-test |subst test=eq key=other 1|
  (assert-equal '()
                (subst 'a 'bb '()
                       :test #'eq
                       :key (lambda (x) (if (eq x 'b) 'bb x)))))

(define-test |subst test=eq key=other 2|
  (assert-equal '(((a . c) . (f . a)) . ((a . d) . (e . a)))
                (subst 'a 'bb '(((b . c) . (f . b)) . ((b . d) . (e . b)))
                       :test #'eq
                       :key (lambda (x) (if (eq x 'b) 'bb x)))))

(define-test |subst test=other key=other 1|
  (assert-equal '()
                (subst 'a 0 '()
                       :test (lambda (x y) (and (numberp y) (= x (1- y))))
                       :key (lambda (x) (if (numberp x) (1+ x) x)))))

(define-test |subst test=other key=other 2|
  (assert-equal '(((a . 1) . (2 . a)) . ((a . 3) . (4 . a)))
                (subst 'a 0 '(((0 . 1) . (2 . 0)) . ((0 . 3) . (4 . 0)))
                        :test (lambda (x y) (and (numberp y) (= x (1- y))))
                        :key (lambda (x) (if (numberp x) (1+ x) x)))))

(define-test |subst test-not=any key=other 2|
  (assert-equal '(((a . 1) . (2 . a)) . ((a . 3) . (4 . a)))
                (subst 'a 0 '(((0 . 1) . (2 . 0)) . ((0 . 3) . (4 . 0)))
                        :test-not (lambda (x y) (not (and (numberp y) (= x (1- y)))))
                        :key (lambda (x) (if (numberp x) (1+ x) x)))))

(define-test |subst test=other test-not=other|
  (assert-error 'error
                (subst 0 1 2 :test #'= :test-not #'=)))

;;; old one left over.  Is it still needed?
(define-test subst.1
  (assert-equal '(a (c))
                (subst 'c '(b) '(a ((b))) :test-not (complement #'equal))))

(define-test |subst test=nil key=identity 1|
  (assert-error 'error
                (subst 'a
                       0
                       '(((0 . 1) . (2 . 0)) . ((0 . 3) . (4 . 0)))
                       :test nil)))

(define-test |subst test-not=nil key=identity 1|
  (assert-error 'error
                (subst 'a
                       0
                       '(((0 . 1) . (2 . 0)) . ((0 . 3) . (4 . 0)))
                       :test-not nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Tests for the subst-if function

(define-test |subst-if key=identity 1|
  (assert-equal '()
                (subst-if 'a
                          (lambda (y) (and (numberp y) (zerop (1- y))))
                          '())))

(define-test |subst-if key=identity 2|
  (assert-equal '(((a . 1) . (2 . a)) . ((a . 3) . (4 . a)))
                (subst-if 'a
                          (lambda (y) (and (numberp y) (zerop y)))
                          '(((0 . 1) . (2 . 0)) . ((0 . 3) . (4 . 0))))))

(define-test |subst-if key=other 1|
  (assert-equal '()
                (subst-if 'a
                          (lambda (y) (and (numberp y) (zerop (1- y))))
                          '()
                          :key (lambda (x) (if (numberp x) (1+ x) x)))))

(define-test |subst-if key=other 2|
  (assert-equal '(((a . 1) . (2 . a)) . ((a . 3) . (4 . a)))
                (subst-if 'a
                          (lambda (y) (and (numberp y) (zerop (1- y))))
                          '(((0 . 1) . (2 . 0)) . ((0 . 3) . (4 . 0)))
                          :key (lambda (x) (if (numberp x) (1+ x) x)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Tests for the subst-if-not function

(define-test |subst-if-not key=identity 1|
  (assert-equal '()
                (subst-if-not 'a
                              (lambda (y) (not (and (numberp y) (zerop (1- y)))))
                              '())))

(define-test |subst-if-not key=identity 2|
  (assert-equal '(((a . 1) . (2 . a)) . ((a . 3) . (4 . a)))
                (subst-if-not 'a
                              (lambda (y) (not (and (numberp y) (zerop y))))
                              '(((0 . 1) . (2 . 0)) . ((0 . 3) . (4 . 0))))))

(define-test |subst-if-not key=other 1|
  (assert-equal '()
                (subst-if-not 'a
                              (lambda (y) (not (and (numberp y) (zerop (1- y)))))
                              '()
                              :key (lambda (x) (if (numberp x) (1+ x) x)))))

(define-test |subst-if-not key=other 2|
  (assert-equal '(((a . 1) . (2 . a)) . ((a . 3) . (4 . a)))
                (subst-if-not 'a
                              (lambda (y) (not (and (numberp y) (zerop (1- y)))))
                              '(((0 . 1) . (2 . 0)) . ((0 . 3) . (4 . 0)))
                              :key (lambda (x) (if (numberp x) (1+ x) x)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Tests for the nsubst function

(define-test |nsubst test=eql key=identity 1a|
  (let ((tree (copy-tree '())))
    (assert-equal '()
                  (nsubst 'a 0 tree))))

(define-test |nsubst test=eql key=identity 1b|
  (let ((tree (copy-tree '())))
    (assert-equal '()
                  (nsubst 'a 0 tree :test #'eql))))

(define-test |nsubst test=eql key=identity 1c|
  (let ((tree (copy-tree '())))
    (assert-equal '()
                  (nsubst 'a 0 tree :test 'eql))))

(define-test |nsubst test=eql key=identity 2a|
  (let ((tree (copy-tree '(((0 . 1) . (2 . 0)) . ((0 . 3) . (4 . 0)))))
        (tree2))
    (assert-equal '(((a . 1) . (2 . a)) . ((a . 3) . (4 . a)))
                  (setf tree2 (nsubst 'a 0 tree)))
    (assert-eq tree tree2)))

(define-test |nsubst test=eql key=identity 2b|
  (let ((tree (copy-tree '(((0 . 1) . (2 . 0)) . ((0 . 3) . (4 . 0)))))
        (tree2))
    (assert-equal '(((a . 1) . (2 . a)) . ((a . 3) . (4 . a)))
                  (setf tree2 (nsubst 'a 0 tree :test #'eql)))
    (assert-eq tree tree2)))

(define-test |nsubst test=eql key=identity 2c|
  (let ((tree (copy-tree '(((0 . 1) . (2 . 0)) . ((0 . 3) . (4 . 0)))))
        (tree2))
    (assert-equal '(((a . 1) . (2 . a)) . ((a . 3) . (4 . a)))
                  (setf tree2 (nsubst 'a 0 tree :test 'eql)))
    (assert-eq tree tree2)))    

(define-test |nsubst test=eql key=identity 3a|
  (assert-equal 2
                (nsubst 2 1 1)))

(define-test |nsubst test=eql key=identity 3b|
  (assert-equal 2
                (nsubst 2 1 1 :test #'eql)))

(define-test |nsubst test=eql key=identity 3c|
  (assert-equal 2
                (nsubst 2 1 1 :test 'eql)))

(define-test |nsubst test=eql key=nil 1|
  (let ((tree (copy-tree '(((0 . 1) . (2 . 0)) . ((0 . 3) . (4 . 0)))))
        (tree2))
    (assert-equal '(((a . 1) . (2 . a)) . ((a . 3) . (4 . a)))
                  (setf tree2 (nsubst 'a 0 tree :key nil)))
    (assert-eq tree tree2)))    

(define-test |nsubst test=eq key=identity 1a|
  (let ((tree (copy-tree '())))
    (assert-equal '()
                  (nsubst 'a 'b tree :test #'eq))))

(define-test |nsubst test=eq key=identity 1b|
  (let ((tree (copy-tree '())))
    (assert-equal '()
                  (nsubst 'a 'b tree :test 'eq))))

(define-test |nsubst test=eq key=identity 2a|
  (let ((tree (copy-tree '(((b . c) . (f . b)) . ((b . d) . (e . b)))))
        (tree2))
    (assert-equal '(((a . c) . (f . a)) . ((a . d) . (e . a)))
                  (setf tree2 (nsubst 'a 'b tree :test #'eq)))
    (assert-eq tree tree2)))

(define-test |nsubst test=eq key=identity 2a|
  (let ((tree (copy-tree '(((b . c) . (f . b)) . ((b . d) . (e . b)))))
        (tree2))
    (assert-equal '(((a . c) . (f . a)) . ((a . d) . (e . a)))
                  (setf tree2 (nsubst 'a 'b tree :test 'eq)))
    (assert-eq tree tree2)))

(define-test |nsubst test=eq key=identity 3a|
  (assert-equal 'b
                (nsubst 'b 'a 'a :test #'eq)))

(define-test |nsubst test=eq key=identity 3b|
  (assert-equal 'b
                (nsubst 'b 'a 'a :test 'eq)))

(define-test |nsubst test=other key=identity 1|
  (let ((tree (copy-tree '())))
    (assert-equal '()
                  (nsubst 'a 0 tree
                          :test (lambda (x y) (and (numberp y) (= x (1- y))))))))

(define-test |nsubst test=other key=identity 2|
  (let ((tree (copy-tree '(((0 . 1) . (2 . 0)) . ((0 . 3) . (4 . 0)))))
        (tree2))
    (assert-equal '(((a . 1) . (2 . a)) . ((a . 3) . (4 . a)))
                  (setf tree2 (nsubst 'a 0 tree
                                      :test (lambda (x y) (and (numberp y) (= x y))))))
    (assert-eq tree tree2)))

(define-test |nsubst test=other key=identity 3|
  (assert-equal 2
                (nsubst 2 1 0 :test (lambda (x y)
                                      (and (numberp x)
                                           (numberp y)
                                           (<= (abs (- x y)) 1))))))

(define-test |nsubst test=eql key=other 1a|
  (let ((tree (copy-tree '())))
    (assert-equal '()
                  (nsubst 'a 0 tree
                          :key (lambda (x) (if (numberp x) (1+ x) x))))))

(define-test |nsubst test=eql key=other 1b|
  (let ((tree (copy-tree '())))
    (assert-equal '()
                  (nsubst 'a 0 tree
                          :key (lambda (x) (if (numberp x) (1+ x) x))
                          :test #'eql))))

(define-test |nsubst test=eql key=other 1c|
  (let ((tree (copy-tree '())))
    (assert-equal '()
                  (nsubst 'a 0 tree
                          :key (lambda (x) (if (numberp x) (1+ x) x))
                          :test 'eql))))

(define-test |nsubst test=eql key=other 2a|
  (let ((tree (copy-tree '(((0 . 1) . (2 . 0)) . ((0 . 3) . (4 . 0)))))
        (tree2))
    (assert-equal '(((a . 1) . (2 . a)) . ((a . 3) . (4 . a)))
                  (setf tree2 (nsubst 'a 1 tree
                                      :key (lambda (x) (if (numberp x) (1+ x) x)))))
    (assert-eq tree tree2)))

(define-test |nsubst test=eql key=other 2b|
  (let ((tree (copy-tree '(((0 . 1) . (2 . 0)) . ((0 . 3) . (4 . 0)))))
        (tree2))
    (assert-equal '(((a . 1) . (2 . a)) . ((a . 3) . (4 . a)))
                  (setf tree2 (nsubst 'a 1 tree
                                      :key (lambda (x) (if (numberp x) (1+ x) x))
                                      :test #'eql)))
    (assert-eq tree tree2)))

(define-test |nsubst test=eql key=other 2c|
  (let ((tree (copy-tree '(((0 . 1) . (2 . 0)) . ((0 . 3) . (4 . 0)))))
        (tree2))
    (assert-equal '(((a . 1) . (2 . a)) . ((a . 3) . (4 . a)))
                  (setf tree2 (nsubst 'a 1 tree
                                      :key (lambda (x) (if (numberp x) (1+ x) x))
                                      :test 'eql)))
    (assert-eq tree tree2)))

(define-test |nsubst test=eql key=other 3a|
  (assert-equal 2
                (nsubst 2 1 0 :key #'1+)))

(define-test |nsubst test=eql key=other 3b|
  (assert-equal 2
                (nsubst 2 1 0 :key #'1+ :test #'eql)))

(define-test |nsubst test=eql key=other 3c|
  (assert-equal 2
                (nsubst 2 1 0 :key #'1+ :test 'eql)))

(define-test |nsubst test=eq key=other 1a|
  (let ((tree (copy-tree '())))
    (assert-equal '()
                  (nsubst 'a 'bb tree
                          :test #'eq
                          :key (lambda (x) (if (eq x 'b) 'bb x))))))

(define-test |nsubst test=eq key=other 1b|
  (let ((tree (copy-tree '())))
    (assert-equal '()
                  (nsubst 'a 'bb tree
                          :test #'eq
                          :key (lambda (x) (if (eq x 'b) 'bb x))))))

(define-test |nsubst test=eq key=other 2a|
  (let ((tree (copy-tree '(((b . c) . (f . b)) . ((b . d) . (e . b)))))
        (tree2))
    (assert-equal '(((a . c) . (f . a)) . ((a . d) . (e . a)))
                  (setf tree2 (nsubst 'a 'bb tree
                                      :test #'eq
                                      :key (lambda (x) (if (eq x 'b) 'bb x)))))
    (assert-eq tree tree2)))

(define-test |nsubst test=eq key=other 2b|
  (let ((tree (copy-tree '(((b . c) . (f . b)) . ((b . d) . (e . b)))))
        (tree2))
    (assert-equal '(((a . c) . (f . a)) . ((a . d) . (e . a)))
                  (setf tree2 (nsubst 'a 'bb tree
                                      :test 'eq
                                      :key (lambda (x) (if (eq x 'b) 'bb x)))))
    (assert-eq tree tree2)))

(define-test |nsubst test=eq key=other 3a|
  (assert-equal 'b
                (nsubst 'b 'a 'c
                        :test #'eq
                        :key (lambda (x) (if (eq x 'c) 'a x)))))

(define-test |nsubst test=eq key=other 3b|
  (assert-equal 'b
                (nsubst 'b 'a 'c
                        :test 'eq
                        :key (lambda (x) (if (eq x 'c) 'a x)))))

(define-test |nsubst test=other key=other 1|
  (let ((tree (copy-tree '())))
    (assert-equal '()
                  (nsubst 'a 0 tree
                          :test (lambda (x y) (and (numberp y) (= x (1- y))))
                          :key (lambda (x) (if (numberp x) (1+ x) x))))))

(define-test |nsubst test=other key=other 2|
  (let ((tree (copy-tree '(((0 . 1) . (2 . 0)) . ((0 . 3) . (4 . 0)))))
        (tree2))
    (assert-equal '(((a . 1) . (2 . a)) . ((a . 3) . (4 . a)))
                  (setf tree2 (nsubst 'a 0 tree
                                      :test (lambda (x y) (and (numberp y) (= x (1- y))))
                                      :key (lambda (x) (if (numberp x) (1+ x) x)))))
    (assert-eq tree tree2)))

(define-test |nsubst test=other key=other 3|
  (assert-equal 2
                (nsubst 2 1 -1
                        :test (lambda (x y)
                                (and (numberp x)
                                     (numberp y)
                                     (<= (abs (- x y)) 1)))
                        :key #'1+)))

(define-test |nsubst test-not=other key=identity 1|
  (let ((tree (copy-tree '())))
    (assert-equal '()
                  (nsubst 'a 0 tree
                          :test-not (lambda (x y)
                                      (not (and (numberp y) (= x (1- y)))))))))

(define-test |nsubst test-not=other key=identity 2|
  (let ((tree (copy-tree '(((0 . 1) . (2 . 0)) . ((0 . 3) . (4 . 0)))))
        (tree2))
    (assert-equal '(((a . 1) . (2 . a)) . ((a . 3) . (4 . a)))
                  (setf tree2 (nsubst 'a 0 tree
                                      :test-not (lambda (x y)
                                                  (not (and (numberp y) (= x y)))))))
    (assert-eq tree tree2)))

(define-test |nsubst test-not=other key=identity 3|
  (assert-equal 2
                (nsubst 2 1 0
                        :test-not (lambda (x y)
                                    (not (and (numberp x)
                                              (numberp y)
                                              (<= (abs (- x y)) 1)))))))

(define-test |nsubst test-not=any key=other 2|
  (let ((tree (copy-tree '(((0 . 1) . (2 . 0)) . ((0 . 3) . (4 . 0)))))
        (tree2))
    (assert-equal '(((a . 1) . (2 . a)) . ((a . 3) . (4 . a)))
                  (setf tree2 (nsubst 'a 0 tree
                                      :test-not (lambda (x y) (not (and (numberp y) (= x (1- y)))))
                                      :key (lambda (x) (if (numberp x) (1+ x) x)))))
    (assert-eq tree tree2)))

(define-test |nsubst test-not=other key=other 3|
  (assert-equal 2
                (nsubst 2 1 -1
                        :test-not (lambda (x y)
                                    (not (and (numberp x)
                                              (numberp y)
                                              (<= (abs (- x y)) 1))))
                        :key #'1+)))

(define-test |nsubst test-not=other key=other 4|
  (assert-equal -1
                (nsubst 2 1 -1
                        :test-not (lambda (x y)
                                    (not (and (numberp x)
                                              (numberp y)
                                              (<= (abs (- x y)) 1))))
                        :key #'1-)))

(define-test |nsubst test=other test-not=other|
  (assert-error 'error
                (nsubst 0 1 2 :test #'= :test-not #'=)))

(define-test |nsubst test=nil key=identity 1|
  (let ((tree (copy-tree '(((0 . 1) . (2 . 0)) . ((0 . 3) . (4 . 0)))))
        (tree2))
    (assert-error 'error
                  (setf tree2 (nsubst 'a 0 tree :test nil)))))

(define-test |nsubst test-not=nil key=identity 1|
  (let ((tree (copy-tree '(((0 . 1) . (2 . 0)) . ((0 . 3) . (4 . 0)))))
        (tree2))
    (assert-error 'error
                  (setf tree2 (nsubst 'a 0 tree :test-not nil)))))

;;; old one left over.  Is it still needed?
(define-test nsubst.1
  (let ((tree (copy-tree '(a ((b))))))
    (assert-equal '(a (c))
                  (nsubst 'c '(b) tree :test-not (complement #'equal)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Tests for the nsubst-if function

(define-test |nsubst-if key=identity 1|
  (let ((tree (copy-tree '())))
    (assert-equal '()
                  (nsubst-if 'a
                             (lambda (y) (and (numberp y) (zerop (1- y))))
                             tree))))

(define-test |nsubst-if key=identity 2|
  (let ((tree (copy-tree '(((0 . 1) . (2 . 0)) . ((0 . 3) . (4 . 0)))))
        (tree2))
    (assert-equal '(((a . 1) . (2 . a)) . ((a . 3) . (4 . a)))
                  (setf tree2 (nsubst-if 'a
                                         (lambda (y) (and (numberp y) (zerop y)))
                                         tree)))
    (assert-eq tree tree2)))

(define-test |nsubst-if key=identity 3|
  (assert-equal 2
                (nsubst-if 2 #'zerop 0)))

(define-test |nsubst-if key=identity 4|
  (assert-equal 1
                (nsubst-if 2 #'zerop 1)))

(define-test |nsubst-if key=other 1|
  (let ((tree (copy-tree '())))
    (assert-equal '()
                  (nsubst-if 'a
                             (lambda (y) (and (numberp y) (zerop (1- y))))
                             tree
                             :key (lambda (x) (if (numberp x) (1+ x) x))))))

(define-test |nsubst-if key=other 2|
  (let ((tree (copy-tree '(((0 . 1) . (2 . 0)) . ((0 . 3) . (4 . 0)))))
        (tree2))
    (assert-equal '(((a . 1) . (2 . a)) . ((a . 3) . (4 . a)))
                  (setf tree2 (nsubst-if 'a
                                         (lambda (y) (and (numberp y) (zerop (1- y))))
                                         tree
                                         :key (lambda (x) (if (numberp x) (1+ x) x)))))
    (assert-eq tree tree2)))

(define-test |nsubst-if key=other 3|
  (assert-equal 2
                (nsubst-if 2 #'zerop 1 :key #'1-)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Tests for the nsubst-if-not function

(define-test |nsubst-if-not key=identity 1|
  (let ((tree (copy-tree '())))
    (assert-equal '()
                  (nsubst-if-not 'a
                                 (lambda (y) (not (and (numberp y) (zerop (1- y)))))
                                 tree))))

(define-test |nsubst-if-not key=identity 2|
  (let ((tree (copy-tree '(((0 . 1) . (2 . 0)) . ((0 . 3) . (4 . 0)))))
        (tree2))
    (assert-equal '(((a . 1) . (2 . a)) . ((a . 3) . (4 . a)))
                  (setf tree2 (nsubst-if-not 'a
                                             (lambda (y) (not (and (numberp y) (zerop y))))
                                             tree)))
    (assert-eq tree tree2)))

(define-test |nsubst-if-not key=identity 3|
  (assert-equal 2
                (nsubst-if-not 2 #'zerop 1)))

(define-test |nsubst-if-not key=other 1|
  (let ((tree (copy-tree '())))
    (assert-equal '()
                  (nsubst-if-not 'a
                                 (lambda (y) (not (and (numberp y) (zerop (1- y)))))
                                 tree
                                 :key (lambda (x) (if (numberp x) (1+ x) x))))))

(define-test |nsubst-if-not key=other 2|
  (let ((tree (copy-tree '(((0 . 1) . (2 . 0)) . ((0 . 3) . (4 . 0)))))
        (tree2))
    (assert-equal '(((a . 1) . (2 . a)) . ((a . 3) . (4 . a)))
                  (setf tree2 (nsubst-if-not 'a
                                             (lambda (y) (not (and (numberp y) (zerop (1- y)))))
                                             tree
                                             :key (lambda (x) (if (numberp x) (1+ x) x)))))
    (assert-eq tree tree2)))

(define-test |nsubst-if-not key=other 3|
  (assert-equal 2
                (nsubst-if-not 2 #'zerop 0 :key #'1+)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Tests for the null function

(define-test null.1
  (assert-equal t
                (null '())))

(define-test null.2
  (assert-equal nil
                (null 1)))

(define-test null.3
  (assert-equal nil
                (null #\a)))

(define-test null.4
  (assert-equal nil
                (null #())))

(define-test null.5
  (assert-equal nil
                (null "")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Tests for the acons function

(define-test acons.1
  (assert-equal '((a . b))
                (acons 'a 'b '())))

(define-test acons.2
  (assert-equal '((a . b) c)
                (acons 'a 'b '(c))))

(define-test acons.3
  (assert-equal '((a . b) . c)
                (acons 'a 'b 'c)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Tests for the member function

(define-test |member test=eql key=identity 1|
  (assert-equal nil
                (member 234 '())))

(define-test |member test=eql key=identity 2a|
  (assert-equal '(123 b c)
                (member 123 '(a b 123 b c))))

(define-test |member test=eql key=identity 2b|
  (assert-equal '(123 b c)
                (member 123 '(a b 123 b c) :test #'eql)))

(define-test |member test=eql key=identity 2c|
  (assert-equal '(123 b c)
                (member 123 '(a b 123 b c) :test 'eql)))

(define-test |member test=eql key=identity 3|
  (assert-equal nil
                (member 123 '(a b c d e))))

(define-test |member test=eql key=other 1|
  (assert-equal '(123 b c)
                (member 124 '(a b 123 b c)
                        :key (lambda (x) (if (numberp x) (1+ x) x)))))

(define-test |member test=eql key=other 2|
  (assert-equal '(123 b c)
                (member 124 '(a b 123 b c)
                        :key (lambda (x) (if (numberp x) (1+ x) x))
                        :test 'eql)))

(define-test |member test=eql key=nil 1|
  (assert-equal '(123 b c)
                (member 123 '(a b 123 b c) :key nil)))

(define-test |member test=eq key=identity 1|
  (assert-equal '(d e)
                (member 'd '(a b c d e)
                        :test #'eq)))

(define-test |member test=eq key=identity 2|
  (assert-equal nil
                (member 'f '(a b c d e)
                        :test #'eq)))

(define-test |member test=eq key=other 1|
  (assert-equal '((d) (e))
                (member 'd '((a) (b) (c) (d) (e))
                        :key #'car
                        :test #'eq)))

(define-test |member test=eq key=other 2|
  (assert-equal nil
                (member 'f '((a) (b) (c) (d) (e))
                        :key #'car
                        :test #'eq)))

(define-test |member test-not=eql key=identity 1|
  (assert-equal '(3 4 5 2)
                (member 2 '(2 2 2 3 4 5 2)
                        :test-not #'eql)))

(define-test |member test-not=eql key=identity 2|
  (assert-equal nil
                (member 2 '(2 2 2)
                        :test-not #'eql)))

(define-test |member test-not=eql key=other 1|
  (assert-equal '(3 4 5 2)
                (member 3 '(2 2 2 3 4 5 2)
                        :key #'1+
                        :test-not #'eql)))

(define-test |member test-not=eql key=other 2|
  (assert-equal nil
                (member 3 '(2 2 2)
                        :key #'1+
                        :test-not #'eql)))

(define-test |member test-not=eq key=identity 1|
  (assert-equal '(3 4 5 a)
                (member 'a '(a a a 3 4 5 a)
                        :test-not #'eq)))

(define-test |member test-not=eq key=identity 2|
  (assert-equal nil
                (member 'a '(a a a)
                        :test-not #'eq)))

(define-test |member test-not=eq key=other 1|
  (assert-equal '((3) 4 5 a)
                (member 'a '((a) (a) (a) (3) 4 5 a)
                        :key #'car
                        :test-not #'eq)))

(define-test |member test-not=eq key=other 2|
  (assert-equal nil
                (member 'a '((a) (a) (a))
                        :key #'car
                        :test-not #'eq)))

(define-test |member test=other key=identity 1|
  (assert-equal '((123) b c)
                (member '(123) '(a b (123) b c)
                        :test #'equal)))

(define-test |member test=other key=other 1|
  (assert-equal '(3 4 5)
                (member 5 '(1 2 3 4 5)
                        :key #'1+
                        :test (lambda (x y) (= x (1+ y))))))

(define-test |member test=other key=other 2|
  (assert-equal nil
                (member 10 '(1 2 3 4 5)
                        :key #'1+
                        :test (lambda (x y) (= x (1+ y))))))

(define-test |member test-not=other key=other 1|
  (assert-equal '(3 4 5)
                (member 4 '(1 2 3 4 5)
                        :key #'1+
                        :test-not (lambda (x y) (not (zerop (mod y x)))))))

(define-test |member test-not=other key=other 2|
  (assert-equal nil
                (member 10 '(1 2 3 4 5)
                        :key #'1+
                        :test-not (lambda (x y) (not (zerop (mod y x)))))))

(define-test |member error 1|
  (assert-error 'type-error
                (member 123 '(a b . 123))))

(define-test |member error 2|
  (assert-error 'error
                (member 123 '(1 2 3)
                        :test #'eql
                        :test-not #'eq)))
                        
(define-test |member test=nil key=identity 1|
  (assert-error 'error
                (member 123 '(a b 123 b c) :test nil)))

(define-test |member test-not=nil key=identity 1|
  (assert-error 'error
                (member 123 '(a b 123 b c) :test-not nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Tests for the member-if function

(define-test |member-if key=identity 1|
  (assert-equal nil
                (member-if #'oddp '(2 4 6))))

(define-test |member-if key=identity 2|
  (assert-equal '(3 4)
                (member-if #'oddp '(2 6 3 4))))

(define-test |member-if key=other 1|
  (assert-equal '(3 4)
                (member-if #'evenp '(2 6 3 4)
                           :key #'1+)))

(define-test |member-if key=other 2|
  (assert-equal nil
                (member-if #'evenp '(2 6 4)
                           :key #'1+)))

(define-test |member-if error 1|
  (assert-error 'type-error
                (member-if #'oddp '(2 4 6 . 7))))

(define-test |member-if key=other error 1|
  (assert-error 'type-error
                (member-if #'oddp '(2 4 6 . 7) :key #'identity)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Tests for the member-if-not function

(define-test |member-if-not key=identity 1|
  (assert-equal nil
                (member-if-not #'evenp '(2 4 6))))

(define-test |member-if-not key=identity 2|
  (assert-equal '(3 4)
                (member-if-not #'evenp '(2 6 3 4))))

(define-test |member-if-not key=other 1|
  (assert-equal nil
                (member-if-not #'oddp '(2 4 6)
                               :key #'1+)))

(define-test |member-if-not key=other 2|
  (assert-equal '(3 4)
                (member-if-not #'oddp '(2 6 3 4)
                               :key #'1+)))

(define-test |member-if-not error 1|
  (assert-error 'type-error
                (member-if-not #'evenp '(2 4 6 . 7))))

(define-test |member-if-not key=other error 1|
  (assert-error 'type-error
                (member-if-not #'evenp '(2 4 6 . 7) :key #'identity)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Tests for the assoc function

(define-test |assoc test=eql key=identity 1a|
  (assert-equal '(a . b)
                (assoc 'a '((b) nil (a . b) (a . c)))))

(define-test |assoc test=eql key=identity 1b|
  (assert-equal '(a . b)
                (assoc 'a '((b) nil (a . b) (a . c))
                       :test #'eql)))

(define-test |assoc test=eql key=identity 1c|
  (assert-equal '(a . b)
                (assoc 'a '((b) nil (a . b) (a . c))
                       :test 'eql)))

(define-test |assoc test=eql key=identity 2a|
  (assert-equal nil
                (assoc 'c '((b) nil (a . b) (a . c)))))

(define-test |assoc test=eql key=identity 2b|
  (assert-equal nil
                (assoc 'c '((b) nil (a . b) (a . c))
                       :test #'eql)))

(define-test |assoc test=eql key=identity 2c|
  (assert-equal nil
                (assoc 'c '((b) nil (a . b) (a . c))
                       :test 'eql)))

(define-test |assoc test=eql key=identity 3a|
  (assert-equal '(nil . c)
                (assoc 'nil '((b) nil (a . b) (nil . c)))))

(define-test |assoc test=eql key=identity 3b|
  (assert-equal '(nil . c)
                (assoc 'nil '((b) nil (a . b) (nil . c))
                       :test #'eql)))

(define-test |assoc test=eql key=identity 3c|
  (assert-equal '(nil . c)
                (assoc 'nil '((b) nil (a . b) (nil . c))
                       :test 'eql)))

(define-test |assoc test=eql key=identity 4a|
  (assert-equal '(123 . b)
                (assoc 123 '((b) nil (123 . b) (nil . c)))))

(define-test |assoc test=eql key=identity 4b|
  (assert-equal '(123 . b)
                (assoc 123 '((b) nil (123 . b) (nil . c))
                       :test #'eql)))

(define-test |assoc test=eql key=identity 4c|
  (assert-equal '(123 . b)
                (assoc 123 '((b) nil (123 . b) (nil . c))
                       :test 'eql)))

(define-test |assoc test=eql key=identity 5a|
  (assert-equal '(#\a . b)
                (assoc #\a '((b) nil (#\a . b) (nil . c)))))

(define-test |assoc test=eql key=identity 5b|
  (assert-equal '(#\a . b)
                (assoc #\a '((b) nil (#\a . b) (nil . c))
                       :test #'eql)))

(define-test |assoc test=eql key=identity 5c|
  (assert-equal '(#\a . b)
                (assoc #\a '((b) nil (#\a . b) (nil . c))
                       :test 'eql)))

(define-test |assoc test=eql key=nil 1|
  (assert-equal '(nil . c)
                (assoc 'nil '((b) nil (a . b) (nil . c))
                       :key nil)))

(define-test |assoc test=eq key=identity 1a|
  (assert-equal '(a . b)
                (assoc 'a '((b) nil (a . b) (a . c))
                       :test #'eq)))

(define-test |assoc test=eq key=identity 1b|
  (assert-equal '(a . b)
                (assoc 'a '((b) nil (a . b) (a . c))
                       :test 'eq)))

(define-test |assoc test=eq key=identity 2a|
  (assert-equal nil
                (assoc 'c '((b) nil (a . b) (a . c))
                       :test #'eq)))

(define-test |assoc test=eq key=identity 2b|
  (assert-equal nil
                (assoc 'c '((b) nil (a . b) (a . c))
                       :test 'eq)))

(define-test |assoc test=eq key=identity 3a|
  (assert-equal '(nil . c)
                (assoc 'nil '((b) nil (a . b) (nil . c))
                       :test #'eq)))

(define-test |assoc test=eq key=identity 3b|
  (assert-equal '(nil . c)
                (assoc 'nil '((b) nil (a . b) (nil . c))
                       :test 'eq)))

(define-test |assoc test=other key=identity 1|
  (assert-equal '((a b) c)
                (assoc '(a b) '((a . b) nil ((a b) c) (d e))
                       :test #'equal)))

(define-test |assoc test-not=other key=identity 1|
  (assert-equal '((a b) c)
                (assoc '(a b) '((a . b) nil ((a b) c) (d e))
                       :test-not (complement #'equal))))

(define-test |assoc test=eql key=other 1a|
  (assert-equal '((a b) c)
                (assoc 'a '(((b a) . b) nil ((a b) c) ((d) e))
                       :key #'car)))

(define-test |assoc test=eql key=other 1b|
  (assert-equal '((a b) c)
                (assoc 'a '(((b a) . b) nil ((a b) c) ((d) e))
                       :test #'eql
                       :key #'car)))

(define-test |assoc test=eql key=other 1c|
  (assert-equal '((a b) c)
                (assoc 'a '(((b a) . b) nil ((a b) c) ((d) e))
                       :test 'eql
                       :key #'car)))

(define-test |assoc test=eq key=other 1|
  (assert-equal '((a b) c)
                (assoc 'a '(((b a) . b) nil ((a b) c) ((d) e))
                       :test #'eq
                       :key #'car)))

(define-test |assoc test=other key=other 1|
  (assert-equal '((a b) c)
                (assoc 'a '(((b a) . b) nil ((a b) c) ((d) e))
                       :test (lambda (x y) (eq x y))
                       :key #'car)))

(define-test |assoc test-not=eql key=identity 1a|
  (assert-equal '(345 b)
                (assoc '234 '((234 a) (345 b) (234 c))
                       :test-not #'eql)))
                 
(define-test |assoc test-not=eql key=identity 1b|
  (assert-equal '(345 b)
                (assoc '234 '((234 a) (345 b) (234 c))
                       :test-not 'eql)))
                 
(define-test |assoc test-not=eql key=identity 2|
  (assert-equal nil
                (assoc '234 '((234 a) (234 b) (234 c))
                       :test-not #'eql)))
                 
(define-test |assoc test-not=eql key=identity 3|
  (assert-equal '(345 b)
                (assoc '234 '((234 a) nil (345 b) (234 c))
                       :test-not #'eql)))
                 
(define-test |assoc test-not=eq key=identity 1a|
  (assert-equal '(y b)
                (assoc 'x '((x a) (y b) (x c))
                       :test-not #'eq)))
                 
(define-test |assoc test-not=eq key=identity 1b|
  (assert-equal '(y b)
                (assoc 'x '((x a) (y b) (x c))
                       :test-not 'eq)))
                 
(define-test |assoc test-not=eq key=identity 2|
  (assert-equal nil
                (assoc 'x '((x a) (x b) (x c))
                       :test-not #'eq)))
                 
(define-test |assoc test-not=eq key=identity 3|
  (assert-equal '(y b)
                (assoc 'x '((x a) nil (y b) (x c))
                       :test-not #'eq)))
                 
(define-test assoc.error.1
  (assert-error 'type-error
                (assoc 'a '((b . c) nil d (a b)))))

(define-test |assoc test-not=eql key=other 1a|
  (assert-equal '(345 b)
                (assoc '235 '((234 a) (345 b) (234 c))
                       :test-not #'eql
                       :key #'1+)))
                 
(define-test |assoc test-not=eql key=other 1b|
  (assert-equal '(345 b)
                (assoc '235 '((234 a) (345 b) (234 c))
                       :test-not 'eql
                       :key #'1+)))
                 
(define-test |assoc test-not=eql key=other 2|
  (assert-equal nil
                (assoc '235 '((234 a) (234 b) (234 c))
                       :test-not #'eql
                       :key #'1+)))
                 
(define-test |assoc test-not=eql key=other 3|
  (assert-equal '(345 b)
                (assoc '235 '((234 a) nil (345 b) (234 c))
                       :test-not #'eql
                       :key #'1+)))
                 
(define-test |assoc test-not=eq key=other 1a|
  (assert-equal '((y) b)
                (assoc 'x '(((x) a) ((y) b) ((x) c))
                       :test-not #'eq
                       :key #'car)))
                 
(define-test |assoc test-not=eq key=other 1b|
  (assert-equal '((y) b)
                (assoc 'x '(((x) a) ((y) b) ((x) c))
                       :test-not 'eq
                       :key #'car)))
                 
(define-test |assoc test-not=eq key=other 2|
  (assert-equal nil
                (assoc 'x '(((x) a) ((x) b) ((x) c))
                       :test-not #'eq
                       :key #'car)))
                 
(define-test |assoc test-not=eq key=other 3|
  (assert-equal '((y) b)
                (assoc 'x '(((x) a) nil ((y) b) ((x) c))
                       :test-not #'eq
                       :key #'car)))
                 
(define-test |assoc test-not=other key=other 1a|
  (assert-equal '(345 b)
                (assoc '234 '((234 a) (345 b) (234 c))
                       :test-not (lambda (x y) (eql (1+ x) y))
                       :key #'1+)))
                 
(define-test |assoc test-not=other key=other 1b|
  (assert-equal '(345 b)
                (assoc '234 '((234 a) (345 b) (234 c))
                       :test-not (lambda (x y) (eql (1+ x) y))
                       :key #'1+)))
                 
(define-test |assoc test-not=other key=other 2|
  (assert-equal nil
                (assoc '234 '((234 a) (234 b) (234 c))
                       :test-not (lambda (x y) (eql (1+ x) y))
                       :key #'1+)))
                 
(define-test |assoc test-not=other key=other 3|
  (assert-equal '(345 b)
                (assoc '234 '((234 a) nil (345 b) (234 c))
                       :test-not (lambda (x y) (eql (1+ x) y))
                       :key #'1+)))
                 
(define-test |assoc error 1|
  (assert-error 'type-error
                (assoc 'a '((b . c) nil d (a b)))))

(define-test |assoc error 2|
  (assert-error 'error
                (assoc 'a '((b . c) nil d (a b))
                       :test #'eq
                       :test-not #'eq)))

(define-test |assoc test=nil key=identity 1|
  (assert-error 'error
                (assoc 'nil '((b) nil (a . b) (nil . c))
                       :test nil)))

(define-test |assoc test-not=nil key=identity 1|
  (assert-error 'error
                (assoc 'nil '((b) nil (a . b) (nil . c))
                       :test-not nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Tests for the assoc-if function

(define-test |assoc-if key=identity 1|
  (assert-equal '(a . b)
                (assoc-if (lambda (x) (eq x 'a))
                          '((b) nil (a . b) (a . c)))))

(define-test |assoc-if key=identity 2|
  (assert-equal nil
                (assoc-if (lambda (x) (eq x 'c))
                          '((b) nil (a . b) (a . c)))))

(define-test |assoc-if key=identity 3|
  (assert-equal '(nil . c)
                (assoc-if #'null
                          '((b) nil (a . b) (nil . c)))))

(define-test |assoc-if key=identity 4|
  (assert-equal '(123 . b)
                (assoc-if (lambda (x) (eql x 123))
                          '((b) nil (123 . b) (nil . c)))))

(define-test |assoc-if key=identity 5|
  (assert-equal '(#\a . b)
                (assoc-if (lambda (x) (eql x #\a))
                          '((b) nil (#\a . b) (nil . c)))))

(define-test |assoc-if key=other 1|
  (assert-equal '((a b) c)
                (assoc-if (lambda (x) (eq x 'a))
                          '(((b a) . b) nil ((a b) c) ((d) e))
                          :key #'car)))

(define-test |assoc-if error 1|
  (assert-error 'type-error
                (assoc-if (lambda (x) (eq x 'a))
                          '((b . c) nil d (a b)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Tests for the assoc-if-not function

(define-test |assoc-if-not key=identity 1|
  (assert-equal '(a . b)
                (assoc-if-not (complement (lambda (x) (eq x 'a)))
                              '((b) nil (a . b) (a . c)))))

(define-test |assoc-if-not key=identity 2|
  (assert-equal nil
                (assoc-if-not (complement (lambda (x) (eq x 'c)))
                              '((b) nil (a . b) (a . c)))))

(define-test |assoc-if-not key=identity 3|
  (assert-equal '(nil . c)
                (assoc-if-not (complement #'null)
                              '((b) nil (a . b) (nil . c)))))

(define-test |assoc-if-not key=identity 4|
  (assert-equal '(123 . b)
                (assoc-if-not (complement (lambda (x) (eql x 123)))
                              '((b) nil (123 . b) (nil . c)))))

(define-test |assoc-if-not key=identity 5|
  (assert-equal '(#\a . b)
                (assoc-if-not (complement (lambda (x) (eql x #\a)))
                              '((b) nil (#\a . b) (nil . c)))))

(define-test |assoc-if-not key=other 6|
  (assert-equal '((a b) c)
                (assoc-if-not (complement (lambda (x) (eq x 'a)))
                              '(((b a) . b) nil ((a b) c) ((d) e))
                              :key #'car)))

(define-test |assoc-if-not error 1|
  (assert-error 'type-error
                (assoc-if-not (complement (lambda (x) (eq x 'a)))
                              '((b . c) nil d (a b)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Tests for the rassoc function

(define-test |rassoc test=eql key=identity 1a|
  (assert-equal '(b . a)
                (rassoc 'a '((b) nil (b . a) (c . a)))))

(define-test |rassoc test=eql key=identity 1b|
  (assert-equal '(b . a)
                (rassoc 'a '((b) nil (b . a) (c . a))
                       :test #'eql)))

(define-test |rassoc test=eql key=identity 1c|
  (assert-equal '(b . a)
                (rassoc 'a '((b) nil (b . a) (c . a))
                       :test 'eql)))

(define-test |rassoc test=eql key=identity 2a|
  (assert-equal nil
                (rassoc 'c '((b) nil (b . a) (c . a)))))

(define-test |rassoc test=eql key=identity 2b|
  (assert-equal nil
                (rassoc 'c '((b) nil (b . a) (c . a))
                       :test #'eql)))

(define-test |rassoc test=eql key=identity 2c|
  (assert-equal nil
                (rassoc 'c '((b) nil (b . a) (c . a))
                       :test 'eql)))

(define-test |rassoc test=eql key=identity 3a|
  (assert-equal '(c . nil)
                (rassoc 'nil '((b . x) nil (b . a) (c . nil)))))

(define-test |rassoc test=eql key=identity 3b|
  (assert-equal '(c . nil)
                (rassoc 'nil '((x . b) nil (b . a) (c . nil))
                       :test #'eql)))

(define-test |rassoc test=eql key=identity 3c|
  (assert-equal '(c . nil)
                (rassoc 'nil '((b . x) nil (b . a) (c . nil))
                       :test 'eql)))

(define-test |rassoc test=eql key=identity 4a|
  (assert-equal '(b . 123)
                (rassoc 123 '((b . x) nil (b . 123) (c . nil)))))

(define-test |rassoc test=eql key=identity 4b|
  (assert-equal '(b . 123)
                (rassoc 123 '((b . x) nil (b . 123) (c . nil))
                       :test #'eql)))

(define-test |rassoc test=eql key=identity 4c|
  (assert-equal '(b . 123)
                (rassoc 123 '((b . x) nil (b . 123) (c . nil))
                       :test 'eql)))

(define-test |rassoc test=eql key=identity 5a|
  (assert-equal '(b . #\a)
                (rassoc #\a '((b . x) nil (b . #\a) (c . nil)))))

(define-test |rassoc test=eql key=identity 5b|
  (assert-equal '(b . #\a)
                (rassoc #\a '((b . x) nil (b . #\a) (c . nil))
                       :test #'eql)))

(define-test |rassoc test=eql key=identity 5c|
  (assert-equal '(b . #\a)
                (rassoc #\a '((b . x) nil (b . #\a) (c . nil))
                       :test 'eql)))

(define-test |rassoc test=eql key=nil 1|
  (assert-equal '(b . 123)
                (rassoc 123 '((b . x) nil (b . 123) (c . nil))
                       :key nil)))

(define-test |rassoc test=eq key=identity 1a|
  (assert-equal '(b . a)
                (rassoc 'a '((b . x) nil (b . a) (c . a))
                       :test #'eq)))

(define-test |rassoc test=eq key=identity 1b|
  (assert-equal '(b . a)
                (rassoc 'a '((b . x) nil (b . a) (c . a))
                       :test 'eq)))

(define-test |rassoc test=eq key=identity 2a|
  (assert-equal nil
                (rassoc 'c '((b . x) nil (b . a) (c . a))
                       :test #'eq)))

(define-test |rassoc test=eq key=identity 2b|
  (assert-equal nil
                (rassoc 'c '((b . x) nil (b . a) (c . a))
                       :test 'eq)))

(define-test |rassoc test=eq key=identity 3a|
  (assert-equal '(c . nil)
                (rassoc 'nil '((b . x) nil (b . a) (c . nil))
                       :test #'eq)))

(define-test |rassoc test=eq key=identity 3b|
  (assert-equal '(c . nil)
                (rassoc 'nil '((b . x) nil (b . a) (c . nil))
                       :test 'eq)))

(define-test |rassoc test=other key=identity 1|
  (assert-equal '(c . (a b))
                (rassoc '(a b) '((b . a) nil (c . (a b)) (e . d))
                       :test #'equal)))

(define-test |rassoc test-not=other key=identity 1|
  (assert-equal '(c . (a b))
                (rassoc '(a b) '((b . a) nil (c . (a b)) (e . d))
                       :test-not (complement #'equal))))

(define-test |rassoc test=eql key=other 1a|
  (assert-equal '(c . (a b))
                (rassoc 'a '((b . (b a)) nil (c . (a b)) (e . (d)))
                       :key #'car)))

(define-test |rassoc test=eql key=other 1b|
  (assert-equal '(c . (a b))
                (rassoc 'a '((b . (b a)) nil (c . (a b)) (e . (d)))
                       :test #'eql
                       :key #'car)))

(define-test |rassoc test=eql key=other 1c|
  (assert-equal '(c . (a b))
                (rassoc 'a '((b . (b a)) nil (c . (a b)) (e . (d)))
                       :test 'eql
                       :key #'car)))

(define-test |rassoc test=eq key=other 1|
  (assert-equal '(c . (a b))
                (rassoc 'a '((b . (b a)) nil (c . (a b)) (e . (d)))
                       :test #'eq
                       :key #'car)))

(define-test |rassoc test=other key=other 1|
  (assert-equal '(c . (a b))
                (rassoc 'a '((b . (b a)) nil (c . (a b)) (e . (d)))
                       :test (lambda (x y) (eq x y))
                       :key #'car)))

(define-test |rassoc test-not=eql key=identity 1a|
  (assert-equal '(b . 345)
                (rassoc '234 '((a . 234) (b . 345) (c . 234))
                       :test-not #'eql)))
                 
(define-test |rassoc test-not=eql key=identity 1b|
  (assert-equal '(b . 345)
                (rassoc '234 '((a . 234) (b . 345) (c . 234))
                       :test-not 'eql)))
                 
(define-test |rassoc test-not=eql key=identity 2|
  (assert-equal nil
                (rassoc '234 '((a . 234) (b . 234) (c . 234))
                       :test-not #'eql)))
                 
(define-test |rassoc test-not=eql key=identity 3|
  (assert-equal '(b . 345)
                (rassoc '234 '((a . 234) nil (b . 345) (c . 234))
                       :test-not #'eql)))
                 
(define-test |rassoc test-not=eq key=identity 1a|
  (assert-equal '(b . y)
                (rassoc 'x '((a . x) (b . y) (c . x))
                       :test-not #'eq)))
                 
(define-test |rassoc test-not=eq key=identity 1b|
  (assert-equal '(b . y)
                (rassoc 'x '((a . x) (b . y) (c . x))
                       :test-not 'eq)))
                 
(define-test |rassoc test-not=eq key=identity 2|
  (assert-equal nil
                (rassoc 'x '((a . x) (b . x) (c . x))
                       :test-not #'eq)))
                 
(define-test |rassoc test-not=eq key=identity 3|
  (assert-equal '(b . y)
                (rassoc 'x '((a . x) nil (b . y) (c . x))
                       :test-not #'eq)))
                 
(define-test rassoc.error.1
  (assert-error 'type-error
                (rassoc 'a '((b . c) nil d (a b)))))

(define-test |rassoc test-not=eql key=other 1a|
  (assert-equal '(b . 345)
                (rassoc '235 '((a . 234) (b . 345) (c . 234))
                       :test-not #'eql
                       :key #'1+)))
                 
(define-test |rassoc test-not=eql key=other 1b|
  (assert-equal '(b . 345)
                (rassoc '235 '((a . 234) (b . 345) (c . 234))
                       :test-not 'eql
                       :key #'1+)))
                 
(define-test |rassoc test-not=eql key=other 2|
  (assert-equal nil
                (rassoc '235 '((a . 234) (b . 234) (c . 234))
                       :test-not #'eql
                       :key #'1+)))
                 
(define-test |rassoc test-not=eql key=other 3|
  (assert-equal '(b . 345)
                (rassoc '235 '((a . 234) nil (b . 345) (c . 234))
                       :test-not #'eql
                       :key #'1+)))
                 
(define-test |rassoc test-not=eq key=other 1a|
  (assert-equal '(b . (y))
                (rassoc 'x '((a . (x)) (b . (y)) (c . (x)))
                       :test-not #'eq
                       :key #'car)))
                 
(define-test |rassoc test-not=eq key=other 1b|
  (assert-equal '(b . (y))
                (rassoc 'x '((a . (x)) (b . (y)) (c . (x)))
                       :test-not 'eq
                       :key #'car)))
                 
(define-test |rassoc test-not=eq key=other 2|
  (assert-equal nil
                (rassoc 'x '((a . (x)) (b . (x)) (c . (x)))
                       :test-not #'eq
                       :key #'car)))
                 
(define-test |rassoc test-not=eq key=other 3|
  (assert-equal '(b . (y))
                (rassoc 'x '((a . (x)) nil (b . (y)) (c . (x)))
                       :test-not #'eq
                       :key #'car)))
                 
(define-test |rassoc test-not=other key=other 1a|
  (assert-equal '(b . 345)
                (rassoc '234 '((a . 234) (b . 345) (c . 234))
                       :test-not (lambda (x y) (eql (1+ x) y))
                       :key #'1+)))
                 
(define-test |rassoc test-not=other key=other 1b|
  (assert-equal '(b . 345)
                (rassoc '234 '((a . 234) (b . 345) (c . 234))
                       :test-not (lambda (x y) (eql (1+ x) y))
                       :key #'1+)))
                 
(define-test |rassoc test-not=other key=other 2|
  (assert-equal nil
                (rassoc '234 '((a . 234) (b . 234) (c . 234))
                       :test-not (lambda (x y) (eql (1+ x) y))
                       :key #'1+)))
                 
(define-test |rassoc test-not=other key=other 3|
  (assert-equal '(b . 345)
                (rassoc '234 '((a . 234) nil (b . 345) (c . 234))
                       :test-not (lambda (x y) (eql (1+ x) y))
                       :key #'1+)))
                 
(define-test |rassoc error 1|
  (assert-error 'type-error
                (rassoc 'a '((b . c) nil d (a b)))))

(define-test |rassoc error 2|
  (assert-error 'error
                (rassoc 'a '((b . c) nil d (a b))
                       :test #'eq
                       :test-not #'eq)))

(define-test |rassoc test=nil key=identity 1|
  (assert-error 'error
                (rassoc 123 '((b . x) nil (b . 123) (c . nil))
                       :test nil)))

(define-test |rassoc test-not=nil key=identity 1|
  (assert-error 'error
                (rassoc 123 '((b . x) nil (b . 123) (c . nil))
                       :test-not nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Tests for the rassoc-if function

(define-test rassoc-if.1
  (assert-equal '(a . b)
                (rassoc-if (lambda (x) (eq x 'b))
                          '((b) nil (a . b) (a . c)))))

(define-test rassoc-if.2
  (assert-equal nil
                (rassoc-if (lambda (x) (eq x 'd))
                          '((b) nil (a . b) (a . c)))))

(define-test rassoc-if.3
  (assert-equal '(b)
                (rassoc-if #'null
                          '((b) nil (a . b) (nil . c)))))

(define-test rassoc-if.4
  (assert-equal '(b . 123)
                (rassoc-if (lambda (x) (eql x 123))
                          '((b) nil (b . 123) (nil . c)))))

(define-test rassoc-if.5
  (assert-equal '(b . #\a)
                (rassoc-if (lambda (x) (eql x #\a))
                          '((b) nil (b . #\a) (nil . c)))))

(define-test rassoc-if.6
  (assert-equal '((a b) c)
                (rassoc-if (lambda (x) (eq x 'c))
                          '(((b a) b) ((a b) . (c)) ((d) . e))
                          :key #'car)))

(define-test rassoc-if.7
  (assert-equal '((a b) c)
                (rassoc-if (lambda (x) (eq x 'c))
                          '(((b a) b) nil ((a b) . (c)) ((d) . e))
                          :key #'car)))

(define-test rassoc-if.error.1
  (assert-error 'type-error
                (rassoc-if (lambda (x) (eq x 'a))
                          '((b . c) nil d (a b)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Tests for the rassoc-if-not function

(define-test rassoc-if-not.1
  (assert-equal '(a . b)
                (rassoc-if-not (complement (lambda (x) (eq x 'b)))
                              '((b) nil (a . b) (a . c)))))

(define-test rassoc-if-not.2
  (assert-equal nil
                (rassoc-if-not (complement (lambda (x) (eq x 'd)))
                              '((b) nil (a . b) (a . c)))))

(define-test rassoc-if-not.3
  (assert-equal '(b)
                (rassoc-if-not (complement #'null)
                              '((b) nil (a . b) (nil . c)))))

(define-test rassoc-if-not.4
  (assert-equal '(b . 123)
                (rassoc-if-not (complement (lambda (x) (eql x 123)))
                              '((b) nil (b . 123) (nil . c)))))

(define-test rassoc-if-not.5
  (assert-equal '(b . #\a)
                (rassoc-if-not (complement (lambda (x) (eql x #\a)))
                              '((b) nil (b . #\a) (nil . c)))))

(define-test rassoc-if-not.6
  (assert-equal '((a b) c)
                (rassoc-if-not (complement (lambda (x) (eq x 'c)))
                              '(((b a) b) nil ((a b) c) ((d) e))
                              :key #'car)))

(define-test rassoc-if-not.error.1
  (assert-error 'type-error
                (rassoc-if-not (complement (lambda (x) (eq x 'a)))
                              '((b . c) nil d (a b)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Tests for the sublis function

(define-test |sublis test=eql key=identity 1a|
  (assert-equal '()
                (sublis '((10 . xx) (0 . a))
                        '())))

(define-test |sublis test=eql key=identity 1b|
  (assert-equal '()
                (sublis '((10 . xx) (0 . a))
                        '()
                        :test #'eql)))

(define-test |sublis test=eql key=identity 1c|
  (assert-equal '()
                (sublis '((10 . xx) (0 . a))
                        '()
                        :test 'eql)))

(define-test |sublis test=eql key=identity 2a|
  (assert-equal '(((a . 1) . (2 . a)) . ((a . 3) . (4 . a)))
                (sublis '((10 . xx) (0 . a))
                        '(((0 . 1) . (2 . 0)) . ((0 . 3) . (4 . 0))))))

(define-test |sublis test=eql key=identity 2b|
  (assert-equal '(((a . 1) . (2 . a)) . ((a . 3) . (4 . a)))
                (sublis '((10 . xx) (0 . a))
                        '(((0 . 1) . (2 . 0)) . ((0 . 3) . (4 . 0)))
                        :test #'eql)))

(define-test |sublis test=eql key=identity 2c|
  (assert-equal '(((a . 1) . (2 . a)) . ((a . 3) . (4 . a)))
                (sublis '((10 . xx) (0 . a))
                        '(((0 . 1) . (2 . 0)) . ((0 . 3) . (4 . 0)))
                        :test 'eql)))

(define-test |sublis test=eql key=nil 1|
  (assert-equal '(((a . 1) . (2 . a)) . ((a . 3) . (4 . a)))
                (sublis '((10 . xx) (0 . a))
                        '(((0 . 1) . (2 . 0)) . ((0 . 3) . (4 . 0)))
                        :key nil)))

(define-test |sublis test=eq key=identity 1|
  (assert-equal '()
                (sublis '((xx . y) (b . a)) '()
                       :test #'eq)))

(define-test |sublis test=eq key=identity 2|
  (assert-equal '(((a . c) . (f . a)) . ((a . d) . (e . a)))
                (sublis '((xx . y) (b . a))
                        '(((b . c) . (f . b)) . ((b . d) . (e . b)))
                        :test #'eq)))

(define-test |sublis test=other key=identity 1|
  (assert-equal '()
                (sublis '((10 . xx) (0 . a))
                        '()
                        :test (lambda (x y) (and (numberp y) (= x (1- y)))))))

(define-test |sublis test=other key=identity 2|
  (assert-equal '(((a . 1) . (2 . a)) . ((a . 3) . (4 . a)))
                (sublis '((10 . xx) (0 . a))
                        '(((0 . 1) . (2 . 0)) . ((0 . 3) . (4 . 0)))
                        :test (lambda (x y) (and (numberp y) (= x y))))))

(define-test |sublis test=eql key=other 1a|
  (assert-equal '()
                (sublis '((10 . xx) (0 . a))
                        '()
                        :key (lambda (x) (if (numberp x) (1+ x) x)))))

(define-test |sublis test=eql key=other 1b|
  (assert-equal '()
                (sublis '((10 . xx) (0 . a))
                        '()
                        :key (lambda (x) (if (numberp x) (1+ x) x))
                        :test #'eql)))

(define-test |sublis test=eql key=other 1c|
  (assert-equal '()
                (sublis '((10 . xx) (0 . a))
                        '()
                        :key (lambda (x) (if (numberp x) (1+ x) x))
                        :test 'eql)))

(define-test |sublis test=eql key=other 2a|
  (assert-equal '(((a . 1) . (2 . a)) . ((a . 3) . (4 . a)))
                (sublis '((10 . xx) (1 . a))
                        '(((0 . 1) . (2 . 0)) . ((0 . 3) . (4 . 0)))
                        :key (lambda (x) (if (numberp x) (1+ x) x)))))

(define-test |sublis test=eql key=other 2b|
  (assert-equal '(((a . 1) . (2 . a)) . ((a . 3) . (4 . a)))
                (sublis '((10 . xx) (1 . a))
                        '(((0 . 1) . (2 . 0)) . ((0 . 3) . (4 . 0)))
                        :key (lambda (x) (if (numberp x) (1+ x) x))
                        :test #'eql)))

(define-test |sublis test=eql key=other 2c|
  (assert-equal '(((a . 1) . (2 . a)) . ((a . 3) . (4 . a)))
                (sublis '((10 . xx) (1 . a))
                        '(((0 . 1) . (2 . 0)) . ((0 . 3) . (4 . 0)))
                        :key (lambda (x) (if (numberp x) (1+ x) x))
                        :test 'eql)))

(define-test |sublis test=eq key=other 1|
  (assert-equal '()
                (sublis '((xx . y) (bb . a))
                        '()
                        :test #'eq
                        :key (lambda (x) (if (eq x 'b) 'bb x)))))

(define-test |sublis test=eq key=other 2|
  (assert-equal '(((a . c) . (f . a)) . ((a . d) . (e . a)))
                (sublis '((xx . y) (bb . a))
                        '(((b . c) . (f . b)) . ((b . d) . (e . b)))
                        :test #'eq
                        :key (lambda (x) (if (eq x 'b) 'bb x)))))

(define-test |sublis test=other key=other 1|
  (assert-equal '()
                (sublis '((10 . xx) (0 . a))
                        '()
                        :test (lambda (x y) (and (numberp y) (= x (1- y))))
                        :key (lambda (x) (if (numberp x) (1+ x) x)))))

(define-test |sublis test=other key=other 2|
  (assert-equal '(((a . 1) . (2 . a)) . ((a . 3) . (4 . a)))
                (sublis '((10 . xx) (0 . a))
                        '(((0 . 1) . (2 . 0)) . ((0 . 3) . (4 . 0)))
                        :test (lambda (x y) (and (numberp y) (= x (1- y))))
                        :key (lambda (x) (if (numberp x) (1+ x) x)))))

(define-test |sublis test-not=any key=identity 2|
  (assert-equal '(((a . 5) . (2 . a)) . ((a . 3) . (4 . a)))
                (sublis '((10 . xx) (0 . a))
                        '(((1 . 5) . (2 . 1)) . ((1 . 3) . (4 . 1)))
                        :test-not (lambda (x y) (not (and (numberp y) (= x (1- y))))))))

(define-test |sublis test-not=any key=other 2|
  (assert-equal '(((a . 1) . (2 . a)) . ((a . 3) . (4 . a)))
                (sublis '((10 . xx) (0 . a))
                        '(((0 . 1) . (2 . 0)) . ((0 . 3) . (4 . 0)))
                        :test-not (lambda (x y) (not (and (numberp y) (= x (1- y)))))
                        :key (lambda (x) (if (numberp x) (1+ x) x)))))

(define-test |sublis test=other test-not=other 1|
  (assert-error 'error
                (sublis 0 '((1 . 2)) :test #'= :test-not #'=)))

(define-test |sublis test=nil key=identity 1|
  (assert-error 'error
                (sublis '((10 . xx) (0 . a))
                        '(((0 . 1) . (2 . 0)) . ((0 . 3) . (4 . 0)))
                        :test nil)))

(define-test |sublis test-not=nil key=identity 1|
  (assert-error 'error
                (sublis '((10 . xx) (0 . a))
                        '(((0 . 1) . (2 . 0)) . ((0 . 3) . (4 . 0)))
                        :test-not nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Tests for the nsublis function

(define-test |nsublis test=eql key=identity 1a|
  (let ((tree (copy-tree '())))
    (assert-equal '()
                  (nsublis '((10 . xx) (0 . a))
                           tree))))

(define-test |nsublis test=eql key=identity 1b|
  (let ((tree (copy-tree '())))
    (assert-equal '()
                  (nsublis '((10 . xx) (0 . a))
                           tree
                           :test #'eql))))

(define-test |nsublis test=eql key=identity 1c|
  (let ((tree (copy-tree '())))
    (assert-equal '()
                  (nsublis '((10 . xx) (0 . a))
                           tree
                           :test 'eql))))

(define-test |nsublis test=eql key=identity 2a|
  (let ((tree (copy-tree '(((0 . 1) . (2 . 0)) . ((0 . 3) . (4 . 0)))))
        (tree2))
    (assert-equal '(((a . 1) . (2 . a)) . ((a . 3) . (4 . a)))
                  (setf tree2 (nsublis
                               '((10 . xx) (0 . a))
                               tree)))
    (assert-eq tree tree2)))

(define-test |nsublis test=eql key=identity 2b|
  (let ((tree (copy-tree '(((0 . 1) . (2 . 0)) . ((0 . 3) . (4 . 0)))))
        (tree2))
    (assert-equal '(((a . 1) . (2 . a)) . ((a . 3) . (4 . a)))
                  (setf tree2 (nsublis
                               '((10 . xx) (0 . a))
                               tree
                               :test #'eql)))
    (assert-eq tree tree2)))

(define-test |nsublis test=eql key=identity 2c|
  (let ((tree (copy-tree '(((0 . 1) . (2 . 0)) . ((0 . 3) . (4 . 0)))))
        (tree2))
    (assert-equal '(((a . 1) . (2 . a)) . ((a . 3) . (4 . a)))
                  (setf tree2 (nsublis '((10 . xx) (0 . a))
                                       tree
                                       :test 'eql)))
    (assert-eq tree tree2)))    

(define-test |nsublis test=eql key=identity 3a|
  (assert-equal 'a
                (nsublis '((10 . xx) (0 . a))
                         0)))

(define-test |nsublis test=eql key=identity 3b|
  (assert-equal 'a
                (nsublis '((10 . xx) (0 . a))
                         0
                         :test #'eql)))

(define-test |nsublis test=eql key=identity 3c|
  (assert-equal 'a
                (nsublis '((10 . xx) (0 . a))
                         0
                         :test 'eql)))

(define-test |nsublis test=eql key=nil 1|
  (let ((tree (copy-tree '(((0 . 1) . (2 . 0)) . ((0 . 3) . (4 . 0)))))
        (tree2))
    (assert-equal '(((a . 1) . (2 . a)) . ((a . 3) . (4 . a)))
                  (setf tree2 (nsublis
                               '((10 . xx) (0 . a))
                               tree
                               :key nil)))
    (assert-eq tree tree2)))

(define-test |nsublis test=eq key=identity 1|
  (let ((tree (copy-tree '())))
    (assert-equal '()
                  (nsublis
                   '((xx . y) (b . a))
                   tree
                   :test #'eq))))

(define-test |nsublis test=eq key=identity 2|
  (let ((tree (copy-tree '(((b . c) . (f . b)) . ((b . d) . (e . b)))))
        (tree2))
    (assert-equal '(((a . c) . (f . a)) . ((a . d) . (e . a)))
                  (setf tree2 (nsublis
                               '((xx . y) (b . a))
                               tree
                               :test #'eq)))
    (assert-eq tree tree2)))

(define-test |nsublis test=eq key=identity 3a|
  (assert-equal 'a
                (nsublis '((10 . xx) (c . a))
                         'c
                         :test #'eq)))

(define-test |nsublis test=eq key=identity 3b|
  (assert-equal 'a
                (nsublis '((10 . xx) (c . a))
                         'c
                         :test 'eq)))

(define-test |nsublis test=other key=identity 1|
  (let ((tree (copy-tree '())))
    (assert-equal '()
                  (nsublis '((10 . xx) (0 . a))
                           tree
                           :test (lambda (x y) (and (numberp y) (= x (1- y))))))))

(define-test |nsublis test=other key=identity 2|
  (let ((tree (copy-tree '(((0 . 1) . (2 . 0)) . ((0 . 3) . (4 . 0)))))
        (tree2))
    (assert-equal '(((a . 1) . (2 . a)) . ((a . 3) . (4 . a)))
                  (setf tree2 (nsublis '((10 . xx) (0 . a))
                                       tree
                                       :test (lambda (x y) (and (numberp y) (= x y))))))
    (assert-eq tree tree2)))

(define-test |nsublis test=other key=identity 3|
  (assert-equal 'a
                (nsublis '((10 . xx) (0 . a))
                         1
                         :test (lambda (x y) (and (numberp y) (= x (1- y)))))))

(define-test |nsublis test=eql key=other 1a|
  (let ((tree (copy-tree '())))
    (assert-equal '()
                  (nsublis '((10 . xx) (0 . a))
                           tree
                           :key (lambda (x) (if (numberp x) (1+ x) x))))))

(define-test |nsublis test=eql key=other 1b|
  (let ((tree (copy-tree '())))
    (assert-equal '()
                  (nsublis '((10 . xx) (0 . a))
                           tree
                           :key (lambda (x) (if (numberp x) (1+ x) x))
                           :test #'eql))))

(define-test |nsublis test=eql key=other 1c|
  (let ((tree (copy-tree '())))
    (assert-equal '()
                  (nsublis '((10 . xx) (0 . a))
                           tree
                           :key (lambda (x) (if (numberp x) (1+ x) x))
                           :test 'eql))))

(define-test |nsublis test=eql key=other 2a|
  (let ((tree (copy-tree '(((0 . 1) . (2 . 0)) . ((0 . 3) . (4 . 0)))))
        (tree2))
    (assert-equal '(((a . 1) . (2 . a)) . ((a . 3) . (4 . a)))
                  (setf tree2 (nsublis '((10 . xx) (1 . a))
                                       tree
                                       :key (lambda (x) (if (numberp x) (1+ x) x)))))
    (assert-eq tree tree2)))

(define-test |nsublis test=eql key=other 2b|
  (let ((tree (copy-tree '(((0 . 1) . (2 . 0)) . ((0 . 3) . (4 . 0)))))
        (tree2))
    (assert-equal '(((a . 1) . (2 . a)) . ((a . 3) . (4 . a)))
                  (setf tree2 (nsublis '((10 . xx) (1 . a))
                                       tree
                                       :key (lambda (x) (if (numberp x) (1+ x) x))
                                       :test #'eql)))
    (assert-eq tree tree2)))

(define-test |nsublis test=eql key=other 2c|
  (let ((tree (copy-tree '(((0 . 1) . (2 . 0)) . ((0 . 3) . (4 . 0)))))
        (tree2))
    (assert-equal '(((a . 1) . (2 . a)) . ((a . 3) . (4 . a)))
                  (setf tree2 (nsublis '((10 . xx) (1 . a))
                                       tree
                                       :key (lambda (x) (if (numberp x) (1+ x) x))
                                       :test 'eql)))
    (assert-eq tree tree2)))

(define-test |nsublis test=eql key=other 3a|
  (assert-equal 'a
                (nsublis '((10 . xx) (1 . a))
                         0
                         :key (lambda (x) (if (numberp x) (1+ x) x)))))


(define-test |nsublis test=eq key=other 1|
  (let ((tree (copy-tree '())))
    (assert-equal '()
                  (nsublis '((xx . y) (bb . a))
                           tree
                           :test #'eq
                           :key (lambda (x) (if (eq x 'b) 'bb x))))))

(define-test |nsublis test=eq key=other 2|
  (let ((tree (copy-tree '(((b . c) . (f . b)) . ((b . d) . (e . b)))))
        (tree2))
    (assert-equal '(((a . c) . (f . a)) . ((a . d) . (e . a)))
                  (setf tree2 (nsublis '((xx . y) (bb . a))
                                       tree
                                       :test #'eq
                                       :key (lambda (x) (if (eq x 'b) 'bb x)))))
    (assert-eq tree tree2)))

(define-test |nsublis test=eq key=other 3|
  (assert-equal 'a
                (nsublis '((xx . y) (bb . a))
                         'b
                         :test #'eq
                         :key (lambda (x) (if (eq x 'b) 'bb x)))))

(define-test |nsublis test=other key=other 1|
  (let ((tree (copy-tree '())))
    (assert-equal '()
                  (nsublis '((10 . xx) (0 . a))
                           tree
                           :test (lambda (x y) (and (numberp y) (= x (1- y))))
                           :key (lambda (x) (if (numberp x) (1+ x) x))))))

(define-test |nsublis test=other key=other 2|
  (let ((tree (copy-tree '(((0 . 1) . (2 . 0)) . ((0 . 3) . (4 . 0)))))
        (tree2))
    (assert-equal '(((a . 1) . (2 . a)) . ((a . 3) . (4 . a)))
                  (setf tree2 (nsublis '((10 . xx) (0 . a))
                                       tree
                                       :test (lambda (x y) (and (numberp y) (= x (1- y))))
                                       :key (lambda (x) (if (numberp x) (1+ x) x)))))
    (assert-eq tree tree2)))

(define-test |nsublis test=other key=other 3|
  (assert-equal 'a
                (nsublis '((10 . xx) (0 . a))
                         0
                         :test (lambda (x y) (and (numberp y) (= x (1- y))))
                         :key (lambda (x) (if (numberp x) (1+ x) x)))))

(define-test |nsublis test-not=any key=identity 2|
  (let ((tree (copy-tree '(((1 . 5) . (2 . 1)) . ((1 . 3) . (4 . 1)))))
        (tree2))
    (assert-equal '(((a . 5) . (2 . a)) . ((a . 3) . (4 . a)))
                  (setf tree2 (nsublis '((10 . xx) (0 . a))
                                       tree
                                       :test-not (lambda (x y) (not (and (numberp y) (= x (1- y))))))))
    (assert-eq tree tree2)))

(define-test |nsublis test-not=any key=identity 3|
  (assert-equal 'a
                (nsublis '((10 . xx) (0 . a))
                         1
                         :test-not (lambda (x y) (not (and (numberp y) (= x (1- y))))))))

(define-test |nsublis test-not=any key=identity 4|
  (assert-equal 123
                (nsublis '((10 . xx) (0 . a))
                         123
                         :test-not (lambda (x y) (not (and (numberp y) (= x (1- y))))))))

(define-test |nsublis test-not=any key=other 2|
  (let ((tree (copy-tree '(((0 . 1) . (2 . 0)) . ((0 . 3) . (4 . 0)))))
        (tree2))
    (assert-equal '(((a . 1) . (2 . a)) . ((a . 3) . (4 . a)))
                  (setf tree2 (nsublis '((10 . xx) (0 . a))
                                       tree
                                       :test-not (lambda (x y) (not (and (numberp y) (= x (1- y)))))
                                       :key (lambda (x) (if (numberp x) (1+ x) x)))))
    (assert-eq tree tree2)))

(define-test |nsublis test-not=any key=other 3|
  (assert-equal 'a
                (nsublis '((10 . xx) (0 . a))
                         0
                         :test-not (lambda (x y) (not (and (numberp y) (= x (1- y)))))
                         :key (lambda (x) (if (numberp x) (1+ x) x)))))

(define-test |nsublis test-not=any key=other 4|
  (assert-equal 123
                (nsublis '((10 . xx) (0 . a))
                         123
                         :test-not (lambda (x y) (not (and (numberp y) (= x (1- y)))))
                         :key (lambda (x) (if (numberp x) (1+ x) x)))))

(define-test |nsublis test=other test-not=other|
  (assert-error 'error
                (nsublis 0 '((1 . 2)) :test #'= :test-not #'=)))

(define-test |nsublis test=nil key=identity 1|
  (assert-error 'error
                (sublis '((10 . xx) (0 . a))
                        (copy-tree '(((0 . 1) . (2 . 0)) . ((0 . 3) . (4 . 0))))
                        :test nil)))

(define-test |nsublis test-not=nil key=identity 1|
  (assert-error 'error
                (nsublis '((10 . xx) (0 . a))
                        (copy-tree '(((0 . 1) . (2 . 0)) . ((0 . 3) . (4 . 0))))
                        :test-not nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Tests for the pairlis function

(define-test |pairlis 1|
  (assert-equal '()
                (pairlis '() '())))

(define-test |pairlis 2|
  (let ((tail '((a b) (c d))))
    (assert-eq tail
               (cddr (pairlis '(1 2) '(2 3) tail)))))

(define-test |pairlis 3|
  (assert-equal '((c . d) (a . b) (e f))
                (pairlis '(a c) '(b d) '((e f)))))

(define-test |pairlis error 1|
  (assert-error 'type-error
                (pairlis '(a . b) '(c d) '((e f)))))

(define-test |pairlis error 2|
  (assert-error 'type-error
                (pairlis '(c d) '(a . b) '((e f)))))

(define-test |pairlis error 3|
  (assert-error 'error
                (pairlis '(a b) '(c d e))))

(define-test |pairlis error 4|
  (assert-error 'error
                (pairlis '(c d e) '(a b))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Tests for the copy-alist function

(define-test |copy-alist 1|
  (assert-equal '()
                (copy-alist '())))

(define-test |copy-alist 2|
  (assert-equal '((a) (b))
                (copy-alist '((a) (b)))))

(define-test |copy-alist 3|
  (assert-false (let ((alist '((a) (b))))
                  (eq (car alist)
                      (car (copy-alist alist))))))

(define-test |copy-alist 4|
  (assert-equal '((a b) c (d e))
                (copy-alist '((a b) c (d e)))))

(define-test |copy-alist 5|
  (assert-equal '((a b) nil (d e))
                (copy-alist '((a b) nil (d e)))))

(define-test |copy-alist error 2|
  (assert-error 'type-error
                (copy-alist '((a) . b))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Tests for the tailp function

(define-test tailp.1
  (assert-false (tailp 123 '())))

(define-test tailp.2
  (assert-false (tailp 123 '(123))))

(define-test tailp.3
  (assert-true (tailp 123 123)))

(define-test tailp.4
  (assert-true (tailp 123 '(a . 123))))

(define-test tailp.5
  (let ((tail '(a b)))
    (assert-true (tailp tail (list* 1 2 tail)))))

(define-test tailp.6
  (let ((tail '(a b)))
    (assert-false (tailp tail (list* 1 2 (copy-list tail))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Tests for the ldiff function

(define-test ldiff.1
  (let ((list '(a b c . d)))
    (assert-equal nil (ldiff list list))))

(define-test ldiff.2
  (let ((list '(a b c . d)))
    (assert-equal '(a) (ldiff list (cdr list)))))

(define-test ldiff.3
  (let ((list '(a b c . d)))
    (assert-equal '(a b c) (ldiff list 'd))))

(define-test ldiff.4
  (let ((list '(a b c . d)))
    (assert-equal '(a b c . d) (ldiff list (cons 'c 'd)))))

(define-test ldiff.5
  (let ((list '(a b c . d)))
    (assert-equal '(a b c . d) (ldiff list 234))))

(define-test ldiff.6
  (let ((list '(a b c . d)))
    (assert-equal '(a b c . d) (ldiff list nil))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Tests for the union function

(define-test |union test=eql key=identity 1a|
  (assert-equal '()
                (union '() '())))

(define-test |union test=eql key=identity 1b|
  (assert-equal '()
                (union '() '() :test #'eql)))

(define-test |union test=eql key=identity 1c|
  (assert-equal '()
                (union '() '() :test 'eql)))

(define-test |union test=eql key=identity 2a|
  (assert-equal '()
                (set-difference (union '(1 2 3) '(3 4 5))
                                '(1 2 3 4 5))))

(define-test |union test=eql key=identity 2b|
  (assert-equal '()
                (set-difference (union '(1 2 3) '(3 4 5) :test #'eql)
                                '(1 2 3 4 5))))

(define-test |union test=eql key=identity 2|
  (assert-equal '()
                (set-difference (union '(1 2 3) '(3 4 5) :test 'eql)
                                '(1 2 3 4 5))))

(define-test |union test=eql key=identity 3a|
  (let ((l1 (loop for i from 30 below 40 collect i))
        (l2 (loop for i from 40 below 100 collect i))
        (l3 (loop for i from 100 below 110 collect i)))
    (assert-equal '()
                  (set-difference (union (append l1 l2) (append l2 l3))
                                  (append l1 l2 l3)))))

(define-test |union test=eql key=identity 3b|
  (let ((l1 (loop for i from 30 below 40 collect i))
        (l2 (loop for i from 40 below 100 collect i))
        (l3 (loop for i from 100 below 110 collect i)))
    (assert-equal '()
                  (set-difference (union (append l1 l2)
                                         (append l2 l3)
                                         :test #'eql)
                                  (append l1 l2 l3)
                                  :test #'eql))))

(define-test |union test=eql key=identity 3c|
  (let ((l1 (loop for i from 30 below 40 collect i))
        (l2 (loop for i from 40 below 100 collect i))
        (l3 (loop for i from 100 below 110 collect i)))
    (assert-equal '()
                  (set-difference (union (append l1 l2)
                                         (append l2 l3)
                                         :test 'eql)
                                  (append l1 l2 l3)
                                  :test 'eql))))

(define-test |union test=eql key=identity dotted-list 1|
  (assert-error 'type-error
                (union '(1 2 . 3) '(2 3 4))))

(define-test |union test=eql key=identity dotted-list 2|
  (assert-error 'type-error
                (union '(1 2 3) '(2 3 . 4))))

(define-test |union test=eql key=nil 2a|
  (assert-equal '()
                (set-difference (union '(1 2 3) '(3 4 5) :key nil)
                                '(1 2 3 4 5))))

(define-test |union test=eq key=identity 1|
  (assert-equal '()
                (union '() '() :test #'eq)))

(define-test |union test=eq key=identity 2|
  (assert-equal '()
                (set-difference (union '(abc def) '(abc ghi)
                                       :test #'eq)
                                '(abc def ghi)
                                :test #'eq)))

(define-test |union test=eq key=identity 3|
  (let ((l1 (loop for code from 30 below 40 collect (string (code-char code))))
        (l2 (loop for code from 40 below 100 collect (string (code-char code))))
        (l3 (loop for code from 100 below 110 collect (string (code-char code)))))
    (assert-equal '()
                  (set-difference (union (append l1 l2)
                                         (append l2 l3)
                                         :test #'eq)
                                  (append l1 l2 l3)
                                  :test #'eq))))

(define-test |union test=eq key=identity dotted-list 1|
  (assert-error 'type-error
                (union '(a b . c) '(b c d) :test #'eq)))

(define-test |union test=eq key=identity dotted-list 2|
  (assert-error 'type-error
                (union '(a b c) '(b c . d) :test #'eq)))

(define-test |union test=equal key=identity 1|
  (assert-equal '()
                (union '() '() :test #'equal)))

(define-test |union test=equal key=identity 2|
  (assert-equal '()
                (set-difference (union '("abc" "def") '("abc" "ghi")
                                       :test #'equal)
                                '("abc" "def" "ghi")
                                :test #'equal)))

(define-test |union test=equal key=identity 3|
  (let ((l1 (loop for code from 30 below 100 collect (string (code-char code))))
        (l2 (loop for code from 40 below 110 collect (string (code-char code))))
        (l3 (loop for code from 30 below 110 collect (string (code-char code)))))
    (assert-equal '()
                  (set-difference (union l1 l2 :test #'equal)
                                  l3
                                  :test #'equal))))

(define-test |union test=equal key=identity dotted-list 1|
  (assert-error 'type-error
                (union '(a b . c) '(b c d) :test #'equal)))

(define-test |union test=equal key=identity dotted-list 2|
  (assert-error 'type-error
                (union '(a b c) '(b c . d) :test #'equal)))

(define-test |union test=equalp key=identity 1|
  (assert-equal '()
                (union '() '() :test #'equalp)))

(define-test |union test=equalp key=identity 3|
  (let ((l1 (loop for i from 30 below 100
                  collect (make-array 1 :initial-element i)))
        (l2 (loop for i from 40 below 110
                  collect (make-array 1 :initial-element i)))
        (l3 (loop for i from 30 below 110
                  collect (make-array 1 :initial-element i))))
    (assert-equal '()
                  (set-difference (union l1 l2 :test #'equalp)
                                  l3
                                  :test #'equalp))))

(define-test |union test=equalp key=identity dotted-list 1|
  (assert-error 'type-error
                (union '(a b . c) '(b c d) :test #'equalp)))

(define-test |union test=equalp key=identity dotted-list 2|
  (assert-error 'type-error
                (union '(a b c) '(b c . d) :test #'equalp)))

(define-test |union test=other key=identity 1|
  (assert-equal '()
                (union '() '() :test #'<)))

(define-test |union test=other key=identity dotted-list 1|
  (assert-error 'type-error
                (union '(a b . c) '(b c d) :test #'(lambda (x y) (eq x y)))))

(define-test |union test=other key=identity dotted-list 2|
  (assert-error 'type-error
                (union '(a b c) '(b c . d) :test #'(lambda (x y) (eq x y)))))

(define-test |union test=eql key=other 1a|
  (assert-equal 1
                (length (union '(1) '(3) :key #'oddp))))

(define-test |union test=eql key=other 1b|
  (assert-equal 1
                (length (union '(1) '(3) :key #'oddp :test #'eql))))

(define-test |union test=eql key=other 1c|
  (assert-equal 1
                (length (union '(1) '(3) :key #'oddp :test 'eql))))

(define-test |union test=eql key=other 2a|
  (assert-equal 2
                (length (union '(4 1) '(3) :key #'oddp))))

(define-test |union test=eql key=other 2b|
  (assert-equal 2
                (length (union '(4 1) '(3) :key #'oddp :test #'eql))))

(define-test |union test=eql key=other 2c|
  (assert-equal 2
                (length (union '(4 1) '(3) :key #'oddp :test 'eql))))

(define-test |union test=eql key=other 3|
  (let ((l1 (loop for i from 30 below 40 collect (list i)))
        (l2 (loop for i from 40 below 100 collect (list i)))
        (l3 (loop for i from 100 below 110 collect (list i))))
    (assert-equal '()
                  (set-difference (union (append l1 l2)
                                         (append l2 l3)
                                         :key #'car)
                                  (append l1 l2 l3)
                                  :key #'car))))

(define-test |union test=eql key=other 4|
  (let ((l1 (loop for i from 30 below 40 collect (list i)))
        (l2 (loop for i from 40 below 100 collect (list i)))
        (l3 (loop for i from 100 below 110 collect (list i))))
    (assert-equal '()
                  (set-difference (union (append l1 l2)
                                         (append l2 l3)
                                         :test #'eql
                                         :key #'car)
                                  (append l1 l2 l3)
                                  :test #'eql
                                  :key #'car))))

(define-test |union test=eql key=other 5|
  (let ((l1 (loop for i from 30 below 40 collect (list i)))
        (l2 (loop for i from 40 below 100 collect (list i)))
        (l3 (loop for i from 100 below 110 collect (list i))))
    (assert-equal '()
                  (set-difference (union (append l1 l2)
                                         (append l2 l3)
                                         :test 'eql
                                         :key #'car)
                                  (append l1 l2 l3)
                                  :test 'eql
                                  :key #'car))))

(define-test |union test=eql key=other dotted-list 1|
  (assert-error 'type-error
                (union '(a b . c) '(b c d) :key #'identity)))

(define-test |union test=eql key=other dotted-list 2|
  (assert-error 'type-error
                (union '(a b c) '(b c . d) :key #'identity)))

(define-test |union test=eq key=other 1|
  (assert-equal '()
                (set-difference
                 (union '((a) (b) (c))
                        '((b) (c) (d))
                        :key #'car
                        :test #'eq)
                 '((a) (b) (c) (d))
                 :key #'car
                 :test #'eq)))

(define-test |union test=eq key=other 3|
  (let ((l1 (loop for code from 30 below 40 collect (list (string (code-char code)))))
        (l2 (loop for code from 40 below 100 collect (list (string (code-char code)))))
        (l3 (loop for code from 100 below 110 collect (list (string (code-char code))))))
    (assert-equal '()
                  (set-difference (union (append l1 l2)
                                         (append l2 l3)
                                         :key #'car
                                         :test #'eq)
                                  (append l1 l2 l3)
                                  :test #'eq
                                  :key #'car))))

(define-test |union test=eq key=other dotted-list 1|
  (assert-error 'type-error
                (union '(a b . c) '(b c d) :test #'eq :key #'identity)))

(define-test |union test=eq key=other dotted-list 2|
  (assert-error 'type-error
                (union '(a b c) '(b c . d) :test #'eq :key #'identity)))

(define-test |union test=equal key=other 1|
  (assert-equal '()
                (union '() '() :test #'equal :key #'car)))

(define-test |union test=equal key=other 2|
  (assert-equal '()
                (set-difference (union '(("abc") ("def")) '(("abc") ("ghi"))
                                       :key #'car
                                       :test #'equal)
                                '(("abc") ("def") ("ghi"))
                                :key #'car
                                :test #'equal)))

(define-test |union test=equal key=other 3|
  (let ((l1 (loop for code from 30 below 100 collect (list (string (code-char code)))))
        (l2 (loop for code from 40 below 110 collect (list (string (code-char code)))))
        (l3 (loop for code from 30 below 110 collect (list (string (code-char code))))))
    (assert-equal '()
                  (set-difference (union l1 l2 :test #'equal :key #'car)
                                  l3
                                  :test #'equal :key #'car))))

(define-test |union test=equal key=other dotted-list 1|
  (assert-error 'type-error
                (union '(a b . c) '(b c d) :test #'equal :key #'identity)))

(define-test |union test=equal key=other dotted-list 2|
  (assert-error 'type-error
                (union '(a b c) '(b c . d) :test #'equal :key #'identity)))

(define-test |union test=equalp key=other 1|
  (assert-equal '()
                (union '() '() :test #'equalp :key #'car)))

(define-test |union test=equalp key=other 2|
  (assert-equal '()
                (set-difference (union '((#(1)) (#(2))) '((#(1)) (#(3)))
                                       :key #'car
                                       :test #'equalp)
                                '((#(1)) (#(2)) (#(3)))
                                :key #'car
                                :test #'equalp)))

(define-test |union test=equalp key=other 3|
  (let ((l1 (loop for i from 30 below 100
                  collect (list (make-array 1 :initial-element i))))
        (l2 (loop for i from 40 below 110
                  collect (list (make-array 1 :initial-element i))))
        (l3 (loop for i from 30 below 110
                  collect (list (make-array 1 :initial-element i)))))
    (assert-equal '()
                  (set-difference (union l1 l2 :test #'equalp :key #'car)
                                  l3
                                  :test #'equalp :key #'car))))

(define-test |union test=equalp key=other dotted-list 1|
  (assert-error 'type-error
                (union '(a b . c) '(b c d) :test #'equalp :key #'identity)))

(define-test |union test=equalp key=other dotted-list 2|
  (assert-error 'type-error
                (union '(a b c) '(b c . d) :test #'equalp :key #'identity)))

(define-test |union test=other key=other 1|
  (assert-equal '()
                (set-difference
                 (union '(1 3 5 7)
                        '(2 4 10 3)
                        :key (lambda (x) (mod x 5))
                        :test #'=)
                 '(0 1 2 3 4) :key (lambda (x) (mod x 5)))))

(define-test |union test=other key=other dotted-list 1|
  (assert-error 'type-error
                (union '(a b . c) '(b c d) :test (lambda (x y) (eq x y)) :key #'identity)))

(define-test |union test=other key=other dotted-list 2|
  (assert-error 'type-error
                (union '(a b c) '(b c . d) :test (lambda (x y) (eq x y)) :key #'identity)))

(define-test |union test-not=other key=identity 1|
  (assert-equal '()
                (set-difference
                 (union '(1 2 3 4)
                        '(3 4 5 6)
                        :test-not #'/=)
                 '(1 2 3 4 5 6))))

(define-test |union test-not=other key=other 1|
  (assert-equal '()
                (set-difference
                 (union '(1 3 5 7)
                        '(2 4 10 3)
                        :key (lambda (x) (mod x 5))
                        :test-not #'/=)
                 '(0 1 2 3 4) :key (lambda (x) (mod x 5)))))

(define-test |union test=other test-not=other 1|
  (assert-error 'error
                (union '(1 2 3) '(2 3 4) :test #'eql :test-not #'eql)))

(define-test |union test=nil key=identity 2a|
  (assert-error 'error
                (set-difference (union '(1 2 3) '(3 4 5) :test nil)
                                '(1 2 3 4 5))))

(define-test |union test-not=nil key=identity 2a|
  (assert-error 'error
                (set-difference (union '(1 2 3) '(3 4 5) :test-not nil)
                                '(1 2 3 4 5))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Tests for the nunion function

(define-test |nunion test=eql key=identity 1a|
  (assert-equal '()
                (nunion '() '())))

(define-test |nunion test=eql key=identity 1b|
  (assert-equal '()
                (nunion '() '() :test #'eql)))

(define-test |nunion test=eql key=identity 1c|
  (assert-equal '()
                (nunion '() '() :test 'eql)))

(define-test |nunion test=eql key=identity 2a|
  (assert-equal '()
                (set-difference (nunion (copy-list '(1 2 3))
                                        (copy-list '(3 4 5)))
                                '(1 2 3 4 5))))

(define-test |nunion test=eql key=identity 2b|
  (assert-equal '()
                (set-difference (nunion (copy-list '(1 2 3))
                                        (copy-list '(3 4 5))
                                        :test #'eql)
                                '(1 2 3 4 5))))

(define-test |nunion test=eql key=identity 2|
  (assert-equal '()
                (set-difference (nunion (copy-list '(1 2 3))
                                        (copy-list '(3 4 5))
                                        :test 'eql)
                                '(1 2 3 4 5))))

(define-test |nunion test=eql key=identity 3a|
  (let ((l1 (loop for i from 30 below 40 collect i))
        (l2 (loop for i from 40 below 100 collect i))
        (l3 (loop for i from 100 below 110 collect i)))
    (assert-equal '()
                  (set-difference (nunion (copy-list (append l1 l2))
                                          (copy-list (append l2 l3)))
                                  (append l1 l2 l3)))))

(define-test |nunion test=eql key=identity 3b|
  (let ((l1 (loop for i from 30 below 40 collect i))
        (l2 (loop for i from 40 below 100 collect i))
        (l3 (loop for i from 100 below 110 collect i)))
    (assert-equal '()
                  (set-difference (nunion (copy-list (append l1 l2))
                                          (copy-list (append l2 l3))
                                          :test #'eql)
                                  (append l1 l2 l3)
                                  :test #'eql))))

(define-test |nunion test=eql key=identity 3c|
  (let ((l1 (loop for i from 30 below 40 collect i))
        (l2 (loop for i from 40 below 100 collect i))
        (l3 (loop for i from 100 below 110 collect i)))
    (assert-equal '()
                  (set-difference (nunion (copy-list (append l1 l2))
                                          (copy-list (append l2 l3))
                                          :test 'eql)
                                  (append l1 l2 l3)
                                  :test 'eql))))

(define-test |nunion test=eql key=nil 1|
  (assert-equal '()
                (set-difference (nunion (copy-list '(1 2 3))
                                        (copy-list '(3 4 5))
                                        :key nil)
                                '(1 2 3 4 5))))

(define-test |nunion test=eq key=identity 1|
  (assert-equal '()
                (nunion '() '() :test #'eq)))

(define-test |nunion test=eq key=identity 2|
  (assert-equal '()
                (set-difference (nunion '(abc def) '(abc ghi)
                                       :test #'eq)
                                '(abc def ghi)
                                :test #'eq)))

(define-test |nunion test=eq key=identity 3|
  (let ((l1 (loop for code from 30 below 40 collect (string (code-char code))))
        (l2 (loop for code from 40 below 100 collect (string (code-char code))))
        (l3 (loop for code from 100 below 110 collect (string (code-char code)))))
    (assert-equal '()
                  (set-difference (nunion (copy-list (append l1 l2))
                                          (copy-list (append l2 l3))
                                          :test #'eq)
                                  (append l1 l2 l3)
                                  :test #'eq))))

(define-test |nunion test=equal key=identity 1|
  (assert-equal '()
                (nunion '() '() :test #'equal)))

(define-test |nunion test=equal key=identity 2|
  (assert-equal '()
                (set-difference (nunion (copy-list '("abc" "def"))
                                        (copy-list '("abc" "ghi"))
                                        :test #'equal)
                                '("abc" "def" "ghi")
                                :test #'equal)))

(define-test |nunion test=equal key=identity 3|
  (let ((l1 (loop for code from 30 below 100 collect (string (code-char code))))
        (l2 (loop for code from 40 below 110 collect (string (code-char code))))
        (l3 (loop for code from 30 below 110 collect (string (code-char code)))))
    (assert-equal '()
                  (set-difference (nunion (copy-list l1)
                                          (copy-list l2)
                                          :test #'equal)
                                  l3
                                  :test #'equal))))

(define-test |nunion test=equalp key=identity 1|
  (assert-equal '()
                (nunion '() '() :test #'equalp)))

(define-test |nunion test=equalp key=identity 3|
  (let ((l1 (loop for i from 30 below 100
                  collect (make-array 1 :initial-element i)))
        (l2 (loop for i from 40 below 110
                  collect (make-array 1 :initial-element i)))
        (l3 (loop for i from 30 below 110
                  collect (make-array 1 :initial-element i))))
    (assert-equal '()
                  (set-difference (nunion (copy-list l1)
                                          (copy-list l2)
                                          :test #'equalp)
                                  l3
                                  :test #'equalp))))

(define-test |nunion test=other key=identity 1|
  (assert-equal '()
                (nunion '() '() :test #'<)))

(define-test |nunion test=eql key=other 1a|
  (assert-equal 1
                (length (nunion (copy-list '(1))
                                (copy-list '(3))
                                :key #'oddp))))

(define-test |nunion test=eql key=other 1b|
  (assert-equal 1
                (length (nunion (copy-list '(1))
                                (copy-list '(3))
                                :key #'oddp
                                :test #'eql))))

(define-test |nunion test=eql key=other 1c|
  (assert-equal 1
                (length (nunion (copy-list '(1))
                                (copy-list '(3))
                                :key #'oddp
                                :test 'eql))))

(define-test |nunion test=eql key=other 2a|
  (assert-equal 2
                (length (nunion (copy-list '(4 1))
                                (copy-list '(3))
                                :key #'oddp))))

(define-test |nunion test=eql key=other 2b|
  (assert-equal 2
                (length (nunion (copy-list '(4 1))
                                (copy-list '(3))
                                :key #'oddp
                                :test #'eql))))

(define-test |nunion test=eql key=other 2c|
  (assert-equal 2
                (length (nunion (copy-list '(4 1))
                                (copy-list '(3))
                                :key #'oddp
                                :test 'eql))))

(define-test |nunion test=eql key=other 3|
  (let ((l1 (loop for i from 30 below 40 collect (list i)))
        (l2 (loop for i from 40 below 100 collect (list i)))
        (l3 (loop for i from 100 below 110 collect (list i))))
    (assert-equal '()
                  (set-difference (nunion (copy-list (append l1 l2))
                                          (copy-list (append l2 l3))
                                          :key #'car)
                                  (append l1 l2 l3)
                                  :key #'car))))

(define-test |nunion test=eql key=other 4|
  (let ((l1 (loop for i from 30 below 40 collect (list i)))
        (l2 (loop for i from 40 below 100 collect (list i)))
        (l3 (loop for i from 100 below 110 collect (list i))))
    (assert-equal '()
                  (set-difference (nunion (copy-list (append l1 l2))
                                          (copy-list (append l2 l3))
                                          :test #'eql
                                          :key #'car)
                                  (append l1 l2 l3)
                                  :test #'eql
                                  :key #'car))))

(define-test |nunion test=eql key=other 5|
  (let ((l1 (loop for i from 30 below 40 collect (list i)))
        (l2 (loop for i from 40 below 100 collect (list i)))
        (l3 (loop for i from 100 below 110 collect (list i))))
    (assert-equal '()
                  (set-difference (nunion (copy-list (append l1 l2))
                                          (copy-list (append l2 l3))
                                          :test 'eql
                                          :key #'car)
                                  (append l1 l2 l3)
                                  :test 'eql
                                  :key #'car))))

(define-test |nunion test=eq key=other 1|
  (assert-equal '()
                (set-difference
                 (nunion (copy-list '((a) (b) (c)))
                         (copy-list '((b) (c) (d)))
                         :key #'car
                         :test #'eq)
                 '((a) (b) (c) (d))
                 :key #'car
                 :test #'eq)))

(define-test |nunion test=eq key=other 3|
  (let ((l1 (loop for code from 30 below 40 collect (list (string (code-char code)))))
        (l2 (loop for code from 40 below 100 collect (list (string (code-char code)))))
        (l3 (loop for code from 100 below 110 collect (list (string (code-char code))))))
    (assert-equal '()
                  (set-difference (nunion (copy-list (append l1 l2))
                                          (copy-list (append l2 l3))
                                          :key #'car
                                          :test #'eq)
                                  (append l1 l2 l3)
                                  :test #'eq
                                  :key #'car))))

(define-test |nunion test=equal key=other 1|
  (assert-equal '()
                (nunion '() '() :test #'equal :key #'car)))

(define-test |nunion test=equal key=other 2|
  (assert-equal '()
                (set-difference (nunion (copy-list '(("abc") ("def")))
                                        (copy-list '(("abc") ("ghi")))
                                        :key #'car
                                        :test #'equal)
                                '(("abc") ("def") ("ghi"))
                                :key #'car
                                :test #'equal)))

(define-test |nunion test=equal key=other 3|
  (let ((l1 (loop for code from 30 below 100 collect (list (string (code-char code)))))
        (l2 (loop for code from 40 below 110 collect (list (string (code-char code)))))
        (l3 (loop for code from 30 below 110 collect (list (string (code-char code))))))
    (assert-equal '()
                  (set-difference (nunion (copy-list l1)
                                          (copy-list l2)
                                          :test #'equal
                                          :key #'car)
                                  l3
                                  :test #'equal :key #'car))))

(define-test |nunion test=equalp key=other 1|
  (assert-equal '()
                (nunion '() '() :test #'equalp :key #'car)))

(define-test |nunion test=equalp key=other 2|
  (assert-equal '()
                (set-difference (nunion (copy-list '((#(1)) (#(2))))
                                        (copy-list '((#(1)) (#(3))))
                                        :key #'car
                                        :test #'equalp)
                                '((#(1)) (#(2)) (#(3)))
                                :key #'car
                                :test #'equalp)))

(define-test |nunion test=equalp key=other 3|
  (let ((l1 (loop for i from 30 below 100
                  collect (list (make-array 1 :initial-element i))))
        (l2 (loop for i from 40 below 110
                  collect (list (make-array 1 :initial-element i))))
        (l3 (loop for i from 30 below 110
                  collect (list (make-array 1 :initial-element i)))))
    (assert-equal '()
                  (set-difference (nunion (copy-list l1)
                                          (copy-list l2)
                                          :test #'equalp
                                          :key #'car)
                                  l3
                                  :test #'equalp :key #'car))))

(define-test |nunion test=other key=other 1|
  (assert-equal '()
                (set-difference
                 (nunion (copy-list '(1 3 5 7))
                         (copy-list '(2 4 10 3))
                         :key (lambda (x) (mod x 5))
                         :test #'=)
                 '(0 1 2 3 4) :key (lambda (x) (mod x 5)))))

(define-test |nunion test-not=other key=identity 1|
  (assert-equal '()
                (set-difference
                 (nunion (copy-list '(1 2 3 4))
                         (copy-list '(3 4 5 6))
                         :test-not #'/=)
                 '(1 2 3 4 5 6))))

(define-test |nunion test-not=other key=other 1|
  (assert-equal '()
                (set-difference
                 (nunion (copy-list '(1 3 5 7))
                         (copy-list '(2 4 10 3))
                         :key (lambda (x) (mod x 5))
                         :test-not #'/=)
                 '(0 1 2 3 4) :key (lambda (x) (mod x 5)))))

(define-test |nunion test=other test-not=other 1|
  (assert-error 'error
                (nunion (copy-list '(1 2 3))
                        (copy-list '(2 3 4))
                        :test #'eql
                        :test-not #'eql)))

(define-test |nunion test=nil key=identity 1|
  (assert-error 'error
                (set-difference (nunion (copy-list '(1 2 3))
                                        (copy-list '(3 4 5))
                                        :test nil)
                                '(1 2 3 4 5))))

(define-test |nunion test-not=nil key=identity 1|
  (assert-error 'error
                (set-difference (nunion (copy-list '(1 2 3))
                                        (copy-list '(3 4 5))
                                        :test-not nil)
                                '(1 2 3 4 5))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Tests for the intersection function

(define-test |intersection test=eql key=identity 1a|
  (assert-equal '()
                (intersection '() '())))

(define-test |intersection test=eql key=identity 1b|
  (assert-equal '()
                (intersection '() '() :test #'eql)))

(define-test |intersection test=eql key=identity 1c|
  (assert-equal '()
                (intersection '() '() :test 'eql)))

(define-test |intersection test=eql key=identity 2a|
  (assert-equal '()
                (set-difference (intersection (copy-list '(1 2 3 4))
                                              (copy-list '(2 3 4 5)))
                                '(2 3 4))))

(define-test |intersection test=eql key=identity 2b|
  (assert-equal '()
                (set-difference (intersection (copy-list '(1 2 3 4))
                                              (copy-list '(2 3 4 5))
                                              :test #'eql)
                                '(2 3 4))))

(define-test |intersection test=eql key=identity 2|
  (assert-equal '()
                (set-difference (intersection (copy-list '(1 2 3 4))
                                              (copy-list '(2 3 4 5))
                                              :test 'eql)
                                '(2 3 4))))

(define-test |intersection test=eql key=identity 3a|
  (let ((l1 (loop for i from 30 below 40 collect i))
        (l2 (loop for i from 40 below 100 collect i))
        (l3 (loop for i from 100 below 110 collect i)))
    (assert-equal '()
                  (set-difference (intersection (copy-list (append l1 l2))
                                                (copy-list (append l2 l3)))
                                  l2))))

(define-test |intersection test=eql key=identity 3b|
  (let ((l1 (loop for i from 30 below 40 collect i))
        (l2 (loop for i from 40 below 100 collect i))
        (l3 (loop for i from 100 below 110 collect i)))
    (assert-equal '()
                  (set-difference (intersection (copy-list (append l1 l2))
                                                (copy-list (append l2 l3))
                                                :test #'eql)
                                  l2
                                  :test #'eql))))

(define-test |intersection test=eql key=identity 3c|
  (let ((l1 (loop for i from 30 below 40 collect i))
        (l2 (loop for i from 40 below 100 collect i))
        (l3 (loop for i from 100 below 110 collect i)))
    (assert-equal '()
                  (set-difference (intersection (copy-list (append l1 l2))
                                                (copy-list (append l2 l3))
                                                :test 'eql)
                                  l2
                                  :test 'eql))))

(define-test |intersection test=eql key=nil 1|
  (let ((l1 (loop for i from 30 below 40 collect i))
        (l2 (loop for i from 40 below 100 collect i))
        (l3 (loop for i from 100 below 110 collect i)))
    (assert-equal '()
                  (set-difference (intersection (copy-list (append l1 l2))
                                                (copy-list (append l2 l3))
                                                :key nil)
                                  l2))))

(define-test |intersection test=eq key=identity 1|
  (assert-equal '()
                (intersection '() '() :test #'eq)))

(define-test |intersection test=eq key=identity 2|
  (assert-equal '()
                (set-difference (intersection '(abc def) '(abc ghi)
                                              :test #'eq)
                                '(abc)
                                :test #'eq)))

(define-test |intersection test=eq key=identity 3|
  (let ((l1 (loop for code from 30 below 40 collect (string (code-char code))))
        (l2 (loop for code from 40 below 100 collect (string (code-char code))))
        (l3 (loop for code from 100 below 110 collect (string (code-char code)))))
    (assert-equal '()
                  (set-difference (intersection (copy-list (append l1 l2))
                                                (copy-list (append l2 l3))
                                                :test #'eq)
                                  l2
                                  :test #'eq))))

(define-test |intersection test=equal key=identity 1|
  (assert-equal '()
                (intersection '() '() :test #'equal)))

(define-test |intersection test=equal key=identity 2|
  (assert-equal '()
                (set-difference (intersection (copy-list '("abc" "def"))
                                              (copy-list '("abc" "ghi"))
                                              :test #'equal)
                                '("abc")
                                :test #'equal)))

(define-test |intersection test=equal key=identity 3|
  (let ((l1 (loop for code from 30 below 100 collect (string (code-char code))))
        (l2 (loop for code from 40 below 110 collect (string (code-char code))))
        (l3 (loop for code from 40 below 100 collect (string (code-char code)))))
    (assert-equal '()
                  (set-difference (intersection (copy-list l1)
                                                (copy-list l2)
                                                :test #'equal)
                                  l3
                                  :test #'equal))))

(define-test |intersection test=equalp key=identity 1|
  (assert-equal '()
                (intersection '() '() :test #'equalp)))

(define-test |intersection test=equalp key=identity 3|
  (let ((l1 (loop for i from 30 below 100
                  collect (make-array 1 :initial-element i)))
        (l2 (loop for i from 40 below 110
                  collect (make-array 1 :initial-element i)))
        (l3 (loop for i from 40 below 100
                  collect (make-array 1 :initial-element i))))
    (assert-equal '()
                  (set-difference (intersection (copy-list l1)
                                                (copy-list l2)
                                                :test #'equalp)
                                  l3
                                  :test #'equalp))))

(define-test |intersection test=other key=identity 1|
  (assert-equal '()
                (intersection '() '() :test #'<)))

(define-test |intersection test=eql key=other 1a|
  (assert-equal 1
                (length (intersection (copy-list '(1))
                                      (copy-list '(3))
                                      :key #'oddp))))

(define-test |intersection test=eql key=other 1b|
  (assert-equal 1
                (length (intersection (copy-list '(1))
                                      (copy-list '(3))
                                      :key #'oddp
                                      :test #'eql))))

(define-test |intersection test=eql key=other 1c|
  (assert-equal 1
                (length (intersection (copy-list '(1))
                                      (copy-list '(3))
                                      :key #'oddp
                                      :test 'eql))))

(define-test |intersection test=eql key=other 2a|
  (assert-equal 1
                (length (intersection (copy-list '(4 1))
                                      (copy-list '(3))
                                      :key #'oddp))))

(define-test |intersection test=eql key=other 2b|
  (assert-equal 1
                (length (intersection (copy-list '(4 1))
                                      (copy-list '(3))
                                      :key #'oddp
                                      :test #'eql))))

(define-test |intersection test=eql key=other 2c|
  (assert-equal 1
                (length (intersection (copy-list '(4 1))
                                      (copy-list '(3))
                                      :key #'oddp
                                      :test 'eql))))

(define-test |intersection test=eql key=other 3|
  (let ((l1 (loop for i from 30 below 40 collect (list i)))
        (l2 (loop for i from 40 below 100 collect (list i)))
        (l3 (loop for i from 100 below 110 collect (list i))))
    (assert-equal '()
                  (set-difference (intersection (copy-list (append l1 l2))
                                                (copy-list (append l2 l3))
                                                :key #'car)
                                  l2
                                  :key #'car))))

(define-test |intersection test=eql key=other 4|
  (let ((l1 (loop for i from 30 below 40 collect (list i)))
        (l2 (loop for i from 40 below 100 collect (list i)))
        (l3 (loop for i from 100 below 110 collect (list i))))
    (assert-equal '()
                  (set-difference (intersection (copy-list (append l1 l2))
                                                (copy-list (append l2 l3))
                                                :test #'eql
                                                :key #'car)
                                  l2
                                  :test #'eql
                                  :key #'car))))

(define-test |intersection test=eql key=other 5|
  (let ((l1 (loop for i from 30 below 40 collect (list i)))
        (l2 (loop for i from 40 below 100 collect (list i)))
        (l3 (loop for i from 100 below 110 collect (list i))))
    (assert-equal '()
                  (set-difference (intersection (copy-list (append l1 l2))
                                                (copy-list (append l2 l3))
                                                :test 'eql
                                                :key #'car)
                                  l2
                                  :test 'eql
                                  :key #'car))))

(define-test |intersection test=eq key=other 1|
  (assert-equal '()
                (set-difference
                 (intersection (copy-list '((a) (b) (c)))
                               (copy-list '((b) (c) (d)))
                               :key #'car
                               :test #'eq)
                 '((b) (c))
                 :key #'car
                 :test #'eq)))

(define-test |intersection test=eq key=other 3|
  (let ((l1 (loop for code from 30 below 40 collect (list (string (code-char code)))))
        (l2 (loop for code from 40 below 100 collect (list (string (code-char code)))))
        (l3 (loop for code from 100 below 110 collect (list (string (code-char code))))))
    (assert-equal '()
                  (set-difference (intersection (copy-list (append l1 l2))
                                                (copy-list (append l2 l3))
                                                :key #'car
                                                :test #'eq)
                                  l2
                                  :test #'eq
                                  :key #'car))))

(define-test |intersection test=equal key=other 1|
  (assert-equal '()
                (intersection '()
                              '()
                              :test #'equal
                              :key #'car)))

(define-test |intersection test=equal key=other 2|
  (assert-equal '()
                (set-difference (intersection (copy-list '(("abc") ("def")))
                                              (copy-list '(("abc") ("ghi")))
                                              :key #'car
                                              :test #'equal)
                                '(("abc"))
                                :key #'car
                                :test #'equal)))

(define-test |intersection test=equal key=other 3|
  (let ((l1 (loop for code from 30 below 100 collect (list (string (code-char code)))))
        (l2 (loop for code from 40 below 110 collect (list (string (code-char code)))))
        (l3 (loop for code from 40 below 100 collect (list (string (code-char code))))))
    (assert-equal '()
                  (set-difference (intersection (copy-list l1)
                                                (copy-list l2)
                                                :test #'equal
                                                :key #'car)
                                  l3
                                  :test #'equal :key #'car))))

(define-test |intersection test=equalp key=other 1|
  (assert-equal '()
                (intersection '() '() :test #'equalp :key #'car)))

(define-test |intersection test=equalp key=other 2|
  (assert-equal '()
                (set-difference (intersection (copy-list '((#(1)) (#(2))))
                                              (copy-list '((#(1)) (#(3))))
                                              :key #'car
                                              :test #'equalp)
                                '((#(1)))
                                :key #'car
                                :test #'equalp)))

(define-test |intersection test=equalp key=other 3|
  (let ((l1 (loop for i from 30 below 100
                  collect (list (make-array 1 :initial-element i))))
        (l2 (loop for i from 40 below 110
                  collect (list (make-array 1 :initial-element i))))
        (l3 (loop for i from 40 below 100
                  collect (list (make-array 1 :initial-element i)))))
    (assert-equal '()
                  (set-difference (intersection (copy-list l1)
                                                (copy-list l2)
                                                :test #'equalp
                                                :key #'car)
                                  l3
                                  :test #'equalp :key #'car))))

(define-test |intersection test=other key=other 1|
  (assert-equal '()
                (set-difference
                 (intersection (copy-list '(1 3 5 7))
                               (copy-list '(2 4 10 3))
                               :key (lambda (x) (mod x 5))
                               :test #'=)
                 '(3 5 7) :key (lambda (x) (mod x 5)))))

(define-test |intersection test-not=other key=identity 1|
  (assert-equal '()
                (set-difference
                 (intersection (copy-list '(1 2 3 4))
                               (copy-list '(3 4 5 6))
                               :test-not #'/=)
                 '(3 4))))

(define-test |intersection test-not=other key=other 1|
  (assert-equal '()
                (set-difference
                 (intersection (copy-list '(1 3 5 7))
                               (copy-list '(2 4 10 3))
                               :key (lambda (x) (mod x 5))
                               :test-not #'/=)
                 '(3 5 7) :key (lambda (x) (mod x 5)))))

(define-test |intersection test=other test-not=other 1|
  (assert-error 'error
                (intersection (copy-list '(1 2 3))
                              (copy-list '(2 3 4))
                              :test #'eql
                              :test-not #'eql)))

(define-test |intersection test=nil key=identity 1|
  (let ((l1 (loop for i from 30 below 40 collect i))
        (l2 (loop for i from 40 below 100 collect i))
        (l3 (loop for i from 100 below 110 collect i)))
    (assert-error 'error
                  (set-difference (intersection (copy-list (append l1 l2))
                                                (copy-list (append l2 l3))
                                                :test nil)
                                  l2))))

(define-test |intersection test-not=nil key=identity 1|
  (let ((l1 (loop for i from 30 below 40 collect i))
        (l2 (loop for i from 40 below 100 collect i))
        (l3 (loop for i from 100 below 110 collect i)))
    (assert-error 'error
                  (set-difference (intersection (copy-list (append l1 l2))
                                                (copy-list (append l2 l3))
                                                :test-not nil)
                                  l2))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Tests for the nintersection function

(define-test |nintersection test=eql key=identity 1a|
  (assert-equal '()
                (nintersection '() '())))

(define-test |nintersection test=eql key=identity 1b|
  (assert-equal '()
                (nintersection '() '() :test #'eql)))

(define-test |nintersection test=eql key=identity 1c|
  (assert-equal '()
                (nintersection '() '() :test 'eql)))

(define-test |nintersection test=eql key=identity 2a|
  (assert-equal '()
                (set-difference (nintersection (copy-list '(1 2 3 4))
                                               (copy-list '(2 3 4 5)))
                                '(2 3 4))))

(define-test |nintersection test=eql key=identity 2b|
  (assert-equal '()
                (set-difference (nintersection (copy-list '(1 2 3 4))
                                               (copy-list '(2 3 4 5))
                                               :test #'eql)
                                '(2 3 4))))

(define-test |nintersection test=eql key=identity 2|
  (assert-equal '()
                (set-difference (nintersection (copy-list '(1 2 3 4))
                                               (copy-list '(2 3 4 5))
                                               :test 'eql)
                                '(2 3 4))))

(define-test |nintersection test=eql key=identity 3a|
  (let ((l1 (loop for i from 30 below 40 collect i))
        (l2 (loop for i from 40 below 100 collect i))
        (l3 (loop for i from 100 below 110 collect i)))
    (assert-equal '()
                  (set-difference (nintersection (copy-list (append l1 l2))
                                                 (copy-list (append l2 l3)))
                                  l2))))

(define-test |nintersection test=eql key=identity 3b|
  (let ((l1 (loop for i from 30 below 40 collect i))
        (l2 (loop for i from 40 below 100 collect i))
        (l3 (loop for i from 100 below 110 collect i)))
    (assert-equal '()
                  (set-difference (nintersection (copy-list (append l1 l2))
                                                 (copy-list (append l2 l3))
                                                 :test #'eql)
                                  l2
                                  :test #'eql))))

(define-test |nintersection test=eql key=identity 3c|
  (let ((l1 (loop for i from 30 below 40 collect i))
        (l2 (loop for i from 40 below 100 collect i))
        (l3 (loop for i from 100 below 110 collect i)))
    (assert-equal '()
                  (set-difference (nintersection (copy-list (append l1 l2))
                                                 (copy-list (append l2 l3))
                                                 :test 'eql)
                                  l2
                                  :test 'eql))))

(define-test |nintersection test=eql key=nil 1|
  (let ((l1 (loop for i from 30 below 40 collect i))
        (l2 (loop for i from 40 below 100 collect i))
        (l3 (loop for i from 100 below 110 collect i)))
    (assert-equal '()
                  (set-difference (nintersection (copy-list (append l1 l2))
                                                 (copy-list (append l2 l3))
                                                 :key nil)
                                  l2))))

(define-test |nintersection test=eq key=identity 1|
  (assert-equal '()
                (nintersection '() '() :test #'eq)))

(define-test |nintersection test=eq key=identity 2|
  (assert-equal '()
                (set-difference (nintersection '(abc def) '(abc ghi)
                                               :test #'eq)
                                '(abc)
                                :test #'eq)))

(define-test |nintersection test=eq key=identity 3|
  (let ((l1 (loop for code from 30 below 40 collect (string (code-char code))))
        (l2 (loop for code from 40 below 100 collect (string (code-char code))))
        (l3 (loop for code from 100 below 110 collect (string (code-char code)))))
    (assert-equal '()
                  (set-difference (nintersection (copy-list (append l1 l2))
                                                 (copy-list (append l2 l3))
                                                 :test #'eq)
                                  l2
                                  :test #'eq))))

(define-test |nintersection test=equal key=identity 1|
  (assert-equal '()
                (nintersection '() '() :test #'equal)))

(define-test |nintersection test=equal key=identity 2|
  (assert-equal '()
                (set-difference (nintersection (copy-list '("abc" "def"))
                                               (copy-list '("abc" "ghi"))
                                               :test #'equal)
                                '("abc")
                                :test #'equal)))

(define-test |nintersection test=equal key=identity 3|
  (let ((l1 (loop for code from 30 below 100 collect (string (code-char code))))
        (l2 (loop for code from 40 below 110 collect (string (code-char code))))
        (l3 (loop for code from 40 below 100 collect (string (code-char code)))))
    (assert-equal '()
                  (set-difference (nintersection (copy-list l1)
                                                 (copy-list l2)
                                                 :test #'equal)
                                  l3
                                  :test #'equal))))

(define-test |nintersection test=equalp key=identity 1|
  (assert-equal '()
                (nintersection '() '() :test #'equalp)))

(define-test |nintersection test=equalp key=identity 3|
  (let ((l1 (loop for i from 30 below 100
                  collect (make-array 1 :initial-element i)))
        (l2 (loop for i from 40 below 110
                  collect (make-array 1 :initial-element i)))
        (l3 (loop for i from 40 below 100
                  collect (make-array 1 :initial-element i))))
    (assert-equal '()
                  (set-difference (nintersection (copy-list l1)
                                                 (copy-list l2)
                                                 :test #'equalp)
                                  l3
                                  :test #'equalp))))

(define-test |nintersection test=other key=identity 1|
  (assert-equal '()
                (nintersection '() '() :test #'<)))

(define-test |nintersection test=eql key=other 1a|
  (assert-equal 1
                (length (nintersection (copy-list '(1))
                                       (copy-list '(3))
                                       :key #'oddp))))

(define-test |nintersection test=eql key=other 1b|
  (assert-equal 1
                (length (nintersection (copy-list '(1))
                                       (copy-list '(3))
                                       :key #'oddp
                                       :test #'eql))))

(define-test |nintersection test=eql key=other 1c|
  (assert-equal 1
                (length (nintersection (copy-list '(1))
                                       (copy-list '(3))
                                       :key #'oddp
                                       :test 'eql))))

(define-test |nintersection test=eql key=other 2a|
  (assert-equal 1
                (length (nintersection (copy-list '(4 1))
                                       (copy-list '(3))
                                       :key #'oddp))))

(define-test |nintersection test=eql key=other 2b|
  (assert-equal 1
                (length (nintersection (copy-list '(4 1))
                                       (copy-list '(3))
                                       :key #'oddp
                                       :test #'eql))))

(define-test |nintersection test=eql key=other 2c|
  (assert-equal 1
                (length (nintersection (copy-list '(4 1))
                                       (copy-list '(3))
                                       :key #'oddp
                                       :test 'eql))))

(define-test |nintersection test=eql key=other 3|
  (let ((l1 (loop for i from 30 below 40 collect (list i)))
        (l2 (loop for i from 40 below 100 collect (list i)))
        (l3 (loop for i from 100 below 110 collect (list i))))
    (assert-equal '()
                  (set-difference (nintersection (copy-list (append l1 l2))
                                                 (copy-list (append l2 l3))
                                                 :key #'car)
                                  l2
                                  :key #'car))))

(define-test |nintersection test=eql key=other 4|
  (let ((l1 (loop for i from 30 below 40 collect (list i)))
        (l2 (loop for i from 40 below 100 collect (list i)))
        (l3 (loop for i from 100 below 110 collect (list i))))
    (assert-equal '()
                  (set-difference (nintersection (copy-list (append l1 l2))
                                                 (copy-list (append l2 l3))
                                                 :test #'eql
                                                 :key #'car)
                                  l2
                                  :test #'eql
                                  :key #'car))))

(define-test |nintersection test=eql key=other 5|
  (let ((l1 (loop for i from 30 below 40 collect (list i)))
        (l2 (loop for i from 40 below 100 collect (list i)))
        (l3 (loop for i from 100 below 110 collect (list i))))
    (assert-equal '()
                  (set-difference (nintersection (copy-list (append l1 l2))
                                                 (copy-list (append l2 l3))
                                                 :test 'eql
                                                 :key #'car)
                                  l2
                                  :test 'eql
                                  :key #'car))))

(define-test |nintersection test=eq key=other 1|
  (assert-equal '()
                (set-difference
                 (nintersection (copy-list '((a) (b) (c)))
                                (copy-list '((b) (c) (d)))
                                :key #'car
                                :test #'eq)
                 '((b) (c))
                 :key #'car
                 :test #'eq)))

(define-test |nintersection test=eq key=other 3|
  (let ((l1 (loop for code from 30 below 40 collect (list (string (code-char code)))))
        (l2 (loop for code from 40 below 100 collect (list (string (code-char code)))))
        (l3 (loop for code from 100 below 110 collect (list (string (code-char code))))))
    (assert-equal '()
                  (set-difference (nintersection (copy-list (append l1 l2))
                                                 (copy-list (append l2 l3))
                                                 :key #'car
                                                 :test #'eq)
                                  l2
                                  :test #'eq
                                  :key #'car))))

(define-test |nintersection test=equal key=other 1|
  (assert-equal '()
                (nintersection '()
                               '()
                               :test #'equal
                               :key #'car)))

(define-test |nintersection test=equal key=other 2|
  (assert-equal '()
                (set-difference (nintersection (copy-list '(("abc") ("def")))
                                               (copy-list '(("abc") ("ghi")))
                                               :key #'car
                                               :test #'equal)
                                '(("abc"))
                                :key #'car
                                :test #'equal)))

(define-test |nintersection test=equal key=other 3|
  (let ((l1 (loop for code from 30 below 100 collect (list (string (code-char code)))))
        (l2 (loop for code from 40 below 110 collect (list (string (code-char code)))))
        (l3 (loop for code from 40 below 100 collect (list (string (code-char code))))))
    (assert-equal '()
                  (set-difference (nintersection (copy-list l1)
                                                 (copy-list l2)
                                                 :test #'equal
                                                 :key #'car)
                                  l3
                                  :test #'equal :key #'car))))

(define-test |nintersection test=equalp key=other 1|
  (assert-equal '()
                (nintersection '() '() :test #'equalp :key #'car)))

(define-test |nintersection test=equalp key=other 2|
  (assert-equal '()
                (set-difference (nintersection (copy-list '((#(1)) (#(2))))
                                               (copy-list '((#(1)) (#(3))))
                                               :key #'car
                                               :test #'equalp)
                                '((#(1)))
                                :key #'car
                                :test #'equalp)))

(define-test |nintersection test=equalp key=other 3|
  (let ((l1 (loop for i from 30 below 100
                  collect (list (make-array 1 :initial-element i))))
        (l2 (loop for i from 40 below 110
                  collect (list (make-array 1 :initial-element i))))
        (l3 (loop for i from 40 below 100
                  collect (list (make-array 1 :initial-element i)))))
    (assert-equal '()
                  (set-difference (nintersection (copy-list l1)
                                                 (copy-list l2)
                                                 :test #'equalp
                                                 :key #'car)
                                  l3
                                  :test #'equalp :key #'car))))

(define-test |nintersection test=other key=other 1|
  (assert-equal '()
                (set-difference
                 (nintersection (copy-list '(1 3 5 7))
                                (copy-list '(2 4 10 3))
                                :key (lambda (x) (mod x 5))
                                :test #'=)
                 '(3 5 7) :key (lambda (x) (mod x 5)))))

(define-test |nintersection test-not=other key=identity 1|
  (assert-equal '()
                (set-difference
                 (nintersection (copy-list '(1 2 3 4))
                                (copy-list '(3 4 5 6))
                                :test-not #'/=)
                 '(3 4))))

(define-test |nintersection test-not=other key=other 1|
  (assert-equal '()
                (set-difference
                 (nintersection (copy-list '(1 3 5 7))
                                (copy-list '(2 4 10 3))
                                :key (lambda (x) (mod x 5))
                                :test-not #'/=)
                 '(3 5 7) :key (lambda (x) (mod x 5)))))

(define-test |nintersection test=other test-not=other 1|
  (assert-error 'error
                (nintersection (copy-list '(1 2 3))
                               (copy-list '(2 3 4))
                               :test #'eql
                               :test-not #'eql)))

(define-test |nintersection test=nil key=identity 1|
  (let ((l1 (loop for i from 30 below 40 collect i))
        (l2 (loop for i from 40 below 100 collect i))
        (l3 (loop for i from 100 below 110 collect i)))
    (assert-error 'error
                  (set-difference (nintersection (copy-list (append l1 l2))
                                                 (copy-list (append l2 l3))
                                                 :test nil)
                                  l2))))

(define-test |nintersection test-not=nil key=identity 1|
  (let ((l1 (loop for i from 30 below 40 collect i))
        (l2 (loop for i from 40 below 100 collect i))
        (l3 (loop for i from 100 below 110 collect i)))
    (assert-error 'error
                  (set-difference (nintersection (copy-list (append l1 l2))
                                                 (copy-list (append l2 l3))
                                                 :test-not nil)
                                  l2))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Tests for the set-difference function

(define-test |set-difference test=eql key=identity 1a|
  (assert-equal '()
                (set-difference '() '())))

(define-test |set-difference test=eql key=identity 1b|
  (assert-equal '()
                (set-difference '()
                                '()
                                :test #'eql)))

(define-test |set-difference test=eql key=identity 1c|
  (assert-equal '()
                (set-difference '()
                                '()
                                :test 'eql)))

(define-test |set-difference test=eql key=identity 2a|
  (let ((l1 (loop for i from 1 below 10 collect i))
        (l2 (loop for i from 2 below 20 collect i)))
    (assert-equal '(1)
                  (set-difference (copy-list l1)
                                  (copy-list l2)))))

(define-test |set-difference test=eql key=identity 2b|
  (let ((l1 (loop for i from 1 below 10 collect i))
        (l2 (loop for i from 2 below 20 collect i)))
    (assert-equal '(1)
                  (set-difference (copy-list l1)
                                  (copy-list l2)
                                  :test #'eql))))

(define-test |set-difference test=eql key=identity 2c|
  (let ((l1 (loop for i from 1 below 10 collect i))
        (l2 (loop for i from 2 below 20 collect i)))
    (assert-equal '(1)
                  (set-difference (copy-list l1)
                                  (copy-list l2)
                                  :test 'eql))))

(define-test |set-difference test=eql key=identity 3a|
  (let ((l1 (loop for i from 1 below 10 collect i))
        (l2 (loop for i from 2 below 200 collect i)))
    (assert-equal '(1)
                  (set-difference (copy-list l1)
                                  (copy-list l2)))))

(define-test |set-difference test=eql key=identity 3b|
  (let ((l1 (loop for i from 1 below 10 collect i))
        (l2 (loop for i from 2 below 200 collect i)))
    (assert-equal '(1)
                  (set-difference (copy-list l1)
                                  (copy-list l2)
                                  :test #'eql))))

(define-test |set-difference test=eql key=identity 3c|
  (let ((l1 (loop for i from 1 below 10 collect i))
        (l2 (loop for i from 2 below 200 collect i)))
    (assert-equal '(1)
                  (set-difference (copy-list l1)
                                  (copy-list l2)
                                  :test 'eql))))

(define-test |set-difference test=eql key=nil 1|
  (let ((l1 (loop for i from 1 below 10 collect i))
        (l2 (loop for i from 2 below 20 collect i)))
    (assert-equal '(1)
                  (set-difference (copy-list l1)
                                  (copy-list l2)
                                  :key nil))))

(define-test |set-difference test=eq key=identity 1|
  (assert-equal '()
                (set-difference '()
                                '()
                                :test #'eq)))

(define-test |set-difference test=eq key=identity 2a|
  (let ((l1 (loop for i from 1 below 10 collect (list i)))
        (l2 (loop for i from 2 below 20 collect (list i))))
    (assert-equal 1
                  (length (set-difference (copy-list l1)
                                          (copy-list (append (cdr l1) l2))
                                          :test #'eq)))
    (assert-eq (car l1)
               (car (set-difference (copy-list l1)
                                    (copy-list (append (cdr l1) l2))
                                    :test #'eq)))))

(define-test |set-difference test=eq key=identity 2b|
  (let ((l1 (loop for i from 1 below 10 collect (list i)))
        (l2 (loop for i from 2 below 20 collect (list i))))
    (assert-equal 1
                  (length (set-difference (copy-list l1)
                                          (copy-list (append (cdr l1) l2))
                                          :test 'eq)))
    (assert-eq (car l1)
               (car (set-difference (copy-list l1)
                                    (copy-list (append (cdr l1) l2))
                                    :test 'eq)))))

(define-test |set-difference test=eq key=identity 3|
  (let ((l1 (loop for i from 1 below 10 collect (list i)))
        (l2 (loop for i from 2 below 200 collect (list i))))
    (assert-equal 1
                  (length (set-difference (copy-list l1)
                                          (copy-list (append (cdr l1) l2))
                                          :test #'eq)))
    (assert-eq (car l1)
               (car (set-difference (copy-list l1)
                                    (copy-list (append (cdr l1) l2))
                                    :test #'eq)))))

(define-test |set-difference test=equal key=identity 1|
  (assert-equal '()
                (set-difference '()
                                '()
                                :test #'equal)))

(define-test |set-difference test=equal key=identity 2a|
  (let ((l1 (loop for i from 1 below 10 collect (list i)))
        (l2 (loop for i from 2 below 20 collect (list i))))
    (assert-equal '((1))
                  (set-difference (copy-list l1)
                                  (copy-list (append (cdr l1) l2))
                                  :test #'equal))))

(define-test |set-difference test=equal key=identity 2b|
  (let ((l1 (loop for i from 1 below 10 collect (list i)))
        (l2 (loop for i from 2 below 20 collect (list i))))
    (assert-equal '((1))
                  (set-difference (copy-list l1)
                                  (copy-list (append (cdr l1) l2))
                                  :test 'equal))))

(define-test |set-difference test=equal key=identity 3|
  (let ((l1 (loop for i from 1 below 10 collect (list i)))
        (l2 (loop for i from 2 below 200 collect (list i))))
    (assert-equal '((1))
                  (set-difference (copy-list l1)
                                  (copy-list (append (cdr l1) l2))
                                  :test #'equal))))

(define-test |set-difference test=equalp key=identity 1|
  (assert-equal '()
                (set-difference '()
                                '()
                                :test #'equalp)))

(define-test |set-difference test=equalp key=identity 2a|
  (let ((l1 (loop for i from 1 below 10 collect (make-array 1 :initial-element i)))
        (l2 (loop for i from 2 below 20 collect (make-array 1 :initial-element i))))
    (assert-equalp '(#(1))
                   (set-difference (copy-list l1)
                                   (copy-list (append (cdr l1) l2))
                                   :test #'equalp))))

(define-test |set-difference test=equalp key=identity 2b|
  (let ((l1 (loop for i from 1 below 10 collect (make-array 1 :initial-element i)))
        (l2 (loop for i from 2 below 20 collect (make-array 1 :initial-element i))))
    (assert-equalp '(#(1))
                   (set-difference (copy-list l1)
                                   (copy-list (append (cdr l1) l2))
                                   :test 'equalp))))

(define-test |set-difference test=equalp key=identity 3a|
  (let ((l1 (loop for i from 1 below 10 collect (make-array 1 :initial-element i)))
        (l2 (loop for i from 2 below 200 collect (make-array 1 :initial-element i))))
    (assert-equalp '(#(1))
                   (set-difference (copy-list l1)
                                   (copy-list (append (cdr l1) l2))
                                   :test #'equalp))))

(define-test |set-difference test=equalp key=identity 3b|
  (let ((l1 (loop for i from 1 below 10 collect (make-array 1 :initial-element i)))
        (l2 (loop for i from 2 below 200 collect (make-array 1 :initial-element i))))
    (assert-equalp '(#(1))
                   (set-difference (copy-list l1)
                                   (copy-list (append (cdr l1) l2))
                                   :test #'equalp))))

(define-test |set-difference test=other key=identity 1|
  (let ((l1 (loop for i from 1 below 10 collect i))
        (l2 (loop for i from 1 below 20 collect i)))
    (assert-equal '(1)
                  (set-difference (copy-list l1)
                                  (copy-list l2)
                                  :test #'>))))

(define-test |set-difference test-not=other key=identity 1|
  (let ((l1 (loop for i from 1 below 10 collect i))
        (l2 (loop for i from 1 below 20 collect i)))
    (assert-equal '(1)
                  (set-difference (copy-list l1)
                                  (copy-list l2)
                                  :test-not #'<=))))

(define-test |set-difference test=eql key=other 1a|
  (assert-equal '()
                (set-difference '()
                                '()
                                :key #'car)))

(define-test |set-difference test=eql key=other 1b|
  (assert-equal '()
                (set-difference '()
                                '()
                                :key #'car
                                :test #'eql)))

(define-test |set-difference test=eql key=other 1c|
  (assert-equal '()
                (set-difference '()
                                '()
                                :key #'car
                                :test 'eql)))

(define-test |set-difference test=eql key=other 2a|
  (let ((l1 (loop for i from 1 below 10 collect (list i)))
        (l2 (loop for i from 2 below 20 collect (list i))))
    (assert-equal '((1))
                  (set-difference (copy-list l1)
                                  (copy-list l2)
                                  :key #'car))))

(define-test |set-difference test=eql key=other 2b|
  (let ((l1 (loop for i from 1 below 10 collect (list i)))
        (l2 (loop for i from 2 below 20 collect (list i))))
    (assert-equal '((1))
                  (set-difference (copy-list l1)
                                  (copy-list l2)
                                  :key #'car
                                  :test #'eql))))

(define-test |set-difference test=eql key=other 2c|
  (let ((l1 (loop for i from 1 below 10 collect (list i)))
        (l2 (loop for i from 2 below 20 collect (list i))))
    (assert-equal '((1))
                  (set-difference (copy-list l1)
                                  (copy-list l2)
                                  :key #'car
                                  :test 'eql))))

(define-test |set-difference test=eql key=other 3a|
  (let ((l1 (loop for i from 1 below 10 collect (list i)))
        (l2 (loop for i from 2 below 200 collect (list i))))
    (assert-equal '((1))
                  (set-difference (copy-list l1)
                                  (copy-list l2)
                                  :key #'car))))

(define-test |set-difference test=eql key=other 3b|
  (let ((l1 (loop for i from 1 below 10 collect (list i)))
        (l2 (loop for i from 2 below 200 collect (list i))))
    (assert-equal '((1))
                  (set-difference (copy-list l1)
                                  (copy-list l2)
                                  :key #'car
                                  :test #'eql))))

(define-test |set-difference test=eql key=other 3c|
  (let ((l1 (loop for i from 1 below 10 collect (list i)))
        (l2 (loop for i from 2 below 200 collect (list i))))
    (assert-equal '((1))
                  (set-difference (copy-list l1)
                                  (copy-list l2)
                                  :key #'car
                                  :test 'eql))))

(define-test |set-difference test=eq key=other 1|
  (assert-equal '()
                (set-difference '()
                                '()
                                :key #'car
                                :test #'eq)))

(define-test |set-difference test=eq key=other 2a|
  (let ((l1 (loop for i from 1 below 10 collect (list (list i))))
        (l2 (loop for i from 2 below 20 collect (list (list i)))))
    (assert-equal 1
                  (length (set-difference (copy-list l1)
                                          (copy-list (append (mapcar #'copy-list (cdr l1))
                                                             l2))
                                          :key #'car
                                          :test #'eq)))
    (assert-eq (car l1)
               (car (set-difference (copy-list l1)
                                    (copy-list (append (mapcar #'copy-list (cdr l1))
                                                       l2))
                                    :key #'car
                                    :test #'eq)))))

(define-test |set-difference test=eq key=other 2b|
  (let ((l1 (loop for i from 1 below 10 collect (list (list i))))
        (l2 (loop for i from 2 below 20 collect (list (list i)))))
    (assert-equal 1
                  (length (set-difference (copy-list l1)
                                          (copy-list (append (mapcar #'copy-list (cdr l1))
                                                             l2))
                                          :key #'car
                                          :test 'eq)))
    (assert-eq (car l1)
               (car (set-difference (copy-list l1)
                                    (copy-list (append (mapcar #'copy-list (cdr l1))
                                                       l2))
                                    :key #'car
                                    :test 'eq)))))

(define-test |set-difference test=eq key=other 3|
  (let ((l1 (loop for i from 1 below 10 collect (list (list i))))
        (l2 (loop for i from 2 below 200 collect (list (list i)))))
    (assert-equal 1
                  (length (set-difference (copy-list l1)
                                          (copy-list (append (mapcar #'copy-list (cdr l1))
                                                             l2))
                                          :key #'car
                                          :test #'eq)))
    (assert-eq (car l1)
               (car (set-difference (copy-list l1)
                                    (copy-list (append (mapcar #'copy-list (cdr l1))
                                                       l2))
                                    :key #'car
                                    :test #'eq)))))

(define-test |set-difference test=equal key=other 1|
  (assert-equal '()
                (set-difference '()
                                '()
                                :key #'car
                                :test #'equal)))

(define-test |set-difference test=equal key=other 2a|
  (let ((l1 (loop for i from 1 below 10 collect (list (list i))))
        (l2 (loop for i from 2 below 20 collect (list (list i)))))
    (assert-equal '(((1)))
                  (set-difference (copy-list l1)
                                  (copy-list (append (cdr l1) l2))
                                  :key #'car
                                  :test #'equal))))

(define-test |set-difference test=equal key=other 2b|
  (let ((l1 (loop for i from 1 below 10 collect (list (list i))))
        (l2 (loop for i from 2 below 20 collect (list (list i)))))
    (assert-equal '(((1)))
                  (set-difference (copy-list l1)
                                  (copy-list (append (cdr l1) l2))
                                  :key #'car
                                  :test 'equal))))

(define-test |set-difference test=equal key=other 3|
  (let ((l1 (loop for i from 1 below 10 collect (list (list i))))
        (l2 (loop for i from 2 below 200 collect (list (list i)))))
    (assert-equal '(((1)))
                  (set-difference (copy-list l1)
                                  (copy-list (append (cdr l1) l2))
                                  :key #'car
                                  :test #'equal))))

(define-test |set-difference test=equalp key=other 1|
  (assert-equal '()
                (set-difference '()
                                '()
                                :key #'car
                                :test #'equalp)))

(define-test |set-difference test=equalp key=other 2a|
  (let ((l1 (loop for i from 1 below 10 collect (list (make-array 1 :initial-element i))))
        (l2 (loop for i from 2 below 20 collect (list (make-array 1 :initial-element i)))))
    (assert-equalp '((#(1)))
                   (set-difference (copy-list l1)
                                   (copy-list (append (cdr l1) l2))
                                   :key #'car
                                   :test #'equalp))))

(define-test |set-difference test=equalp key=other 2b|
  (let ((l1 (loop for i from 1 below 10 collect (list (make-array 1 :initial-element i))))
        (l2 (loop for i from 2 below 20 collect (list (make-array 1 :initial-element i)))))
    (assert-equalp '((#(1)))
                   (set-difference (copy-list l1)
                                   (copy-list (append (cdr l1) l2))
                                   :key #'car
                                   :test 'equalp))))

(define-test |set-difference test=equalp key=other 3a|
  (let ((l1 (loop for i from 1 below 10 collect (list (make-array 1 :initial-element i))))
        (l2 (loop for i from 2 below 200 collect (list (make-array 1 :initial-element i)))))
    (assert-equalp '((#(1)))
                   (set-difference (copy-list l1)
                                   (copy-list (append (cdr l1) l2))
                                   :key #'car
                                   :test #'equalp))))

(define-test |set-difference test=equalp key=other 3b|
  (let ((l1 (loop for i from 1 below 10 collect (list (make-array 1 :initial-element i))))
        (l2 (loop for i from 2 below 200 collect (list (make-array 1 :initial-element i)))))
    (assert-equalp '((#(1)))
                   (set-difference (copy-list l1)
                                   (copy-list (append (cdr l1) l2))
                                   :key #'car
                                   :test 'equalp))))

(define-test |set-difference test=other key=other 1|
  (let ((l1 (loop for i from 1 below 10 collect (list i)))
        (l2 (loop for i from 1 below 20 collect (list i))))
    (assert-equal '((1))
                  (set-difference (copy-list l1)
                                  (copy-list l2)
                                  :key #'car
                                  :test #'>))))

(define-test |set-difference test-not=other key=other 1|
  (let ((l1 (loop for i from 1 below 10 collect (list i)))
        (l2 (loop for i from 1 below 20 collect (list i))))
    (assert-equal '((1))
                  (set-difference (copy-list l1)
                                  (copy-list l2)
                                  :key #'car
                                  :test-not #'<=))))

(define-test |set-difference test=other test-not=other 1|
  (assert-error 'error
                (set-difference '(1 2 3) '(2 3 4)
                                :test #'eql
                                :test-not #'eql)))

(define-test |set-difference test=nil key=identity 1|
  (let ((l1 (loop for i from 1 below 10 collect i))
        (l2 (loop for i from 2 below 20 collect i)))
    (assert-error 'error
                  (set-difference (copy-list l1)
                                  (copy-list l2)
                                  :test nil))))

(define-test |set-difference test-not=nil key=identity 1|
  (let ((l1 (loop for i from 1 below 10 collect i))
        (l2 (loop for i from 2 below 20 collect i)))
    (assert-error 'error
                  (set-difference (copy-list l1)
                                  (copy-list l2)
                                  :test-not nil))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Tests for the nset-difference function

(define-test |nset-difference test=eql key=identity 1a|
  (assert-equal '()
                (nset-difference '() '())))

(define-test |nset-difference test=eql key=identity 1b|
  (assert-equal '()
                (nset-difference '()
                                 '()
                                 :test #'eql)))

(define-test |nset-difference test=eql key=identity 1c|
  (assert-equal '()
                (nset-difference '()
                                 '()
                                 :test 'eql)))

(define-test |nset-difference test=eql key=identity 2a|
  (let ((l1 (loop for i from 1 below 10 collect i))
        (l2 (loop for i from 2 below 20 collect i)))
    (assert-equal '(1)
                  (nset-difference (copy-list l1)
                                   (copy-list l2)))))

(define-test |nset-difference test=eql key=identity 2b|
  (let ((l1 (loop for i from 1 below 10 collect i))
        (l2 (loop for i from 2 below 20 collect i)))
    (assert-equal '(1)
                  (nset-difference (copy-list l1)
                                   (copy-list l2)
                                   :test #'eql))))

(define-test |nset-difference test=eql key=identity 2c|
  (let ((l1 (loop for i from 1 below 10 collect i))
        (l2 (loop for i from 2 below 20 collect i)))
    (assert-equal '(1)
                  (nset-difference (copy-list l1)
                                   (copy-list l2)
                                   :test 'eql))))

(define-test |nset-difference test=eql key=identity 3a|
  (let ((l1 (loop for i from 1 below 10 collect i))
        (l2 (loop for i from 2 below 200 collect i)))
    (assert-equal '(1)
                  (nset-difference (copy-list l1)
                                   (copy-list l2)))))

(define-test |nset-difference test=eql key=identity 3b|
  (let ((l1 (loop for i from 1 below 10 collect i))
        (l2 (loop for i from 2 below 200 collect i)))
    (assert-equal '(1)
                  (nset-difference (copy-list l1)
                                   (copy-list l2)
                                   :test #'eql))))

(define-test |nset-difference test=eql key=identity 3c|
  (let ((l1 (loop for i from 1 below 10 collect i))
        (l2 (loop for i from 2 below 200 collect i)))
    (assert-equal '(1)
                  (nset-difference (copy-list l1)
                                   (copy-list l2)
                                   :test 'eql))))

(define-test |nset-difference test=eql key=nil 1|
  (let ((l1 (loop for i from 1 below 10 collect i))
        (l2 (loop for i from 2 below 20 collect i)))
    (assert-equal '(1)
                  (nset-difference (copy-list l1)
                                   (copy-list l2)
                                   :key nil))))

(define-test |nset-difference test=eq key=identity 1|
  (assert-equal '()
                (nset-difference '()
                                 '()
                                 :test #'eq)))

(define-test |nset-difference test=eq key=identity 2a|
  (let ((l1 (loop for i from 1 below 10 collect (list i)))
        (l2 (loop for i from 2 below 20 collect (list i))))
    (assert-equal 1
                  (length (nset-difference (copy-list l1)
                                           (copy-list (append (cdr l1) l2))
                                           :test #'eq)))
    (assert-eq (car l1)
               (car (nset-difference (copy-list l1)
                                     (copy-list (append (cdr l1) l2))
                                     :test #'eq)))))

(define-test |nset-difference test=eq key=identity 2b|
  (let ((l1 (loop for i from 1 below 10 collect (list i)))
        (l2 (loop for i from 2 below 20 collect (list i))))
    (assert-equal 1
                  (length (nset-difference (copy-list l1)
                                           (copy-list (append (cdr l1) l2))
                                           :test 'eq)))
    (assert-eq (car l1)
               (car (nset-difference (copy-list l1)
                                     (copy-list (append (cdr l1) l2))
                                     :test 'eq)))))

(define-test |nset-difference test=eq key=identity 3|
  (let ((l1 (loop for i from 1 below 10 collect (list i)))
        (l2 (loop for i from 2 below 200 collect (list i))))
    (assert-equal 1
                  (length (nset-difference (copy-list l1)
                                           (copy-list (append (cdr l1) l2))
                                           :test #'eq)))
    (assert-eq (car l1)
               (car (nset-difference (copy-list l1)
                                     (copy-list (append (cdr l1) l2))
                                     :test #'eq)))))

(define-test |nset-difference test=equal key=identity 1|
  (assert-equal '()
                (nset-difference '()
                                 '()
                                 :test #'equal)))

(define-test |nset-difference test=equal key=identity 2a|
  (let ((l1 (loop for i from 1 below 10 collect (list i)))
        (l2 (loop for i from 2 below 20 collect (list i))))
    (assert-equal '((1))
                  (nset-difference (copy-list l1)
                                   (copy-list (append (cdr l1) l2))
                                   :test #'equal))))

(define-test |nset-difference test=equal key=identity 2b|
  (let ((l1 (loop for i from 1 below 10 collect (list i)))
        (l2 (loop for i from 2 below 20 collect (list i))))
    (assert-equal '((1))
                  (nset-difference (copy-list l1)
                                   (copy-list (append (cdr l1) l2))
                                   :test 'equal))))

(define-test |nset-difference test=equal key=identity 3|
  (let ((l1 (loop for i from 1 below 10 collect (list i)))
        (l2 (loop for i from 2 below 200 collect (list i))))
    (assert-equal '((1))
                  (nset-difference (copy-list l1)
                                   (copy-list (append (cdr l1) l2))
                                   :test #'equal))))

(define-test |nset-difference test=equalp key=identity 1|
  (assert-equal '()
                (nset-difference '()
                                 '()
                                 :test #'equalp)))

(define-test |nset-difference test=equalp key=identity 2a|
  (let ((l1 (loop for i from 1 below 10 collect (make-array 1 :initial-element i)))
        (l2 (loop for i from 2 below 20 collect (make-array 1 :initial-element i))))
    (assert-equalp '(#(1))
                   (nset-difference (copy-list l1)
                                    (copy-list (append (cdr l1) l2))
                                    :test #'equalp))))

(define-test |nset-difference test=equalp key=identity 2b|
  (let ((l1 (loop for i from 1 below 10 collect (make-array 1 :initial-element i)))
        (l2 (loop for i from 2 below 20 collect (make-array 1 :initial-element i))))
    (assert-equalp '(#(1))
                   (nset-difference (copy-list l1)
                                    (copy-list (append (cdr l1) l2))
                                    :test 'equalp))))

(define-test |nset-difference test=equalp key=identity 3a|
  (let ((l1 (loop for i from 1 below 10 collect (make-array 1 :initial-element i)))
        (l2 (loop for i from 2 below 200 collect (make-array 1 :initial-element i))))
    (assert-equalp '(#(1))
                   (nset-difference (copy-list l1)
                                    (copy-list (append (cdr l1) l2))
                                    :test #'equalp))))

(define-test |nset-difference test=equalp key=identity 3b|
  (let ((l1 (loop for i from 1 below 10 collect (make-array 1 :initial-element i)))
        (l2 (loop for i from 2 below 200 collect (make-array 1 :initial-element i))))
    (assert-equalp '(#(1))
                   (nset-difference (copy-list l1)
                                    (copy-list (append (cdr l1) l2))
                                    :test #'equalp))))

(define-test |nset-difference test=other key=identity 1|
  (let ((l1 (loop for i from 1 below 10 collect i))
        (l2 (loop for i from 1 below 20 collect i)))
    (assert-equal '(1)
                  (nset-difference (copy-list l1)
                                   (copy-list l2)
                                   :test #'>))))

(define-test |nset-difference test-not=other key=identity 1|
  (let ((l1 (loop for i from 1 below 10 collect i))
        (l2 (loop for i from 1 below 20 collect i)))
    (assert-equal '(1)
                  (nset-difference (copy-list l1)
                                   (copy-list l2)
                                   :test-not #'<=))))

(define-test |nset-difference test=eql key=other 1a|
  (assert-equal '()
                (nset-difference '()
                                 '()
                                 :key #'car)))

(define-test |nset-difference test=eql key=other 1b|
  (assert-equal '()
                (nset-difference '()
                                 '()
                                 :key #'car
                                 :test #'eql)))

(define-test |nset-difference test=eql key=other 1c|
  (assert-equal '()
                (nset-difference '()
                                 '()
                                 :key #'car
                                 :test 'eql)))

(define-test |nset-difference test=eql key=other 2a|
  (let ((l1 (loop for i from 1 below 10 collect (list i)))
        (l2 (loop for i from 2 below 20 collect (list i))))
    (assert-equal '((1))
                  (nset-difference (copy-list l1)
                                   (copy-list l2)
                                   :key #'car))))

(define-test |nset-difference test=eql key=other 2b|
  (let ((l1 (loop for i from 1 below 10 collect (list i)))
        (l2 (loop for i from 2 below 20 collect (list i))))
    (assert-equal '((1))
                  (nset-difference (copy-list l1)
                                   (copy-list l2)
                                   :key #'car
                                   :test #'eql))))

(define-test |nset-difference test=eql key=other 2c|
  (let ((l1 (loop for i from 1 below 10 collect (list i)))
        (l2 (loop for i from 2 below 20 collect (list i))))
    (assert-equal '((1))
                  (nset-difference (copy-list l1)
                                   (copy-list l2)
                                   :key #'car
                                   :test 'eql))))

(define-test |nset-difference test=eql key=other 3a|
  (let ((l1 (loop for i from 1 below 10 collect (list i)))
        (l2 (loop for i from 2 below 200 collect (list i))))
    (assert-equal '((1))
                  (nset-difference (copy-list l1)
                                   (copy-list l2)
                                   :key #'car))))

(define-test |nset-difference test=eql key=other 3b|
  (let ((l1 (loop for i from 1 below 10 collect (list i)))
        (l2 (loop for i from 2 below 200 collect (list i))))
    (assert-equal '((1))
                  (nset-difference (copy-list l1)
                                   (copy-list l2)
                                   :key #'car
                                   :test #'eql))))

(define-test |nset-difference test=eql key=other 3c|
  (let ((l1 (loop for i from 1 below 10 collect (list i)))
        (l2 (loop for i from 2 below 200 collect (list i))))
    (assert-equal '((1))
                  (nset-difference (copy-list l1)
                                   (copy-list l2)
                                   :key #'car
                                   :test 'eql))))

(define-test |nset-difference test=eq key=other 1|
  (assert-equal '()
                (nset-difference '()
                                 '()
                                 :key #'car
                                 :test #'eq)))

(define-test |nset-difference test=eq key=other 2a|
  (let ((l1 (loop for i from 1 below 10 collect (list (list i))))
        (l2 (loop for i from 2 below 20 collect (list (list i)))))
    (assert-equal 1
                  (length (nset-difference (copy-list l1)
                                           (copy-list (append (mapcar #'copy-list (cdr l1))
                                                              l2))
                                           :key #'car
                                           :test #'eq)))
    (assert-eq (car l1)
               (car (nset-difference (copy-list l1)
                                     (copy-list (append (mapcar #'copy-list (cdr l1))
                                                        l2))
                                     :key #'car
                                     :test #'eq)))))

(define-test |nset-difference test=eq key=other 2b|
  (let ((l1 (loop for i from 1 below 10 collect (list i)))
        (l2 (loop for i from 2 below 20 collect (list i))))
    (assert-equal 1
                  (length (nset-difference (copy-list l1)
                                           (copy-list (append (mapcar #'copy-list (cdr l1))
                                                              l2))
                                           :key #'car
                                           :test 'eq)))
    (assert-eq (car l1)
               (car (nset-difference (copy-list l1)
                                     (copy-list (append (mapcar #'copy-list (cdr l1))
                                                        l2))
                                     :key #'car
                                     :test 'eq)))))

(define-test |nset-difference test=eq key=other 3|
  (let ((l1 (loop for i from 1 below 10 collect (list i)))
        (l2 (loop for i from 2 below 200 collect (list i))))
    (assert-equal 1
                  (length (nset-difference (copy-list l1)
                                           (copy-list (append (mapcar #'copy-list (cdr l1))
                                                              l2))
                                           :key #'car
                                           :test #'eq)))
    (assert-eq (car l1)
               (car (nset-difference (copy-list l1)
                                     (copy-list (append (mapcar #'copy-list (cdr l1))
                                                        l2))
                                     :key #'car
                                     :test #'eq)))))

(define-test |nset-difference test=equal key=other 1|
  (assert-equal '()
                (nset-difference '()
                                 '()
                                 :key #'car
                                 :test #'equal)))

(define-test |nset-difference test=equal key=other 2a|
  (let ((l1 (loop for i from 1 below 10 collect (list (list i))))
        (l2 (loop for i from 2 below 20 collect (list (list i)))))
    (assert-equal '(((1)))
                  (nset-difference (copy-list l1)
                                   (copy-list (append (cdr l1) l2))
                                   :key #'car
                                   :test #'equal))))

(define-test |nset-difference test=equal key=other 2b|
  (let ((l1 (loop for i from 1 below 10 collect (list (list i))))
        (l2 (loop for i from 2 below 20 collect (list (list i)))))
    (assert-equal '(((1)))
                  (nset-difference (copy-list l1)
                                   (copy-list (append (cdr l1) l2))
                                   :key #'car
                                   :test 'equal))))

(define-test |nset-difference test=equal key=other 3|
  (let ((l1 (loop for i from 1 below 10 collect (list (list i))))
        (l2 (loop for i from 2 below 200 collect (list (list i)))))
    (assert-equal '(((1)))
                  (nset-difference (copy-list l1)
                                   (copy-list (append (cdr l1) l2))
                                   :key #'car
                                   :test #'equal))))

(define-test |nset-difference test=equalp key=other 1|
  (assert-equal '()
                (nset-difference '()
                                 '()
                                 :key #'car
                                 :test #'equalp)))

(define-test |nset-difference test=equalp key=other 2a|
  (let ((l1 (loop for i from 1 below 10 collect (list (make-array 1 :initial-element i))))
        (l2 (loop for i from 2 below 20 collect (list (make-array 1 :initial-element i)))))
    (assert-equalp '((#(1)))
                   (nset-difference (copy-list l1)
                                    (copy-list (append (cdr l1) l2))
                                    :key #'car
                                    :test #'equalp))))

(define-test |nset-difference test=equalp key=other 2b|
  (let ((l1 (loop for i from 1 below 10 collect (list (make-array 1 :initial-element i))))
        (l2 (loop for i from 2 below 20 collect (list (make-array 1 :initial-element i)))))
    (assert-equalp '((#(1)))
                   (nset-difference (copy-list l1)
                                    (copy-list (append (cdr l1) l2))
                                    :key #'car
                                    :test 'equalp))))

(define-test |nset-difference test=equalp key=other 3a|
  (let ((l1 (loop for i from 1 below 10 collect (list (make-array 1 :initial-element i))))
        (l2 (loop for i from 2 below 200 collect (list (make-array 1 :initial-element i)))))
    (assert-equalp '((#(1)))
                   (nset-difference (copy-list l1)
                                    (copy-list (append (cdr l1) l2))
                                    :key #'car
                                    :test #'equalp))))

(define-test |nset-difference test=equalp key=other 3b|
  (let ((l1 (loop for i from 1 below 10 collect (list (make-array 1 :initial-element i))))
        (l2 (loop for i from 2 below 200 collect (list (make-array 1 :initial-element i)))))
    (assert-equalp '((#(1)))
                   (nset-difference (copy-list l1)
                                    (copy-list (append (cdr l1) l2))
                                    :key #'car
                                    :test 'equalp))))

(define-test |nset-difference test=other key=other 1|
  (let ((l1 (loop for i from 1 below 10 collect (list i)))
        (l2 (loop for i from 1 below 20 collect (list i))))
    (assert-equal '((1))
                  (nset-difference (copy-list l1)
                                   (copy-list l2)
                                   :key #'car
                                   :test #'>))))

(define-test |nset-difference test-not=other key=other 1|
  (let ((l1 (loop for i from 1 below 10 collect (list i)))
        (l2 (loop for i from 1 below 20 collect (list i))))
    (assert-equal '((1))
                  (nset-difference (copy-list l1)
                                   (copy-list l2)
                                   :key #'car
                                   :test-not #'<=))))

(define-test |nset-difference test=other test-not=other 1|
  (assert-error 'error
                (nset-difference '(1 2 3) '(2 3 4)
                                 :test #'eql
                                 :test-not #'eql)))

(define-test |nset-difference test=nil key=identity 1|
  (let ((l1 (loop for i from 1 below 10 collect i))
        (l2 (loop for i from 2 below 20 collect i)))
    (assert-error 'error
                  (nset-difference (copy-list l1)
                                   (copy-list l2)
                                   :test nil))))

(define-test |nset-difference test-not=nil key=identity 1|
  (let ((l1 (loop for i from 1 below 10 collect i))
        (l2 (loop for i from 2 below 20 collect i)))
    (assert-error 'error
                  (nset-difference (copy-list l1)
                                   (copy-list l2)
                                   :test-not nil))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Tests for the adjoin function

(define-test |adjoin test=eql key=identity 1a|
  (assert-equal '(1)
                (adjoin 1 '())))

(define-test |adjoin test=eql key=identity 1b|
  (assert-equal '(1)
                (adjoin 1 '() :test #'eql)))

(define-test |adjoin test=eql key=identity 1c|
  (assert-equal '(1)
                (adjoin 1 '() :test 'eql)))

(define-test |adjoin test=eql key=identity 2a|
  (assert-equal '(1 2)
                (adjoin 1 '(2))))

(define-test |adjoin test=eql key=identity 2b|
  (assert-equal '(1 2)
                (adjoin 1 '(2) :test #'eql)))

(define-test |adjoin test=eql key=identity 2c|
  (assert-equal '(1 2)
                (adjoin 1 '(2) :test 'eql)))

(define-test |adjoin test=eql key=identity 3a|
  (assert-equal '(2 1)
                (adjoin 1 '(2 1))))

(define-test |adjoin test=eql key=identity 3b|
  (assert-equal '(2 1)
                (adjoin 1 '(2 1) :test #'eql)))

(define-test |adjoin test=eql key=identity 3c|
  (assert-equal '(2 1)
                (adjoin 1 '(2 1) :test 'eql)))

(define-test |adjoin test=eql key=nil 3b|
  (assert-equal '(2 1)
                (adjoin 1 '(2 1) :key nil)))

(define-test |adjoin test=eq key=identity 1a|
  (assert-equal '(a)
                (adjoin 'a '() :test #'eq)))

(define-test |adjoin test=eq key=identity 1b|
  (assert-equal '(a)
                (adjoin 'a '() :test 'eq)))

(define-test |adjoin test=eq key=identity 2a|
  (assert-equal '(a b)
                (adjoin 'a '(b) :test #'eq)))

(define-test |adjoin test=eq key=identity 2b|
  (assert-equal '(a b)
                (adjoin 'a '(b) :test 'eq)))

(define-test |adjoin test=eq key=identity 3a|
  (assert-equal '(b a)
                (adjoin 'a '(b a) :test #'eq)))

(define-test |adjoin test=eq key=identity 3b|
  (assert-equal '(b a)
                (adjoin 'a '(b a) :test 'eq)))

(define-test |adjoin test=other key=identity 1|
  (assert-equal '(1 2)
                (adjoin 1 '(2) :test #'=)))

(define-test |adjoin test=other key=identity 2|
  (assert-equal '(2 1)
                (adjoin 1 '(2 1) :test #'=)))

(define-test |adjoin test-not=other key=identity 1|
  (assert-equal '(1 2)
                (adjoin 1 '(2) :test-not #'/=)))

(define-test |adjoin test-not=other key=identity 2|
  (assert-equal '(2 1)
                (adjoin 1 '(2 1) :test-not #'/=)))

(define-test |adjoin test=eql key=other 1a|
  (assert-equal '((1))
                (adjoin '(1) '() :key #'car)))

(define-test |adjoin test=eql key=other 1b|
  (assert-equal '((1))
                (adjoin '(1) '() :key #'car :test #'eql)))

(define-test |adjoin test=eql key=other 1c|
  (assert-equal '((1))
                (adjoin '(1) '() :key #'car :test 'eql)))

(define-test |adjoin test=eql key=other 2a|
  (assert-equal '((1) (2))
                (adjoin '(1) '((2)) :key #'car)))

(define-test |adjoin test=eql key=other 2b|
  (assert-equal '((1) (2))
                (adjoin '(1) '((2)) :key #'car :test #'eql)))

(define-test |adjoin test=eql key=other 2c|
  (assert-equal '((1) (2))
                (adjoin '(1) '((2)) :key #'car :test 'eql)))

(define-test |adjoin test=eql key=other 3a|
  (assert-equal '((2) (1))
                (adjoin '(1) '((2) (1)) :key #'car)))

(define-test |adjoin test=eql key=other 3b|
  (assert-equal '((2) (1))
                (adjoin '(1) '((2) (1)) :key #'car :test #'eql)))

(define-test |adjoin test=eql key=other 3c|
  (assert-equal '((2) (1))
                (adjoin '(1) '((2) (1)) :key #'car :test 'eql)))

(define-test |adjoin test=eq key=other 1a|
  (assert-equal '((a))
                (adjoin '(a) '() :key #'car :test #'eq)))

(define-test |adjoin test=eq key=other 1b|
  (assert-equal '((a))
                (adjoin '(a) '() :key #'car :test 'eq)))

(define-test |adjoin test=eq key=other 2a|
  (assert-equal '((a) (b))
                (adjoin '(a) '((b)) :key #'car :test #'eq)))

(define-test |adjoin test=eq key=other 2b|
  (assert-equal '((a) (b))
                (adjoin '(a) '((b)) :key #'car :test 'eq)))

(define-test |adjoin test=eq key=other 3a|
  (assert-equal '((b) (a))
                (adjoin '(a) '((b) (a)) :key #'car :test #'eq)))

(define-test |adjoin test=eq key=other 3b|
  (assert-equal '((b) (a))
                (adjoin '(a) '((b) (a)) :key #'car :test 'eq)))

(define-test |adjoin test=other key=other 1|
  (assert-equal '((1) (2))
                (adjoin '(1) '((2)) :key #'car :test #'=)))

(define-test |adjoin test=other key=other 2|
  (assert-equal '((2) (1))
                (adjoin '(1) '((2) (1)) :key #'car :test #'=)))

(define-test |adjoin test-not=other key=other 1|
  (assert-equal '((1) (2))
                (adjoin '(1) '((2)) :key #'car :test-not #'/=)))

(define-test |adjoin test-not=other key=other 2|
  (assert-equal '((2) (1))
                (adjoin '(1) '((2) (1)) :key #'car :test-not #'/=)))

(define-test |adjoin test=other test-not=other 1|
  (assert-error 'error (adjoin 1 '() :test #'eql :test-not #'eql)))

(define-test |adjoin test=nil key=identity 3b|
  (assert-error 'error
                (adjoin 1 '(2 1) :test nil)))

(define-test |adjoin test-not=nil key=identity 3b|
  (assert-error 'error
                (adjoin 1 '(2 1) :test-not nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Tests for the set-exclusiv-or function

(define-test |set-exclusive-or test=eql key=identity 1|
  (assert-equal '()
                (set-exclusive-or '() '())))

(define-test |set-exclusive-or test=eql key=identity 2a|
  (let ((l (loop for i from 2 to 10 collect i)))
    (assert-equal
     '(0 1)
     (sort (copy-list (set-exclusive-or (cons 0 l)
                                        (append l (list 1))))
           #'<))))

(define-test |set-exclusive-or test=eql key=identity 2b|
  (let ((l (loop for i from 2 to 10 collect i)))
    (assert-equal
     '(0 1)
     (sort (copy-list (set-exclusive-or (cons 0 l)
                                        (append l (list 1))
                                        :test #'eql))
           #'<))))

(define-test |set-exclusive-or test=eql key=identity 2c|
  (let ((l (loop for i from 2 to 10 collect i)))
    (assert-equal
     '(0 1)
     (sort (copy-list (set-exclusive-or (cons 0 l)
                                        (append l (list 1))
                                        :test 'eql))
           #'<))))

(define-test |set-exclusive-or test=eql key=identity 3a|
  (let ((l (loop for i from 2 to 50 collect i)))
    (assert-equal
     '(0 1)
     (sort (copy-list (set-exclusive-or (cons 0 l)
                                        (append l (list 1))))
           #'<))))

(define-test |set-exclusive-or test=eql key=identity 3b|
  (let ((l (loop for i from 2 to 50 collect i)))
    (assert-equal
     '(0 1)
     (sort (copy-list (set-exclusive-or (cons 0 l)
                                        (append l (list 1))
                                        :test #'eql))
           #'<))))

(define-test |set-exclusive-or test=eql key=identity 3c|
  (let ((l (loop for i from 2 to 50 collect i)))
    (assert-equal
     '(0 1)
     (sort (copy-list (set-exclusive-or (cons 0 l)
                                        (append l (list 1))
                                        :test 'eql))
           #'<))))

(define-test |set-exclusive-or test=eql key=nil 1|
  (let ((l (loop for i from 2 to 50 collect i)))
    (assert-equal
     '(0 1)
     (sort (copy-list (set-exclusive-or (cons 0 l)
                                        (append l (list 1))
                                        :key nil))
           #'<))))

(define-test |set-exclusive-or test=eq key=identity 2a|
  (let ((l (loop for i from 2 to 10 collect (list i))))
    (assert-equal
     '((0) (1))
     (sort (copy-list (set-exclusive-or (cons '(0) l)
                                        (append l (list (list 1)))
                                        :test #'eq))
           #'<
           :key #'car))))

(define-test |set-exclusive-or test=eq key=identity 2b|
  (let ((l (loop for i from 2 to 10 collect (list i))))
    (assert-equal
     '((0) (1))
     (sort (copy-list (set-exclusive-or (cons '(0) l)
                                        (append l (list (list 1)))
                                        :test 'eq))
           #'<
           :key #'car))))

(define-test |set-exclusive-or test=eq key=identity 3a|
  (let ((l (loop for i from 2 to 50 collect (list i))))
    (assert-equal
     '((0) (1))
     (sort (copy-list (set-exclusive-or (cons '(0) l)
                                        (append l (list (list 1)))
                                        :test #'eq))
           #'<
           :key #'car))))

(define-test |set-exclusive-or test=eq key=identity 3b|
  (let ((l (loop for i from 2 to 50 collect (list i))))
    (assert-equal
     '((0) (1))
     (sort (copy-list (set-exclusive-or (cons '(0) l)
                                        (append l (list (list 1)))
                                        :test 'eq))
           #'<
           :key #'car))))

(define-test |set-exclusive-or test=equal key=identity 2a|
  (let ((l1 (loop for i from 2 to 10 collect (list i)))
        (l2 (loop for i from 2 to 10 collect (list i))))
    (assert-equal
     '((0) (1))
     (sort (copy-list (set-exclusive-or (cons '(0) l1)
                                        (append l2 (list (list 1)))
                                        :test #'equal))
           #'<
           :key #'car))))

(define-test |set-exclusive-or test=equal key=identity 2b|
  (let ((l1 (loop for i from 2 to 10 collect (list i)))
        (l2 (loop for i from 2 to 10 collect (list i))))
    (assert-equal
     '((0) (1))
     (sort (copy-list (set-exclusive-or (cons '(0) l1)
                                        (append l2 (list (list 1)))
                                        :test 'equal))
           #'<
           :key #'car))))

(define-test |set-exclusive-or test=equal key=identity 3a|
  (let ((l1 (loop for i from 2 to 50 collect (list i)))
        (l2 (loop for i from 2 to 50 collect (list i))))
    (assert-equal
     '((0) (1))
     (sort (copy-list (set-exclusive-or (cons '(0) l1)
                                        (append l2 (list (list 1)))
                                        :test #'equal))
           #'<
           :key #'car))))

(define-test |set-exclusive-or test=equal key=identity 3b|
  (let ((l1 (loop for i from 2 to 50 collect (list i)))
        (l2 (loop for i from 2 to 50 collect (list i))))
    (assert-equal
     '((0) (1))
     (sort (copy-list (set-exclusive-or (cons '(0) l1)
                                        (append l2 (list (list 1)))
                                        :test 'equal))
           #'<
           :key #'car))))

(define-test |set-exclusive-or test=equalp key=identity 2a|
  (let ((l1 (loop for i from 2 to 10 collect (make-array 1 :initial-element 1)))
        (l2 (loop for i from 2 to 10 collect (make-array 1 :initial-element 1))))
    (assert-equal
     '((0) (1))
     (sort (copy-list (set-exclusive-or (cons '(0) l1)
                                        (append l2 (list (list 1)))
                                        :test #'equalp))
           #'<
           :key #'car))))

(define-test |set-exclusive-or test=equalp key=identity 2b|
  (let ((l1 (loop for i from 2 to 10 collect (make-array 1 :initial-element 1)))
        (l2 (loop for i from 2 to 10 collect (make-array 1 :initial-element 1))))
    (assert-equal
     '((0) (1))
     (sort (copy-list (set-exclusive-or (cons '(0) l1)
                                        (append l2 (list (list 1)))
                                        :test 'equalp))
           #'<
           :key #'car))))

(define-test |set-exclusive-or test=equalp key=identity 3a|
  (let ((l1 (loop for i from 2 to 50 collect (make-array 1 :initial-element 1)))
        (l2 (loop for i from 2 to 50 collect (make-array 1 :initial-element 1))))
    (assert-equal
     '((0) (1))
     (sort (copy-list (set-exclusive-or (cons '(0) l1)
                                        (append l2 (list (list 1)))
                                        :test #'equalp))
           #'<
           :key #'car))))

(define-test |set-exclusive-or test=equalp key=identity 3b|
  (let ((l1 (loop for i from 2 to 50 collect (make-array 1 :initial-element 1)))
        (l2 (loop for i from 2 to 50 collect (make-array 1 :initial-element 1))))
    (assert-equal
     '((0) (1))
     (sort (copy-list (set-exclusive-or (cons '(0) l1)
                                        (append l2 (list (list 1)))
                                        :test 'equalp))
           #'<
           :key #'car))))

(define-test |set-exclusive-or test=other key=identity 1|
  (assert-equal '()
                (set-exclusive-or '() '() :test #'=)))

(define-test |set-exclusive-or test=other key=identity 2|
  (let ((l (loop for i from 2 to 10 collect i)))
    (assert-equal
     '(0 1)
     (sort (copy-list (set-exclusive-or (cons 0 l)
                                        (append l (list 1))
                                        :test '=))
           #'<))))

(define-test |set-exclusive-or test=other key=identity 3|
  (let ((l (loop for i from 2 to 50 collect i)))
    (assert-equal
     '(0 1)
     (sort (copy-list (set-exclusive-or (cons 0 l)
                                        (append l (list 1))
                                        :test '=))
           #'<))))

(define-test |set-exclusive-or test-not=other key=identity 2|
  (let ((l (loop for i from 2 to 10 collect i)))
    (assert-equal
     '(0 1)
     (sort (copy-list (set-exclusive-or (cons 0 l)
                                        (append l (list 1))
                                        :test-not '/=))
           #'<))))

(define-test |set-exclusive-or test-not=other key=identity 3|
  (let ((l (loop for i from 2 to 50 collect i)))
    (assert-equal
     '(0 1)
     (sort (copy-list (set-exclusive-or (cons 0 l)
                                        (append l (list 1))
                                        :test-not '/=))
           #'<))))

(define-test |set-exclusive-or test=eql key=other 1|
  (assert-equal '()
                (set-exclusive-or '() '() :key #'car)))

(define-test |set-exclusive-or test=eql key=other 2a|
  (let ((l (loop for i from 2 to 10 collect (list i))))
    (assert-equal
     '((0) (1))
     (sort (copy-list (set-exclusive-or (cons '(0) l)
                                        (append l (list (list 1)))
                                        :key #'car))
           #'<
           :key #'car))))

(define-test |set-exclusive-or test=eql key=other 2b|
  (let ((l (loop for i from 2 to 10 collect (list i))))
    (assert-equal
     '((0) (1))
     (sort (copy-list (set-exclusive-or (cons '(0) l)
                                        (append l (list (list 1)))
                                        :key #'car
                                        :test #'eql))
           #'<
           :key #'car))))

(define-test |set-exclusive-or test=eql key=other 2c|
  (let ((l (loop for i from 2 to 10 collect (list i))))
    (assert-equal
     '((0) (1))
     (sort (copy-list (set-exclusive-or (cons '(0) l)
                                        (append l (list (list 1)))
                                        :key #'car
                                        :test 'eql))
           #'<
           :key #'car))))

(define-test |set-exclusive-or test=eql key=other 3a|
  (let ((l (loop for i from 2 to 50 collect (list i))))
    (assert-equal
     '((0) (1))
     (sort (copy-list (set-exclusive-or (cons '(0) l)
                                        (append l (list (list 1)))
                                        :key #'car))
           #'<
           :key #'car))))

(define-test |set-exclusive-or test=eql key=other 3b|
  (let ((l (loop for i from 2 to 50 collect (list i))))
    (assert-equal
     '((0) (1))
     (sort (copy-list (set-exclusive-or (cons '(0) l)
                                        (append l (list (list 1)))
                                        :key #'car
                                        :test #'eql))
           #'<
           :key #'car))))

(define-test |set-exclusive-or test=eql key=other 3c|
  (let ((l (loop for i from 2 to 50 collect (list i))))
    (assert-equal
     '((0) (1))
     (sort (copy-list (set-exclusive-or (cons '(0) l)
                                        (append l (list (list 1)))
                                        :key #'car
                                        :test 'eql))
           #'<
           :key #'car))))

(define-test |set-exclusive-or test=eq key=other 2a|
  (let ((l (loop for i from 2 to 10 collect (list (list i)))))
    (assert-equal
     '(((0)) ((1)))
     (sort (copy-list (set-exclusive-or (cons '((0)) l)
                                        (append l (list (list (list 1))))
                                        :key #'car
                                        :test #'eq))
           #'<
           :key #'caar))))

(define-test |set-exclusive-or test=eq key=other 2b|
  (let ((l (loop for i from 2 to 10 collect (list (list i)))))
    (assert-equal
     '(((0)) ((1)))
     (sort (copy-list (set-exclusive-or (cons '((0)) l)
                                        (append l (list (list (list 1))))
                                        :key #'car
                                        :test 'eq))
           #'<
           :key #'caar))))

(define-test |set-exclusive-or test=eq key=other 3a|
  (let ((l (loop for i from 2 to 50 collect (list (list i)))))
    (assert-equal
     '(((0)) ((1)))
     (sort (copy-list (set-exclusive-or (cons '((0)) l)
                                        (append l (list (list (list 1))))
                                        :key #'car
                                        :test #'eq))
           #'<
           :key #'caar))))

(define-test |set-exclusive-or test=eq key=other 3b|
  (let ((l (loop for i from 2 to 50 collect (list (list i)))))
    (assert-equal
     '(((0)) ((1)))
     (sort (copy-list (set-exclusive-or (cons '((0)) l)
                                        (append l (list (list (list 1))))
                                        :key #'car
                                        :test 'eq))
           #'<
           :key #'caar))))

(define-test |set-exclusive-or test=equal key=other 2a|
  (let ((l1 (loop for i from 2 to 10 collect (list (list i))))
        (l2 (loop for i from 2 to 10 collect (list (list i)))))
    (assert-equal
     '(((0)) ((1)))
     (sort (copy-list (set-exclusive-or (cons '((0)) l1)
                                        (append l2 (list (list (list 1))))
                                        :key #'car
                                        :test #'equal))
           #'<
           :key #'caar))))

(define-test |set-exclusive-or test=equal key=other 2b|
  (let ((l1 (loop for i from 2 to 10 collect (list (list i))))
        (l2 (loop for i from 2 to 10 collect (list (list i)))))
    (assert-equal
     '(((0)) ((1)))
     (sort (copy-list (set-exclusive-or (cons '((0)) l1)
                                        (append l2 (list (list (list 1))))
                                        :key #'car
                                        :test 'equal))
           #'<
           :key #'caar))))

(define-test |set-exclusive-or test=equal key=other 3a|
  (let ((l1 (loop for i from 2 to 50 collect (list (list i))))
        (l2 (loop for i from 2 to 50 collect (list (list i)))))
    (assert-equal
     '(((0)) ((1)))
     (sort (copy-list (set-exclusive-or (cons '((0)) l1)
                                        (append l2 (list (list (list 1))))
                                        :key #'car
                                        :test #'equal))
           #'<
           :key #'caar))))

(define-test |set-exclusive-or test=equal key=other 3b|
  (let ((l1 (loop for i from 2 to 50 collect (list (list i))))
        (l2 (loop for i from 2 to 50 collect (list (list i)))))
    (assert-equal
     '(((0)) ((1)))
     (sort (copy-list (set-exclusive-or (cons '((0)) l1)
                                        (append l2 (list (list (list 1))))
                                        :key #'car
                                        :test 'equal))
           #'<
           :key #'caar))))

(define-test |set-exclusive-or test=equalp key=other 2a|
  (let ((l1 (loop for i from 2 to 10 collect (list (make-array 1 :initial-element 1))))
        (l2 (loop for i from 2 to 10 collect (list (make-array 1 :initial-element 1)))))
    (assert-equal
     '((0) (1))
     (sort (copy-list (set-exclusive-or (cons '(0) l1)
                                        (append l2 (list (list 1)))
                                        :key #'car
                                        :test #'equalp))
           #'<
           :key #'car))))

(define-test |set-exclusive-or test=equalp key=other 2b|
  (let ((l1 (loop for i from 2 to 10 collect (list (make-array 1 :initial-element 1))))
        (l2 (loop for i from 2 to 10 collect (list (make-array 1 :initial-element 1)))))
    (assert-equal
     '((0) (1))
     (sort (copy-list (set-exclusive-or (cons '(0) l1)
                                        (append l2 (list (list 1)))
                                        :key #'car
                                        :test 'equalp))
           #'<
           :key #'car))))

(define-test |set-exclusive-or test=equalp key=other 3a|
  (let ((l1 (loop for i from 2 to 50 collect (list (make-array 1 :initial-element 1))))
        (l2 (loop for i from 2 to 50 collect (list (make-array 1 :initial-element 1)))))
    (assert-equal
     '((0) (1))
     (sort (copy-list (set-exclusive-or (cons '(0) l1)
                                        (append l2 (list (list 1)))
                                        :key #'car
                                        :test #'equalp))
           #'<
           :key #'car))))

(define-test |set-exclusive-or test=equalp key=other 3b|
  (let ((l1 (loop for i from 2 to 50 collect (list (make-array 1 :initial-element 1))))
        (l2 (loop for i from 2 to 50 collect (list (make-array 1 :initial-element 1)))))
    (assert-equal
     '((0) (1))
     (sort (copy-list (set-exclusive-or (cons '(0) l1)
                                        (append l2 (list (list 1)))
                                        :key #'car
                                        :test 'equalp))
           #'<
           :key #'car))))

(define-test |set-exclusive-or test=other key=other 1|
  (assert-equal '()
                (set-exclusive-or '() '() :test #'= :key #'car)))

(define-test |set-exclusive-or test=other key=other 2|
  (let ((l (loop for i from 2 to 10 collect (list i))))
    (assert-equal
     '((0) (1))
     (sort (copy-list (set-exclusive-or (cons '(0) l)
                                        (append l (list (list 1)))
                                        :key #'car
                                        :test '=))
           #'<
           :key #'car))))

(define-test |set-exclusive-or test=other key=other 3|
  (let ((l (loop for i from 2 to 50 collect (list i))))
    (assert-equal
     '((0) (1))
     (sort (copy-list (set-exclusive-or (cons '(0) l)
                                        (append l (list (list 1)))
                                        :key #'car
                                        :test '=))
           #'<
           :key #'car))))

(define-test |set-exclusive-or test-not=other key=other 2|
  (let ((l (loop for i from 2 to 10 collect (list i))))
    (assert-equal
     '((0) (1))
     (sort (copy-list (set-exclusive-or (cons '(0) l)
                                        (append l (list (list 1)))
                                        :key #'car
                                        :test-not '/=))
           #'<
           :key #'car))))

(define-test |set-exclusive-or test-not=other key=other 3|
  (let ((l (loop for i from 2 to 50 collect (list i))))
    (assert-equal
     '((0) (1))
     (sort (copy-list (set-exclusive-or (cons '(0) l)
                                        (append l (list (list 1)))
                                        :key #'car
                                        :test-not '/=))
           #'<
           :key #'car))))

(define-test |set-exclusive-or test=other test-not=other 1|
  (assert-error 'error
                (set-exclusive-or '() '() :test #'eql :test-not #'eql)))

(define-test |set-exclusive-or test=nil key=identity 1|
  (let ((l (loop for i from 2 to 50 collect i)))
    (assert-error
     'error
     (sort (copy-list (set-exclusive-or (cons 0 l)
                                        (append l (list 1))
                                        :test nil))
           #'<))))

(define-test |set-exclusive-or test-not=nil key=identity 1|
  (let ((l (loop for i from 2 to 50 collect i)))
    (assert-error
     'error
     (sort (copy-list (set-exclusive-or (cons 0 l)
                                        (append l (list 1))
                                        :test-not nil))
           #'<))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Tests for the nset-exclusiv-or function

(define-test |nset-exclusive-or test=eql key=identity 1|
  (assert-equal '()
                (nset-exclusive-or '() '())))

(define-test |nset-exclusive-or test=eql key=identity 2a|
  (let ((l (loop for i from 2 to 10 collect i)))
    (assert-equal
     '(0 1)
     (sort (copy-list (nset-exclusive-or (copy-list (cons 0 l))
                                         (copy-list (append l (list 1)))))
           #'<))))

(define-test |nset-exclusive-or test=eql key=identity 2b|
  (let ((l (loop for i from 2 to 10 collect i)))
    (assert-equal
     '(0 1)
     (sort (copy-list (nset-exclusive-or (copy-list (cons 0 l))
                                         (copy-list (append l (list 1)))
                                         :test #'eql))
           #'<))))

(define-test |nset-exclusive-or test=eql key=identity 2c|
  (let ((l (loop for i from 2 to 10 collect i)))
    (assert-equal
     '(0 1)
     (sort (copy-list (nset-exclusive-or (copy-list (cons 0 l))
                                         (copy-list (append l (list 1)))
                                         :test 'eql))
           #'<))))

(define-test |nset-exclusive-or test=eql key=identity 3a|
  (let ((l (loop for i from 2 to 50 collect i)))
    (assert-equal
     '(0 1)
     (sort (copy-list (nset-exclusive-or (copy-list (cons 0 l))
                                         (copy-list (append l (list 1)))))
           #'<))))

(define-test |nset-exclusive-or test=eql key=identity 3b|
  (let ((l (loop for i from 2 to 50 collect i)))
    (assert-equal
     '(0 1)
     (sort (copy-list (nset-exclusive-or (copy-list (cons 0 l))
                                         (copy-list (append l (list 1)))
                                         :test #'eql))
           #'<))))

(define-test |nset-exclusive-or test=eql key=identity 3c|
  (let ((l (loop for i from 2 to 50 collect i)))
    (assert-equal
     '(0 1)
     (sort (copy-list (nset-exclusive-or (copy-list (cons 0 l))
                                         (copy-list (append l (list 1)))
                                         :test 'eql))
           #'<))))

(define-test |nset-exclusive-or test=eql key=nil 3b|
  (let ((l (loop for i from 2 to 50 collect i)))
    (assert-equal
     '(0 1)
     (sort (copy-list (nset-exclusive-or (copy-list (cons 0 l))
                                         (copy-list (append l (list 1)))
                                         :key nil))
           #'<))))

(define-test |nset-exclusive-or test=eq key=identity 2a|
  (let ((l (loop for i from 2 to 10 collect (list i))))
    (assert-equal
     '((0) (1))
     (sort (copy-list (nset-exclusive-or (copy-list (cons '(0) l))
                                         (copy-list (append l (list (list 1))))
                                         :test #'eq))
           #'<
           :key #'car))))

(define-test |nset-exclusive-or test=eq key=identity 2b|
  (let ((l (loop for i from 2 to 10 collect (list i))))
    (assert-equal
     '((0) (1))
     (sort (copy-list (nset-exclusive-or (copy-list (cons '(0) l))
                                         (copy-list (append l (list (list 1))))
                                         :test 'eq))
           #'<
           :key #'car))))

(define-test |nset-exclusive-or test=eq key=identity 3a|
  (let ((l (loop for i from 2 to 50 collect (list i))))
    (assert-equal
     '((0) (1))
     (sort (copy-list (nset-exclusive-or (copy-list (cons '(0) l))
                                         (copy-list (append l (list (list 1))))
                                         :test #'eq))
           #'<
           :key #'car))))

(define-test |nset-exclusive-or test=eq key=identity 3b|
  (let ((l (loop for i from 2 to 50 collect (list i))))
    (assert-equal
     '((0) (1))
     (sort (copy-list (nset-exclusive-or (copy-list (cons '(0) l))
                                         (copy-list (append l (list (list 1))))
                                         :test 'eq))
           #'<
           :key #'car))))

(define-test |nset-exclusive-or test=equal key=identity 2a|
  (let ((l1 (loop for i from 2 to 10 collect (list i)))
        (l2 (loop for i from 2 to 10 collect (list i))))
    (assert-equal
     '((0) (1))
     (sort (copy-list (nset-exclusive-or (copy-list (cons '(0) l1))
                                         (copy-list (append l2 (list (list 1))))
                                         :test #'equal))
           #'<
           :key #'car))))

(define-test |nset-exclusive-or test=equal key=identity 2b|
  (let ((l1 (loop for i from 2 to 10 collect (list i)))
        (l2 (loop for i from 2 to 10 collect (list i))))
    (assert-equal
     '((0) (1))
     (sort (copy-list (nset-exclusive-or (copy-list (cons '(0) l1))
                                         (copy-list (append l2 (list (list 1))))
                                         :test 'equal))
           #'<
           :key #'car))))

(define-test |nset-exclusive-or test=equal key=identity 3a|
  (let ((l1 (loop for i from 2 to 50 collect (list i)))
        (l2 (loop for i from 2 to 50 collect (list i))))
    (assert-equal
     '((0) (1))
     (sort (copy-list (nset-exclusive-or (copy-list (cons '(0) l1))
                                         (copy-list (append l2 (list (list 1))))
                                         :test #'equal))
           #'<
           :key #'car))))

(define-test |nset-exclusive-or test=equal key=identity 3b|
  (let ((l1 (loop for i from 2 to 50 collect (list i)))
        (l2 (loop for i from 2 to 50 collect (list i))))
    (assert-equal
     '((0) (1))
     (sort (copy-list (nset-exclusive-or (copy-list (cons '(0) l1))
                                         (copy-list (append l2 (list (list 1))))
                                         :test 'equal))
           #'<
           :key #'car))))

(define-test |nset-exclusive-or test=equalp key=identity 2a|
  (let ((l1 (loop for i from 2 to 10 collect (make-array 1 :initial-element 1)))
        (l2 (loop for i from 2 to 10 collect (make-array 1 :initial-element 1))))
    (assert-equal
     '((0) (1))
     (sort (copy-list (nset-exclusive-or (copy-list (cons '(0) l1))
                                         (copy-list (append l2 (list (list 1))))
                                         :test #'equalp))
           #'<
           :key #'car))))

(define-test |nset-exclusive-or test=equalp key=identity 2b|
  (let ((l1 (loop for i from 2 to 10 collect (make-array 1 :initial-element 1)))
        (l2 (loop for i from 2 to 10 collect (make-array 1 :initial-element 1))))
    (assert-equal
     '((0) (1))
     (sort (copy-list (nset-exclusive-or (copy-list (cons '(0) l1))
                                         (copy-list (append l2 (list (list 1))))
                                         :test 'equalp))
           #'<
           :key #'car))))

(define-test |nset-exclusive-or test=equalp key=identity 3a|
  (let ((l1 (loop for i from 2 to 50 collect (make-array 1 :initial-element 1)))
        (l2 (loop for i from 2 to 50 collect (make-array 1 :initial-element 1))))
    (assert-equal
     '((0) (1))
     (sort (copy-list (nset-exclusive-or (copy-list (cons '(0) l1))
                                         (copy-list (append l2 (list (list 1))))
                                         :test #'equalp))
           #'<
           :key #'car))))

(define-test |nset-exclusive-or test=equalp key=identity 3b|
  (let ((l1 (loop for i from 2 to 50 collect (make-array 1 :initial-element 1)))
        (l2 (loop for i from 2 to 50 collect (make-array 1 :initial-element 1))))
    (assert-equal
     '((0) (1))
     (sort (copy-list (nset-exclusive-or (copy-list (cons '(0) l1))
                                         (copy-list (append l2 (list (list 1))))
                                         :test 'equalp))
           #'<
           :key #'car))))

(define-test |nset-exclusive-or test=other key=identity 1|
  (assert-equal '()
                (nset-exclusive-or '() '() :test #'=)))

(define-test |nset-exclusive-or test=other key=identity 2|
  (let ((l (loop for i from 2 to 10 collect i)))
    (assert-equal
     '(0 1)
     (sort (copy-list (nset-exclusive-or (copy-list (cons 0 l))
                                         (copy-list (append l (list 1)))
                                         :test '=))
           #'<))))

(define-test |nset-exclusive-or test=other key=identity 3|
  (let ((l (loop for i from 2 to 50 collect i)))
    (assert-equal
     '(0 1)
     (sort (copy-list (nset-exclusive-or (copy-list (cons 0 l))
                                         (copy-list (append l (list 1)))
                                         :test '=))
           #'<))))

(define-test |nset-exclusive-or test-not=other key=identity 2|
  (let ((l (loop for i from 2 to 10 collect i)))
    (assert-equal
     '(0 1)
     (sort (copy-list (nset-exclusive-or (copy-list (cons 0 l))
                                         (copy-list (append l (list 1)))
                                         :test-not '/=))
           #'<))))

(define-test |nset-exclusive-or test-not=other key=identity 3|
  (let ((l (loop for i from 2 to 50 collect i)))
    (assert-equal
     '(0 1)
     (sort (copy-list (nset-exclusive-or (copy-list (cons 0 l))
                                         (copy-list (append l (list 1)))
                                         :test-not '/=))
           #'<))))

(define-test |nset-exclusive-or test=eql key=other 1|
  (assert-equal '()
                (nset-exclusive-or '() '() :key #'car)))

(define-test |nset-exclusive-or test=eql key=other 2a|
  (let ((l (loop for i from 2 to 10 collect (list i))))
    (assert-equal
     '((0) (1))
     (sort (copy-list (nset-exclusive-or (copy-list (cons '(0) l))
                                         (copy-list (append l (list (list 1))))
                                         :key #'car))
           #'<
           :key #'car))))

(define-test |nset-exclusive-or test=eql key=other 2b|
  (let ((l (loop for i from 2 to 10 collect (list i))))
    (assert-equal
     '((0) (1))
     (sort (copy-list (nset-exclusive-or (copy-list (cons '(0) l))
                                         (copy-list (append l (list (list 1))))
                                         :key #'car
                                         :test #'eql))
           #'<
           :key #'car))))

(define-test |nset-exclusive-or test=eql key=other 2c|
  (let ((l (loop for i from 2 to 10 collect (list i))))
    (assert-equal
     '((0) (1))
     (sort (copy-list (nset-exclusive-or (copy-list (cons '(0) l))
                                         (copy-list (append l (list (list 1))))
                                         :key #'car
                                         :test 'eql))
           #'<
           :key #'car))))

(define-test |nset-exclusive-or test=eql key=other 3a|
  (let ((l (loop for i from 2 to 50 collect (list i))))
    (assert-equal
     '((0) (1))
     (sort (copy-list (nset-exclusive-or (copy-list (cons '(0) l))
                                         (copy-list (append l (list (list 1))))
                                         :key #'car))
           #'<
           :key #'car))))

(define-test |nset-exclusive-or test=eql key=other 3b|
  (let ((l (loop for i from 2 to 50 collect (list i))))
    (assert-equal
     '((0) (1))
     (sort (copy-list (nset-exclusive-or (copy-list (cons '(0) l))
                                         (copy-list (append l (list (list 1))))
                                         :key #'car
                                         :test #'eql))
           #'<
           :key #'car))))

(define-test |nset-exclusive-or test=eql key=other 3c|
  (let ((l (loop for i from 2 to 50 collect (list i))))
    (assert-equal
     '((0) (1))
     (sort (copy-list (nset-exclusive-or (copy-list (cons '(0) l))
                                         (copy-list (append l (list (list 1))))
                                         :key #'car
                                         :test 'eql))
           #'<
           :key #'car))))

(define-test |nset-exclusive-or test=eq key=other 2a|
  (let ((l (loop for i from 2 to 10 collect (list (list i)))))
    (assert-equal
     '(((0)) ((1)))
     (sort (copy-list (nset-exclusive-or (copy-list (cons '((0)) l))
                                         (copy-list (append l (list (list (list 1)))))
                                         :key #'car
                                         :test #'eq))
           #'<
           :key #'caar))))

(define-test |nset-exclusive-or test=eq key=other 2b|
  (let ((l (loop for i from 2 to 10 collect (list (list i)))))
    (assert-equal
     '(((0)) ((1)))
     (sort (copy-list (nset-exclusive-or (copy-list (cons '((0)) l))
                                         (copy-list (append l (list (list (list 1)))))
                                         :key #'car
                                         :test 'eq))
           #'<
           :key #'caar))))

(define-test |nset-exclusive-or test=eq key=other 3a|
  (let ((l (loop for i from 2 to 50 collect (list (list i)))))
    (assert-equal
     '(((0)) ((1)))
     (sort (copy-list (nset-exclusive-or (copy-list (cons '((0)) l))
                                         (copy-list (append l (list (list (list 1)))))
                                         :key #'car
                                         :test #'eq))
           #'<
           :key #'caar))))

(define-test |nset-exclusive-or test=eq key=other 3b|
  (let ((l (loop for i from 2 to 50 collect (list (list i)))))
    (assert-equal
     '(((0)) ((1)))
     (sort (copy-list (nset-exclusive-or (copy-list (cons '((0)) l))
                                         (copy-list (append l (list (list (list 1)))))
                                         :key #'car
                                         :test 'eq))
           #'<
           :key #'caar))))

(define-test |nset-exclusive-or test=equal key=other 2a|
  (let ((l1 (loop for i from 2 to 10 collect (list (list i))))
        (l2 (loop for i from 2 to 10 collect (list (list i)))))
    (assert-equal
     '(((0)) ((1)))
     (sort (copy-list (nset-exclusive-or (copy-list (cons '((0)) l1))
                                         (copy-list (append l2 (list (list (list 1)))))
                                         :key #'car
                                         :test #'equal))
           #'<
           :key #'caar))))

(define-test |nset-exclusive-or test=equal key=other 2b|
  (let ((l1 (loop for i from 2 to 10 collect (list (list i))))
        (l2 (loop for i from 2 to 10 collect (list (list i)))))
    (assert-equal
     '(((0)) ((1)))
     (sort (copy-list (nset-exclusive-or (copy-list (cons '((0)) l1))
                                         (copy-list (append l2 (list (list (list 1)))))
                                         :key #'car
                                         :test 'equal))
           #'<
           :key #'caar))))

(define-test |nset-exclusive-or test=equal key=other 3a|
  (let ((l1 (loop for i from 2 to 50 collect (list (list i))))
        (l2 (loop for i from 2 to 50 collect (list (list i)))))
    (assert-equal
     '(((0)) ((1)))
     (sort (copy-list (nset-exclusive-or (copy-list (cons '((0)) l1))
                                         (copy-list (append l2 (list (list (list 1)))))
                                         :key #'car
                                         :test #'equal))
           #'<
           :key #'caar))))

(define-test |nset-exclusive-or test=equal key=other 3b|
  (let ((l1 (loop for i from 2 to 50 collect (list (list i))))
        (l2 (loop for i from 2 to 50 collect (list (list i)))))
    (assert-equal
     '(((0)) ((1)))
     (sort (copy-list (nset-exclusive-or (copy-list (cons '((0)) l1))
                                         (copy-list (append l2 (list (list (list 1)))))
                                         :key #'car
                                         :test 'equal))
           #'<
           :key #'caar))))

(define-test |nset-exclusive-or test=equalp key=other 2a|
  (let ((l1 (loop for i from 2 to 10 collect (list (make-array 1 :initial-element 1))))
        (l2 (loop for i from 2 to 10 collect (list (make-array 1 :initial-element 1)))))
    (assert-equal
     '((0) (1))
     (sort (copy-list (nset-exclusive-or (copy-list (cons '(0) l1))
                                         (copy-list (append l2 (list (list 1))))
                                         :key #'car
                                         :test #'equalp))
           #'<
           :key #'car))))

(define-test |nset-exclusive-or test=equalp key=other 2b|
  (let ((l1 (loop for i from 2 to 10 collect (list (make-array 1 :initial-element 1))))
        (l2 (loop for i from 2 to 10 collect (list (make-array 1 :initial-element 1)))))
    (assert-equal
     '((0) (1))
     (sort (copy-list (nset-exclusive-or (copy-list (cons '(0) l1))
                                         (copy-list (append l2 (list (list 1))))
                                         :key #'car
                                         :test 'equalp))
           #'<
           :key #'car))))

(define-test |nset-exclusive-or test=equalp key=other 3a|
  (let ((l1 (loop for i from 2 to 50 collect (list (make-array 1 :initial-element 1))))
        (l2 (loop for i from 2 to 50 collect (list (make-array 1 :initial-element 1)))))
    (assert-equal
     '((0) (1))
     (sort (copy-list (nset-exclusive-or (copy-list (cons '(0) l1))
                                         (copy-list (append l2 (list (list 1))))
                                         :key #'car
                                         :test #'equalp))
           #'<
           :key #'car))))

(define-test |nset-exclusive-or test=equalp key=other 3b|
  (let ((l1 (loop for i from 2 to 50 collect (list (make-array 1 :initial-element 1))))
        (l2 (loop for i from 2 to 50 collect (list (make-array 1 :initial-element 1)))))
    (assert-equal
     '((0) (1))
     (sort (copy-list (nset-exclusive-or (copy-list (cons '(0) l1))
                                         (copy-list (append l2 (list (list 1))))
                                         :key #'car
                                         :test 'equalp))
           #'<
           :key #'car))))

(define-test |nset-exclusive-or test=other key=other 1|
  (assert-equal '()
                (nset-exclusive-or '() '() :test #'= :key #'car)))

(define-test |nset-exclusive-or test=other key=other 2|
  (let ((l (loop for i from 2 to 10 collect (list i))))
    (assert-equal
     '((0) (1))
     (sort (copy-list (nset-exclusive-or (copy-list (cons '(0) l))
                                         (copy-list (append l (list (list 1))))
                                         :key #'car
                                         :test '=))
           #'<
           :key #'car))))

(define-test |nset-exclusive-or test=other key=other 3|
  (let ((l (loop for i from 2 to 50 collect (list i))))
    (assert-equal
     '((0) (1))
     (sort (copy-list (nset-exclusive-or (copy-list (cons '(0) l))
                                         (copy-list (append l (list (list 1))))
                                         :key #'car
                                         :test '=))
           #'<
           :key #'car))))

(define-test |nset-exclusive-or test-not=other key=other 2|
  (let ((l (loop for i from 2 to 10 collect (list i))))
    (assert-equal
     '((0) (1))
     (sort (copy-list (nset-exclusive-or (copy-list (cons '(0) l))
                                         (copy-list (append l (list (list 1))))
                                         :key #'car
                                         :test-not '/=))
           #'<
           :key #'car))))

(define-test |nset-exclusive-or test-not=other key=other 3|
  (let ((l (loop for i from 2 to 50 collect (list i))))
    (assert-equal
     '((0) (1))
     (sort (copy-list (nset-exclusive-or (copy-list (cons '(0) l))
                                         (copy-list (append l (list (list 1))))
                                         :key #'car
                                         :test-not '/=))
           #'<
           :key #'car))))

(define-test |nset-exclusive-or test=other test-not=other 1|
  (assert-error 'error
                (nset-exclusive-or '() '() :test #'eql :test-not #'eql)))

(define-test |nset-exclusive-or test=nil key=identity 3b|
  (let ((l (loop for i from 2 to 50 collect i)))
    (assert-error
     'error
     (sort (copy-list (nset-exclusive-or (copy-list (cons 0 l))
                                         (copy-list (append l (list 1)))
                                         :test nil))
           #'<))))

(define-test |nset-exclusive-or test-not=nil key=identity 3b|
  (let ((l (loop for i from 2 to 50 collect i)))
    (assert-error
     'error
     (sort (copy-list (nset-exclusive-or (copy-list (cons 0 l))
                                         (copy-list (append l (list 1)))
                                         :test-not nil))
           #'<))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Tests for the subsetp function

(define-test |subsetp test=eql key=identity 1a|
  (assert-true (subsetp '()
                        '())))

(define-test |subsetp test=eql key=identity 1b|
  (assert-true (subsetp '()
                        '()
                        :test #'eql)))

(define-test |subsetp test=eql key=identity 1c|
  (assert-true (subsetp '()
                        '()
                        :test 'eql)))

(define-test |subsetp test=eql key=identity 2a|
  (let ((l1 (loop for i from 1 to 10 collect i))
        (l2 (loop for i downfrom 10 to 0 collect i)))
    (assert-true (subsetp l1
                          l2))))

(define-test |subsetp test=eql key=identity 2b|
  (let ((l1 (loop for i from 1 to 10 collect i))
        (l2 (loop for i downfrom 10 to 0 collect i)))
    (assert-true (subsetp l1
                          l2
                          :test #'eql))))

(define-test |subsetp test=eql key=identity 2c|
  (let ((l1 (loop for i from 1 to 10 collect i))
        (l2 (loop for i downfrom 10 to 0 collect i)))
    (assert-true (subsetp l1
                          l2
                          :test 'eql))))

(define-test |subsetp test=eql key=identity 3a|
  (let ((l1 (loop for i from 1 to 50 collect i))
        (l2 (loop for i downfrom 50 to 0 collect i)))
    (assert-true (subsetp l1
                          l2))))

(define-test |subsetp test=eql key=identity 3b|
  (let ((l1 (loop for i from 1 to 50 collect i))
        (l2 (loop for i downfrom 50 to 0 collect i)))
    (assert-true (subsetp l1
                          l2
                          :test #'eql))))

(define-test |subsetp test=eql key=identity 3c|
  (let ((l1 (loop for i from 1 to 50 collect i))
        (l2 (loop for i downfrom 50 to 0 collect i)))
    (assert-true (subsetp l1
                          l2
                          :test 'eql))))

(define-test |subsetp test=eql key=identity 4a|
  (let ((l1 (loop for i from -1 to 10 collect i))
        (l2 (loop for i downfrom 10 to 0 collect i)))
    (assert-false (subsetp l1
                           l2))))

(define-test |subsetp test=eql key=identity 4b|
  (let ((l1 (loop for i from -1 to 10 collect i))
        (l2 (loop for i downfrom 10 to 0 collect i)))
    (assert-false (subsetp l1
                           l2
                           :test #'eql))))

(define-test |subsetp test=eql key=identity 4c|
  (let ((l1 (loop for i from -1 to 10 collect i))
        (l2 (loop for i downfrom 10 to 0 collect i)))
    (assert-false (subsetp l1
                           l2
                           :test 'eql))))

(define-test |subsetp test=eql key=identity 5a|
  (let ((l1 (loop for i from -1 to 50 collect i))
        (l2 (loop for i downfrom 50 to 0 collect i)))
    (assert-false (subsetp l1
                           l2))))

(define-test |subsetp test=eql key=identity 5b|
  (let ((l1 (loop for i from -1 to 50 collect i))
        (l2 (loop for i downfrom 50 to 0 collect i)))
    (assert-false (subsetp l1
                           l2
                           :test #'eql))))

(define-test |subsetp test=eql key=identity 5c|
  (let ((l1 (loop for i from -1 to 50 collect i))
        (l2 (loop for i downfrom 50 to 0 collect i)))
    (assert-false (subsetp l1
                           l2
                           :test 'eql))))

(define-test |subsetp test=eql key=nil 1|
  (let ((l1 (loop for i from 1 to 10 collect i))
        (l2 (loop for i downfrom 10 to 0 collect i)))
    (assert-true (subsetp l1
                          l2
                          :key nil))))

(define-test |subsetp test=eq key=identity 2a|
  (let ((l (loop repeat 10 collect (make-array 1 :initial-element 1))))
    (assert-true (subsetp (reverse (cdr (butlast l)))
                          l
                          :test #'eq))))

(define-test |subsetp test=eq key=identity 2b|
  (let ((l (loop repeat 10 collect (make-array 1 :initial-element 1))))
    (assert-true (subsetp (reverse (cdr (butlast l)))
                          l
                          :test 'eq))))

(define-test |subsetp test=eq key=identity 3a|
  (let ((l (loop repeat 50 collect (make-array 1 :initial-element 1))))
    (assert-true (subsetp (reverse (cdr (butlast l)))
                          l
                          :test #'eq))))

(define-test |subsetp test=eq key=identity 3b|
  (let ((l (loop repeat 50 collect (make-array 1 :initial-element 1))))
    (assert-true (subsetp (reverse (cdr (butlast l)))
                          l
                          :test 'eq))))

(define-test |subsetp test=eq key=identity 4a|
  (let ((l (loop repeat 10 collect (make-array 1 :initial-element 1))))
    (assert-false (subsetp l
                           (reverse (cdr (butlast l)))
                           :test #'eq))))

(define-test |subsetp test=eq key=identity 4b|
  (let ((l (loop repeat 10 collect (make-array 1 :initial-element 1))))
    (assert-false (subsetp l
                           (reverse (cdr (butlast l)))
                           :test 'eq))))

(define-test |subsetp test=eq key=identity 5a|
  (let ((l (loop repeat 50 collect (make-array 1 :initial-element 1))))
    (assert-false (subsetp l
                           (reverse (cdr (butlast l)))
                           :test #'eq))))

(define-test |subsetp test=eq key=identity 5b|
  (let ((l (loop repeat 50 collect (make-array 1 :initial-element 1))))
    (assert-false (subsetp l
                           (reverse (cdr (butlast l)))
                           :test 'eq))))

(define-test |subsetp test=equal key=identity 2a|
  (let ((l1 (loop for i from 1 to 10 collect (list i)))
        (l2 (loop for i downfrom 10 to 0 collect (list i))))
    (assert-true (subsetp l1
                          l2
                          :test #'equal))))

(define-test |subsetp test=equal key=identity 2b|
  (let ((l1 (loop for i from 1 to 10 collect (list i)))
        (l2 (loop for i downfrom 10 to 0 collect (list i))))
    (assert-true (subsetp l1
                          l2
                          :test 'equal))))

(define-test |subsetp test=equal key=identity 3a|
  (let ((l1 (loop for i from 1 to 50 collect (list i)))
        (l2 (loop for i downfrom 50 to 0 collect (list i))))
    (assert-true (subsetp l1
                          l2
                          :test #'equal))))

(define-test |subsetp test=equal key=identity 3b|
  (let ((l1 (loop for i from 1 to 50 collect (list i)))
        (l2 (loop for i downfrom 50 to 0 collect (list i))))
    (assert-true (subsetp l1
                          l2
                          :test 'equal))))

(define-test |subsetp test=equal key=identity 4a|
  (let ((l1 (loop for i from -1 to 10 collect (list i)))
        (l2 (loop for i downfrom 10 to 0 collect (list i))))
    (assert-false (subsetp l1
                           l2
                           :test #'equal))))

(define-test |subsetp test=equal key=identity 4b|
  (let ((l1 (loop for i from -1 to 10 collect (list i)))
        (l2 (loop for i downfrom 10 to 0 collect (list i))))
    (assert-false (subsetp l1
                           l2
                           :test 'equal))))

(define-test |subsetp test=equal key=identity 5a|
  (let ((l1 (loop for i from -1 to 50 collect (list i)))
        (l2 (loop for i downfrom 50 to 0 collect (list i))))
    (assert-false (subsetp l1
                           l2
                           :test #'equal))))

(define-test |subsetp test=equal key=identity 5b|
  (let ((l1 (loop for i from -1 to 50 collect (list i)))
        (l2 (loop for i downfrom 50 to 0 collect (list i))))
    (assert-false (subsetp l1
                           l2
                           :test 'equal))))

(define-test |subsetp test=equalp key=identity 2a|
  (let ((l1 (loop for i from 1 to 10 collect (make-array 1 :initial-element i)))
        (l2 (loop for i downfrom 10 to 0 collect (make-array 1 :initial-element i))))
    (assert-true (subsetp l1
                          l2
                          :test #'equalp))))

(define-test |subsetp test=equalp key=identity 2b|
  (let ((l1 (loop for i from 1 to 10 collect (make-array 1 :initial-element i)))
        (l2 (loop for i downfrom 10 to 0 collect (make-array 1 :initial-element i))))
    (assert-true (subsetp l1
                          l2
                          :test 'equalp))))

(define-test |subsetp test=equalp key=identity 3a|
  (let ((l1 (loop for i from 1 to 50 collect (make-array 1 :initial-element i)))
        (l2 (loop for i downfrom 50 to 0 collect (make-array 1 :initial-element i))))
    (assert-true (subsetp l1
                          l2
                          :test #'equalp))))

(define-test |subsetp test=equalp key=identity 3b|
  (let ((l1 (loop for i from 1 to 50 collect (make-array 1 :initial-element i)))
        (l2 (loop for i downfrom 50 to 0 collect (make-array 1 :initial-element i))))
    (assert-true (subsetp l1
                          l2
                          :test 'equalp))))

(define-test |subsetp test=equalp key=identity 4a|
  (let ((l1 (loop for i from -1 to 10 collect (make-array 1 :initial-element i)))
        (l2 (loop for i downfrom 10 to 0 collect (make-array 1 :initial-element i))))
    (assert-false (subsetp l1
                           l2
                           :test #'equalp))))

(define-test |subsetp test=equalp key=identity 4b|
  (let ((l1 (loop for i from -1 to 10 collect (make-array 1 :initial-element i)))
        (l2 (loop for i downfrom 10 to 0 collect (make-array 1 :initial-element i))))
    (assert-false (subsetp l1
                           l2
                           :test 'equalp))))

(define-test |subsetp test=equalp key=identity 5a|
  (let ((l1 (loop for i from -1 to 50 collect (make-array 1 :initial-element i)))
        (l2 (loop for i downfrom 50 to 0 collect (make-array 1 :initial-element i))))
    (assert-false (subsetp l1
                           l2
                           :test #'equalp))))

(define-test |subsetp test=equalp key=identity 5b|
  (let ((l1 (loop for i from -1 to 50 collect (make-array 1 :initial-element i)))
        (l2 (loop for i downfrom 50 to 0 collect (make-array 1 :initial-element i))))
    (assert-false (subsetp l1
                           l2
                           :test 'equalp))))

(define-test |subsetp test=other key=identity 2b|
  (let ((l1 (loop for i from 1 to 10 collect i))
        (l2 (loop for i downfrom 10 to 0 collect i)))
    (assert-true (subsetp l1
                          l2
                          :test #'=))))

(define-test |subsetp test=other key=identity 3b|
  (let ((l1 (loop for i from 1 to 50 collect i))
        (l2 (loop for i downfrom 50 to 0 collect i)))
    (assert-true (subsetp l1
                          l2
                          :test #'=))))

(define-test |subsetp test=other key=identity 4b|
  (let ((l1 (loop for i from -1 to 10 collect i))
        (l2 (loop for i downfrom 10 to 0 collect i)))
    (assert-false (subsetp l1
                           l2
                           :test #'=))))

(define-test |subsetp test=other key=identity 5b|
  (let ((l1 (loop for i from -1 to 50 collect i))
        (l2 (loop for i downfrom 50 to 0 collect i)))
    (assert-false (subsetp l1
                           l2
                           :test #'=))))

(define-test |subsetp test-not=other key=identity 2b|
  (let ((l1 (loop for i from 1 to 10 collect i))
        (l2 (loop for i downfrom 10 to 0 collect i)))
    (assert-true (subsetp l1
                          l2
                          :test-not #'/=))))

(define-test |subsetp test-not=other key=identity 3b|
  (let ((l1 (loop for i from 1 to 50 collect i))
        (l2 (loop for i downfrom 50 to 0 collect i)))
    (assert-true (subsetp l1
                          l2
                          :test-not #'/=))))

(define-test |subsetp test-not=other key=identity 4b|
  (let ((l1 (loop for i from -1 to 10 collect i))
        (l2 (loop for i downfrom 10 to 0 collect i)))
    (assert-false (subsetp l1
                           l2
                           :test-not #'/=))))

(define-test |subsetp test-not=other key=identity 5b|
  (let ((l1 (loop for i from -1 to 50 collect i))
        (l2 (loop for i downfrom 50 to 0 collect i)))
    (assert-false (subsetp l1
                           l2
                           :test-not #'/=))))

(define-test |subsetp test=eql key=other 1a|
  (assert-true (subsetp '()
                        '()
                        :key #'car)))

(define-test |subsetp test=eql key=other 1b|
  (assert-true (subsetp '()
                        '()
                        :key #'car
                        :test #'eql)))

(define-test |subsetp test=eql key=other 1c|
  (assert-true (subsetp '()
                        '()
                        :key #'car
                        :test 'eql)))

(define-test |subsetp test=eql key=other 2a|
  (let ((l1 (loop for i from 1 to 10 collect (list i)))
        (l2 (loop for i downfrom 10 to 0 collect (list i))))
    (assert-true (subsetp l1
                          l2
                          :key #'car))))

(define-test |subsetp test=eql key=other 2b|
  (let ((l1 (loop for i from 1 to 10 collect (list i)))
        (l2 (loop for i downfrom 10 to 0 collect (list i))))
    (assert-true (subsetp l1
                          l2
                          :key #'car
                          :test #'eql))))

(define-test |subsetp test=eql key=other 2c|
  (let ((l1 (loop for i from 1 to 10 collect (list i)))
        (l2 (loop for i downfrom 10 to 0 collect (list i))))
    (assert-true (subsetp l1
                          l2
                          :key #'car
                          :test 'eql))))

(define-test |subsetp test=eql key=other 3a|
  (let ((l1 (loop for i from 1 to 50 collect (list i)))
        (l2 (loop for i downfrom 50 to 0 collect (list i))))
    (assert-true (subsetp l1
                          l2
                          :key #'car))))

(define-test |subsetp test=eql key=other 3b|
  (let ((l1 (loop for i from 1 to 50 collect (list i)))
        (l2 (loop for i downfrom 50 to 0 collect (list i))))
    (assert-true (subsetp l1
                          l2
                          :key #'car
                          :test #'eql))))

(define-test |subsetp test=eql key=other 3c|
  (let ((l1 (loop for i from 1 to 50 collect (list i)))
        (l2 (loop for i downfrom 50 to 0 collect (list i))))
    (assert-true (subsetp l1
                          l2
                          :key #'car
                          :test 'eql))))

(define-test |subsetp test=eql key=other 4a|
  (let ((l1 (loop for i from -1 to 10 collect (list i)))
        (l2 (loop for i downfrom 10 to 0 collect (list i))))
    (assert-false (subsetp l1
                           l2
                           :key #'car))))

(define-test |subsetp test=eql key=other 4b|
  (let ((l1 (loop for i from -1 to 10 collect (list i)))
        (l2 (loop for i downfrom 10 to 0 collect (list i))))
    (assert-false (subsetp l1
                           l2
                           :key #'car
                           :test #'eql))))

(define-test |subsetp test=eql key=other 4c|
  (let ((l1 (loop for i from -1 to 10 collect (list i)))
        (l2 (loop for i downfrom 10 to 0 collect (list i))))
    (assert-false (subsetp l1
                           l2
                           :key #'car
                           :test 'eql))))

(define-test |subsetp test=eql key=other 5a|
  (let ((l1 (loop for i from -1 to 50 collect (list i)))
        (l2 (loop for i downfrom 50 to 0 collect (list i))))
    (assert-false (subsetp l1
                           l2
                           :key #'car))))

(define-test |subsetp test=eql key=other 5b|
  (let ((l1 (loop for i from -1 to 50 collect (list i)))
        (l2 (loop for i downfrom 50 to 0 collect (list i))))
    (assert-false (subsetp l1
                           l2
                           :key #'car
                           :test #'eql))))

(define-test |subsetp test=eql key=other 5c|
  (let ((l1 (loop for i from -1 to 50 collect (list i)))
        (l2 (loop for i downfrom 50 to 0 collect (list i))))
    (assert-false (subsetp l1
                           l2
                           :key #'car
                           :test 'eql))))

(define-test |subsetp test=eq key=other 2a|
  (let ((l (loop repeat 10 collect (list (make-array 1 :initial-element 1)))))
    (assert-true (subsetp (reverse (cdr (butlast l)))
                          l
                          :key #'car
                          :test #'eq))))

(define-test |subsetp test=eq key=other 2b|
  (let ((l (loop repeat 10 collect (list (make-array 1 :initial-element 1)))))
    (assert-true (subsetp (reverse (cdr (butlast l)))
                          l
                          :key #'car
                          :test 'eq))))

(define-test |subsetp test=eq key=other 3a|
  (let ((l (loop repeat 50 collect (list (make-array 1 :initial-element 1)))))
    (assert-true (subsetp (reverse (cdr (butlast l)))
                          l
                          :key #'car
                          :test #'eq))))

(define-test |subsetp test=eq key=other 3b|
  (let ((l (loop repeat 50 collect (list (make-array 1 :initial-element 1)))))
    (assert-true (subsetp (reverse (cdr (butlast l)))
                          l
                          :key #'car
                          :test 'eq))))

(define-test |subsetp test=eq key=other 4a|
  (let ((l (loop repeat 10 collect (list (make-array 1 :initial-element 1)))))
    (assert-false (subsetp l
                           (reverse (cdr (butlast l)))
                           :key #'car
                           :test #'eq))))

(define-test |subsetp test=eq key=other 4b|
  (let ((l (loop repeat 10 collect (list (make-array 1 :initial-element 1)))))
    (assert-false (subsetp l
                           (reverse (cdr (butlast l)))
                           :key #'car
                           :test 'eq))))

(define-test |subsetp test=eq key=other 5a|
  (let ((l (loop repeat 50 collect (list (make-array 1 :initial-element 1)))))
    (assert-false (subsetp l
                           (reverse (cdr (butlast l)))
                           :key #'car
                           :test #'eq))))

(define-test |subsetp test=eq key=other 5b|
  (let ((l (loop repeat 50 collect (list (make-array 1 :initial-element 1)))))
    (assert-false (subsetp l
                           (reverse (cdr (butlast l)))
                           :key #'car
                           :test 'eq))))

(define-test |subsetp test=equal key=other 2a|
  (let ((l1 (loop for i from 1 to 10 collect (list (list i))))
        (l2 (loop for i downfrom 10 to 0 collect (list (list i)))))
    (assert-true (subsetp l1
                          l2
                          :key #'car
                          :test #'equal))))

(define-test |subsetp test=equal key=other 2b|
  (let ((l1 (loop for i from 1 to 10 collect (list (list i))))
        (l2 (loop for i downfrom 10 to 0 collect (list (list i)))))
    (assert-true (subsetp l1
                          l2
                          :key #'car
                          :test 'equal))))

(define-test |subsetp test=equal key=other 3a|
  (let ((l1 (loop for i from 1 to 50 collect (list (list i))))
        (l2 (loop for i downfrom 50 to 0 collect (list (list i)))))
    (assert-true (subsetp l1
                          l2
                          :key #'car
                          :test #'equal))))

(define-test |subsetp test=equal key=other 3b|
  (let ((l1 (loop for i from 1 to 50 collect (list (list i))))
        (l2 (loop for i downfrom 50 to 0 collect (list (list i)))))
    (assert-true (subsetp l1
                          l2
                          :key #'car
                          :test 'equal))))

(define-test |subsetp test=equal key=other 4a|
  (let ((l1 (loop for i from -1 to 10 collect (list (list i))))
        (l2 (loop for i downfrom 10 to 0 collect (list (list i)))))
    (assert-false (subsetp l1
                           l2
                           :key #'car
                           :test #'equal))))

(define-test |subsetp test=equal key=other 4b|
  (let ((l1 (loop for i from -1 to 10 collect (list (list i))))
        (l2 (loop for i downfrom 10 to 0 collect (list (list i)))))
    (assert-false (subsetp l1
                           l2
                           :key #'car
                           :test 'equal))))

(define-test |subsetp test=equal key=other 5a|
  (let ((l1 (loop for i from -1 to 50 collect (list (list i))))
        (l2 (loop for i downfrom 50 to 0 collect (list (list i)))))
    (assert-false (subsetp l1
                           l2
                           :key #'car
                           :test #'equal))))

(define-test |subsetp test=equal key=other 5b|
  (let ((l1 (loop for i from -1 to 50 collect (list (list i))))
        (l2 (loop for i downfrom 50 to 0 collect (list (list i)))))
    (assert-false (subsetp l1
                           l2
                           :key #'car
                           :test 'equal))))

(define-test |subsetp test=equalp key=other 2a|
  (let ((l1 (loop for i from 1 to 10 collect (list (make-array 1 :initial-element i))))
        (l2 (loop for i downfrom 10 to 0 collect (list (make-array 1 :initial-element i)))))
    (assert-true (subsetp l1
                          l2
                          :key #'car
                          :test #'equalp))))

(define-test |subsetp test=equalp key=other 2b|
  (let ((l1 (loop for i from 1 to 10 collect (list (make-array 1 :initial-element i))))
        (l2 (loop for i downfrom 10 to 0 collect (list (make-array 1 :initial-element i)))))
    (assert-true (subsetp l1
                          l2
                          :key #'car
                          :test 'equalp))))

(define-test |subsetp test=equalp key=other 3a|
  (let ((l1 (loop for i from 1 to 50 collect (list (make-array 1 :initial-element i))))
        (l2 (loop for i downfrom 50 to 0 collect (list (make-array 1 :initial-element i)))))
    (assert-true (subsetp l1
                          l2
                          :key #'car
                          :test #'equalp))))

(define-test |subsetp test=equalp key=other 3b|
  (let ((l1 (loop for i from 1 to 50 collect (list (make-array 1 :initial-element i))))
        (l2 (loop for i downfrom 50 to 0 collect (list (make-array 1 :initial-element i)))))
    (assert-true (subsetp l1
                          l2
                          :key #'car
                          :test 'equalp))))

(define-test |subsetp test=equalp key=other 4a|
  (let ((l1 (loop for i from -1 to 10 collect (list (make-array 1 :initial-element i))))
        (l2 (loop for i downfrom 10 to 0 collect (list (make-array 1 :initial-element i)))))
    (assert-false (subsetp l1
                           l2
                           :key #'car
                           :test #'equalp))))

(define-test |subsetp test=equalp key=other 4b|
  (let ((l1 (loop for i from -1 to 10 collect (list (make-array 1 :initial-element i))))
        (l2 (loop for i downfrom 10 to 0 collect (list (make-array 1 :initial-element i)))))
    (assert-false (subsetp l1
                           l2
                           :key #'car
                           :test 'equalp))))

(define-test |subsetp test=equalp key=other 5a|
  (let ((l1 (loop for i from -1 to 50 collect (list (make-array 1 :initial-element i))))
        (l2 (loop for i downfrom 50 to 0 collect (list (make-array 1 :initial-element i)))))
    (assert-false (subsetp l1
                           l2
                           :key #'car
                           :test #'equalp))))

(define-test |subsetp test=equalp key=other 5b|
  (let ((l1 (loop for i from -1 to 50 collect (list (make-array 1 :initial-element i))))
        (l2 (loop for i downfrom 50 to 0 collect (list (make-array 1 :initial-element i)))))
    (assert-false (subsetp l1
                           l2
                           :key #'car
                           :test 'equalp))))

(define-test |subsetp test=other key=other 2b|
  (let ((l1 (loop for i from 1 to 10 collect (list i)))
        (l2 (loop for i downfrom 10 to 0 collect (list i))))
    (assert-true (subsetp l1
                          l2
                          :key #'car
                          :test #'=))))

(define-test |subsetp test=other key=other 3b|
  (let ((l1 (loop for i from 1 to 50 collect (list i)))
        (l2 (loop for i downfrom 50 to 0 collect (list i))))
    (assert-true (subsetp l1
                          l2
                          :key #'car
                          :test #'=))))

(define-test |subsetp test=other key=other 4b|
  (let ((l1 (loop for i from -1 to 10 collect (list i)))
        (l2 (loop for i downfrom 10 to 0 collect (list i))))
    (assert-false (subsetp l1
                           l2
                           :key #'car
                           :test #'=))))

(define-test |subsetp test=other key=other 5b|
  (let ((l1 (loop for i from -1 to 50 collect (list i)))
        (l2 (loop for i downfrom 50 to 0 collect (list i))))
    (assert-false (subsetp l1
                           l2
                           :key #'car
                           :test #'=))))

(define-test |subsetp test-not=other key=other 2b|
  (let ((l1 (loop for i from 1 to 10 collect (list i)))
        (l2 (loop for i downfrom 10 to 0 collect (list i))))
    (assert-true (subsetp l1
                          l2
                          :key #'car
                          :test-not #'/=))))

(define-test |subsetp test-not=other key=other 3b|
  (let ((l1 (loop for i from 1 to 50 collect (list i)))
        (l2 (loop for i downfrom 50 to 0 collect (list i))))
    (assert-true (subsetp l1
                          l2
                          :key #'car
                          :test-not #'/=))))

(define-test |subsetp test-not=other key=other 4b|
  (let ((l1 (loop for i from -1 to 10 collect (list i)))
        (l2 (loop for i downfrom 10 to 0 collect (list i))))
    (assert-false (subsetp l1
                           l2
                           :key #'car
                           :test-not #'/=))))

(define-test |subsetp test-not=other key=other 5b|
  (let ((l1 (loop for i from -1 to 50 collect (list i)))
        (l2 (loop for i downfrom 50 to 0 collect (list i))))
    (assert-false (subsetp l1
                           l2
                           :key #'car
                           :test-not #'/=))))

(define-test |subsetp test=other test-not=other 1|
  (assert-error 'error
                (subsetp '() '() :test #'eql :test-not #'eql)))

(define-test |subsetp test=nil key=identity 1|
  (let ((l1 (loop for i from 1 to 10 collect i))
        (l2 (loop for i downfrom 10 to 0 collect i)))
    (assert-error 'error
                  (subsetp l1
                           l2
                          :test nil))))

(define-test |subsetp test-not=nil key=identity 1|
  (let ((l1 (loop for i from 1 to 10 collect i))
        (l2 (loop for i downfrom 10 to 0 collect i)))
    (assert-error 'error
                  (subsetp l1
                           l2
                          :test-not nil))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Tests for the pushnew macro

(define-test |pushnew test=eql key=identity 1a|
  (let ((list '()))
    (assert-equal '(1)
                  (pushnew 1 list))
    (assert-equal '(1)
                  list)))

(define-test |pushnew test=eql key=identity 1b|
  (let ((list '()))
    (assert-equal '(1)
                  (pushnew 1 list :test #'eql))
    (assert-equal '(1)
                  list)))

(define-test |pushnew test=eql key=identity 1c|
  (let ((list '()))
    (assert-equal '(1)
                  (pushnew 1 list :test 'eql))
    (assert-equal '(1)
                  list)))

(define-test |pushnew test=eql key=identity 2a|
  (let ((list '(1)))
    (assert-equal '(1)
                  (pushnew 1 list))
    (assert-equal '(1)
                  list)))

(define-test |pushnew test=eql key=identity 2b|
  (let ((list '(1)))
    (assert-equal '(1)
                  (pushnew 1 list :test #'eql))
    (assert-equal '(1)
                  list)))

(define-test |pushnew test=eql key=identity 2c|
  (let ((list '(1)))
    (assert-equal '(1)
                  (pushnew 1 list :test 'eql))
    (assert-equal '(1)
                  list)))

(define-test |pushnew test=eql key=identity 3a|
  (let ((list '(2)))
    (assert-equal '(1 2)
                  (pushnew 1 list))
    (assert-equal '(1 2)
                  list)))

(define-test |pushnew test=eql key=identity 3b|
  (let ((list '(2)))
    (assert-equal '(1 2)
                  (pushnew 1 list :test #'eql))
    (assert-equal '(1 2)
                  list)))

(define-test |pushnew test=eql key=identity 3c|
  (let ((list '(2)))
    (assert-equal '(1 2)
                  (pushnew 1 list :test 'eql))
    (assert-equal '(1 2)
                  list)))

(define-test |pushnew test=eql key=identity 4a|
  (let ((list '(2 1)))
    (assert-equal '(2 1)
                  (pushnew 1 list))
    (assert-equal '(2 1)
                  list)))

(define-test |pushnew test=eql key=identity 4b|
  (let ((list '(2 1)))
    (assert-equal '(2 1)
                  (pushnew 1 list :test #'eql))
    (assert-equal '(2 1)
                  list)))

(define-test |pushnew test=eql key=identity 4c|
  (let ((list '(2 1)))
    (assert-equal '(2 1)
                  (pushnew 1 list :test 'eql))
    (assert-equal '(2 1)
                  list)))

(define-test |pushnew test=eql key=nil 3b|
  (let ((list '(2)))
    (assert-equal '(1 2)
                  (pushnew 1 list :key nil))
    (assert-equal '(1 2)
                  list)))

(define-test |pushnew test=eq key=identity 1b|
  (let ((list '()))
    (assert-equal '(a)
                  (pushnew 'a list :test #'eq))
    (assert-equal '(a)
                  list)))

(define-test |pushnew test=eq key=identity 1c|
  (let ((list '()))
    (assert-equal '(a)
                  (pushnew 'a list :test 'eq))
    (assert-equal '(a)
                  list)))

(define-test |pushnew test=eq key=identity 2b|
  (let ((list '(a)))
    (assert-equal '(a)
                  (pushnew 'a list :test #'eq))
    (assert-equal '(a)
                  list)))

(define-test |pushnew test=eq key=identity 2c|
  (let ((list '(a)))
    (assert-equal '(a)
                  (pushnew 'a list :test 'eq))
    (assert-equal '(a)
                  list)))

(define-test |pushnew test=eq key=identity 3b|
  (let ((list '(b)))
    (assert-equal '(a b)
                  (pushnew 'a list :test #'eq))
    (assert-equal '(a b)
                  list)))

(define-test |pushnew test=eq key=identity 3c|
  (let ((list '(b)))
    (assert-equal '(a b)
                  (pushnew 'a list :test 'eq))
    (assert-equal '(a b)
                  list)))

(define-test |pushnew test=eq key=identity 4b|
  (let ((list '(b a)))
    (assert-equal '(b a)
                  (pushnew 'a list :test #'eq))
    (assert-equal '(b a)
                  list)))

(define-test |pushnew test=eq key=identity 4c|
  (let ((list '(b a)))
    (assert-equal '(b a)
                  (pushnew 'a list :test 'eq))
    (assert-equal '(b a)
                  list)))

(define-test |pushnew test=eql key=other 1|
  (let ((list '()))
    (assert-equal '((1))
                  (pushnew '(1) list :key #'car))
    (assert-equal '((1))
                  list)))

(define-test |pushnew test=eql key=other 2|
  (let ((list '((1))))
    (assert-equal '((1))
                  (pushnew '(1) list :key #'car))
    (assert-equal '((1))
                  list)))

(define-test |pushnew test=eql key=other 3|
  (let ((list '((2))))
    (assert-equal '((1) (2))
                  (pushnew '(1) list :key #'car))
    (assert-equal '((1) (2))
                  list)))

(define-test |pushnew test=eql key=other 4|
  (let ((list '((2) (1))))
    (assert-equal '((2) (1))
                  (pushnew '(1) list :key #'car))
    (assert-equal '((2) (1))
                  list)))

(define-test |pushnew test=eq key=other 1|
  (let ((list '()))
    (assert-equal '((a))
                  (pushnew '(a) list :test #'eq :key #'car))
    (assert-equal '((a))
                  list)))

(define-test |pushnew test=eq key=other 2|
  (let ((list '((a))))
    (assert-equal '((a))
                  (pushnew '(a) list :test #'eq :key #'car))
    (assert-equal '((a))
                  list)))

(define-test |pushnew test=eq key=other 3|
  (let ((list '((b))))
    (assert-equal '((a) (b))
                  (pushnew '(a) list :test #'eq :key #'car))
    (assert-equal '((a) (b))
                  list)))

(define-test |pushnew test=eq key=other 4|
  (let ((list '((b) (a))))
    (assert-equal '((b) (a))
                  (pushnew '(a) list :test #'eq :key #'car))
    (assert-equal '((b) (a))
                  list)))

(define-test |pushnew test=other key=identity 1b|
  (let ((list '()))
    (assert-equal '(1)
                  (pushnew 1 list
                           :test (lambda (x y) (= (1- x) y))))
    (assert-equal '(1)
                  list)))

(define-test |pushnew test=other key=identity 2b|
  (let ((list '(1)))
    (assert-equal '(1 1)
                  (pushnew 1 list
                           :test (lambda (x y) (= (1- x) y))))
    (assert-equal '(1 1)
                  list)))

(define-test |pushnew test=other key=identity 3b|
  (let ((list '(2)))
    (assert-equal '(1 2)
                  (pushnew 1 list
                           :test (lambda (x y) (= (1- x) y))))
    (assert-equal '(1 2)
                  list)))

(define-test |pushnew test=other key=identity 4b|
  (let ((list '(2 1)))
    (assert-equal '(1 2 1)
                  (pushnew 1 list
                           :test (lambda (x y) (= (1- x) y))))
    (assert-equal '(1 2 1)
                  list)))

(define-test |pushnew test=other key=identity 5b|
  (let ((list '(1)))
    (assert-equal '(1)
                  (pushnew 2 list
                           :test (lambda (x y) (= (1- x) y))))
    (assert-equal '(1)
                  list)))

(define-test |pushnew test=other key=identity 6b|
  (let ((list '(2)))
    (assert-equal '(2 2)
                  (pushnew 2 list
                           :test (lambda (x y) (= (1- x) y))))
    (assert-equal '(2 2)
                  list)))

(define-test |pushnew test=other key=identity 7b|
  (let ((list '(2 1)))
    (assert-equal '(2 1)
                  (pushnew 2 list
                           :test (lambda (x y) (= (1- x) y))))
    (assert-equal '(2 1)
                  list)))

(define-test |pushnew test=other key=other 3b|
  (let ((list '((2))))
    (assert-equal '((1) (2))
                  (pushnew '(1) list
                           :test (lambda (x y) (= (1- x) y))
                           :key #'car))
    (assert-equal '((1) (2))
                  list)))

(define-test |pushnew test=other key=other 4b|
  (let ((list '((2) (1))))
    (assert-equal '((1) (2) (1))
                  (pushnew '(1) list
                           :test (lambda (x y) (= (1- x) y))
                           :key #'car))
    (assert-equal '((1) (2) (1))
                  list)))

(define-test |pushnew test=other key=other 5b|
  (let ((list '((1))))
    (assert-equal '((1))
                  (pushnew '(2) list
                           :test (lambda (x y) (= (1- x) y))
                           :key #'car))
    (assert-equal '((1))
                  list)))

(define-test |pushnew test=other key=other 6b|
  (let ((list '((2))))
    (assert-equal '((2) (2))
                  (pushnew '(2) list
                           :test (lambda (x y) (= (1- x) y))
                           :key #'car))
    (assert-equal '((2) (2))
                  list)))

(define-test |pushnew test=other key=other 7b|
  (let ((list '((2) (1))))
    (assert-equal '((2) (1))
                  (pushnew '(2) list
                           :test (lambda (x y) (= (1- x) y))
                           :key #'car))
    (assert-equal '((2) (1))
                  list)))

(define-test |pushnew test-not=other key=identity 1b|
  (let ((list '()))
    (assert-equal '(1)
                  (pushnew 1 list
                           :test-not (lambda (x y) (/= (1- x) y))))
    (assert-equal '(1)
                  list)))

(define-test |pushnew test-not=other key=identity 2b|
  (let ((list '(1)))
    (assert-equal '(1 1)
                  (pushnew 1 list
                           :test-not (lambda (x y) (/= (1- x) y))))
    (assert-equal '(1 1)
                  list)))

(define-test |pushnew test-not=other key=identity 3b|
  (let ((list '(2)))
    (assert-equal '(1 2)
                  (pushnew 1 list
                           :test-not (lambda (x y) (/= (1- x) y))))
    (assert-equal '(1 2)
                  list)))

(define-test |pushnew test-not=other key=identity 4b|
  (let ((list '(2 1)))
    (assert-equal '(1 2 1)
                  (pushnew 1 list
                           :test-not (lambda (x y) (/= (1- x) y))))
    (assert-equal '(1 2 1)
                  list)))

(define-test |pushnew test-not=other key=identity 5b|
  (let ((list '(1)))
    (assert-equal '(1)
                  (pushnew 2 list
                           :test-not (lambda (x y) (/= (1- x) y))))
    (assert-equal '(1)
                  list)))

(define-test |pushnew test-not=other key=identity 6b|
  (let ((list '(2)))
    (assert-equal '(2 2)
                  (pushnew 2 list
                           :test-not (lambda (x y) (/= (1- x) y))))
    (assert-equal '(2 2)
                  list)))

(define-test |pushnew test-not=other key=identity 7b|
  (let ((list '(2 1)))
    (assert-equal '(2 1)
                  (pushnew 2 list
                           :test-not (lambda (x y) (/= (1- x) y))))
    (assert-equal '(2 1)
                  list)))

(define-test |pushnew test-not=other key=other 3b|
  (let ((list '((2))))
    (assert-equal '((1) (2))
                  (pushnew '(1) list
                           :test-not (lambda (x y) (/= (1- x) y))
                           :key #'car))
    (assert-equal '((1) (2))
                  list)))

(define-test |pushnew test-not=other key=other 4b|
  (let ((list '((2) (1))))
    (assert-equal '((1) (2) (1))
                  (pushnew '(1) list
                           :test-not (lambda (x y) (/= (1- x) y))
                           :key #'car))
    (assert-equal '((1) (2) (1))
                  list)))

(define-test |pushnew test-not=other key=other 5b|
  (let ((list '((1))))
    (assert-equal '((1))
                  (pushnew '(2) list
                           :test-not (lambda (x y) (/= (1- x) y))
                           :key #'car))
    (assert-equal '((1))
                  list)))

(define-test |pushnew test-not=other key=other 6b|
  (let ((list '((2))))
    (assert-equal '((2) (2))
                  (pushnew '(2) list
                           :test-not (lambda (x y) (/= (1- x) y))
                           :key #'car))
    (assert-equal '((2) (2))
                  list)))

(define-test |pushnew test-not=other key=other 7b|
  (let ((list '((2) (1))))
    (assert-equal '((2) (1))
                  (pushnew '(2) list
                           :test-not (lambda (x y) (/= (1- x) y))
                           :key #'car))
    (assert-equal '((2) (1))
                  list)))

(define-test |pushnew test=other test-not=other 1|
  (let ((fun nil)
        (warned nil))
    (handler-bind ((warning (lambda (condition)
                              (setf warned t)
                              (muffle-warning condition))))
      (setf fun
            (compile nil '(lambda ()
                           (declare (special list))
                           (pushnew '2 list
                            :test #'eql
                            :test-not #'eql)))))
    (assert-true warned)
    (assert-error 'error (funcall fun))))

(define-test |pushnew test=nil key=identity 3b|
  (let ((list '(2)))
    (assert-error 'error
                  (pushnew 1 list :test nil))))
    

(define-test |pushnew test-not=nil key=identity 3b|
  (let ((list '(2)))
    (assert-error 'error
                  (pushnew 1 list :test-not nil))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Tests for the pop macro

(define-test |pop 1|
  (let ((var (list 1)))
    (assert-equal 1
                  (pop var))
    (assert-equal '()
                  var)))

(define-test |pop 2|
  (let ((var (list 1 2)))
    (assert-equal 1
                  (pop var))
    (assert-equal '(2)
                  var)))

(define-test |pop 3|
  (let ((list (list (list 1))))
    (assert-equal 1
                  (pop (car list)))
    (assert-equal '(())
                  list)))

(define-test |pop 4|
  (let ((list (list (list 1 2))))
    (assert-equal 1
                  (pop (car list)))
    (assert-equal '((2))
                  list)))

(define-test |pop error 1|
  (let ((list (copy-tree '(1 . 2))))
    (assert-equal 1
                  (pop list))
    (assert-error 'type-error
                  (pop list))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Tests for the nsubst function

(define-test nsubst.1
  (assert-equal '(a (c))
                (nsubst 'c '(b) (copy-tree '(a ((b)))) :test-not (complement #'equal))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Tests for the getf function

(define-test |getf 1|
  (assert-equal nil
                (getf '() 'a)))

(define-test |getf 2|
  (assert-equal 1
                (getf '(a 1) 'a)))

(define-test |getf 3|
  (assert-equal 1
                (getf '(b 2 a 1) 'a)))

(define-test |getf 4|
  (assert-equal nil
                (getf '(b 2 a 1) 'c)))

(define-test |getf error 1|
  (assert-error 'type-error
                (getf '(b 2 a) 'c)))

(define-test |getf error 2|
  (assert-error 'type-error
                (getf 1 'c)))

(define-test |getf error 3|
  (assert-error 'type-error
                (getf '(b 2 a 3 . f) 'c)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Tests for the get-properties function

(define-test |get-properties 1|
  (multiple-value-bind (indicator value tail)
      (get-properties '() '(a))
    (assert-equal nil indicator)
    (assert-equal nil value)
    (assert-equal nil tail)))

(define-test |get-properties 2|
  (multiple-value-bind (indicator value tail)
      (get-properties '(a 1) '(b a))
    (assert-equal 'a indicator)
    (assert-equal 1 value)
    (assert-equal '(a 1) tail)))

(define-test |get-properties 3|
  (multiple-value-bind (indicator value tail)
      (get-properties '(b 2 a 1) '(c a d))
    (assert-equal 'a indicator)
    (assert-equal 1 value)
    (assert-equal '(a 1) tail)))

(define-test |get-properties 4|
  (multiple-value-bind (indicator value tail)
      (get-properties '(b 2 a 1) '(c d e))
    (assert-equal nil indicator)
    (assert-equal nil value)
    (assert-equal nil tail)))

(define-test |get-properties 5|
  (multiple-value-bind (indicator value tail)
      (get-properties '(b 2 a 1 e 10) '(c a d))
    (assert-equal 'a indicator)
    (assert-equal 1 value)
    (assert-equal '(a 1 e 10) tail)))

(define-test |get-properties error 1|
  (assert-error 'type-error
                (get-properties '(b 2 a) '(c d e))))

(define-test |get-properties error 2|
  (assert-error 'type-error
                (get-properties '(b 2 a 3 . f) '(c d e))))

(define-test |get-properties error 3|
  (assert-error 'type-error
                (get-properties 1 '(c d e))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Tests for the setf expander for getf

(define-test |(setf getf) 1|
  (let ((list (copy-list '(:a 0 :c 1 :e 2 :c 3))))
    (assert-equal 4
                  (setf (getf list :c) 4))
    (assert-equal '(:a 0 :c 4 :e 2 :c 3)
                  list)))

(define-test |(setf getf) 2|
  (let ((list (copy-list '(:a 0 :c 1 :e 2 :c 3)))
        (thing 0))
    (assert-equal 4
                  (setf (getf list :d (incf thing)) 4))
    (assert-equal 4
                  (getf list :d))
    (assert-equal 1 thing)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Tests for the remf macro

(define-test remf.1
  (let ((list (copy-tree '(a 1 b 2 c 3))))
    (assert-true (remf list 'c))
    (assert-equal '(a 1 b 2)
                  list)))
