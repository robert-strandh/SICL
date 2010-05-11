(in-package #:sicl-cons-high-test)

;;; These tests have been taken from the cxr.lisp file of the
;;; ansi test suite by Paul Dietz, and have been adapted for 
;;; use with lisp-unit.

(define-test cons.25
  (assert-equal 'a
                (caar '((a)))))

(define-test cons.26
  (assert-equal 'b
                (cdar '((a . b)))))

(define-test cons.27
  (assert-equal 'b
                (cadr '(a b))))

(define-test cons.28
  (assert-equal 'c
                (cddr '(a b . c))))

(define-test cons.29
  (assert-equal 'a
                (caaar '(((a))))))

(define-test cons.30
  (assert-equal 'b
                (cdaar '(((a . b))))))

(define-test cons.31
  (assert-equal 'b
                (cadar (cons (cons 'a (cons 'b 'c)) 'd))))

(define-test cons.32
  (assert-equal 'c
                (cddar (cons (cons 'a (cons 'b 'c)) 'd))))

(define-test cons.33
  (assert-equal 'b
                (caadr (cons 'a (cons (cons 'b 'c) 'd)))))

(define-test cons.34
  (assert-equal 'c
                (caddr (cons 'a (cons 'b (cons 'c 'd))))))

(define-test cons.36
  (assert-equal 'c
                (cdadr (cons 'a (cons (cons 'b 'c) 'd)))))

(define-test cons.37
  (assert-equal 'd
                (cdddr (cons 'a (cons 'b (cons 'c 'd))))))

(defvar *cons-test-4*
  (cons (cons (cons (cons 'a 'b)
		    (cons 'c 'd))
	      (cons (cons 'e 'f)
		    (cons 'g 'h)))
	(cons (cons (cons 'i 'j)
		    (cons 'k 'l))
	      (cons (cons 'm 'n)
		    (cons 'o 'p)))))

(define-test cons.38
  (assert-equal 'a
                (caaaar *cons-test-4*)))

(define-test cons.39
  (assert-equal 'b
                (cdaaar *cons-test-4*)))

(define-test cons.40
  (assert-equal 'c
                (cadaar *cons-test-4*)))

(define-test cons.41
  (assert-equal 'd
                (cddaar *cons-test-4*)))

(define-test cons.42
  (assert-equal 'e
                (caadar *cons-test-4*)))

(define-test cons.43
  (assert-equal 'f
                (cdadar *cons-test-4*)))

(define-test cons.44
  (assert-equal 'g
                (caddar *cons-test-4*)))

(define-test cons.45
  (assert-equal 'h
                (cdddar *cons-test-4*)))

;;;

(define-test cons.46
  (assert-equal 'i
                (caaadr *cons-test-4*)))

(define-test cons.47
  (assert-equal 'j
                (cdaadr *cons-test-4*)))

(define-test cons.48
  (assert-equal 'k
                (cadadr *cons-test-4*)))

(define-test cons.49
  (assert-equal 'l
                (cddadr *cons-test-4*)))

(define-test cons.50
  (assert-equal 'm
                (caaddr *cons-test-4*)))

(define-test cons.51
  (assert-equal 'n
                (cdaddr *cons-test-4*)))

(define-test cons.52
  (assert-equal 'o
                (cadddr *cons-test-4*)))

(define-test cons.53
  (assert-equal 'p
                (cddddr *cons-test-4*)))

;;; Error checking of c*r functions

(define-test caar.error.1
  (assert-error 'type-error (caar 'a)))

(define-test caar.error.2
  (assert-error 'type-error (caar '(a))))

(define-test cadr.error.1
  (assert-error 'type-error (cadr 'a)))

(define-test cadr.error.2
  (assert-error 'type-error (cadr '(a . b))))

(define-test cdar.error.1
  (assert-error 'type-error (cdar 'a)))

(define-test cdar.error.2
  (assert-error 'type-error (cdar '(a . b))))

(define-test cddr.error.1
  (assert-error 'type-error (cddr 'a)))

(define-test cddr.error.2
  (assert-error 'type-error (cddr '(a . b))))

(define-test caaar.error.1
  (assert-error 'type-error (caaar 'a)))

(define-test caaar.error.2
  (assert-error 'type-error (caaar '(a))))

(define-test caaar.error.3
  (assert-error 'type-error (caaar '((a)))))

(define-test caadr.error.1
  (assert-error 'type-error (caadr 'a)))

(define-test caadr.error.2
  (assert-error 'type-error (caadr '(a . b))))

(define-test caadr.error.3
  (assert-error 'type-error (caadr '(a . (b)))))

(define-test cadar.error.1
  (assert-error 'type-error (cadar 'a)))

(define-test cadar.error.2
  (assert-error 'type-error (cadar '(a . b))))

(define-test cadar.error.3
  (assert-error 'type-error (cadar '((a . c) . b))))

(define-test caddr.error.1
  (assert-error 'type-error (caddr 'a)))

(define-test caddr.error.2
  (assert-error 'type-error (caddr '(a . b))))

(define-test caddr.error.3
  (assert-error 'type-error (caddr '(a c . b))))

(define-test cdaar.error.1
  (assert-error 'type-error (cdaar 'a)))

(define-test cdaar.error.2
  (assert-error 'type-error (cdaar '(a))))

(define-test cdaar.error.3
  (assert-error 'type-error (cdaar '((a . b)))))

(define-test cdadr.error.1
  (assert-error 'type-error (cdadr 'a)))

(define-test cdadr.error.2
  (assert-error 'type-error (cdadr '(a . b))))

(define-test cdadr.error.3
  (assert-error 'type-error (cdadr '(a b . c))))

(define-test cddar.error.1
  (assert-error 'type-error (cddar 'a)))

(define-test cddar.error.2
  (assert-error 'type-error (cddar '(a . b))))

(define-test cddar.error.3
  (assert-error 'type-error (cddar '((a . b) . b))))

(define-test cdddr.error.1
  (assert-error 'type-error (cdddr 'a)))

(define-test cdddr.error.2
  (assert-error 'type-error (cdddr '(a . b))))

(define-test cdddr.error.3
  (assert-error 'type-error (cdddr '(a c . b))))

;;

(define-test caaaar.error.1
  (assert-error 'type-error (caaaar 'a)))

(define-test caaaar.error.2
  (assert-error 'type-error (caaaar '(a))))

(define-test caaaar.error.3
  (assert-error 'type-error (caaaar '((a)))))

(define-test caaaar.error.4
  (assert-error 'type-error (caaaar '(((a))))))

(define-test caaadr.error.1
  (assert-error 'type-error (caaadr 'a)))

(define-test caaadr.error.2
  (assert-error 'type-error (caaadr '(a . b))))

(define-test caaadr.error.3
  (assert-error 'type-error (caaadr '(a . (b)))))

(define-test caaadr.error.4
  (assert-error 'type-error (caaadr '(a . ((b))))))

(define-test caadar.error.1
  (assert-error 'type-error (caadar 'a)))

(define-test caadar.error.2
  (assert-error 'type-error (caadar '(a . b))))

(define-test caadar.error.3
  (assert-error 'type-error (caadar '((a . c) . b))))

(define-test caadar.error.4
  (assert-error 'type-error (caadar '((a . (c)) . b))))

(define-test caaddr.error.1
  (assert-error 'type-error (caaddr 'a)))

(define-test caaddr.error.2
  (assert-error 'type-error (caaddr '(a . b))))

(define-test caaddr.error.3
  (assert-error 'type-error (caaddr '(a c . b))))

(define-test caaddr.error.4
  (assert-error 'type-error (caaddr '(a c . (b)))))

(define-test cadaar.error.1
  (assert-error 'type-error (cadaar 'a)))

(define-test cadaar.error.2
  (assert-error 'type-error (cadaar '(a))))

(define-test cadaar.error.3
  (assert-error 'type-error (cadaar '((a . b)))))

(define-test cadaar.error.4
  (assert-error 'type-error (cadaar '((a . (b))))))

(define-test cadadr.error.1
  (assert-error 'type-error (cadadr 'a)))

(define-test cadadr.error.2
  (assert-error 'type-error (cadadr '(a . b))))

(define-test cadadr.error.3
  (assert-error 'type-error (cadadr '(a b . c))))

(define-test cadadr.error.4
  (assert-error 'type-error (cadadr '(a (b . e) . c))))

(define-test caddar.error.1
  (assert-error 'type-error (caddar 'a)))

(define-test caddar.error.2
  (assert-error 'type-error (caddar '(a . b))))

(define-test caddar.error.3
  (assert-error 'type-error (caddar '((a . b) . b))))

(define-test caddar.error.4
  (assert-error 'type-error (caddar '((a  b . c) . b))))

(define-test cadddr.error.1
  (assert-error 'type-error (cadddr 'a)))

(define-test cadddr.error.2
  (assert-error 'type-error (cadddr '(a . b))))

(define-test cadddr.error.3
  (assert-error 'type-error (cadddr '(a c . b))))

(define-test cadddr.error.4
  (assert-error 'type-error (cadddr '(a c e . b))))

(define-test cdaaar.error.1
  (assert-error 'type-error (cdaaar 'a)))

(define-test cdaaar.error.2
  (assert-error 'type-error (cdaaar '(a))))

(define-test cdaaar.error.3
  (assert-error 'type-error (cdaaar '((a)))))

(define-test cdaaar.error.4
  (assert-error 'type-error (cdaaar '(((a . b))))))

(define-test cdaadr.error.1
  (assert-error 'type-error (cdaadr 'a)))

(define-test cdaadr.error.2
  (assert-error 'type-error (cdaadr '(a . b))))

(define-test cdaadr.error.3
  (assert-error 'type-error (cdaadr '(a . (b)))))

(define-test cdaadr.error.4
  (assert-error 'type-error (cdaadr '(a . ((b . c))))))

(define-test cdadar.error.1
  (assert-error 'type-error (cdadar 'a)))

(define-test cdadar.error.2
  (assert-error 'type-error (cdadar '(a . b))))

(define-test cdadar.error.3
  (assert-error 'type-error (cdadar '((a . c) . b))))

(define-test cdadar.error.4
  (assert-error 'type-error (cdadar '((a . (c . d)) . b))))

(define-test cdaddr.error.1
  (assert-error 'type-error (cdaddr 'a)))

(define-test cdaddr.error.2
  (assert-error 'type-error (cdaddr '(a . b))))

(define-test cdaddr.error.3
  (assert-error 'type-error (cdaddr '(a c . b))))

(define-test cdaddr.error.4
  (assert-error 'type-error (cdaddr '(a c b . d))))

(define-test cddaar.error.1
  (assert-error 'type-error (cddaar 'a)))

(define-test cddaar.error.2
  (assert-error 'type-error (cddaar '(a))))

(define-test cddaar.error.3
  (assert-error 'type-error (cddaar '((a . b)))))

(define-test cddaar.error.4
  (assert-error 'type-error (cddaar '((a . (b))))))

(define-test cddadr.error.1
  (assert-error 'type-error (cddadr 'a)))

(define-test cddadr.error.2
  (assert-error 'type-error (cddadr '(a . b))))

(define-test cddadr.error.3
  (assert-error 'type-error (cddadr '(a b . c))))

(define-test cddadr.error.4
  (assert-error 'type-error (cddadr '(a (b . e) . c))))

(define-test cdddar.error.1
  (assert-error 'type-error (cdddar 'a)))

(define-test cdddar.error.2
  (assert-error 'type-error (cdddar '(a . b))))

(define-test cdddar.error.3
  (assert-error 'type-error (cdddar '((a . b) . b))))

(define-test cdddar.error.4
  (assert-error 'type-error (cdddar '((a  b . c) . b))))

(define-test cddddr.error.1
  (assert-error 'type-error (cddddr 'a)))

(define-test cddddr.error.2
  (assert-error 'type-error (cddddr '(a . b))))

(define-test cddddr.error.3
  (assert-error 'type-error (cddddr '(a c . b))))

(define-test cddddr.error.4
  (assert-error 'type-error (cddddr '(a c e . b))))

;;; This test verifies that append preserves the structure of
;;; the last list.  We use (loop ... append ...) to implement
;;; the append function, and some implementations of loop are
;;; known not to respect that rule. 

(define-test append.sharing
  (assert-equal t
                (let ((list1 '(1 2))
                      (list2 '(3 4)))
                  (eq (cddr (append list1 list2)) list2))))
