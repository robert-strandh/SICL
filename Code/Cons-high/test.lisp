(in-package #:sicl-cons-high-test)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Tests for the caar function 

(define-test caar.1
  (assert-equal 'a (caar '((a)))))

(define-test caar.error.1
  (assert-error 'type-error (caar 'a)))

(define-test caar.error.2
  (assert-error 'type-error (caar '(a))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Tests for the cadr function

(define-test cdar.1
  (assert-equal 'b (cdar '((a . b)))))

(define-test cdar.error.1
  (assert-error 'type-error (cdar 'a)))

(define-test cdar.error.2
  (assert-error 'type-error (cdar '(a . b))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Tests for the cadr function

(define-test cadr.1
  (assert-equal 'b (cadr '(a b))))

(define-test cadr.error.1
  (assert-error 'type-error (cadr 'a)))

(define-test cadr.error.2
  (assert-error 'type-error (cadr '(a . b))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Tests for the cddr function

(define-test cddr.1
  (assert-equal 'c (cddr '(a b . c))))

(define-test cddr.error.1
  (assert-error 'type-error (cddr 'a)))

(define-test cddr.error.2
  (assert-error 'type-error (cddr '(a . b))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Tests for the caaar function

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
;;; Tests for the cdaar function

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
;;; Tests for the cadar function

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
;;; Tests for the cddar function

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
;;; Tests for the caadr function

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
;;; Tests for the caddr function

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
;;; Tests for the cdadr function

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
;;; Tests for the cdddr function

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
;;; Tests for the caaaar function

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
;;; Tests for the caaadr function

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
;;; Tests for the caadar function

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
;;; Tests for the caaddr function

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
;;; Tests for the cadaar function

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
;;; Tests for the cadadr function

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
;;; Tests for the caddar function

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
;;; Tests for the cadddr function

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
;;; Tests for the cdaaar function

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
;;; Tests for the cdaadr function

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
;;; Tests for the cdadar function

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
;;; Tests for the cdaddr function

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
;;; Tests for the cddaar function

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
;;; Tests for the cddadr function

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
;;; Tests for the cdddar function

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
;;; Tests for the cddddr function

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
;;; Tests for the list function

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
;;; Tests for the list* function

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

(define-test tree-equal.1
  (assert-true (tree-equal nil nil)))

(define-test tree-equal.2
  (assert-true (tree-equal 1 1)))

(define-test tree-equal.3
  (assert-true (tree-equal '(1) '(1))))

(define-test tree-equal.4
  (assert-true (tree-equal '(1 . 2) '(1 . 2))))

(define-test tree-equal.5
  (assert-false (tree-equal 1 2)))

(define-test tree-equal.6
  (assert-false (tree-equal '(1) '(2))))

(define-test tree-equal.7
  (assert-true (tree-equal '(1) '(2)
                           :test (lambda (x y)
                                   (or (eql x y)
                                       (and (numberp x)
                                            (numberp y)
                                            (<= (abs (- x y)) 1)))))))

(define-test tree-equal.8
  (assert-false (tree-equal '(1) '(2) :test-not #'eql)))

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

(define-test mapcar.1
  (assert-equal '() (mapcar #'1+ '())))

(define-test mapcar.2
  (assert-equal '(1) (mapcar #'1+ '(0))))

(define-test mapcar.3
  (assert-equal '(-1 -2 -3) (mapcar #'- '(1 2 3))))

(define-test mapcar.4
  (assert-equal '(2 4 6) (mapcar #'+ '(1 2 3) '(1 2 3))))

(define-test mapcar.5
  (assert-equal '(3 6 9) (mapcar #'+ '(1 2 3) '(1 2 3) '(1 2 3))))

(define-test mapcar.6
  (assert-equal '(2 4) (mapcar #'+ '(1 2 3 4 5) '(1 2))))

(define-test mapcar.7
  (assert-equal '(2 4) (mapcar #'+ '(1 2) '(1 2 3 4 5))))

(define-test mapcar.error.1
  (assert-error 'type-error (mapcar #'1+ 1)))

(define-test mapcar.error.2
  (assert-error 'type-error (mapcar #'1+ #(1 2 3))))

(define-test mapcar.error.3
  (assert-error 'type-error (mapcar #'1+ '(1 2 . 3))))

(define-test mapcar.error.4
  (assert-error 'type-error (mapcar #'1+ "1")))

(define-test mapcar.order.1
  (let ((i 0)
        (funs (vector #'1+ #'1-))
        (lists '((1 2) (3 4) (5 6))))
    (assert-equal '(4 5) (mapcar (aref funs (incf i)) (nth (incf i) lists)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Tests for the mapc function

(define-test mapc.1
  (assert-equal '() (mapcar #'1+ '())))

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

(define-test mapc.error.1
  (assert-error 'type-error (mapc #'1+ 1)))

(define-test mapc.error.2
  (assert-error 'type-error (mapc #'1+ #(1 2 3))))

(define-test mapc.error.3
  (assert-error 'type-error (mapc #'1+ '(1 2 . 3))))

(define-test mapc.error.4
  (assert-error 'type-error (mapc #'1+ "1")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Tests for the maplist function

(define-test maplist.1
  (assert-equal '() (maplist #'car '())))

(define-test maplist.1
  (assert-equal '(1 2 3) (maplist #'car '(1 2 3))))

(define-test maplist.error.1
  (assert-error 'type-error (maplist #'car 1)))

(define-test maplist.error.2
  (assert-error 'type-error (maplist #'car #(1 2 3))))

(define-test maplist.error.3
  (assert-error 'type-error (maplist #'car '(1 2 . 3))))

(define-test maplist.error.4
  (assert-error 'type-error (maplist #'car "1")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Tests for the append function

(define-test append.1
  (assert-equal nil (append)))

(define-test append.2
  (assert-equal 'x (append 'x)))

(define-test append.4
  (assert-equal
   '(a b c d e f g . h)
   (append (list 'a) (list 'b) (list 'c)
           (list 'd) (list 'e) (list 'f)
           (list 'g) 'h)))

(define-test append.5
  (assert-equal 'a (append nil nil nil nil nil nil nil nil 'a)))

;;; Test suggested by Peter Graves
(define-test append.7
  (assert-equal
   nil
   (let ((x (list 'a 'b 'c 'd)))
     (eq (append x nil) x))))

;;; Order of evaluation tests

(define-test append.order.1
  (assert-equal
   '((a b c d e f g h i) 3 1 2 3)
   (let ((i 0) x y z)
     (list
       (append (progn (setf x (incf i)) (copy-list '(a b c)))
               (progn (setf y (incf i)) (copy-list '(d e f)))
               (progn (setf z (incf i)) (copy-list '(g h i))))
       i x y z))))

(define-test append.order.2
  (assert-equal '(1 1) (let ((i 0)) (append (list (incf i)) (list i)))))

;;; Error tests

(define-test append.error.1
  (assert-error 'type-error (append '(a . b) '(z))))

(define-test append.error.2
  (assert-error 'type-error (append '(x y z) '(a . b) '(z))))

;;; This test verifies that append preserves the structure of
;;; the last list.

(define-test append.sharing.1
  (let ((list1 '(1 2))
        (list2 '(3 4)))
    (assert-eq (cddr (append list1 list2)) list2)))

(define-test append.apply.1
  (let ((list1 '(1 2))
        (list2 '(3 4)))
    (assert-equal '(1 2 3 4)
                  (apply (cadr (list 'a #'append)) (list list1 list2)))
    (assert-equal '(1 2 3 4)
                  (apply (cadr (list 'a 'append)) (list list1 list2)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Tests for the nconc function

(define-test nconc.1
  (assert-eq '() (nconc)))

(define-test nconc.2
  (let ((list (copy-tree '(a b c))))
    (assert-eq list (nconc list))))

(define-test nconc.3
  (let ((list (copy-tree '(a b c))))
    (assert-eq list (nconc '() list))))

(define-test nconc.4
  (let ((list1 (copy-tree '(a b c)))
        (list2 (copy-tree '(d e f))))
    (assert-eq list1 (nconc list1 list2))
    (assert-eq list2 (cdddr list1))))

(define-test nconc.error.1
  (assert-error 'type-error (nconc (copy-tree '(a . b))
                                   (copy-tree '(z)))))

(define-test nconc.error.2
  (assert-error 'type-error (nconc (copy-tree '(x y z))
                                   (copy-tree '(a . b))
                                   (copy-tree '(z)))))

(define-test nconc.order.1
  (assert-equal
   '((a b c d e f g h i) 3 1 2 3)
   (let ((i 0) x y z)
     (list
       (nconc (progn (setf x (incf i)) (copy-list '(a b c)))
              (progn (setf y (incf i)) (copy-list '(d e f)))
              (progn (setf z (incf i)) (copy-list '(g h i))))
       i x y z))))

(define-test nconc.apply.1
  (let ((list1 '(1 2))
        (list2 '(3 4)))
    (assert-equal '(1 2 3 4)
                  (apply (cadr (list 'a #'nconc))
                         (copy-tree (list list1 list2))))
    (assert-equal '(1 2 3 4)
                  (apply (cadr (list 'a 'nconc))
                         (copy-tree (list list1 list2))))))

