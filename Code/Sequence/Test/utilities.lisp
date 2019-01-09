(cl:in-package #:sicl-sequence-test)

(defun relevant-elements-1 ()
  (let ((v1 #(a b c))
        (result '()))
    (sicl-sequence::for-each-relevant-element
        (element index v1 0 3 nil)
      (push element result)
      (push index result))
    (assert (equal (reverse result) '(a 0 b 1 c 2)))))

(defun relevant-elements-2 ()
  (let ((v1 #(a b c))
        (result '()))
    (sicl-sequence::for-each-relevant-element
        (element index v1 0 2 nil)
      (push element result)
      (push index result))
    (assert (equal (reverse result) '(a 0 b 1)))))

(defun relevant-elements-3 ()
  (let ((v1 #(a b c))
        (result '()))
    (sicl-sequence::for-each-relevant-element
        (element index v1 0 1 nil)
      (push element result)
      (push index result))
    (assert (equal (reverse result) '(a 0)))))

(defun relevant-elements-4 ()
  (let ((v1 #(a b c))
        (result '()))
    (sicl-sequence::for-each-relevant-element
        (element index v1 0 0 nil)
      (push element result)
      (push index result))
    (assert (equal (reverse result) '()))))

(defun relevant-elements-5 ()
  (let ((v1 #(a b c))
        (result '()))
    (sicl-sequence::for-each-relevant-element
        (element index v1 1 3 nil)
      (push element result)
      (push index result))
    (assert (equal (reverse result) '(b 1 c 2)))))

(defun relevant-elements-6 ()
  (let ((v1 #(a b c))
        (result '()))
    (sicl-sequence::for-each-relevant-element
        (element index v1 2 3 nil)
      (push element result)
      (push index result))
    (assert (equal (reverse result) '(c 2)))))

(defun relevant-elements-7 ()
  (let ((v1 #(a b c))
        (result '()))
    (sicl-sequence::for-each-relevant-element
        (element index v1 3 3 nil)
      (push element result)
      (push index result))
    (assert (equal (reverse result) '()))))

(defun relevant-elements-8 ()
  (let ((v1 #(a b c))
        (result '()))
    (sicl-sequence::for-each-relevant-element
        (element index v1 0 3 t)
      (push element result)
      (push index result))
    (assert (equal (reverse result) '(c 2 b 1 a 0)))))

(defun relevant-elements-9 ()
  (let ((v1 #(a b c))
        (result '()))
    (sicl-sequence::for-each-relevant-element
        (element index v1 0 2 t)
      (push element result)
      (push index result))
    (assert (equal (reverse result) '(b 1 a 0)))))

(defun relevant-elements-10 ()
  (let ((v1 #(a b c))
        (result '()))
    (sicl-sequence::for-each-relevant-element
        (element index v1 0 1 t)
      (push element result)
      (push index result))
    (assert (equal (reverse result) '(a 0)))))

(defun relevant-elements-11 ()
  (let ((v1 #(a b c))
        (result '()))
    (sicl-sequence::for-each-relevant-element
        (element index v1 0 0 t)
      (push element result)
      (push index result))
    (assert (equal (reverse result) '()))))

(defun relevant-elements-12 ()
  (let ((v1 #(a b c))
        (result '()))
    (sicl-sequence::for-each-relevant-element
        (element index v1 1 3 t)
      (push element result)
      (push index result))
    (assert (equal (reverse result) '(c 2 b 1)))))

(defun relevant-elements-13 ()
  (let ((v1 #(a b c))
        (result '()))
    (sicl-sequence::for-each-relevant-element
        (element index v1 2 3 t)
      (push element result)
      (push index result))
    (assert (equal (reverse result) '(c 2)))))

(defun relevant-elements-14 ()
  (let ((v1 #(a b c))
        (result '()))
    (sicl-sequence::for-each-relevant-element
        (element index v1 3 3 t)
      (push element result)
      (push index result))
    (assert (equal (reverse result) '()))))

(defun relevant-elements ()
  (relevant-elements-1)
  (relevant-elements-2)
  (relevant-elements-3)
  (relevant-elements-4)
  (relevant-elements-5)
  (relevant-elements-6)
  (relevant-elements-7)
  (relevant-elements-8)
  (relevant-elements-9)
  (relevant-elements-10)
  (relevant-elements-11)
  (relevant-elements-12)
  (relevant-elements-13)
  (relevant-elements-14))

(defun relevant-conses-1 ()
  (let ((l '(a b c))
        (result '()))
    (sicl-sequence::for-each-relevant-cons
        (cons index l 0 3 nil)
      (push (car cons) result)
      (push index result))
    (assert (equal (reverse result) '(a 0 b 1 c 2)))))

(defun relevant-conses-2 ()
  (let ((l '(a b c))
        (result '()))
    (sicl-sequence::for-each-relevant-cons
        (cons index l 0 2 nil)
      (push (car cons) result)
      (push index result))
    (assert (equal (reverse result) '(a 0 b 1)))))

(defun relevant-conses-3 ()
  (let ((l '(a b c))
        (result '()))
    (sicl-sequence::for-each-relevant-cons
        (cons index l 0 1 nil)
      (push (car cons) result)
      (push index result))
    (assert (equal (reverse result) '(a 0)))))

(defun relevant-conses-4 ()
  (let ((l '(a b c))
        (result '()))
    (sicl-sequence::for-each-relevant-cons
        (cons index l 0 0 nil)
      (push (car cons) result)
      (push index result))
    (assert (equal (reverse result) '()))))

(defun relevant-conses-5 ()
  (let ((l '(a b c))
        (result '()))
    (sicl-sequence::for-each-relevant-cons
        (cons index l 1 3 nil)
      (push (car cons) result)
      (push index result))
    (assert (equal (reverse result) '(b 1 c 2)))))

(defun relevant-conses-6 ()
  (let ((l '(a b c))
        (result '()))
    (sicl-sequence::for-each-relevant-cons
        (cons index l 2 3 nil)
      (push (car cons) result)
      (push index result))
    (assert (equal (reverse result) '(c 2)))))

(defun relevant-conses-7 ()
  (let ((l '(a b c))
        (result '()))
    (sicl-sequence::for-each-relevant-cons
        (cons index l 3 3 nil)
      (push (car cons) result)
      (push index result))
    (assert (equal (reverse result) '()))))

(defun relevant-conses-8 ()
  (let ((l '(a b c))
        (result '()))
    (sicl-sequence::for-each-relevant-cons
        (cons index l 0 3 t)
      (push (car cons) result)
      (push index result))
    (assert (equal (reverse result) '(c 2 b 1 a 0)))))

(defun relevant-conses-9 ()
  (let ((l '(a b c))
        (result '()))
    (sicl-sequence::for-each-relevant-cons
        (cons index l 0 2 t)
      (push (car cons) result)
      (push index result))
    (assert (equal (reverse result) '(b 1 a 0)))))

(defun relevant-conses-10 ()
  (let ((l '(a b c))
        (result '()))
    (sicl-sequence::for-each-relevant-cons
        (cons index l 0 1 t)
      (push (car cons) result)
      (push index result))
    (assert (equal (reverse result) '(a 0)))))

(defun relevant-conses-11 ()
  (let ((l '(a b c))
        (result '()))
    (sicl-sequence::for-each-relevant-cons
        (cons index l 0 0 t)
      (push (car cons) result)
      (push index result))
    (assert (equal (reverse result) '()))))

(defun relevant-conses-12 ()
  (let ((l '(a b c))
        (result '()))
    (sicl-sequence::for-each-relevant-cons
        (cons index l 1 3 t)
      (push (car cons) result)
      (push index result))
    (assert (equal (reverse result) '(c 2 b 1)))))

(defun relevant-conses-13 ()
  (let ((l '(a b c))
        (result '()))
    (sicl-sequence::for-each-relevant-cons
        (cons index l 2 3 t)
      (push (car cons) result)
      (push index result))
    (assert (equal (reverse result) '(c 2)))))

(defun relevant-conses-14 ()
  (let ((l '(a b c))
        (result '()))
    (sicl-sequence::for-each-relevant-cons
        (cons index l 3 3 t)
      (push (car cons) result)
      (push index result))
    (assert (equal (reverse result) '()))))

(defun relevant-conses-15 ()
  (let ((l '(a b c))
        (result '()))
    (sicl-sequence::for-each-relevant-cons
        (cons index l 0 nil nil)
      (push (car cons) result)
      (push index result))
    (assert (equal (reverse result) '(a 0 b 1 c 2)))))

(defun relevant-conses-16 ()
  (let ((l '(a b c))
        (result '()))
    (sicl-sequence::for-each-relevant-cons
        (cons index l 1 nil t)
      (push (car cons) result)
      (push index result))
    (assert (equal (reverse result) '(c 2 b 1)))))

(defun relevant-conses-17 ()
  (let ((l '(a b c))
        (result '()))
    (sicl-sequence::for-each-relevant-cons
        (cons index l 2 nil t)
      (push (car cons) result)
      (push index result))
    (assert (equal (reverse result) '(c 2)))))

(defun relevant-conses-18 ()
  (let ((l '(a b c))
        (result '()))
    (sicl-sequence::for-each-relevant-cons
        (cons index l 3 nil t)
      (push (car cons) result)
      (push index result))
    (assert (equal (reverse result) '()))))

(defun relevant-conses ()
  (relevant-conses-1)
  (relevant-conses-2)
  (relevant-conses-3)
  (relevant-conses-4)
  (relevant-conses-5)
  (relevant-conses-6)
  (relevant-conses-7)
  (relevant-conses-8)
  (relevant-conses-9)
  (relevant-conses-10)
  (relevant-conses-11)
  (relevant-conses-12)
  (relevant-conses-13)
  (relevant-conses-14)
  (relevant-conses-15)
  (relevant-conses-16)
  (relevant-conses-17)
  (relevant-conses-18))
