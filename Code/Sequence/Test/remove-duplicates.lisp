;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sun Sep 29 20:49:47 2002
;;;; Contains: Tests for REMOVE-DUPLICATES, DELETE-DUPLICATES

(in-package #:sicl-sequence-test)




(deftest random-remove-duplicates
  (loop for result = (random-test-remove-dups (1+ (random 20)))
        repeat 1000
        unless (eq result t)
        collect result)
  nil)

(deftest random-delete-duplicates
  (loop for result = (random-test-remove-dups (1+ (random 20)) nil)
        repeat 1000
        unless (eq result t)
        collect result)
  nil)

;;; Look for :KEY NIL bugs

(deftest remove-duplicates.1
  (let* ((orig '(1 2 3 4 1 3 4 1 2 5 6 2 7))
         (x (copy-seq orig))
         (y (remove-duplicates x :key nil)))
    (and (equalp orig x) y))
  (3 4 1 5 6 2 7))

(deftest delete-duplicates.1
  (let* ((orig '(1 2 3 4 1 3 4 1 2 5 6 2 7))
         (x (copy-seq orig))
         (y (delete-duplicates x :key nil)))
    y)
  (3 4 1 5 6 2 7))

(defharmless remove-duplicates.test-and-test-not.1
  (remove-duplicates (list 'a 'b 'c 'd 'a 'e 'f 'd 'g) :test #'eql
                     :test-not #'eql))

(defharmless remove-duplicates.test-and-test-not.2
  (remove-duplicates (list 'a 'b 'c 'd 'a 'e 'f 'd 'g) :test-not #'eql
                     :test #'eql))

(defharmless delete-duplicates.test-and-test-not.1
  (delete-duplicates (list 'a 'b 'c 'd 'a 'e 'f 'd 'g) :test #'eql
                     :test-not #'eql))

(defharmless delete-duplicates.test-and-test-not.2
  (delete-duplicates (list 'a 'b 'c 'd 'a 'e 'f 'd 'g) :test-not #'eql
                     :test #'eql))

;;; Const fold tests

(def-fold-test remove-duplicates.fold.1 (remove-duplicates '(1 2 3 3)))
(def-fold-test remove-duplicates.fold.2 (remove-duplicates #(1 2 3 3)))
(def-fold-test remove-duplicates.fold.3 (remove-duplicates #*0011))
(def-fold-test remove-duplicates.fold.4 (remove-duplicates "1233"))

;;; Order of evaluation tests

(deftest remove-duplicates.order.1
  (let ((i 0) a b c d e f)
    (values
     (remove-duplicates
      (progn (setf a (incf i)) (list 1 2 3 1 3 1 2 4))
      :from-end (progn (setf b (incf i)) nil)
      :start (progn (setf c (incf i)) 0)
      :end (progn (setf d (incf i)) nil)
      :key (progn (setf e (incf i)) #'identity)
      :test (progn (setf f (incf i)) #'=)
      )
     i a b c d e f))
  (3 1 2 4) 6 1 2 3 4 5 6)

(deftest remove-duplicates.order.2
  (let ((i 0) a b c d e f)
    (values
     (remove-duplicates
      (progn (setf a (incf i)) (list 1 2 3 1 3 1 2 4))
      :test-not (progn (setf b (incf i)) #'/=)
      :key (progn (setf c (incf i)) #'identity)
      :end (progn (setf d (incf i)) nil)
      :start (progn (setf e (incf i)) 0)
      :from-end (progn (setf f (incf i)) nil)
      )
     i a b c d e f))
  (3 1 2 4) 6 1 2 3 4 5 6)


;;; Keyword tests

(deftest remove-duplicates.allow-other-keys.1
  (remove-duplicates '(1 2 3 4 2 7 8 1 5) :allow-other-keys t)
  (3 4 2 7 8 1 5))

(deftest remove-duplicates.allow-other-keys.2
  (remove-duplicates '(1 2 3 4 2 7 8 1 5) :allow-other-keys nil)
  (3 4 2 7 8 1 5))

(deftest remove-duplicates.allow-other-keys.3
  (remove-duplicates '(1 2 3 4 2 7 8 1 5) :bad t :allow-other-keys t)
  (3 4 2 7 8 1 5))

(deftest remove-duplicates.allow-other-keys.4
  (remove-duplicates '(1 2 3 4 2 7 8 1 5) :allow-other-keys t :bad t)
  (3 4 2 7 8 1 5))

(deftest remove-duplicates.allow-other-keys.5
  (remove-duplicates '(1 2 3 4 2 7 8 1 5) :bad t
                     :allow-other-keys t :allow-other-keys nil)
  (3 4 2 7 8 1 5))

(deftest remove-duplicates.allow-other-keys.6
  (remove-duplicates '(1 2 3 4 2 7 8 1 5)
                     :allow-other-keys t :bad t :allow-other-keys nil)
  (3 4 2 7 8 1 5))

(deftest remove-duplicates.allow-other-keys.7
  (remove-duplicates '(1 2 3 4 2 7 8 1 5)
                     :allow-other-keys t :allow-other-keys nil :bad t)
  (3 4 2 7 8 1 5))

(deftest remove-duplicates.allow-other-keys.8
  (remove-duplicates '(1 2 3 4 2 7 8 1 5)
                     :allow-other-keys t :from-end t)
  (1 2 3 4 7 8 5))

(deftest remove-duplicates.keywords.1
  (remove-duplicates '(1 2 3 4 2 7 8 1 5) :from-end t :from-end nil)
  (1 2 3 4 7 8 5))


(deftest delete-duplicates.allow-other-keys.1
  (delete-duplicates (list 1 2 3 4 2 7 8 1 5) :allow-other-keys t)
  (3 4 2 7 8 1 5))

(deftest delete-duplicates.allow-other-keys.2
  (delete-duplicates (list 1 2 3 4 2 7 8 1 5) :allow-other-keys nil)
  (3 4 2 7 8 1 5))

(deftest delete-duplicates.allow-other-keys.3
  (delete-duplicates (list 1 2 3 4 2 7 8 1 5) :bad t :allow-other-keys t)
  (3 4 2 7 8 1 5))

(deftest delete-duplicates.allow-other-keys.4
  (delete-duplicates (list 1 2 3 4 2 7 8 1 5) :allow-other-keys t :bad t)
  (3 4 2 7 8 1 5))

(deftest delete-duplicates.allow-other-keys.5
  (delete-duplicates (list 1 2 3 4 2 7 8 1 5) :bad t
                     :allow-other-keys t :allow-other-keys nil)
  (3 4 2 7 8 1 5))

(deftest delete-duplicates.allow-other-keys.6
  (delete-duplicates (list 1 2 3 4 2 7 8 1 5)
                     :allow-other-keys t :bad t :allow-other-keys nil)
  (3 4 2 7 8 1 5))

(deftest delete-duplicates.allow-other-keys.7
  (delete-duplicates (list 1 2 3 4 2 7 8 1 5)
                     :allow-other-keys t :allow-other-keys nil :bad t)
  (3 4 2 7 8 1 5))

(deftest delete-duplicates.allow-other-keys.8
  (delete-duplicates (list 1 2 3 4 2 7 8 1 5)
                     :allow-other-keys t :from-end t)
  (1 2 3 4 7 8 5))

(deftest delete-duplicates.keywords.1
  (delete-duplicates (list 1 2 3 4 2 7 8 1 5) :from-end t :from-end nil)
  (1 2 3 4 7 8 5))

;;; Order of evaluation tests

(deftest delete-duplicates.order.1
  (let ((i 0) a b c d e f)
    (values
     (delete-duplicates
      (progn (setf a (incf i)) (list 1 2 3 1 3 1 2 4))
      :from-end (progn (setf b (incf i)) nil)
      :start (progn (setf c (incf i)) 0)
      :end (progn (setf d (incf i)) nil)
      :key (progn (setf e (incf i)) #'identity)
      :test (progn (setf f (incf i)) #'=)
      )
     i a b c d e f))
  (3 1 2 4) 6 1 2 3 4 5 6)

(deftest delete-duplicates.order.2
  (let ((i 0) a b c d e f)
    (values
     (delete-duplicates
      (progn (setf a (incf i)) (list 1 2 3 1 3 1 2 4))
      :test-not (progn (setf b (incf i)) #'/=)
      :key (progn (setf c (incf i)) #'identity)
      :end (progn (setf d (incf i)) nil)
      :start (progn (setf e (incf i)) 0)
      :from-end (progn (setf f (incf i)) nil)
      )
     i a b c d e f))
  (3 1 2 4) 6 1 2 3 4 5 6)

;;; Error cases

(deftest remove-duplicates.error.1
  (signals-error (remove-duplicates) program-error)
  t)

(deftest remove-duplicates.error.2
  (signals-error (remove-duplicates nil :start) program-error)
  t)

(deftest remove-duplicates.error.3
  (signals-error (remove-duplicates nil 'bad t) program-error)
  t)

(deftest remove-duplicates.error.4
  (signals-error (remove-duplicates nil 'bad t :allow-other-keys nil) program-error)
  t)

(deftest remove-duplicates.error.5
  (signals-error (remove-duplicates nil 1 2) program-error)
  t)

(deftest remove-duplicates.error.6
  (signals-error (remove-duplicates (list 'a 'b 'c) :test #'identity) program-error)
  t)

(deftest remove-duplicates.error.7
  (signals-error (remove-duplicates (list 'a 'b 'c) :test-not #'identity) program-error)
  t)

(deftest remove-duplicates.error.8
  (signals-error (remove-duplicates (list 'a 'b 'c) :key #'cons) program-error)
  t)

(deftest remove-duplicates.error.9
  (signals-error (remove-duplicates (list 'a 'b 'c) :key #'car) type-error)
  t)

(deftest remove-duplicates.error.10
  (check-type-error #'remove-duplicates #'sequencep)
  nil)

;;;

(deftest delete-duplicates.error.1
  (signals-error (delete-duplicates) program-error)
  t)

(deftest delete-duplicates.error.2
  (signals-error (delete-duplicates nil :start) program-error)
  t)

(deftest delete-duplicates.error.3
  (signals-error (delete-duplicates nil 'bad t) program-error)
  t)

(deftest delete-duplicates.error.4
  (signals-error (delete-duplicates nil 'bad t :allow-other-keys nil) program-error)
  t)

(deftest delete-duplicates.error.5
  (signals-error (delete-duplicates nil 1 2) program-error)
  t)

(deftest delete-duplicates.error.6
  (signals-error (delete-duplicates (list 'a 'b 'c) :test #'identity) program-error)
  t)

(deftest delete-duplicates.error.7
  (signals-error (delete-duplicates (list 'a 'b 'c) :test-not #'identity) program-error)
  t)

(deftest delete-duplicates.error.8
  (signals-error (delete-duplicates (list 'a 'b 'c) :key #'cons) program-error)
  t)

(deftest delete-duplicates.error.9
  (signals-error (delete-duplicates (list 'a 'b 'c) :key #'car) type-error)
  t)

(deftest delete-duplicates.error.10
  (check-type-error #'delete-duplicates #'sequencep)
  nil)

;;; Specialized string tests

(deftest remove-duplicates.string.1
  (do-special-strings
   (s "abcadefabgz" nil)
   (let ((s2 (remove-duplicates s)))
     (assert (string= s "abcadefabgz"))
     (assert (equal (array-element-type s)
                    (array-element-type s2)))
     (assert (string= s2 "cdefabgz"))))
  nil)

(deftest remove-duplicates.string.2
  (do-special-strings
   (s "abcadefabgz" nil)
   (let ((s2 (remove-duplicates s :from-end t)))
     (assert (string= s "abcadefabgz"))
     (assert (equal (array-element-type s)
                    (array-element-type s2)))
     (assert (string= s2 "abcdefgz"))))
  nil)

(deftest delete-duplicates.string.1
  (do-special-strings
   (s "abcadefabgz" nil)
   (let ((aet (array-element-type s))
         (s2 (delete-duplicates s)))
     (assert (equal aet (array-element-type s2)))
     (assert (string= s2 "cdefabgz"))))
  nil)

(deftest delete-duplicates.string.2
  (do-special-strings
   (s "abcadefabgz" nil)
   (let ((aet (array-element-type s))
         (s2 (delete-duplicates s :from-end t)))
     (assert (equal aet (array-element-type s2)))
     (assert (string= s2 "abcdefgz"))))
  nil)

;;; Order of elements kept under EQUAL, EQUALP tests

(deftest remove-duplicates.2
  (let* ((x (list 'a))
         (y (list 'a))
         (result (remove-duplicates (list x y) :test 'equal)))
    (values
     result
     (notnot (eql (car result) x))
     (notnot (eql (car result) y))))
  ((a)) nil t)

(deftest remove-duplicates.2a
  (let* ((x (list 'a))
         (y (list 'a))
         (result (remove-duplicates (list x 'x y) :test 'equal)))
    (values
     result
     (notnot (eql (cadr result) x))
     (notnot (eql (cadr result) y))))
  (x (a)) nil t)

(deftest remove-duplicates.3
  (let* ((x (list 'a))
         (y (list 'a))
         (result (remove-duplicates (list x y) :test 'equal :from-end t)))
    (values
     result
     (notnot (eql (car result) x))
     (notnot (eql (car result) y))))
  ((a)) t nil)

(deftest remove-duplicates.3a
  (let* ((x (list 'a))
         (y (list 'a))
         (result (remove-duplicates (list x 'u 'v y) :test 'equal :from-end t)))
    (values
     result
     (notnot (eql (car result) x))
     (notnot (eql (car result) y))))
  ((a) u v) t nil)

(deftest remove-duplicates.4
  (let* ((x (list 'a))
         (y (list 'a))
         (result (remove-duplicates (list x y) :test 'equalp)))
    (values
     result
     (notnot (eql (car result) x))
     (notnot (eql (car result) y))))
  ((a)) nil t)

(deftest remove-duplicates.5
  (let* ((x (list 'a))
         (y (list 'a))
         (result (remove-duplicates (list x y) :test 'equalp :from-end t)))
    (values
     result
     (notnot (eql (car result) x))
     (notnot (eql (car result) y))))
  ((a)) t nil)

;;; Similar, but destructive

(deftest delete-duplicates.2
  (let* ((x (list 'a))
         (y (list 'a))
         (result (delete-duplicates (list x y) :test 'equal)))
    (values
     result
     (notnot (eql (car result) x))
     (notnot (eql (car result) y))))
  ((a)) nil t)

(deftest delete-duplicates.2a
  (let* ((x (list 'a))
         (y (list 'a))
         (result (delete-duplicates (list x 'x y) :test 'equal)))
    (values
     result
     (notnot (eql (cadr result) x))
     (notnot (eql (cadr result) y))))
  (x (a)) nil t)

(deftest delete-duplicates.3
  (let* ((x (list 'a))
         (y (list 'a))
         (result (delete-duplicates (list x y) :test 'equal :from-end t)))
    (values
     result
     (notnot (eql (car result) x))
     (notnot (eql (car result) y))))
  ((a)) t nil)

(deftest delete-duplicates.3a
  (let* ((x (list 'a))
         (y (list 'a))
         (result (delete-duplicates (list x 'u 'v y) :test 'equal :from-end t)))
    (values
     result
     (notnot (eql (car result) x))
     (notnot (eql (car result) y))))
  ((a) u v) t nil)

(deftest delete-duplicates.4
  (let* ((x (list 'a))
         (y (list 'a))
         (result (delete-duplicates (list x y) :test 'equalp)))
    (values
     result
     (notnot (eql (car result) x))
     (notnot (eql (car result) y))))
  ((a)) nil t)

(deftest delete-duplicates.5
  (let* ((x (list 'a))
         (y (list 'a))
         (result (delete-duplicates (list x y) :test 'equalp :from-end t)))
    (values
     result
     (notnot (eql (car result) x))
     (notnot (eql (car result) y))))
  ((a)) t nil)








