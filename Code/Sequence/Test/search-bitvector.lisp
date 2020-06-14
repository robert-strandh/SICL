;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sun Aug 25 13:06:54 2002
;;;; Contains: Tests for SEARCH on bit vectors

(in-package #:sicl-sequence-test)



(deftest search-bitvector.1
  (let ((target *searched-bitvector*)
        (pat #*0))
    (loop for i from 0 to (1- (length target))
          for tail = (subseq target i)
          always
          (let ((pos (search pat tail)))
            (search-check pat tail pos))))
  t)

(deftest search-bitvector.2
  (let ((target *searched-bitvector*)
        (pat #*0))
    (loop for i from 1 to (length target)
          always
          (let ((pos (search pat target :end2 i :from-end t)))
            (search-check pat target pos :end2 i :from-end t))))
  t)

(deftest search-bitvector.3
  (let ((target *searched-bitvector*))
    (loop for pat in *pattern-subbitvectors*
          for pos = (search pat target)
          unless (search-check pat target pos)
          collect pat))
  nil)

(deftest search-bitvector.4
  (let ((target *searched-bitvector*))
    (loop for pat in *pattern-subbitvectors*
          for pos = (search pat target :from-end t)
          unless (search-check pat target pos :from-end t)
          collect pat))
  nil)

(deftest search-bitvector.5
  (let ((target *searched-bitvector*))
    (loop for pat in *pattern-subbitvectors*
          for pos = (search pat target :start2 25 :end2 75)
          unless (search-check pat target pos :start2 25 :end2 75)
          collect pat))
  nil)

(deftest search-bitvector.6
  (let ((target *searched-bitvector*))
    (loop for pat in *pattern-subbitvectors*
          for pos = (search pat target :from-end t :start2 25 :end2 75)
          unless (search-check pat target pos :from-end t
                               :start2 25 :end2 75)
          collect pat))
  nil)

(deftest search-bitvector.7
  (let ((target *searched-bitvector*))
    (loop for pat in *pattern-subbitvectors*
          for pos = (search pat target :start2 20)
          unless (search-check pat target pos :start2 20)
          collect pat))
  nil)

(deftest search-bitvector.8
  (let ((target *searched-bitvector*))
    (loop for pat in *pattern-subbitvectors*
          for pos = (search pat target :from-end t :start2 20)
          unless (search-check pat target pos :from-end t
                               :start2 20)
          collect pat))
  nil)

(deftest search-bitvector.9
  (let ((target *searched-bitvector*))
    (loop for pat in (mapcar #'(lambda (x)
                                 (map 'vector
                                      #'(lambda (y)
                                          (sublis '((a . 2) (b . 3)) y))
                                      x))
                             *pattern-sublists*)
          for pos = (search pat target :start2 20 :key #'evenp)
          unless (search-check pat target pos :start2 20 :key #'evenp)
          collect pat))
  nil)

(deftest search-bitvector.10
  (let ((target *searched-bitvector*))
    (loop for pat in (mapcar #'(lambda (x)
                                 (map 'vector
                                      #'(lambda (y)
                                          (sublis '((a . 2) (b . 3)) y))
                                      x))
                             *pattern-sublists*)
          for pos = (search pat target :from-end t :start2 20 :key 'oddp)
          unless (search-check pat target pos :from-end t
                               :start2 20 :key 'oddp)
          collect pat))
  nil)

(deftest search-bitvector.11
  (let ((target *searched-bitvector*))
    (loop for pat in *pattern-subbitvectors*
          for pos = (search pat target :start2 20 :test (complement #'eql))
          unless (search-check pat target pos :start2 20
                               :test (complement #'eql))
          collect pat))
  nil)

(deftest search-bitvector.12
  (let ((target *searched-bitvector*))
    (loop for pat in *pattern-subbitvectors*
          for pos = (search pat target :from-end t :start2 20 :test-not #'eql)
          unless (search-check pat target pos :from-end t
                               :start2 20 :test (complement #'eql))
          collect pat))
  nil)

(deftest search-bitvector.13
  (let ((target *searched-bitvector*))
    (loop for pat in *pattern-subbitvectors*
          when (and (> (length pat) 0)
                    (let ((pos (search pat target :start1 1
                                       :test (complement #'eql))))
                      (not (search-check pat target pos
                                         :start1 1
                                         :test (complement #'eql)))))
          collect pat))
  nil)

(deftest search-bitvector.14
  (let ((target *searched-bitvector*))
    (loop for pat in *pattern-subbitvectors*
          when (let ((len (length pat)))
                 (and (> len 0)
                      (let ((pos (search pat target :end1 (1- len)
                                         :test (complement #'eql))))
                      (not (search-check pat target pos
                                         :end1 (1- len)
                                         :test (complement #'eql))))))
          collect pat))
  nil)

(deftest search-bitvector.15
  (let ((a (make-array '(10) :initial-contents '(0 1 1 0 0 0 1 0 1 1)
                       :fill-pointer 5
                       :element-type 'bit)))
    (values
     (search #*0 a)
     (search #*0 a :from-end t)
     (search #*01 a)
     (search #*01 a :from-end t)
     (search #*010 a)
     (search #*010 a :from-end t)))
  0 4 0 0 nil nil)

(deftest search-bitvector.16
  (let ((pat (make-array '(3) :initial-contents '(0 1 0)
                         :fill-pointer 1))
        (a #*01100))
    (values
     (search pat a)
     (search pat a :from-end t)
     (progn
       (setf (fill-pointer pat) 2)
       (search pat a))
     (search pat a :from-end t)
     (progn
       (setf (fill-pointer pat) 3)
       (search pat a))
     (search pat a :from-end t)))
  0 4 0 0 nil nil)

;; Order of test, test-not

(deftest search-bitvector.17
  (let ((pat #*10)
        (target #*000011))
    (search pat target :test #'<=))
  4)

(deftest search-bitvector.18
  (let ((pat #*10)
        (target #*000011))
    (search pat target :test-not #'>))
  4)

