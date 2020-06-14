;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sun Aug 25 13:06:54 2002
;;;; Contains: Tests for SEARCH on vectors

(in-package #:sicl-sequence-test)



(deftest search-vector.1
  (let ((target *searched-vector*)
        (pat #(a)))
    (loop for i from 0 to (1- (length target))
          for tail = (subseq target i)
          always
          (let ((pos (search pat tail)))
            (search-check pat tail pos))))
  t)

(deftest search-vector.2
  (let ((target *searched-vector*)
        (pat #(a)))
    (loop for i from 1 to (length target)
          always
          (let ((pos (search pat target :end2 i :from-end t)))
            (search-check pat target pos :end2 i :from-end t))))
  t)

(deftest search-vector.3
  (let ((target *searched-vector*))
    (loop for pat in *pattern-subvectors*
          for pos = (search pat target)
          unless (search-check pat target pos)
          collect pat))
  nil)

(deftest search-vector.4
  (let ((target *searched-vector*))
    (loop for pat in *pattern-subvectors*
          for pos = (search pat target :from-end t)
          unless (search-check pat target pos :from-end t)
          collect pat))
  nil)

(deftest search-vector.5
  (let ((target *searched-vector*))
    (loop for pat in *pattern-subvectors*
          for pos = (search pat target :start2 25 :end2 75)
          unless (search-check pat target pos :start2 25 :end2 75)
          collect pat))
  nil)

(deftest search-vector.6
  (let ((target *searched-vector*))
    (loop for pat in *pattern-subvectors*
          for pos = (search pat target :from-end t :start2 25 :end2 75)
          unless (search-check pat target pos :from-end t
                               :start2 25 :end2 75)
          collect pat))
  nil)

(deftest search-vector.7
  (let ((target *searched-vector*))
    (loop for pat in *pattern-subvectors*
          for pos = (search pat target :start2 20)
          unless (search-check pat target pos :start2 20)
          collect pat))
  nil)

(deftest search-vector.8
  (let ((target *searched-vector*))
    (loop for pat in *pattern-subvectors*
          for pos = (search pat target :from-end t :start2 20)
          unless (search-check pat target pos :from-end t
                               :start2 20)
          collect pat))
  nil)

(deftest search-vector.9
  (let ((target (map 'vector #'(lambda (x) (sublis '((a . 1) (b . 2)) x))
                     *searched-list*)))
    (loop for pat in (mapcar #'(lambda (x)
                                 (map 'vector
                                      #'(lambda (y)
                                          (sublis '((a . 3) (b . 4)) y))
                                      x))
                             *pattern-sublists*)
          for pos = (search pat target :start2 20 :key #'evenp)
          unless (search-check pat target pos :start2 20 :key #'evenp)
          collect pat))
  nil)

(deftest search-vector.10
  (let ((target (map 'vector #'(lambda (x) (sublis '((a . 1) (b . 2)) x))
                     *searched-list*)))
    (loop for pat in (mapcar #'(lambda (x)
                                 (map 'vector
                                      #'(lambda (y)
                                          (sublis '((a . 3) (b . 4)) y))
                                      x))
                             *pattern-sublists*)
          for pos = (search pat target :from-end t :start2 20 :key 'oddp)
          unless (search-check pat target pos :from-end t
                               :start2 20 :key 'oddp)
          collect pat))
  nil)

(deftest search-vector.11
  (let ((target *searched-vector*))
    (loop for pat in *pattern-subvectors*
          for pos = (search pat target :start2 20 :test (complement #'eql))
          unless (search-check pat target pos :start2 20
                               :test (complement #'eql))
          collect pat))
  nil)

(deftest search-vector.12
  (let ((target *searched-vector*))
    (loop for pat in *pattern-subvectors*
          for pos = (search pat target :from-end t :start2 20 :test-not #'eql)
          unless (search-check pat target pos :from-end t
                               :start2 20 :test (complement #'eql))
          collect pat))
  nil)

(deftest search-vector.13
  (let ((target *searched-vector*))
    (loop for pat in *pattern-subvectors*
          when (and (> (length pat) 0)
                    (let ((pos (search pat target :start1 1
                                       :test (complement #'eql))))
                      (not (search-check pat target pos
                                         :start1 1
                                         :test (complement #'eql)))))
          collect pat))
  nil)

(deftest search-vector.14
  (let ((target *searched-vector*))
    (loop for pat in *pattern-subvectors*
          when (let ((len (length pat)))
                 (and (> len 0)
                      (let ((pos (search pat target :end1 (1- len)
                                         :test (complement #'eql))))
                      (not (search-check pat target pos
                                         :end1 (1- len)
                                         :test (complement #'eql))))))
          collect pat))
  nil)

(deftest search-vector.15
  (let ((a (make-array '(10) :initial-contents '(a b b a a a b a b b)
                       :fill-pointer 5)))
    (values
     (search '(a) a)
     (search '(a) a :from-end t)
     (search '(a b) a)
     (search '(a b) a :from-end t)
     (search '(a b a) a)
     (search '(a b a) a :from-end t)))
  0 4 0 0 nil nil)

(deftest search-vector.16
  (let ((pat (make-array '(3) :initial-contents '(a b a)
                         :fill-pointer 1))
        (a #(a b b a a)))
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

(deftest search-vector.17
  (let ((pat #(10))
        (target #(1 4 6 10 15 20)))
    (search pat target :test #'<))
  4)

(deftest search-vector.18
  (let ((pat #(10))
        (target #(1 4 6 10 15 20)))
    (search pat target :test-not #'>=))
  4)
