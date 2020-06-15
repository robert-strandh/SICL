;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sun Aug 25 13:06:54 2002
;;;; Contains: Tests for SEARCH on strings

(in-package #:sicl-sequence-test)



;;; The next test was busted due to to a stupid cut and paste
;;; error.  The loop terminates immediately, doing nothing
;;; useful. -- PFD
#|
(deftest search-string.1
  (let ((target *searched-string*)
        (pat #(a)))
    (loop for i from 0 to (1- (length target))
          for tail on target
          always
          (let ((pos (search pat tail)))
            (search-check pat tail pos))))
  t)
|#

(deftest search-string.2
  (let ((target *searched-string*)
        (pat #(a)))
    (loop for i from 1 to (length target)
          always
          (let ((pos (search pat target :end2 i :from-end t)))
            (search-check pat target pos :end2 i :from-end t))))
  t)

(deftest search-string.3
  (let ((target *searched-string*))
    (loop for pat in *pattern-substrings*
          for pos = (search pat target)
          unless (search-check pat target pos)
          collect pat))
  nil)

(deftest search-string.4
  (let ((target *searched-string*))
    (loop for pat in *pattern-substrings*
          for pos = (search pat target :from-end t)
          unless (search-check pat target pos :from-end t)
          collect pat))
  nil)

(deftest search-string.5
  (let ((target *searched-string*))
    (loop for pat in *pattern-substrings*
          for pos = (search pat target :start2 25 :end2 75)
          unless (search-check pat target pos :start2 25 :end2 75)
          collect pat))
  nil)

(deftest search-string.6
  (let ((target *searched-string*))
    (loop for pat in *pattern-substrings*
          for pos = (search pat target :from-end t :start2 25 :end2 75)
          unless (search-check pat target pos :from-end t
                               :start2 25 :end2 75)
          collect pat))
  nil)

(deftest search-string.7
  (let ((target *searched-string*))
    (loop for pat in *pattern-substrings*
          for pos = (search pat target :start2 20)
          unless (search-check pat target pos :start2 20)
          collect pat))
  nil)

(deftest search-string.8
  (let ((target *searched-string*))
    (loop for pat in *pattern-substrings*
          for pos = (search pat target :from-end t :start2 20)
          unless (search-check pat target pos :from-end t
                               :start2 20)
          collect pat))
  nil)

(deftest search-string.9
  (flet ((%f (x) (case x ((#\0 a) 'c) ((#\1 b) 'd) (t nil))))
    (let ((target *searched-string*))
      (loop for pat in *pattern-sublists*
            for pos = (search pat target :start2 20 :key #'%f)
            unless (search-check pat target pos :start2 20 :key #'%f)
            collect pat)))
  nil)

(deftest search-string.10
  (let ((target *searched-string*))
    (loop for pat in *pattern-substrings*
          for pos = (search pat target :start2 20 :test (complement #'eql))
          unless (search-check pat target pos :start2 20
                               :test (complement #'eql))
          collect pat))
  nil)

(deftest search-string.11
  (let ((target *searched-string*))
    (loop for pat in *pattern-substrings*
          for pos = (search pat target :from-end t :start2 20 :test-not #'eql)
          unless (search-check pat target pos :from-end t
                               :start2 20 :test (complement #'eql))
          collect pat))
  nil)

(deftest search-string.13
  (let ((target *searched-string*))
    (loop for pat in *pattern-substrings*
          when (and (> (length pat) 0)
                    (let ((pos (search pat target :start1 1
                                       :test (complement #'eql))))
                      (not (search-check pat target pos
                                         :start1 1
                                         :test (complement #'eql)))))
          collect pat))
  nil)

(deftest search-string.14
  (let ((target *searched-string*))
    (loop for pat in *pattern-substrings*
          when (let ((len (length pat)))
                 (and (> len 0)
                      (let ((pos (search pat target :end1 (1- len)
                                         :test (complement #'eql))))
                      (not (search-check pat target pos
                                         :end1 (1- len)
                                         :test (complement #'eql))))))
          collect pat))
  nil)

(deftest search-string.15
  (let ((a (make-array '(10) :initial-contents "abbaaababb"
                       :fill-pointer 5
                       :element-type 'character)))
    (values
     (search "a" a)
     (search "a" a :from-end t)
     (search "ab" a)
     (search "ab" a :from-end t)
     (search "aba" a)
     (search "aba" a :from-end t)))
  0 4 0 0 nil nil)

(deftest search-string.16
  (let ((pat (make-array '(3) :initial-contents '(#\a #\b #\a)
                         :fill-pointer 1))
        (a "abbaa"))
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

(deftest search-string.17
  (let ((pat "m")
        (target '"adgmnpq"))
    (search pat target :test #'char<))
  4)

(deftest search-string.18
  (let ((pat "m")
        (target '"adgmnpq"))
    (search pat target :test-not #'char>=))
  4)

;;; Specialized strings

(deftest search-string.19
  (do-special-strings
   (s "a" nil)
   (assert (eql (search s "xyza123apqr") 3))
   (assert (eql (search s "xyza1a3apqr" :start2 4) 5))
   (assert (eql (search s "xyza123apqr" :from-end t) 7)))
  nil)

(deftest search-string.20
  (do-special-strings
   (s "xababcdefabc123ababc18" nil)
   (assert (eql (search "abc" s) 3))
   (assert (eql (search "abc" s :start2 4) 9))
   (assert (eql (search "abc" s :from-end t) 17)))
  nil)

