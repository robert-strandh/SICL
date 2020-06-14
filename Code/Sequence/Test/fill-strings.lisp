;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat Aug 17 08:04:27 2002
;;;; Contains: Test cases for FILL on strings

(in-package #:sicl-sequence-test)

(deftest array-string-fill.1
  (array-string-fill-test-fn "abcde" #\Z)
  t "ZZZZZ")

(deftest array-string-fill.2
  (array-string-fill-test-fn "abcde" #\Z :start 2)
  t "abZZZ")

(deftest array-string-fill.3
  (array-string-fill-test-fn "abcde" #\Z :end 3)
  t "ZZZde")

(deftest array-string-fill.4
  (array-string-fill-test-fn "abcde" #\Z :start 1 :end 4)
  t "aZZZe")

(deftest array-string-fill.5
  (array-string-fill-test-fn "abcde" #\Z :start 2 :end 3)
  t "abZde")
