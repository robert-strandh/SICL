;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Thu Jul 17 06:42:27 2003
;;;; Contains: Routines for testing SORT, NSORT

(in-package #:sicl-sequence-test)

(defun my-numeric-sort (list)
  "Sort (nondestructively) a list of reals."
  (if (null (cdr list))
      list
    (let* ((len2 (ash (length list) -1))
           (l1 (my-numeric-sort (subseq list 0 len2)))
           (l2 (my-numeric-sort (subseq list len2))))
      (my-numeric-merge l1 l2))))

(defun my-numeric-merge (l1 l2)
  (cond
   ((null l1) l2)
   ((null l2) l1)
   ((<= (car l1) (car l2))
    (cons (car l1) (my-numeric-merge (cdr l1) l2)))
   (t
    (cons (car l2) (my-numeric-merge l1 (cdr l2))))))

(defun generate-random-sort-test (n m)
  (loop for i below n collect (random m)))

(defun random-sort-test (n m reps)
  (loop
   for i below reps
   for list = (generate-random-sort-test (random n) m)
   unless (equal (my-numeric-sort list)
                 (sort (copy-seq list) #'<))
   collect list))

(defun random-stable-sort-test (n m reps)
  (loop
   for i below reps
   for list = (generate-random-sort-test (random n) m)
   unless (equal (my-numeric-sort list)
                 (stable-sort (copy-seq list) #'<))
   collect list))
