;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat Aug 24 07:22:10 2002
;;;; Contains: Tests for SEARCH on lists

(in-package #:sicl-sequence-test)



(deftest search-list.1
  (let ((target *searched-list*)
        (pat '(a)))
    (loop for i from 0 to (1- (length target))
          for tail on target
          always
          (let ((pos (search pat tail)))
            (search-check pat tail pos))))
  t)

(deftest search-list.2
  (let ((target *searched-list*)
        (pat '(a)))
    (loop for i from 1 to (length target)
          always
          (let ((pos (search pat target :end2 i :from-end t)))
            (search-check pat target pos :end2 i :from-end t))))
  t)

(deftest search-list.3
  (let ((target *searched-list*))
    (loop for pat in *pattern-sublists*
          for pos = (search pat target)
          unless (search-check pat target pos)
          collect pat))
  nil)

(deftest search-list.4
  (let ((target *searched-list*))
    (loop for pat in *pattern-sublists*
          for pos = (search pat target :from-end t)
          unless (search-check pat target pos :from-end t)
          collect pat))
  nil)

(deftest search-list.5
  (let ((target *searched-list*))
    (loop for pat in *pattern-sublists*
          for pos = (search pat target :start2 25 :end2 75)
          unless (search-check pat target pos :start2 25 :end2 75)
          collect pat))
  nil)

(deftest search-list.6
  (let ((target *searched-list*))
    (loop for pat in *pattern-sublists*
          for pos = (search pat target :from-end t :start2 25 :end2 75)
          unless (search-check pat target pos :from-end t
                               :start2 25 :end2 75)
          collect pat))
  nil)

(deftest search-list.7
  (let ((target *searched-list*))
    (loop for pat in *pattern-sublists*
          for pos = (search pat target :start2 20)
          unless (search-check pat target pos :start2 20)
          collect pat))
  nil)

(deftest search-list.8
  (let ((target *searched-list*))
    (loop for pat in *pattern-sublists*
          for pos = (search pat target :from-end t :start2 20)
          unless (search-check pat target pos :from-end t
                               :start2 20)
          collect pat))
  nil)

(deftest search-list.9
  (let ((target (sublis '((a . 1) (b . 2)) *searched-list*)))
    (loop for pat in (sublis '((a . 3) (b . 4)) *pattern-sublists*)
          for pos = (search pat target :start2 20 :key #'evenp)
          unless (search-check pat target pos :start2 20 :key #'evenp)
          collect pat))
  nil)

(deftest search-list.10
  (let ((target (sublis '((a . 1) (b . 2)) *searched-list*)))
    (loop for pat in (sublis '((a . 3) (b . 4)) *pattern-sublists*)
          for pos = (search pat target :from-end t :start2 20 :key 'oddp)
          unless (search-check pat target pos :from-end t
                               :start2 20 :key 'oddp)
          collect pat))
  nil)

(deftest search-list.11
  (let ((target *searched-list*))
    (loop for pat in *pattern-sublists*
          for pos = (search pat target :start2 20 :test (complement #'eql))
          unless (search-check pat target pos :start2 20
                               :test (complement #'eql))
          collect pat))
  nil)

(deftest search-list.12
  (let ((target *searched-list*))
    (loop for pat in *pattern-sublists*
          for pos = (search pat target :from-end t :start2 20 :test-not #'eql)
          unless (search-check pat target pos :from-end t
                               :start2 20 :test (complement #'eql))
          collect pat))
  nil)

(deftest search-list.13
  (let ((target *searched-list*))
    (loop for pat in *pattern-sublists*
          when (and (> (length pat) 0)
                    (let ((pos (search pat target :start1 1
                                       :test (complement #'eql))))
                      (not (search-check pat target pos
                                         :start1 1
                                         :test (complement #'eql)))))
          collect pat))
  nil)

(deftest search-list.14
  (let ((target *searched-list*))
    (loop for pat in *pattern-sublists*
          when (let ((len (length pat)))
                 (and (> len 0)
                      (let ((pos (search pat target :end1 (1- len)
                                         :test (complement #'eql))))
                      (not (search-check pat target pos
                                         :end1 (1- len)
                                         :test (complement #'eql))))))
          collect pat))
  nil)

;; Order of test, test-not

(deftest search-list.15
  (let ((pat '(10))
        (target '(1 4 6 10 15 20)))
    (search pat target :test #'<))
  4)

(deftest search-list.16
  (let ((pat '(10))
        (target '(1 4 6 10 15 20)))
    (search pat target :test-not #'>=))
  4)

(defharmless search.test-and-test-not.1
  (search '(b c) '(a b c d) :test #'eql :test-not #'eql))

(defharmless search.test-and-test-not.2
  (search '(b c) '(a b c d) :test-not #'eql :test #'eql))

(defharmless search.test-and-test-not.3
  (search #(b c) #(a b c d) :test #'eql :test-not #'eql))

(defharmless search.test-and-test-not.4
  (search #(b c) #(a b c d) :test-not #'eql :test #'eql))

(defharmless search.test-and-test-not.5
  (search "bc" "abcd" :test #'eql :test-not #'eql))

(defharmless search.test-and-test-not.6
  (search "bc" "abcd" :test-not #'eql :test #'eql))

(defharmless search.test-and-test-not.7
  (search #*01 #*0011 :test #'eql :test-not #'eql))

(defharmless search.test-and-test-not.8
  (search #*01 #*0011 :test-not #'eql :test #'eql))


;;; Keyword tests

(deftest search.allow-other-keys.1
  (search '(c d) '(a b c d c d e) :allow-other-keys t)
  2)

(deftest search.allow-other-keys.2
  (search '(c d) '(a b c d c d e) :allow-other-keys nil)
  2)

(deftest search.allow-other-keys.3
  (search '(c d) '(a b c d c d e) :bad t :allow-other-keys t)
  2)

(deftest search.allow-other-keys.4
  (search '(c d) '(a b c d c d e) :allow-other-keys 'foo :bad nil)
  2)

(deftest search.allow-other-keys.5
  (search '(c d) '(a b c d c d e) :bad1 1 :allow-other-keys t :bad2 2
          :allow-other-keys nil :bad3 3)
  2)

(deftest search.allow-other-keys.6
  (search '(c d) '(a b c d c d e) :allow-other-keys 'foo
          :from-end t)
  4)

(deftest search.allow-other-keys.7
  (search '(c d) '(a b c d c d e) :from-end t :allow-other-keys t)
  4)

(deftest search.keywords.8
  (search '(c d) '(a b c d c d e) :start1 0 :start2 0 :start1 1
          :start2 6 :from-end t :from-end nil)
  4)


;;; Error cases

(deftest search.error.1
  (signals-error (search) program-error)
  t)

(deftest search.error.2
  (signals-error (search "a") program-error)
  t)

(deftest search.error.3
  (signals-error (search "a" "a" :key) program-error)
  t)

(deftest search.error.4
  (signals-error (search "a" "a" 'bad t) program-error)
  t)

(deftest search.error.5
  (signals-error (search "a" "a" 'bad t :allow-other-keys nil) program-error)
  t)

(deftest search.error.6
  (signals-error (search "a" "a" 1 2) program-error)
  t)

(deftest search.error.7
  (signals-error (search "c" "abcde" :test #'identity) program-error)
  t)

(deftest search.error.8
  (signals-error (search "c" "abcde" :test-not #'identity) program-error)
  t)

(deftest search.error.9
  (signals-error (search "c" "abcde" :key #'cons) program-error)
  t)

(deftest search.error.10
  (signals-error (search "c" "abcde" :key #'car) type-error)
  t)

;;; Order of evaluation

(deftest search.order.1
  (let ((i 0) a b c d e f g h j)
    (values
     (search
      (progn (setf a (incf i)) '(nil a b nil))
      (progn (setf b (incf i)) '(z z z a a b b z z z))
      :from-end (progn (setf c (incf i)) t)
      :start1 (progn (setf d (incf i)) 1)
      :end1 (progn (setf e (incf i)) 3)
      :start2 (progn (setf f (incf i)) 1)
      :end2 (progn (setf g (incf i)) 8)
      :key (progn (setf h (incf i)) #'identity)
      :test (progn (setf j (incf i)) #'eql)
      )
     i a b c d e f g h j))
  4 9 1 2 3 4 5 6 7 8 9)

(deftest search.order.2
  (let ((i 0) a b c d e f g h j)
    (values
     (search
      (progn (setf a (incf i)) '(nil a b nil))
      (progn (setf b (incf i)) '(z z z a a b b z z z))
      :test-not (progn (setf c (incf i)) (complement #'eql))
      :key (progn (setf d (incf i)) #'identity)
      :end2 (progn (setf e (incf i)) 8)
      :start2 (progn (setf f (incf i)) 1)
      :end1 (progn (setf g (incf i)) 3)
      :start1 (progn (setf h (incf i)) 1)
      :from-end (progn (setf j (incf i)) t)
      )
     i a b c d e f g h j))
  4 9 1 2 3 4 5 6 7 8 9)
