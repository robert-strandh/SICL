;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat Aug 24 07:22:10 2002
;;;; Contains: Aux. functions for testing SEARCH

(in-package #:sicl-sequence-test)

(defparameter *searched-list*
  '(b b a b b b b b b b a b a b b b a b a b b b a a a a b a a b a a a a a
      a b a b b a b a a b a a a b b a a b a a a a b b a b a b a a a b a b
      b a b a a b b b b b a a a a a b a b b b b b a b a b b a b a b))

(defparameter *pattern-sublists*
  (remove-duplicates
   (let* ((s *searched-list*) (len (length s)))
     (loop for x from 0 to 8 nconc
           (loop for y from 0 to (- len x)
                 collect (subseq s y (+ y x)))))
   :test #'equal))

(defparameter *searched-vector*
  (make-array (length *searched-list*)
              :initial-contents *searched-list*))

(defparameter *pattern-subvectors*
  (mapcar #'(lambda (x) (apply #'vector x)) *pattern-sublists*))

(defparameter *searched-bitvector*
  #*1101111111010111010111000010010000001011010010001100100001101010001011010011111000001011111010110101)

(defparameter *pattern-subbitvectors*
  (remove-duplicates
   (let* ((s *searched-bitvector*) (len (length s)))
     (loop for x from 0 to 8 nconc
           (loop for y from 0 to (- len x)
                 collect (subseq s y (+ y x)))))
   :test #'equalp))

(defparameter *searched-string*
  "1101111111010111010111000010010000001011010010001100100001101010001011010011111000001011111010110101")

(defparameter *pattern-substrings*
  (remove-duplicates
   (let* ((s *searched-string*) (len (length s)))
     (loop for x from 0 to 8 nconc
           (loop for y from 0 to (- len x)
                 collect (subseq s y (+ y x)))))
   :test #'equalp))

(defun subseq-equalp (seq1 seq2 start1 start2 len &key (test #'equalp))
  (assert
   (and
    (>= start1 0)
    (>= start2 0)
    (<= (+ start1 len) (length seq1))
    (<= (+ start2 len) (length seq2))))
  (setq test (coerce test 'function))
  (if (and (listp seq1) (listp seq2))
      (loop for i from 0 to (1- len)
            for e1 in (nthcdr start1 seq1)
            for e2 in (nthcdr start2 seq2)
            always (funcall test e1 e2))
    (loop for i from 0 to (1- len)
          always
          (funcall (the function test)
                   (elt seq1 (+ start1 i))
                   (elt seq2 (+ start2 i))))))

(defun search-check (pattern searched pos
                             &key (start1 0) (end1 nil) (start2 0) (end2 nil)
                             key from-end (test #'equalp))
  (unless end1 (setq end1 (length pattern)))
  (unless end2 (setq end2 (length searched)))
  (assert (<= start1 end1))
  (assert (<= start2 end2))
  (let* ((plen (- end1 start1)))
    (when key
      (setq pattern (map 'list key pattern))
      (setq searched (map 'list key searched)))
    (if pos
        (and
         (subseq-equalp searched pattern pos start1 plen :test test)
         (if from-end
             (loop for i from (1+ pos) to (- end2 plen)
                   never
                   (subseq-equalp searched pattern i start1 plen :test test))
           (loop for i from start2 to (1- pos)
                 never
                 (subseq-equalp searched pattern i start1 plen :test test))))
      (loop for i from start2 to (- end2 plen)
            never (subseq-equalp searched pattern i start1 plen :test test)))))


