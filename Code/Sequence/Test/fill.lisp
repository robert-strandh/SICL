;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat Oct 12 19:44:45 2002
;;;; Contains: Tests on FILL

(in-package #:sicl-sequence-test)

(deftest fill.error.1
  (signals-error (fill 'a 'b) type-error)
  t)

(deftest fill.error.2
  (signals-error (fill) program-error)
  t)

(deftest fill.error.3
  (signals-error (fill (list 'a 'b)) program-error)
  t)

(deftest fill.error.4
  (signals-error (fill (list 'a 'b) 'c :bad t) program-error)
  t)

(deftest fill.error.5
  (signals-error (fill (list 'a 'b) 'c :bad t :allow-other-keys nil)
                 program-error)
  t)

(deftest fill.error.6
  (signals-error (fill (list 'a 'b) 'c :start) program-error)
  t)

(deftest fill.error.7
  (signals-error (fill (list 'a 'b) 'c :end) program-error)
  t)

(deftest fill.error.8
  (signals-error (fill (list 'a 'b) 'c 1 2) program-error)
  t)

(deftest fill.error.10
  (signals-error (fill (list 'a 'b) 'c :bad t :allow-other-keys nil
                        :allow-other-keys t)
                 program-error)
  t)

(deftest fill.error.11
  (signals-error (locally (fill 'a 'b) t) type-error)
  t)

;;; Fill on arrays

(deftest array-fill-1
  (let* ((a (make-array '(5) :initial-contents '(a b c d e)))
         (b (fill a 'x)))
    (values (eqt a b)
            (map 'list #'identity a)))
  t (x x x x x))

(deftest array-fill-2
  (let* ((a (make-array '(5) :initial-contents '(a b c d e)))
         (b (fill a 'x :start 2)))
    (values (eqt a b)
            (map 'list #'identity a)))
  t (a b x x x))

(deftest array-fill-3
  (let* ((a (make-array '(5) :initial-contents '(a b c d e)))
         (b (fill a 'x :end 2)))
    (values (eqt a b)
            (map 'list #'identity a)))
  t (x x c d e))

(deftest array-fill-4
  (let* ((a (make-array '(5) :initial-contents '(a b c d e)))
         (b (fill a 'x :start 1 :end 3)))
    (values (eqt a b)
            (map 'list #'identity a)))
  t (a x x d e))

(deftest array-fill-5
  (let* ((a (make-array '(5) :initial-contents '(a b c d e)))
         (b (fill a 'x :start 1 :end nil)))
    (values (eqt a b)
            (map 'list #'identity a)))
  t (a x x x x))

(deftest array-fill-6
  (let* ((a (make-array '(5) :initial-contents '(a b c d e)))
         (b (fill a 'x :end nil)))
    (values (eqt a b)
            (map 'list #'identity a)))
  t (x x x x x))

(deftest array-fill-7
  (signals-error
   (let* ((a (make-array '(5))))
     (fill a 'x :start -1))
   type-error)
  t)

(deftest array-fill-8
  (signals-error
   (let* ((a (make-array '(5))))
     (fill a 'x :start 'a))
   type-error)
  t)

(deftest array-fill-9
  (signals-error
   (let* ((a (make-array '(5))))
     (fill a 'x :end -1))
   type-error)
  t)

(deftest array-fill-10
  (signals-error
   (let* ((a (make-array '(5))))
     (fill a 'x :end 'a))
   type-error)
  t)

;;; fill on arrays of fixnums

(deftest array-fixnum-fill-1
  (let* ((a (make-array '(5) :element-type 'fixnum :initial-contents '(1 2 3 4 5)))
         (b (fill a 6)))
    (values (eqt a b)
            (map 'list #'identity a)))
  t (6 6 6 6 6))

(deftest array-fixnum-fill-2
  (let* ((a (make-array '(5) :element-type 'fixnum :initial-contents '(1 2 3 4 5)))
         (b (fill a 6 :start 2)))
    (values (eqt a b)
            (map 'list #'identity a)))
  t (1 2 6 6 6))

(deftest array-fixnum-fill-3
  (let* ((a (make-array '(5) :element-type 'fixnum :initial-contents '(1 2 3 4 5)))
         (b (fill a 7 :end 2)))
    (values (eqt a b)
            (map 'list #'identity a)))
  t (7 7 3 4 5))

(deftest array-fixnum-fill-4
  (let* ((a (make-array '(5) :element-type 'fixnum :initial-contents '(1 2 3 4 5)))
         (b (fill a 8 :start 1 :end 3)))
    (values (eqt a b)
            (map 'list #'identity a)))
  t (1 8 8 4 5))

(deftest array-fixnum-fill-5
  (let* ((a (make-array '(5) :element-type 'fixnum :initial-contents '(1 2 3 4 5)))
         (b (fill a 0 :start 1 :end nil)))
    (values (eqt a b)
            (map 'list #'identity a)))
  t (1 0 0 0 0))

(deftest array-fixnum-fill-6
  (let* ((a (make-array '(5) :element-type 'fixnum :initial-contents '(1 2 3 4 5)))
         (b (fill a -1 :end nil)))
    (values (eqt a b)
            (map 'list #'identity a)))
  t (-1 -1 -1 -1 -1))

(deftest array-fixnum-fill-7
  (signals-error
   (let* ((a (make-array '(5) :element-type 'fixnum)))
     (fill a 10 :start -1))
   type-error)
  t)

(deftest array-fixnum-fill-8
  (signals-error
   (let* ((a (make-array '(5) :element-type 'fixnum)))
     (fill a 100 :start 'a))
   type-error)
  t)

(deftest array-fixnum-fill-9
  (signals-error
   (let* ((a (make-array '(5) :element-type 'fixnum)))
     (fill a -5 :end -1))
   type-error)
  t)

(deftest array-fixnum-fill-10
  (signals-error
   (let* ((a (make-array '(5) :element-type 'fixnum)))
     (fill a 17 :end 'a))
   type-error)
  t)

;;; fill on arrays of unsigned eight bit bytes

(deftest array-unsigned-byte8-fill-1
  (array-unsigned-byte-fill-test-fn 8 6)
  t (6 6 6 6 6))

(deftest array-unsigned-byte8-fill-2
  (array-unsigned-byte-fill-test-fn 8 6 :start 2)
  t (1 2 6 6 6))

(deftest array-unsigned-byte8-fill-3
  (array-unsigned-byte-fill-test-fn 8 7 :end 2)
  t (7 7 3 4 5))

(deftest array-unsigned-byte8-fill-4
  (array-unsigned-byte-fill-test-fn 8 8 :start 1 :end 3)
  t (1 8 8 4 5))

(deftest array-unsigned-byte8-fill-5
  (array-unsigned-byte-fill-test-fn 8 9 :start 1 :end nil)
  t (1 9 9 9 9))

(deftest array-unsigned-byte8-fill-6
  (array-unsigned-byte-fill-test-fn 8 0 :end nil)
  t (0 0 0 0 0))

(deftest array-unsigned-byte8-fill-7
  (signals-error (array-unsigned-byte-fill-test-fn 8 0 :start -1)
                 type-error)
  t)

(deftest array-unsigned-byte8-fill-8
  (signals-error (array-unsigned-byte-fill-test-fn 8 100 :start 'a)
                 type-error)
  t)

(deftest array-unsigned-byte8-fill-9
  (signals-error (array-unsigned-byte-fill-test-fn 8 19 :end -1)
                 type-error)
  t)

(deftest array-unsigned-byte8-fill-10
  (signals-error (array-unsigned-byte-fill-test-fn 8 17 :end 'a)
                 type-error)
  t)

;;; Tests on arrays with fill pointers

(deftest array-fill-pointer-fill.1
  (let ((s1 (make-array '(10) :fill-pointer 5 :initial-element nil)))
    (fill s1 'a)
    (loop for i from 0 to 9 collect (aref s1 i)))
  (a a a a a nil nil nil nil nil))

(deftest array-fill-pointer-fill.2
  (let ((s1 (make-array '(10) :fill-pointer 5 :initial-element nil)))
    (fill s1 'a :end nil)
    (loop for i from 0 to 9 collect (aref s1 i)))
  (a a a a a nil nil nil nil nil))

;;; Tests on strings

(deftest fill.string.1
  (let* ((s1 (copy-seq "abcde"))
         (s2 (fill s1 #\z)))
    (values (eqt s1 s2) s2))
  t
  "zzzzz")

(deftest fill.string.2
  (let* ((s1 (copy-seq "abcde"))
         (s2 (fill s1 #\z :start 0 :end 1)))
    (values (eqt s1 s2) s2))
  t
  "zbcde")

(deftest fill.string.3
  (let* ((s1 (copy-seq "abcde"))
         (s2 (fill s1 #\z :end 2)))
    (values (eqt s1 s2) s2))
  t
  "zzcde")

(deftest fill.string.4
  (let* ((s1 (copy-seq "abcde"))
         (s2 (fill s1 #\z :end nil)))
    (values (eqt s1 s2) s2))
  t
  "zzzzz")

(deftest fill.string.5
  (let* ((s1 "aaaaaaaa")
         (len (length s1)))
    (loop for start from 0 to (1- len)
          always
          (loop for end from (1+ start) to len
                always
                (let* ((s2 (copy-seq s1))
                       (s3 (fill s2 #\z :start start :end end)))
                  (and (eqt s2 s3)
                       (string= s3
                                (substitute-if #\z (constantly t) s1
                                               :start start :end end))
                       t)))))
  t)

(deftest fill.string.6
  (let* ((s1 "aaaaaaaa")
         (len (length s1)))
    (loop for start from 0 to (1- len)
          always
          (let* ((s2 (copy-seq s1))
                 (s3 (fill s2 #\z :start start)))
            (and (eqt s2 s3)
                 (string= s3
                          (substitute-if #\z (constantly t) s1
                                         :start start))
                 t))))
  t)

(deftest fill.string.7
  (let* ((s1 "aaaaaaaa")
         (len (length s1)))
    (loop for start from 0 to (1- len)
          always
          (let* ((s2 (copy-seq s1))
                 (s3 (fill s2 #\z :end nil :start start)))
            (and (eqt s2 s3)
                 (string= s3
                          (substitute-if #\z (constantly t) s1
                                         :end nil :start start))
                 t))))
  t)

(deftest fill.string.8
  (let* ((s1 "aaaaaaaa")
         (len (length s1)))
    (loop for end from 1 to len
          always
          (let* ((s2 (copy-seq s1))
                 (s3 (fill s2 #\z :end end)))
            (and (eqt s2 s3)
                 (string= s3
                          (substitute-if #\z (constantly t) s1
                                         :end end))
                 t))))
  t)

(deftest fill.string.9
  (let* ((s1 (make-array '(8) :element-type 'character
                         :initial-element #\z
                         :fill-pointer 4))
         (s2 (fill s1 #\a)))
    (and (eqt s1 s2)
         (coerce (loop for i from 0 to 7 collect (aref s2 i))
                 'string)))
  "aaaazzzz")

(deftest fill.string.10
  (let* ((s1 (make-array '(8) :element-type 'base-char
                         :initial-element #\z
                         :fill-pointer 4))
         (s2 (fill s1 #\a)))
    (and (eqt s1 s2)
         (coerce (loop for i from 0 to 7 collect (aref s2 i))
                 'base-string)))
  "aaaazzzz")

;;; Tests for bit vectors

(deftest fill.bit-vector.1
  (let* ((s1 (copy-seq #*01100))
         (s2 (fill s1 0)))
    (values (eqt s1 s2) s2))
  t
  #*00000)

(deftest fill.bit-vector.2
  (let* ((s1 (copy-seq #*00100))
         (s2 (fill s1 1 :start 0 :end 1)))
    (values (eqt s1 s2) s2))
  t
  #*10100)

(deftest fill.bit-vector.3
  (let* ((s1 (copy-seq #*00010))
         (s2 (fill s1 1 :end 2)))
    (values (eqt s1 s2) s2))
  t
  #*11010)

(deftest fill.bit-vector.4
  (let* ((s1 (copy-seq #*00111))
         (s2 (fill s1 0 :end nil)))
    (values (eqt s1 s2) s2))
  t
  #*00000)

(deftest fill.bit-vector.5
  (let* ((s1 #*00000000)
         (len (length s1)))
    (loop for start from 0 to (1- len)
          always
          (loop for end from (1+ start) to len
                always
                (let* ((s2 (copy-seq s1))
                       (s3 (fill s2 1 :start start :end end)))
                  (and (eqt s2 s3)
                       (equalp s3
                               (substitute-if 1 (constantly t) s1
                                              :start start :end end))
                       t)))))
  t)

(deftest fill.bit-vector.6
  (let* ((s1 #*11111111)
         (len (length s1)))
    (loop for start from 0 to (1- len)
          always
          (let* ((s2 (copy-seq s1))
                 (s3 (fill s2 0 :start start)))
            (and (eqt s2 s3)
                 (equalp s3
                         (substitute-if 0 (constantly t) s1
                                        :start start))
                 t))))
  t)

(deftest fill.bit-vector.7
  (let* ((s1 #*00000000)
         (len (length s1)))
    (loop for start from 0 to (1- len)
          always
          (let* ((s2 (copy-seq s1))
                 (s3 (fill s2 1 :end nil :start start)))
            (and (eqt s2 s3)
                 (equalp s3
                         (substitute-if 1 (constantly t) s1
                                        :end nil :start start))
                 t))))
  t)

(deftest fill.bit-vector.8
  (let* ((s1 #*11111111)
         (len (length s1)))
    (loop for end from 1 to len
          always
          (let* ((s2 (copy-seq s1))
                 (s3 (fill s2 0 :end end)))
            (and (eqt s2 s3)
                 (equalp s3
                         (substitute-if 0 (constantly t) s1
                                        :end end))
                 t))))
  t)

(deftest fill.bit-vector.9
  (let* ((s1 (make-array '(8) :element-type 'bit
                         :initial-element 0
                         :fill-pointer 4))
         (s2 (fill s1 1)))
    (and (eqt s1 s2)
         (coerce (loop for i from 0 to 7 collect (aref s2 i))
                 'bit-vector)))
  #*11110000)

;;; Test of :allow-other-keys

(deftest fill.allow-other-keys.1
  (fill (list 'a 'b 'c 'd 'e) 'a :allow-other-keys t)
  (a a a a a))

(deftest fill.allow-other-keys.2
  (fill (list 'a 'b 'c 'd 'e) 'a :allow-other-keys nil)
  (a a a a a))

(deftest fill.allow-other-keys.3
  (fill (list 'a 'b 'c 'd 'e) 'a :allow-other-keys t :bad t)
  (a a a a a))

(deftest fill.allow-other-keys.4
  (fill (list 'a 'b 'c 'd 'e) 'a :bad t :allow-other-keys t)
  (a a a a a))

(deftest fill.allow-other-keys.5
  (fill (list 'a 'b 'c 'd 'e) 'a 'bad t :allow-other-keys t)
  (a a a a a))

(deftest fill.allow-other-keys.6
  (fill (list 'a 'b 'c 'd 'e) 'a  :bad t :allow-other-keys t
        :allow-other-keys nil)
  (a a a a a))

(deftest fill.allow-other-keys.7
  (fill (list 'a 'b 'c 'd 'e) 'a  :allow-other-keys t :allow-other-keys nil
        :bad t)
  (a a a a a))


;;; Tests of evaluation order

(deftest fill.order.1
  (let ((i 0) x y (a (copy-seq #(a a a a))))
    (values
     (fill (progn (setf x (incf i)) a)
           (progn (setf y (incf i)) 'z))
     i x y))
  #(z z z z) 2 1 2)

(deftest fill.order.2
  (let ((i 0) x y z w (a (copy-seq #(a a a a))))
    (values
     (fill (progn (setf x (incf i)) a)
           (progn (setf y (incf i)) 'z)
           :start (progn (setf z (incf i)) 1)
           :end   (progn (setf w (incf i)) 3))
     i x y z w))
  #(a z z a) 4 1 2 3 4)

(deftest fill.order.3
  (let ((i 0) x y z w (a (copy-seq #(a a a a))))
    (values
     (fill (progn (setf x (incf i)) a)
           (progn (setf y (incf i)) 'z)
           :end   (progn (setf z (incf i)) 3)
           :start (progn (setf w (incf i)) 1))
     i x y z w))
  #(a z z a) 4 1 2 3 4)

(deftest fill.order.4
  (let ((i 0) x y z p q r s w (a (copy-seq #(a a a a))))
    (values
     (fill (progn (setf x (incf i)) a)
           (progn (setf y (incf i)) 'z)
           :end   (progn (setf z (incf i)) 3)
           :end   (progn (setf p (incf i)) 1)
           :end   (progn (setf q (incf i)) 1)
           :end   (progn (setf r (incf i)) 1)
           :start (progn (setf s (incf i)) 1)
           :start (progn (setf w (incf i)) 0))
     i x y z p q r s w))
  #(a z z a) 8 1 2 3 4 5 6 7 8)

;;; Specialized strings

(deftest fill.specialized-strings.1
  (do-special-strings
   (s (copy-seq "abcde") nil)
   (assert (string= s "abcde"))
   (assert (eq s (fill s #\x)))
   (assert (string= s "xxxxx")))
  nil)

(deftest fill.specialized-strings.2
  (do-special-strings
   (s (copy-seq "abcde") nil)
   (assert (string= s "abcde"))
   (assert (eq s (fill s #\x  :start 2)))
   (assert (string= s "abxxx")))
  nil)

(deftest fill.specialized-strings.3
  (do-special-strings
   (s (copy-seq "abcde") nil)
   (assert (string= s "abcde"))
   (assert (eq s (fill s #\x  :end 3)))
   (assert (string= s "xxxde")))
  nil)

(deftest fill.specialized-strings.4
  (do-special-strings
   (s (copy-seq "abcde") nil)
   (assert (string= s "abcde"))
   (assert (eq s (fill s #\x  :start 1 :end 4)))
   (assert (string= s "axxxe")))
  nil)

;;; Specialized vector tests

(deftest fill.specialized-vectors.1
  (do-special-integer-vectors
   (v #(0 1 1 0 1) nil)
   (let ((etype (array-element-type v)))
     (assert (eq v (fill v 0)))
     (assert (equal (array-element-type v) etype)))
   (assert (equalp v #(0 0 0 0 0))))
  nil)

(deftest fill.specialized-vectors.2
  (do-special-integer-vectors
   (v #(0 -1 1 0 -1) nil)
   (let ((etype (array-element-type v)))
     (assert (eq v (fill v 1)))
     (assert (equal (array-element-type v) etype)))
   (assert (equalp v #(1 1 1 1 1))))
  nil)

(deftest fill.specialized-vectors.3
  (do-special-integer-vectors
   (v #(1 1 1 1 0) nil)
   (let ((etype (array-element-type v)))
     (assert (eq v (fill v 0 :start 1 :end 3)))
     (assert (equal (array-element-type v) etype)))
   (assert (equalp v #(1 0 0 1 0))))
  nil)
