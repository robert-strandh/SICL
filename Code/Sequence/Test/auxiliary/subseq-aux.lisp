;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Tue Nov 26 20:01:27 2002
;;;; Contains: Aux. functions for subseq tests

(in-package #:sicl-sequence-test)

(defun subseq-list.4-body ()
  (block done
    (let ((x (loop for i from 0 to 19 collect i)))
      (loop
       for i from 0 to 20 do
       (loop
        for j from i to 20 do
        (let ((y (subseq x i j)))
          (loop
           for e in y and k from i to (1- j) do
           (unless (eqlt e k) (return-from done nil)))))))
    t))

(defun subseq-list.5-body ()
  (block done
    (let ((x (loop for i from 0 to 29 collect i)))
      (loop
       for i from 0 to 30 do
       (unless (equalt (subseq x i)
                       (loop for j from i to 29 collect j))
         (return-from done nil))))
    t))

(defun subseq-list.6-body ()
  (let* ((x (make-list 100))
         (z (loop for e on x collect e))
         (y (subseq x 0)))
    (loop
     for e on x
     and f on y
     and g in z do
     (when (or (not (eqt g e))
               (not (eqlt (car e) (car f)))
               (car e)
               (eqt e f))
       (return nil))
     finally (return t))))

(defun subseq-vector.1-body ()
  (block nil
  (let* ((x (make-sequence 'vector 10 :initial-element 'a))
         (y (subseq x 4 8)))
    (unless (every #'(lambda (e) (eqt e 'a)) x)
      (return 1))
    (unless (every #'(lambda (e) (eqt e 'a)) y)
      (return 2))
    (unless (eqlt (length x) 10) (return 3))
    (unless (eqlt (length y) 4)  (return 4))
    (loop for i from 0 to 9 do (setf (elt x i) 'b))
    (unless (every #'(lambda (e) (eqt e 'a)) y)
      (return 5))
    (loop for i from 0 to 3 do (setf (elt y i) 'c))
    (or
     (not (not (every #'(lambda (e) (eqt e 'b)) x)))
     6))))

(defun subseq-vector.2-body ()
  (block nil
  (let* ((x (make-sequence '(vector fixnum) 10 :initial-element 1))
         (y (subseq x 4 8)))
    (unless (every #'(lambda (e) (eqlt e 1)) x)
      (return 1))
    (unless (every #'(lambda (e) (eqlt e 1)) y)
      (return 2))
    (unless (eqlt (length x) 10) (return 3))
    (unless (eqlt (length y) 4)  (return 4))
    (loop for i from 0 to 9 do (setf (elt x i) 2))
    (unless (every #'(lambda (e) (eqlt e 1)) y)
      (return 5))
    (loop for i from 0 to 3 do (setf (elt y i) 3))
    (or
     (not (not (every #'(lambda (e) (eqlt e 2)) x)))
     6))))

(defun subseq-vector.3-body ()
  (block nil
  (let* ((x (make-sequence '(vector single-float) 10 :initial-element 1.0))
         (y (subseq x 4 8)))
    (unless (every #'(lambda (e) (= e 1.0)) x)
      (return 1))
    (unless (every #'(lambda (e) (= e 1.0)) y)
      (return 2))
    (unless (eqlt (length x) 10) (return 3))
    (unless (eqlt (length y) 4)  (return 4))
    (loop for i from 0 to 9 do (setf (elt x i) 2.0))
    (unless (every #'(lambda (e) (= e 1.0)) y)
      (return 5))
    (loop for i from 0 to 3 do (setf (elt y i) 3.0))
    (or
     (not (not (every #'(lambda (e) (= e 2.0)) x)))
     6))))

(defun subseq-vector.4-body ()
  (block nil
  (let* ((x (make-sequence '(vector double-float) 10 :initial-element 1.0d0))
         (y (subseq x 4 8)))
    (unless (every #'(lambda (e) (= e 1.0)) x)
      (return 1))
    (unless (every #'(lambda (e) (= e 1.0)) y)
      (return 2))
    (unless (eqlt (length x) 10) (return 3))
    (unless (eqlt (length y) 4)  (return 4))
    (loop for i from 0 to 9 do (setf (elt x i) 2.0d0))
    (unless (every #'(lambda (e) (= e 1.0)) y)
      (return 5))
    (loop for i from 0 to 3 do (setf (elt y i) 3.0d0))
    (or
     (not (not (every #'(lambda (e) (= e 2.0)) x)))
     6))))

(defun subseq-vector.5-body ()
  (block nil
  (let* ((x (make-sequence '(vector short-float) 10 :initial-element 1.0s0))
         (y (subseq x 4 8)))
    (unless (every #'(lambda (e) (= e 1.0)) x)
      (return 1))
    (unless (every #'(lambda (e) (= e 1.0)) y)
      (return 2))
    (unless (eqlt (length x) 10) (return 3))
    (unless (eqlt (length y) 4)  (return 4))
    (loop for i from 0 to 9 do (setf (elt x i) 2.0s0))
    (unless (every #'(lambda (e) (= e 1.0)) y)
      (return 5))
    (loop for i from 0 to 3 do (setf (elt y i) 3.0s0))
    (or
     (not (not (every #'(lambda (e) (= e 2.0)) x)))
     6))))

(defun subseq-vector.6-body ()
  (block nil
  (let* ((x (make-sequence '(vector long-float) 10 :initial-element 1.0l0))
         (y (subseq x 4 8)))
    (unless (every #'(lambda (e) (= e 1.0)) x)
      (return 1))
    (unless (every #'(lambda (e) (= e 1.0)) y)
      (return 2))
    (unless (eqlt (length x) 10) (return 3))
    (unless (eqlt (length y) 4)  (return 4))
    (loop for i from 0 to 9 do (setf (elt x i) 2.0l0))
    (unless (every #'(lambda (e) (= e 1.0)) y)
      (return 5))
    (loop for i from 0 to 3 do (setf (elt y i) 3.0l0))
    (or
     (not (not (every #'(lambda (e) (= e 2.0)) x)))
     6))))

(defun subseq-string.1-body ()
  (let* ((s1 "abcdefgh")
         (len (length s1)))
    (loop for start from 0 below len
          always
          (string= (subseq s1 start)
                   (coerce (loop for i from start to (1- len)
                                 collect (elt s1 i))
                           'string)))))

(defun subseq-string.2-body ()
  (let* ((s1 "abcdefgh")
         (len (length s1)))
    (loop for start from 0 below len
          always
          (loop for end from (1+ start) to len
                always
                (string= (subseq s1 start end)
                         (coerce (loop for i from start below end
                                       collect (elt s1 i))
                                 'string))))))

(defun subseq-string.3-body ()
  (let* ((s1 (make-array '(10) :initial-contents "abcdefghij"
                         :fill-pointer 8
                         :element-type 'character))
         (len (length s1)))
    (and
     (eqlt len 8)
     (loop for start from 0 below len
          always
          (string= (subseq s1 start)
                   (coerce (loop for i from start to (1- len)
                                 collect (elt s1 i))
                           'string)))
     (loop for start from 0 below len
           always
           (loop for end from (1+ start) to len
                 always
                 (string= (subseq s1 start end)
                          (coerce (loop for i from start below end
                                        collect (elt s1 i))
                                  'string)))))))
(defun subseq-bit-vector.1-body ()
  (let* ((s1 #*11001000)
         (len (length s1)))
    (loop for start from 0 below len
          always
          (equalp (subseq s1 start)
                  (coerce (loop for i from start to (1- len)
                                collect (elt s1 i))
                          'bit-vector)))))

(defun subseq-bit-vector.2-body ()
  (let* ((s1 #*01101011)
         (len (length s1)))
    (loop for start from 0 below len
          always
          (loop for end from (1+ start) to len
                always
                (equalp (subseq s1 start end)
                        (coerce (loop for i from start below end
                                      collect (elt s1 i))
                                'bit-vector))))))

(defun subseq-bit-vector.3-body ()
  (let* ((s1 (make-array '(10) :initial-contents #*1101100110
                         :fill-pointer 8
                         :element-type 'bit))
         (len (length s1)))
    (and
     (eqlt len 8)
     (loop for start from 0 below len
          always
          (equalp (subseq s1 start)
                  (coerce (loop for i from start to (1- len)
                                collect (elt s1 i))
                          'bit-vector)))
     (loop for start from 0 below len
           always
           (loop for end from (1+ start) to len
                 always
                 (equalp (subseq s1 start end)
                         (coerce (loop for i from start below end
                                       collect (elt s1 i))
                                 'bit-vector)))))))
