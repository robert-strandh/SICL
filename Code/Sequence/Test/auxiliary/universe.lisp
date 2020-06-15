;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Thu Apr  9 19:32:56 1998
;;;; Contains: A global variable containing a list of
;;;;           as many kinds of CL objects as we can think of
;;;;           This list is used to test many other CL functions

(in-package #:sicl-sequence-test)

(defparameter *condition-types*
    '(arithmetic-error
      cell-error
      condition
      control-error
      division-by-zero
      end-of-file
      error
      file-error
      floating-point-inexact
      floating-point-invalid-operation
      floating-point-underflow
      floating-point-overflow
      package-error
      parse-error
      print-not-readable
      program-error
      reader-error
      serious-condition
      simple-condition
      simple-error
      simple-type-error
      simple-warning
      storage-condition
      stream-error
      style-warning
      type-error
      unbound-slot
      unbound-variable
      undefined-function
      warning))

(defparameter *condition-objects*
  (locally (declare (optimize safety))
           (loop for tp in *condition-types* append
                 (handler-case (list (make-condition tp))
                               (error () nil)))))

(defparameter *standard-package-names*
  '("COMMON-LISP" "COMMON-LISP-USER" "KEYWORD"))

(defparameter *package-objects*
  (locally (declare (optimize safety))
           (loop for pname in *standard-package-names* append
                 (handler-case (let ((pkg (find-package pname)))
                                 (and pkg (list pkg)))
                               (error () nil)))))

(defparameter *integers*
    (remove-duplicates
     `(
       0
       ;; Integers near the fixnum/bignum boundaries
       ,@(loop for i from -5 to 5 collect (+ i most-positive-fixnum))
       ,@(loop for i from -5 to 5 collect (+ i most-negative-fixnum))
       ;; Powers of two, negatives, and off by one.
       ,@(loop for i from 1 to 64 collect (ash 1 i))
       ,@(loop for i from 1 to 64 collect (1- (ash 1 i)))
       ,@(loop for i from 1 to 64 collect (ash -1 i))
       ,@(loop for i from 1 to 64 collect (1+ (ash -1 i)))
       ;; A big integer
       ,(expt 17 50)
       ;; Some arbitrarily chosen integers
       12387131 1272314 231 -131 -561823 23713 -1234611312123 444121 991)))

(defparameter *floats*
    (append
     (loop for sym in '(pi
                        most-positive-short-float
                        least-positive-short-float
                        least-positive-normalized-short-float
                        most-positive-double-float
                        least-positive-double-float
                        least-positive-normalized-double-float
                        most-positive-long-float
                        least-positive-long-float
                        least-positive-normalized-long-float
                        most-positive-single-float
                        least-positive-single-float
                        least-positive-normalized-single-float
                        most-negative-short-float
                        least-negative-short-float
                        least-negative-normalized-short-float
                        most-negative-single-float
                        least-negative-single-float
                        least-negative-normalized-single-float
                        most-negative-double-float
                        least-negative-double-float
                        least-negative-normalized-double-float
                        most-negative-long-float
                        least-negative-long-float
                        least-negative-normalized-long-float
                        short-float-epsilon
                        short-float-negative-epsilon
                        single-float-epsilon
                        single-float-negative-epsilon
                        double-float-epsilon
                        double-float-negative-epsilon
                        long-float-epsilon
                        long-float-negative-epsilon)
           when (boundp sym) collect (symbol-value sym))
     (list
      0.0 1.0 -1.0 313123.13 283143.231 -314781.9
      1.31283d2 834.13812D-45
      8131238.1E14 -4618926.231e-2
      -37818.131F3 81.318231f-19
      1.31273s3 12361.12S-7
      6124.124l0 13123.1L-23)))

(defparameter *ratios*
    '(1/3 1/1000 1/1000000000000000 -10/3 -1000/7 -987129387912381/13612986912361
      189729874978126783786123/1234678123487612347896123467851234671234))

(defparameter *complexes*
    '(#C(0.0 0.0)
      #C(1.0 0.0)
      #C(0.0 1.0)
      #C(1.0 1.0)
      #C(-1.0 -1.0)
      #C(1289713.12312 -9.12681271)
      #C(1.0D100 1.0D100)
      #C(-1.0D-100 -1.0D-100)
      #C(10.0s0 20.0s0)
      #C(100.0l0 200.0l0)
      #C(1.0s0 2.0f0)
      #C(1.0s0 3.0d0)
      #C(1.0s0 4.0l0)
      #C(1.0f0 5.0d0)
      #C(1.0f0 6.0l0)
      #C(1.0d0 7.0l0)
      #C(1.0f0 2.0s0)
      #C(1.0d0 3.0s0)
      #C(1.0l0 4.0s0)
      #C(1.0d0 5.0f0)
      #C(1.0l0 6.0f0)
      #C(1.0l0 7.0d0)
      #C(1/2 1/3)
      ))

(defparameter *numbers*
    (append *integers*
            *floats*
            *ratios*
            *complexes*))

(defparameter *reals* (append *integers* *floats* *ratios*))

(defparameter *rationals* (append *integers* *ratios*))

(defun try-to-read-chars (&rest namelist)
  (declare (optimize safety))
  (loop
    for name in namelist append
        (handler-case
            (list (read-from-string
                   (concatenate 'string "\#\\" name)))
          (error () nil))))

(defparameter *characters*
    (remove-duplicates
     `(#\Newline
       #\Space
       ,@(try-to-read-chars "Rubout"
                            "Page"
                            "Tab"
                            "Backspace"
                            "Return"
                            "Linefeed"
                            "Null")
       #\a #\A #\0 #\9 #\. #\( #\) #\[ #\]
       )))


(defparameter *strings*
    (append
     (and (code-char 0)
          (list
           (make-string 1 :initial-element (code-char 0))
           (make-string 10 :initial-element (code-char 0))))
     (list
      "" "A" "a" "0" "abcdef"
      "~!@#$%^&*()_+`1234567890-=<,>.?/:;\"'{[}]|\\ abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWYXZ"
      (make-string 100000 :initial-element #\g)
      (let ((s (make-string 256)))
        (loop
            for i from 0 to 255
            do (let ((c (code-char i)))
                 (when c
                   (setf (elt s i) c))))
        s)
      ;; Specialized strings
      (make-array 3
                  :element-type 'character
                  :displaced-to (make-array 5 :element-type 'character
                                            :initial-contents "abcde")
                  :displaced-index-offset 1)
      (make-array 10 :initial-element #\x
                  :fill-pointer 5
                  :element-type 'character)
      (make-array 10 :initial-element #\x
                  :element-type 'base-char)
      (make-array 3 :initial-element #\y
                  :adjustable t
                  :element-type 'base-char)
      )))

(defparameter *conses*
    (list
     (list 'a 'b)
     (list nil)
     (list 1 2 3 4 5 6)))

(defparameter *circular-conses*
    (list
     (let ((s (copy-list '(a b c d))))
       (nconc s s)
       s)
     (let ((s (list nil)))
       (setf (car s) s)
       s)
     (let ((s (list nil)))
       (setf (car s) s)
       (setf (cdr s) s))))

(defparameter *booleans* '(nil t))
(defparameter *keywords* '(:a :b :|| :|a| :|1234|))
(defparameter *uninterned-symbols*
  (list '#:nil '#:t '#:foo '#:||))
(defparameter *cl-test-symbols*
  `(,(intern "a" #.*package*)
    ,(intern "" #.*package*)
    ,@(and (code-char 0)
           (list (intern (make-string 1 :initial-element (code-char 0)) #.*package*)))
    ,@(and (code-char 0)
           (let* ((s (make-string 10 :initial-element (code-char 0)))
                  (s2 (copy-seq s))
                  (s3 (copy-seq s)))
             (setf (subseq s 3 4) "a")
             (setf (subseq s2 4 5) "a")
             (setf (subseq s3 4 5) "a")
             (setf (subseq s3 7 8) "b")
             (list (intern s #.*package*)
                   (intern s2 #.*package*)
                   (intern s3 #.*package*))))
    ))

(defparameter *cl-user-symbols*
  '(cl-user::foo
    cl-user::x
    cl-user::cons
    cl-user::lambda
    cl-user::*print-readably*
    cl-user::push))

(defparameter *symbols*
    (append *booleans* *keywords* *uninterned-symbols*
            *cl-test-symbols*
            *cl-user-symbols*))

(defparameter *array-dimensions*
    (loop
        for i from 0 to 8 collect
          (loop for j from 1 to i collect 2)))

(defparameter *default-array-target* (make-array '(300)))

(defparameter *arrays*
    (append
     (list (make-array '10))
     (mapcar #'make-array *array-dimensions*)

     ;; typed arrays
     (loop for tp in '(fixnum float bit character base-char
                       (signed-byte 8) (unsigned-byte 8))
           for element in '(18 16.0f0 0 #\x #\y 127 200)
         append
           (loop
               for d in *array-dimensions*
               collect (make-array d :element-type tp
                                   :initial-element element)))

     ;; More typed arrays
     (loop for i from 1 to 64
           append
           (list (make-array 10 :element-type `(unsigned-byte ,i)
                             :initial-element 1)
                 (make-array 10 :element-type `(signed-byte ,i)
                             :initial-element 0)))

     ;; adjustable arrays
     (loop
       for d in *array-dimensions*
         collect (make-array d :adjustable t))

     ;; Displaced arrays
     (loop
      for d in *array-dimensions*
      for i from 1
      collect (make-array d :displaced-to *default-array-target*
                          :displaced-index-offset i))

     (list
      #()
      #*
      #*00000
      #*1010101010101101
      (make-array 10 :element-type 'bit
                  :initial-contents '(0 1 1 0 1 1 1 1 0 1)
                  :fill-pointer 8)
      (make-array 5 :element-type 'bit
                  :displaced-to #*0111000110
                  :displaced-index-offset 3)
      (make-array 10 :element-type 'bit
                  :initial-contents '(1 1 0 0 1 1 1 0 1 1)
                  :adjustable t)
      )

     ;; Integer arrays
     (list
      (make-array '(10) :element-type '(integer 0 (256))
                  :initial-contents '(8 9 10 11 12 1 2 3 4 5))
      (make-array '(10) :element-type '(integer -128 (128))
                  :initial-contents '(8 9 -10 11 -12 1 -2 -3 4 5))
      (make-array '(6) :element-type '(integer 0 (#.(ash 1 16)))
                  :initial-contents '(5 9 100 1312 23432 87))
      (make-array '(4) :element-type '(integer 0 (#.(ash 1 28)))
                  :initial-contents '(100000 231213 8123712 19))
      (make-array '(4) :element-type '(integer 0 (#.(ash 1 32)))
                  :initial-contents '(#.(1- (ash 1 32)) 0 872312 10000000))

      (make-array nil :element-type '(integer 0 (256))
                  :initial-element 14)
      (make-array '(2 2) :element-type '(integer 0 (256))
                  :initial-contents '((34 98)(14 119)))
      )

     ;; Float arrays
     (list
      (make-array '(5) :element-type 'short-float
                  :initial-contents '(1.0s0 2.0s0 3.0s0 4.0s0 5.0s0))
      (make-array '(5) :element-type 'single-float
                  :initial-contents '(1.0f0 2.0f0 3.0f0 4.0f0 5.0f0))
      (make-array '(5) :element-type 'double-float
                  :initial-contents '(1.0d0 2.0d0 3.0d0 4.0d0 5.0d0))
      (make-array '(5) :element-type 'long-float
                  :initial-contents '(1.0l0 2.0l0 3.0l0 4.0l0 5.0l0))
      )

     ;; The ever-popular NIL array
     (locally (declare (optimize safety))
              (handler-case
               (list (make-array '(0) :element-type nil))
               (error () nil)))

     ;; more kinds of arrays here later?
     ))

(defparameter *hash-tables*
  (list
   (make-hash-table)
   (make-hash-table :test #'eq)
   (make-hash-table :test #'eql)
   (make-hash-table :test #'equal)
   #-(or CMU ECL) (make-hash-table :test #'equalp)
   ))

(defparameter *pathnames*
  (locally
   (declare (optimize safety))
   (loop for form in '((make-pathname :name "foo")
                       (make-pathname :name "FOO" :case :common)
                       (make-pathname :name "bar")
                       (make-pathname :name "foo" :type "txt")
                       (make-pathname :name "bar" :type "txt")
                       (make-pathname :name "XYZ" :type "TXT" :case :common)
                       (make-pathname :name nil)
                       (make-pathname :name :wild)
                       (make-pathname :name nil :type "txt")
                       (make-pathname :name :wild :type "txt")
                       (make-pathname :name :wild :type "TXT" :case :common)
                       (make-pathname :name :wild :type "abc" :case :common)
                       (make-pathname :directory :wild)
                       (make-pathname :type :wild)
                       (make-pathname :version :wild)
                       (make-pathname :version :newest))
         append (ignore-errors (eval `(list ,form))))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (locally
   (declare (optimize safety))
   (ignore-errors
     (setf (logical-pathname-translations "CLTESTROOT")
           `(("**;*.*.*" ,(merge-pathnames
                           "sandbox/"
                           (make-pathname :directory '(:absolute :wild-inferiors)
                                          :name :wild :type :wild))))))
   (ignore-errors
     (setf (logical-pathname-translations "CLTEST")
           `(("**;*.*.*" ,(merge-pathnames
                           "sandbox/"
                           (make-pathname
                            :directory (append
                                        (pathname-directory
                                         (truename (make-pathname)))
                                        '(:wild-inferiors))
                            :name :wild :type :wild))))))
   ))

(defparameter *logical-pathnames*
  (locally
   (declare (optimize safety))
   (append
    (ignore-errors (list (logical-pathname "CLTESTROOT:")))
    )))

(defparameter *streams*
  (remove-duplicates
   (remove-if
    #'null
    (list
     *debug-io*
     *error-output*
     *query-io*
     *standard-input*
     *standard-output*
     *terminal-io*
     *trace-output*))))

(defparameter *readtables*
  (list *readtable*
        (copy-readtable)))

(defstruct foo-structure
  x y z)

(defstruct bar-structure
  x y z)

(defparameter *structures*
  (list
   (make-foo-structure :x 1 :y 'a :z nil)
   (make-foo-structure :x 1 :y 'a :z nil)
   (make-bar-structure :x 1 :y 'a :z nil)
   ))

(defun meaningless-user-function-for-universe (x y z)
  (list (+ x 1) (+ y 2) (+ z 3)))

(defgeneric meaningless-user-generic-function-for-universe (x y z)
  #+(or (not :gcl) :ansi-cl) (:method ((x integer) (y integer) (z integer)) (+ x y z)))

(eval-when (:load-toplevel :execute)
  (compile 'meaningless-user-function-for-universe)
  ;; Conditionalize to avoid a cmucl bug
  #-(or cmu gcl ecl) (compile 'meaningless-user-generic-function-for-universe)
  )

(defparameter *functions*
  (list #'cons #'car #'append #'values
        (macro-function 'cond)
        #'meaningless-user-function-for-universe
        #'meaningless-user-generic-function-for-universe
        #'(lambda (x) x)
        (compile nil '(lambda (x) x))))

(defparameter *methods*
  (list
   #+(or (not :gcl) :ansi-cl )
   (find-method #'meaningless-user-generic-function-for-universe nil
                (mapcar #'find-class '(integer integer integer)))
   ;; Add more methods here
   ))


(defparameter *random-states*
  (list (make-random-state)))

(defparameter *universe*
  (remove-duplicates
   (append
    *symbols*
    *numbers*
    *characters*
    (mapcar #'copy-seq *strings*)
    *conses*
    *condition-objects*
    *package-objects*
    *arrays*
    *hash-tables*
    *pathnames*
    *logical-pathnames*
    *streams*
    *readtables*
    *structures*
    *functions*
    *random-states*
    *methods*
    nil)))

(defparameter *mini-universe*
  (remove-duplicates
   (append
    (mapcar #'first
            (list *symbols*
                  *numbers*
                  *characters*
                  (list (copy-seq (first *strings*)))
                  *conses*
                  *condition-objects*
                  *package-objects*
                  *arrays*
                  *hash-tables*
                  *pathnames*
                  *logical-pathnames*
                  *streams*
                  *readtables*
                  *structures*
                  *functions*
                  *random-states*
                  *methods*))
    '(;;; Others to fill in gaps
      1.2s0 1.3f0 1.5d0 1.8l0 3/5 10000000000000000000000))))

(defparameter *classes*
  (remove-duplicates (mapcar #'class-of *universe*)))

(defparameter *built-in-classes*
  (remove-if-not #'(lambda (x) (typep x 'built-in-class))
                 *classes*))
