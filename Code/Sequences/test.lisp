(cl:in-package #:sicl-sequence-test)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Objects that can be used in various tests.

;;; Define large integers, the point being that bignums that are eql
;;; might not be eq, allowing us to distinguish those two cases.

;;; Define a large value, hopefully a bignum.
(defparameter *i01* #.(expt 10 100))

;;; Define the same large value, hoping that the compiler
;;; will keep them apart, so they aren't eq.  Tests won't
;;; fail if that is not true, but they might give false positives.
(defparameter *i02* #.(expt 10 100))

;;; Define a few more large distinct values
(defparameter *i1* #.(+ (expt 10 100) 1))
(defparameter *i2* #.(+ (expt 10 100) 2))
(defparameter *i3* #.(+ (expt 10 100) 3))
(defparameter *i4* #.(+ (expt 10 100) 4))
(defparameter *i5* #.(+ (expt 10 100) 5))
(defparameter *i6* #.(+ (expt 10 100) 6))
(defparameter *i7* #.(+ (expt 10 100) 7))
(defparameter *i8* #.(+ (expt 10 100) 8))

(defparameter *l01* (list 'a))
(defparameter *l02* (list 'a))
(defparameter *l1* (list 'b))
(defparameter *l2* (list 'c))
(defparameter *l3* (list 'd))
(defparameter *l4* (list 'e))
(defparameter *l5* (list 'f))
(defparameter *l6* (list 'g))

(defparameter *s01* "abc")
(defparameter *s02* "ABC")
(defparameter *s1* "def")
(defparameter *s2* "ghi")
(defparameter *s3* "jkl")
(defparameter *s4* "mno")
(defparameter *s5* "pqr")
(defparameter *s6* "stu")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Tests for function find

(defun stupid-find (item sequence
                    &key
                    key
                    (test nil test-p)
                    (test-not nil test-not-p)
                    (start 0)
                    end
                    from-end)
  (assert (not (and test-p test-not-p)))
  (when test-not-p
    (setf test (lambda (x y) (not (funcall test-not x y)))))
  (unless test
    (setf test #'eql))
  (unless key
    (setf key #'identity))
  (unless (vectorp sequence)
    (setf sequence (coerce sequence 'vector)))
  (when (null end)
    (setf end (length sequence)))
  (assert (<= 0 start (length sequence)))
  (assert (<= 0 end (length sequence)))
  (if from-end
      (loop for index downfrom (1- end) to start
            when (funcall test item (funcall key (aref sequence index)))
              return (aref sequence index))
      (loop for index from start below end
            when (funcall test item (funcall key (aref sequence index)))
              return (aref sequence index))))

(defun test-find (item sequence key test test-not start end from-end)
  (flet ((make-arg (name thing)
           (if (eql thing -1) '() (list name thing))))
    (let ((arguments (append (make-arg :key key)
                             (make-arg :test test)
                             (make-arg :test-not test-not)
                             (make-arg :start start)
                             (make-arg :end end)
                             (make-arg :from-end from-end))))
      (let ((v1 (apply #'stupid-find item sequence arguments))
            (v2 (apply #'find item sequence arguments)))
      (if (eql v1 v2)
          (format *trace-output* "*")
          (format *trace-output*
                  "for test: FIND ~s ~s ~s~@
                   expected: ~s~%     but saw:  ~s~%"
                  item sequence arguments v1 v2))))))

(defun test-find-1 (item sequence test test-not start end from-end)
  (test-find item sequence
             -1 test test-not start end from-end)
  (test-find item sequence
             nil test test-not start end from-end)
  (test-find item sequence
             #'identity test test-not start end from-end)
  (test-find item sequence
             'identity test test-not start end from-end)

  (test-find item (coerce sequence 'vector)
             -1 test test-not start end from-end)
  (test-find item (coerce sequence 'vector)
             nil test test-not start end from-end)
  (test-find item (coerce sequence 'vector)
             #'identity test test-not start end from-end)

  (test-find item (mapcar #'list sequence)
             #'car test test-not start end from-end)
  (test-find item (mapcar #'list sequence)
             'car test test-not start end from-end)

  (test-find item (map 'vector #'list sequence)
             #'car test test-not start end from-end)
  (test-find item (map 'vector #'list sequence)
             'car test test-not start end from-end))

(defun test-find-2 (item sequence test start end from-end)
  (if (eq test 'eql)
      (progn (test-find-1 item sequence -1 -1 start end from-end)
             (test-find-1 item sequence #'eql -1 start end from-end)
             (test-find-1 item sequence 'eql -1 start end from-end)
             (test-find-1 item sequence -1 #'eql start end from-end)
             (test-find-1 item sequence -1 'eql start end from-end))
      (progn (test-find-1 item sequence test -1 start end from-end)
             (test-find-1 item sequence (symbol-function test) -1 start end from-end)
             (test-find-1 item sequence -1 test start end from-end)
             (test-find-1 item sequence -1 (symbol-function test) start end from-end))))

(defun test-find-3 (item sequence test start end)
  (test-find-2 item sequence test start end -1)
  (test-find-2 item sequence test start end nil)
  (test-find-2 item sequence test start end t))

(defun test-find-4 (item sequence test)
  (let ((length (length sequence)))
    (test-find-3 item sequence test -1 -1)
    (loop for index from 0 to length
          do (test-find-3 item sequence test -1 index)
             (test-find-3 item sequence test index -1))
    (loop for start from 0 to length
          do (loop for end from start to length
                   do (test-find-3 item sequence test start end)))))

(define-test |find normal|
  (assert-equal nil
                (test-find-4 *i01* (list *i1* *i02* *i2* *i02* *i4*) 'eql)
                (test-find-4 *i5* (list *i1* *i02* *i2* *i02* *i4*) 'eql)
                (test-find-4 *l01* (list *l1* *l02* *l2* *l02* *l4*) 'eq)
                (test-find-4 *l5* (list *l1* *l02* *l2* *l02* *l4*) 'eq)
                (test-find-4 *s01* (list *s1* *s02* *s2* *s02* *s4*) 'string-equal)
                (test-find-4 *s5* (list *s1* *s02* *s2* *s02* *s4*) 'string-equal)))

(define-test |find seq-type=list start=0 end=nil from-end=false test=eql key=identity 1a|
  (let ((element '(1)))
    (assert-equal
     nil
     (find element '()))))

(define-test |find seq-type=list start=0 end=nil from-end=false test=eql key=identity 1b|
  (let ((element '(1)))
    (assert-equal
     nil
     (find element '() :test #'eql))))

(define-test |find seq-type=list start=0 end=nil from-end=false test=eql key=identity 1c|
  (let ((element '(1)))
    (assert-equal
     nil
     (find element '() :test 'eql))))

(define-test |find seq-type=list start=0 end=nil from-end=false test=eql key=identity 2a|
  (assert-equal
   1
   (find 1 '(0 1 2))))

(define-test |find seq-type=list start=0 end=nil from-end=false test=eql key=identity 2b|
  (assert-equal
   1
   (find 1 '(0 1 2) :test #'eql)))

(define-test |find seq-type=list start=0 end=nil from-end=false test=eql key=identity 2c|
  (assert-equal
   1
   (find 1 '(0 1 2) :test 'eql)))

(define-test |find seq-type=list start=error from-end=false test=eql key=identity 2a|
  (assert-error 'type-error
                (find 1 '(0 1 2) :start 'a)))

(define-test |find seq-type=list start=error from-end=false test=eql key=identity 2b|
  (assert-error 'type-error
                (find 1 '(0 1 2) :test #'eql :start 4)))

(define-test |find seq-type=list start=error from-end=false test=eql key=identity 2c|
  (assert-error 'type-error
                (find 1 '(0 1 2) :test #'eql :start -1)))

(define-test |find seq-type=list start=0 end=nil from-end=false test=eql key=other 1a|
  (let ((element '(1)))
    (assert-equal
     nil
     (find element '() :key #'1+))))

(define-test |find seq-type=list start=0 end=nil from-end=false test=eql key=other 1b|
  (let ((element '(1)))
    (assert-equal
     nil
     (find element '() :test #'eql :key #'1+))))

(define-test |find seq-type=list start=0 end=nil from-end=false test=eql key=other 1c|
  (let ((element '(1)))
    (assert-equal
     nil
     (find element '() :test 'eql :key #'1+))))

(define-test |find seq-type=list start=0 end=nil from-end=false test=eql key=other 2a|
  (assert-equal
   1
   (find 2 '(0 1 2) :key #'1+)))

(define-test |find seq-type=list start=0 end=nil from-end=false test=eql key=other 2b|
  (assert-equal
   1
   (find 2 '(0 1 2) :test #'eql :key #'1+)))

(define-test |find seq-type=list start=0 end=nil from-end=false test=eql key=other 2a|
  (assert-equal
   1
   (find 2 '(0 1 2) :test 'eql :key #'1+)))

(define-test |find seq-type=list start=0 end=nil from-end=false test=eql key=identity error 1a|
  (assert-error 'type-error
                (find 3 '(0 1 . 2))))

(define-test |find seq-type=list start=0 end=nil from-end=false test=eql key=identity error 1b|
  (assert-error 'type-error
                (find 3 '(0 1 . 2) :test #'eql)))

(define-test |find seq-type=list start=0 end=nil from-end=false test=eql key=identity error 1c|
  (assert-error 'type-error
                (find 3 '(0 1 . 2) :test 'eql)))

(define-test |find seq-type=list start=other from-end=false test=eql key=identity error 1a|
  (assert-error 'type-error
                (find 3 '(0 1 . 2) :start 2)))

(define-test |find seq-type=list start=other from-end=false test=eql key=identity error 1b|
  (assert-error 'type-error
                (find 3 '(0 1 . 2) :start 2 :test #'eql)))

(define-test |find seq-type=list start=other from-end=false test=eql key=identity error 1c|
  (assert-error 'type-error
                (find 3 '(0 1 . 2) :start 2 :test 'eql)))

(define-test |find seq-type=list start=0 end=nil from-end=false test=eq key=identity error 1a|
  (let ((element '(a)))
    (assert-eq
     nil
     (find element '() :test #'eq))))

(define-test |find seq-type=list start=0 end=nil from-end=false test=eq key=identity 1b|
  (let ((element '(a)))
    (assert-eq
     nil
     (find element '() :test 'eq))))

(define-test |find seq-type=list start=0 end=nil from-end=false test=eq key=identity 2|
  (let ((element (copy-tree '(a))))
    (assert-eq
     nil
     (find element '((a)) :test #'eq))))

(define-test |find seq-type=list start=0 end=nil from-end=false test=eq key=identity 3|
  (let ((element (copy-tree '(a))))
    (assert-eq
     element
     (find element (list '(a) element '(a)) :test #'eq))))

(define-test |find seq-type=list start=0 end=nil from-end=false test=eq key=identity 4|
  (let ((element (copy-tree '(a))))
    (assert-error
     'type-error
     (find element (list* '(a) '(a) '(a) 'b) :test #'eq))))

(define-test |find seq-type=list start=other from-end=false test=eq key=identity 1|
  (let ((element (copy-tree '(a))))
    (assert-eq
     element
     (find element (list '(a) element '(a)) :test #'eq :start 1))))

(define-test |find seq-type=list start=other from-end=false test=eq key=identity 2|
  (let ((element (copy-tree '(a))))
    (assert-eq
     element
     (find element (list '(a) element '(a)) :test #'eq :start 1))))

(define-test |find seq-type=list start=other from-end=false test=eq key=identity 3|
  (let ((element (copy-tree '(a))))
    (assert-error
     'type-error
     (find element (list* '(a) element '(a)  'b) :test #'eq :start 2))))

(define-test |find seq-type=list start=other from-end=false test=eq key=other 1|
  (let ((element (copy-tree '(a))))
    (assert-eq
     element
     (car (find element (list '((a)) (list element) '((a)))
                :test #'eq :start 1 :key #'car)))))

(define-test |find seq-type=list start=other from-end=false test=eq key=other 2|
  (let ((element (copy-tree '(a))))
    (assert-eq
     element
     (car (find element (list '((a)) (list element) '((a)))
                :test #'eq :start 1 :key #'car)))))

(define-test |find seq-type=list start=other from-end=false test=eq key=other 3|
  (let ((element (copy-tree '(a))))
    (assert-error
     'type-error
     (car (find element (list* '((a)) (list element) '((a)) 'b)
                :test #'eq :start 2 :key #'car)))))

(define-test |find seq-type=list start=0 end=nil from-end=false test-not=eql key=identity 1a|
  (assert-equal
   1
   (find 0 '(0 1 2) :test-not #'eql)))

(define-test |find seq-type=list start=0 end=nil from-end=false test-not=eql key=identity 1b|
  (assert-equal
   1
   (find 0 '(0 1 2) :test-not 'eql)))

(define-test |find seq-type=list start=0 end=nil from-end=false test-not=eql key=identity 2a|
  (assert-equal
   nil
   (find 0 '(0 0 0) :test-not #'eql)))

(define-test |find seq-type=list start=0 end=nil from-end=false test-not=eql key=identity 2b|
  (assert-equal
   nil
   (find 0 '(0 0 0) :test-not 'eql)))

(define-test |find seq-type=list start=0 end=nil from-end=false test-not=eql key=other 1a|
  (assert-equal
   1
   (find 1 '(0 1 2) :test-not #'eql :key #'1+)))

(define-test |find seq-type=list start=0 end=nil from-end=false test-not=eql key=other 1b|
  (assert-equal
   1
   (find 1 '(0 1 2) :test-not 'eql :key #'1+)))

(define-test |find seq-type=list start=0 end=nil from-end=false test-not=eql key=other 2a|
  (assert-equal
   nil
   (find 1 '(0 0 0) :test-not #'eql :key #'1+)))

(define-test |find seq-type=list start=0 end=nil from-end=false test-not=eql key=other 2b|
  (assert-equal
   nil
   (find 1 '(0 0 0) :test-not 'eql :key #'1+)))

(define-test |find seq-type=list start=0 end=nil from-end=false test-not=eq key=identity 1a|
  (assert-equal
   'a
   (find 'z '(z a b) :test-not #'eq)))

(define-test |find seq-type=list start=0 end=nil from-end=false test-not=eq key=identity 1b|
  (assert-equal
   'a
   (find 'z '(z a b) :test-not 'eq)))

(define-test |find seq-type=list start=0 end=nil from-end=false test-not=eq key=identity 2a|
  (assert-equal
   nil
   (find 'z '(z z z) :test-not #'eq)))

(define-test |find seq-type=list start=0 end=nil from-end=false test-not=eq key=identity 2b|
  (assert-equal
   nil
   (find 'z '(z z z) :test-not 'eq)))

(define-test |find seq-type=list start=0 end=nil from-end=false test-not=eq key=other 1a|
  (assert-equal
   'a
   (find 'a '(z a b) :test-not #'eq :key (lambda (x) (case x (z 'a) (a 'b) (b 'c))))))

(define-test |find seq-type=list start=0 end=nil from-end=false test-not=eq key=other 1b|
  (assert-equal
   'a
   (find 'a '(z a b) :test-not 'eq :key (lambda (x) (case x (z 'a) (a 'b) (b 'c))))))

(define-test |find seq-type=list start=0 end=nil from-end=false test-not=eq key=other 2a|
  (assert-equal
   nil
   (find 'a '(z z z) :test-not #'eq :key (lambda (x) (case x (z 'a) (a 'b) (b 'c))))))

(define-test |find seq-type=list start=0 end=nil from-end=false test-not=eq key=other 2b|
  (assert-equal
   nil
   (find 'a '(z z z) :test-not 'eq :key (lambda (x) (case x (z 'a) (a 'b) (b 'c))))))

(define-test |find seq-type=list start=0 end=other from-end=false test=eql key=identity 1a|
  (assert-equal
   nil
   (find 1 '(0 1 2) :end 1)))

(define-test |find seq-type=list start=0 end=other from-end=false test=eql key=identity 1b|
  (assert-equal
   nil
   (find 1 '(0 1 2) :end 1 :test #'eql)))

(define-test |find seq-type=list start=0 end=other from-end=false test=eql key=identity 1c|
  (assert-equal
   nil
   (find 1 '(0 1 2) :end 1 :test 'eql)))

(define-test |find seq-type=list start=0 end=other from-end=false test=eql key=identity 2a|
  (assert-equal
   1
   (find 1 '(0 1 2) :end 2)))

(define-test |find seq-type=list start=0 end=other from-end=false test=eql key=identity 2b|
  (assert-equal
   1
   (find 1 '(0 1 2) :end 2 :test #'eql)))

(define-test |find seq-type=list start=0 end=other from-end=false test=eql key=identity 2c|
  (assert-equal
   1
   (find 1 '(0 1 2) :end 2 :test 'eql)))

(define-test |find seq-type=list start=0 end=error from-end=false test=eql key=identity 1a|
  (assert-error 'type-error
                (find 3 '(0 1 2) :end 4)))

(define-test |find seq-type=list start=0 end=error from-end=false test=eql key=identity 1b|
  (assert-error 'type-error
                (find 3 '(0 1 2) :end 4 :test #'eql)))

(define-test |find seq-type=list start=0 end=error from-end=false test=eql key=identity 1c|
  (assert-error 'type-error
                (find 3 '(0 1 2) :end 4 :test 'eql)))

(define-test |find seq-type=list start=0 end=other from-end=false test=eql key=other 1a|
  (assert-equal
   nil
   (find 2 '(0 1 2) :end 1 :key #'1+)))

(define-test |find seq-type=list start=0 end=other from-end=false test=eql key=other 1b|
  (assert-equal
   nil
   (find 2 '(0 1 2) :end 1 :test #'eql :key #'1+)))

(define-test |find seq-type=list start=0 end=other from-end=false test=eql key=other 1c|
  (assert-equal
   nil
   (find 2 '(0 1 2) :end 1 :test 'eql :key #'1+)))

(define-test |find seq-type=list start=0 end=other from-end=false test=eql key=other 2a|
  (assert-equal
   1
   (find 2 '(0 1 2) :end 2 :key #'1+)))

(define-test |find seq-type=list start=0 end=other from-end=false test=eql key=other 2b|
  (assert-equal
   1
   (find 2 '(0 1 2) :end 2 :test #'eql :key #'1+)))

(define-test |find seq-type=list start=0 end=other from-end=false test=eql key=other 2c|
  (assert-equal
   1
   (find 2 '(0 1 2) :end 2 :test 'eql :key #'1+)))

(define-test |find seq-type=list start=0 end=error from-end=false test=eql key=other 1a|
  (assert-error 'type-error
                (find 4 '(0 1 2) :end 4 :key #'1+)))

(define-test |find seq-type=list start=0 end=error from-end=false test=eql key=other 1b|
  (assert-error 'type-error
                (find 4 '(0 1 2) :end 4 :test #'eql :key #'1+)))

(define-test |find seq-type=list start=0 end=error from-end=false test=eql key=other 1c|
  (assert-error 'type-error
                (find 4 '(0 1 2) :end 4 :test 'eql :key #'1+)))

(define-test |find seq-type=list start=0 end=other from-end=false test=eq key=identity 1b|
  (assert-equal
   nil
   (find 'b '(a b c) :end 1 :test #'eq)))

(define-test |find seq-type=list start=0 end=other from-end=false test=eq key=identity 1c|
  (assert-equal
   nil
   (find 'b '(a b c) :end 1 :test 'eq)))

(define-test |find seq-type=list start=0 end=other from-end=false test=eq key=identity 2b|
  (assert-equal
   'b
   (find 'b '(a b c) :end 2 :test #'eq)))

(define-test |find seq-type=list start=0 end=other from-end=false test=eq key=identity 2c|
  (assert-equal
   'b
   (find 'b '(a b c) :end 2 :test 'eq)))

(define-test |find seq-type=list start=0 end=error from-end=false test=eq key=identity 1b|
  (assert-error 'type-error
                (find 'd '(a b c) :end 4 :test #'eq)))

(define-test |find seq-type=list start=0 end=error from-end=false test=eq key=identity 1c|
  (assert-error 'type-error
                (find 'd '(a b c) :end 4 :test 'eq)))

(define-test |find seq-type=list start=0 end=other from-end=false test=eq key=other 1b|
  (assert-equal
   nil
   (find 'c '(a b c) :end 1 :test #'eq :key (lambda (x) (case x (a 'b) (b 'c) (c 'd))))))

(define-test |find seq-type=list start=0 end=other from-end=false test=eq key=other 1c|
  (assert-equal
   nil
   (find 'c '(a b c) :end 1 :test 'eq :key (lambda (x) (case x (a 'b) (b 'c) (c 'd))))))

(define-test |find seq-type=list start=0 end=other from-end=false test=eq key=other 2b|
  (assert-equal
   'b
   (find 'c '(a b c) :end 2 :test #'eq :key (lambda (x) (case x (a 'b) (b 'c) (c 'd))))))

(define-test |find seq-type=list start=0 end=other from-end=false test=eq key=other 2c|
  (assert-equal
   'b
   (find 'c '(a b c) :end 2 :test 'eq :key (lambda (x) (case x (a 'b) (b 'c) (c 'd))))))

(define-test |find seq-type=list start=0 end=error from-end=false test=eq key=other 1b|
  (assert-error 'type-error
                (find 'e '(a b c) :end 4 :test #'eq :key (lambda (x) (case x (a 'b) (b 'c) (c 'd))))))

(define-test |find seq-type=list start=0 end=error from-end=false test=eq key=other 1c|
  (assert-error 'type-error
                (find 'e '(a b c) :end 4 :test 'eq :key (lambda (x) (case x (a 'b) (b 'c) (c 'd))))))

(define-test |find seq-type=list start=0 end=other from-end=false test-not=eql key=identity 1a|
  (assert-equal
   nil
   (find '1 '(1 1 1) :end 2 :test-not #'eql)))

(define-test |find seq-type=list start=0 end=other from-end=false test-not=eql key=identity 1b|
  (assert-equal
   nil
   (find '1 '(1 1 1) :end 2 :test-not 'eql)))

(define-test |find seq-type=list start=0 end=other from-end=false test-not=eql key=identity 2a|
  (assert-equal
   2
   (find '1 '(1 2 1) :end 2 :test-not #'eql)))

(define-test |find seq-type=list start=0 end=other from-end=false test-not=eql key=identity 2b|
  (assert-equal
   2
   (find '1 '(1 2 1) :end 2 :test-not 'eql)))

(define-test |find seq-type=list start=0 end=other from-end=false test-not=eql key=other 1a|
  (assert-equal
   nil
   (find '2 '(1 1 1) :end 2 :test-not #'eql :key #'1+)))

(define-test |find seq-type=list start=0 end=other from-end=false test-not=eql key=other 1b|
  (assert-equal
   nil
   (find '2 '(1 1 1) :end 2 :test-not 'eql :key #'1+)))

(define-test |find seq-type=list start=0 end=other from-end=false test-not=eql key=other 2a|
  (assert-equal
   2
   (find '2 '(1 2 1) :end 2 :test-not #'eql :key #'1+)))

(define-test |find seq-type=list start=0 end=other from-end=false test-not=eql key=other 2b|
  (assert-equal
   2
   (find '2 '(1 2 1) :end 2 :test-not 'eql :key #'1+)))

(define-test |find seq-type=list start=0 end=other from-end=false test-not=eq key=identity 1a|
  (assert-equal
   nil
   (find 'a '(a a a) :end 2 :test-not #'eq)))

(define-test |find seq-type=list start=0 end=other from-end=false test-not=eq key=identity 1b|
  (assert-equal
   nil
   (find 'a '(a a a) :end 2 :test-not 'eq)))

(define-test |find seq-type=list start=0 end=other from-end=false test-not=eq key=identity 2a|
  (assert-equal
   'b
   (find 'a '(a b a) :end 2 :test-not #'eq)))

(define-test |find seq-type=list start=0 end=other from-end=false test-not=eq key=identity 2b|
  (assert-equal
   'b
   (find 'a '(a b a) :end 2 :test-not 'eq)))

(define-test |find seq-type=list start=0 end=other from-end=false test-not=eq key=other 1a|
  (assert-equal
   nil
   (find 'b '(a a a) :end 2 :test-not #'eq :key (lambda (x) (case x (a 'b) (b 'c))))))

(define-test |find seq-type=list start=0 end=other from-end=false test-not=eq key=other 1b|
  (assert-equal
   nil
   (find 'b '(a a a) :end 2 :test-not 'eq :key (lambda (x) (case x (a 'b) (b 'c))))))

(define-test |find seq-type=list start=0 end=other from-end=false test-not=eq key=other 2a|
  (assert-equal
   'b
   (find 'b '(a b a) :end 2 :test-not #'eq :key (lambda (x) (case x (a 'b) (b 'c))))))

(define-test |find seq-type=list start=0 end=other from-end=false test-not=eq key=other 2b|
  (assert-equal
   'b
   (find 'b '(a b a) :end 2 :test-not 'eq :key (lambda (x) (case x (a 'b) (b 'c))))))

(define-test |find seq-type=vector start=0 end=nil from-end=false test=eql key=identity 1a|
  (let ((element '(1)))
    (assert-equal
     nil
     (find element #()))))

(define-test |find seq-type=vector start=0 end=nil from-end=false test=eql key=identity 1b|
  (let ((element '(1)))
    (assert-equal
     nil
     (find element #() :test #'eql))))

(define-test |find seq-type=vector start=0 end=nil from-end=false test=eql key=identity 1c|
  (let ((element '(1)))
    (assert-equal
     nil
     (find element #() :test 'eql))))

(define-test |find seq-type=vector start=0 end=nil from-end=false test=eql key=identity 2a|
  (assert-equal
   1
   (find 1 #(0 1 2))))

(define-test |find seq-type=vector start=0 end=nil from-end=false test=eql key=identity 2b|
  (assert-equal
   1
   (find 1 #(0 1 2) :test #'eql)))

(define-test |find seq-type=vector start=0 end=nil from-end=false test=eql key=identity 2c|
  (assert-equal
   1
   (find 1 #(0 1 2) :test 'eql)))

(define-test |find seq-type=vector start=error from-end=false test=eql key=identity 2a|
  (assert-error 'type-error
                (find 1 #(0 1 2) :start 'a)))

(define-test |find seq-type=vector start=error from-end=false test=eql key=identity 2b|
  (assert-error 'type-error
                (find 1 #(0 1 2) :test #'eql :start 4)))

(define-test |find seq-type=vector start=error from-end=false test=eql key=identity 2c|
  (assert-error 'type-error
                (find 1 #(0 1 2) :test #'eql :start -1)))

(define-test |find seq-type=vector start=0 end=nil from-end=false test=eql key=other 1a|
  (let ((element '(1)))
    (assert-equal
     nil
     (find element #() :key #'1+))))

(define-test |find seq-type=vector start=0 end=nil from-end=false test=eql key=other 1b|
  (let ((element '(1)))
    (assert-equal
     nil
     (find element #() :test #'eql :key #'1+))))

(define-test |find seq-type=vector start=0 end=nil from-end=false test=eql key=other 1c|
  (let ((element '(1)))
    (assert-equal
     nil
     (find element #() :test 'eql :key #'1+))))

(define-test |find seq-type=vector start=0 end=nil from-end=false test=eql key=other 2a|
  (assert-equal
   1
   (find 2 #(0 1 2) :key #'1+)))

(define-test |find seq-type=vector start=0 end=nil from-end=false test=eql key=other 2b|
  (assert-equal
   1
   (find 2 #(0 1 2) :test #'eql :key #'1+)))

(define-test |find seq-type=vector start=0 end=nil from-end=false test=eql key=other 2a|
  (assert-equal
   1
   (find 2 #(0 1 2) :test 'eql :key #'1+)))

(define-test |find seq-type=vector start=0 end=nil from-end=false test=eq key=identity error 1a|
  (let ((element '(a)))
    (assert-eq
     nil
     (find element #() :test #'eq))))

(define-test |find seq-type=vector start=0 end=nil from-end=false test=eq key=identity 1b|
  (let ((element '(a)))
    (assert-eq
     nil
     (find element #() :test 'eq))))

(define-test |find seq-type=vector start=0 end=nil from-end=false test=eq key=identity 2|
  (let ((element (copy-tree '(a))))
    (assert-eq
     nil
     (find element #((a)) :test #'eq))))

(define-test |find seq-type=vector start=0 end=nil from-end=false test=eq key=identity 3|
  (let ((element (copy-tree '(a))))
    (assert-eq
     element
     (find element (vector '(a) element '(a)) :test #'eq))))

(define-test |find seq-type=vector start=other from-end=false test=eq key=identity 1|
  (let ((element (copy-tree '(a))))
    (assert-eq
     element
     (find element (vector '(a) element '(a)) :test #'eq :start 1))))

(define-test |find seq-type=vector start=other from-end=false test=eq key=identity 2|
  (let ((element (copy-tree '(a))))
    (assert-eq
     element
     (find element (vector '(a) element '(a)) :test #'eq :start 1))))

(define-test |find seq-type=vector start=other from-end=false test=eq key=other 1|
  (let ((element (copy-tree '(a))))
    (assert-eq
     element
     (car (find element (vector '((a)) (list element) '((a)))
                :test #'eq :start 1 :key #'car)))))

(define-test |find seq-type=vector start=other from-end=false test=eq key=other 2|
  (let ((element (copy-tree '(a))))
    (assert-eq
     element
     (car (find element (vector '((a)) (list element) '((a)))
                :test #'eq :start 1 :key #'car)))))

(define-test |find seq-type=vector start=0 end=nil from-end=false test-not=eql key=identity 1a|
  (assert-equal
   1
   (find 0 #(0 1 2) :test-not #'eql)))

(define-test |find seq-type=vector start=0 end=nil from-end=false test-not=eql key=identity 1b|
  (assert-equal
   1
   (find 0 #(0 1 2) :test-not 'eql)))

(define-test |find seq-type=vector start=0 end=nil from-end=false test-not=eql key=identity 2a|
  (assert-equal
   nil
   (find 0 #(0 0 0) :test-not #'eql)))

(define-test |find seq-type=vector start=0 end=nil from-end=false test-not=eql key=identity 2b|
  (assert-equal
   nil
   (find 0 #(0 0 0) :test-not 'eql)))

(define-test |find seq-type=vector start=0 end=nil from-end=false test-not=eql key=other 1a|
  (assert-equal
   1
   (find 1 #(0 1 2) :test-not #'eql :key #'1+)))

(define-test |find seq-type=vector start=0 end=nil from-end=false test-not=eql key=other 1b|
  (assert-equal
   1
   (find 1 #(0 1 2) :test-not 'eql :key #'1+)))

(define-test |find seq-type=vector start=0 end=nil from-end=false test-not=eql key=other 2a|
  (assert-equal
   nil
   (find 1 #(0 0 0) :test-not #'eql :key #'1+)))

(define-test |find seq-type=vector start=0 end=nil from-end=false test-not=eql key=other 2b|
  (assert-equal
   nil
   (find 1 #(0 0 0) :test-not 'eql :key #'1+)))

(define-test |find seq-type=vector start=0 end=nil from-end=false test-not=eq key=identity 1a|
  (assert-equal
   'a
   (find 'z #(z a b) :test-not #'eq)))

(define-test |find seq-type=vector start=0 end=nil from-end=false test-not=eq key=identity 1b|
  (assert-equal
   'a
   (find 'z #(z a b) :test-not 'eq)))

(define-test |find seq-type=vector start=0 end=nil from-end=false test-not=eq key=identity 2a|
  (assert-equal
   nil
   (find 'z #(z z z) :test-not #'eq)))

(define-test |find seq-type=vector start=0 end=nil from-end=false test-not=eq key=identity 2b|
  (assert-equal
   nil
   (find 'z #(z z z) :test-not 'eq)))

(define-test |find seq-type=vector start=0 end=nil from-end=false test-not=eq key=other 1a|
  (assert-equal
   'a
   (find 'a #(z a b) :test-not #'eq :key (lambda (x) (case x (z 'a) (a 'b) (b 'c))))))

(define-test |find seq-type=vector start=0 end=nil from-end=false test-not=eq key=other 1b|
  (assert-equal
   'a
   (find 'a #(z a b) :test-not 'eq :key (lambda (x) (case x (z 'a) (a 'b) (b 'c))))))

(define-test |find seq-type=vector start=0 end=nil from-end=false test-not=eq key=other 2a|
  (assert-equal
   nil
   (find 'a #(z z z) :test-not #'eq :key (lambda (x) (case x (z 'a) (a 'b) (b 'c))))))

(define-test |find seq-type=vector start=0 end=nil from-end=false test-not=eq key=other 2b|
  (assert-equal
   nil
   (find 'a #(z z z) :test-not 'eq :key (lambda (x) (case x (z 'a) (a 'b) (b 'c))))))

(define-test |find seq-type=vector start=0 end=other from-end=false test=eql key=identity 1a|
  (assert-equal
   nil
   (find 1 #(0 1 2) :end 1)))

(define-test |find seq-type=vector start=0 end=other from-end=false test=eql key=identity 1b|
  (assert-equal
   nil
   (find 1 #(0 1 2) :end 1 :test #'eql)))

(define-test |find seq-type=vector start=0 end=other from-end=false test=eql key=identity 1c|
  (assert-equal
   nil
   (find 1 #(0 1 2) :end 1 :test 'eql)))

(define-test |find seq-type=vector start=0 end=other from-end=false test=eql key=identity 2a|
  (assert-equal
   1
   (find 1 #(0 1 2) :end 2)))

(define-test |find seq-type=vector start=0 end=other from-end=false test=eql key=identity 2b|
  (assert-equal
   1
   (find 1 #(0 1 2) :end 2 :test #'eql)))

(define-test |find seq-type=vector start=0 end=other from-end=false test=eql key=identity 2c|
  (assert-equal
   1
   (find 1 #(0 1 2) :end 2 :test 'eql)))

(define-test |find seq-type=vector start=0 end=error from-end=false test=eql key=identity 1a|
  (assert-error 'type-error
                (find 3 #(0 1 2) :end 4)))

(define-test |find seq-type=vector start=0 end=error from-end=false test=eql key=identity 1b|
  (assert-error 'type-error
                (find 3 #(0 1 2) :end 4 :test #'eql)))

(define-test |find seq-type=vector start=0 end=error from-end=false test=eql key=identity 1c|
  (assert-error 'type-error
                (find 3 #(0 1 2) :end 4 :test 'eql)))

(define-test |find seq-type=vector start=0 end=other from-end=false test=eql key=other 1a|
  (assert-equal
   nil
   (find 2 #(0 1 2) :end 1 :key #'1+)))

(define-test |find seq-type=vector start=0 end=other from-end=false test=eql key=other 1b|
  (assert-equal
   nil
   (find 2 #(0 1 2) :end 1 :test #'eql :key #'1+)))

(define-test |find seq-type=vector start=0 end=other from-end=false test=eql key=other 1c|
  (assert-equal
   nil
   (find 2 #(0 1 2) :end 1 :test 'eql :key #'1+)))

(define-test |find seq-type=vector start=0 end=other from-end=false test=eql key=other 2a|
  (assert-equal
   1
   (find 2 #(0 1 2) :end 2 :key #'1+)))

(define-test |find seq-type=vector start=0 end=other from-end=false test=eql key=other 2b|
  (assert-equal
   1
   (find 2 #(0 1 2) :end 2 :test #'eql :key #'1+)))

(define-test |find seq-type=vector start=0 end=other from-end=false test=eql key=other 2c|
  (assert-equal
   1
   (find 2 #(0 1 2) :end 2 :test 'eql :key #'1+)))

(define-test |find seq-type=vector start=0 end=error from-end=false test=eql key=other 1a|
  (assert-error 'type-error
                (find 4 #(0 1 2) :end 4 :key #'1+)))

(define-test |find seq-type=vector start=0 end=error from-end=false test=eql key=other 1b|
  (assert-error 'type-error
                (find 4 #(0 1 2) :end 4 :test #'eql :key #'1+)))

(define-test |find seq-type=vector start=0 end=error from-end=false test=eql key=other 1c|
  (assert-error 'type-error
                (find 4 #(0 1 2) :end 4 :test 'eql :key #'1+)))

(define-test |find seq-type=vector start=0 end=other from-end=false test=eq key=identity 1b|
  (assert-equal
   nil
   (find 'b #(a b c) :end 1 :test #'eq)))

(define-test |find seq-type=vector start=0 end=other from-end=false test=eq key=identity 1c|
  (assert-equal
   nil
   (find 'b #(a b c) :end 1 :test 'eq)))

(define-test |find seq-type=vector start=0 end=other from-end=false test=eq key=identity 2b|
  (assert-equal
   'b
   (find 'b #(a b c) :end 2 :test #'eq)))

(define-test |find seq-type=vector start=0 end=other from-end=false test=eq key=identity 2c|
  (assert-equal
   'b
   (find 'b #(a b c) :end 2 :test 'eq)))

(define-test |find seq-type=vector start=0 end=error from-end=false test=eq key=identity 1b|
  (assert-error 'type-error
                (find 'd #(a b c) :end 4 :test #'eq)))

(define-test |find seq-type=vector start=0 end=error from-end=false test=eq key=identity 1c|
  (assert-error 'type-error
                (find 'd #(a b c) :end 4 :test 'eq)))

(define-test |find seq-type=vector start=0 end=other from-end=false test=eq key=other 1b|
  (assert-equal
   nil
   (find 'c #(a b c) :end 1 :test #'eq :key (lambda (x) (case x (a 'b) (b 'c) (c 'd))))))

(define-test |find seq-type=vector start=0 end=other from-end=false test=eq key=other 1c|
  (assert-equal
   nil
   (find 'c #(a b c) :end 1 :test 'eq :key (lambda (x) (case x (a 'b) (b 'c) (c 'd))))))

(define-test |find seq-type=vector start=0 end=other from-end=false test=eq key=other 2b|
  (assert-equal
   'b
   (find 'c #(a b c) :end 2 :test #'eq :key (lambda (x) (case x (a 'b) (b 'c) (c 'd))))))

(define-test |find seq-type=vector start=0 end=other from-end=false test=eq key=other 2c|
  (assert-equal
   'b
   (find 'c #(a b c) :end 2 :test 'eq :key (lambda (x) (case x (a 'b) (b 'c) (c 'd))))))

(define-test |find seq-type=vector start=0 end=error from-end=false test=eq key=other 1b|
  (assert-error 'type-error
                (find 'e #(a b c) :end 4 :test #'eq :key (lambda (x) (case x (a 'b) (b 'c) (c 'd))))))

(define-test |find seq-type=vector start=0 end=error from-end=false test=eq key=other 1c|
  (assert-error 'type-error
                (find 'e #(a b c) :end 4 :test 'eq :key (lambda (x) (case x (a 'b) (b 'c) (c 'd))))))

(define-test |find seq-type=vector start=0 end=other from-end=false test-not=eql key=identity 1a|
  (assert-equal
   nil
   (find '1 #(1 1 1) :end 2 :test-not #'eql)))

(define-test |find seq-type=vector start=0 end=other from-end=false test-not=eql key=identity 1b|
  (assert-equal
   nil
   (find '1 #(1 1 1) :end 2 :test-not 'eql)))

(define-test |find seq-type=vector start=0 end=other from-end=false test-not=eql key=identity 2a|
  (assert-equal
   2
   (find '1 #(1 2 1) :end 2 :test-not #'eql)))

(define-test |find seq-type=vector start=0 end=other from-end=false test-not=eql key=identity 2b|
  (assert-equal
   2
   (find '1 #(1 2 1) :end 2 :test-not 'eql)))

(define-test |find seq-type=vector start=0 end=other from-end=false test-not=eql key=other 1a|
  (assert-equal
   nil
   (find '2 #(1 1 1) :end 2 :test-not #'eql :key #'1+)))

(define-test |find seq-type=vector start=0 end=other from-end=false test-not=eql key=other 1b|
  (assert-equal
   nil
   (find '2 #(1 1 1) :end 2 :test-not 'eql :key #'1+)))

(define-test |find seq-type=vector start=0 end=other from-end=false test-not=eql key=other 2a|
  (assert-equal
   2
   (find '2 #(1 2 1) :end 2 :test-not #'eql :key #'1+)))

(define-test |find seq-type=vector start=0 end=other from-end=false test-not=eql key=other 2b|
  (assert-equal
   2
   (find '2 #(1 2 1) :end 2 :test-not 'eql :key #'1+)))

(define-test |find seq-type=vector start=0 end=other from-end=false test-not=eq key=identity 1a|
  (assert-equal
   nil
   (find 'a #(a a a) :end 2 :test-not #'eq)))

(define-test |find seq-type=vector start=0 end=other from-end=false test-not=eq key=identity 1b|
  (assert-equal
   nil
   (find 'a #(a a a) :end 2 :test-not 'eq)))

(define-test |find seq-type=vector start=0 end=other from-end=false test-not=eq key=identity 2a|
  (assert-equal
   'b
   (find 'a #(a b a) :end 2 :test-not #'eq)))

(define-test |find seq-type=vector start=0 end=other from-end=false test-not=eq key=identity 2b|
  (assert-equal
   'b
   (find 'a #(a b a) :end 2 :test-not 'eq)))

(define-test |find seq-type=vector start=0 end=other from-end=false test-not=eq key=other 1a|
  (assert-equal
   nil
   (find 'b #(a a a) :end 2 :test-not #'eq :key (lambda (x) (case x (a 'b) (b 'c))))))

(define-test |find seq-type=vector start=0 end=other from-end=false test-not=eq key=other 1b|
  (assert-equal
   nil
   (find 'b #(a a a) :end 2 :test-not 'eq :key (lambda (x) (case x (a 'b) (b 'c))))))

(define-test |find seq-type=vector start=0 end=other from-end=false test-not=eq key=other 2a|
  (assert-equal
   'b
   (find 'b #(a b a) :end 2 :test-not #'eq :key (lambda (x) (case x (a 'b) (b 'c))))))

(define-test |find seq-type=vector start=0 end=other from-end=false test-not=eq key=other 2b|
  (assert-equal
   'b
   (find 'b #(a b a) :end 2 :test-not 'eq :key (lambda (x) (case x (a 'b) (b 'c))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Tests for function find-if

(define-test find-if-list.1
  (assert-equal 'nil
                (find-if #'identity ())))

(define-test find-if-list.2
  (assert-equal 'a
                (find-if #'identity '(a))))

(define-test find-if-list.2a
  (assert-equal 'a
                (find-if 'identity '(a))))

(define-test find-if-list.3
  (assert-equal '2
                (find-if #'evenp '(1 2 4 8 3 1 6 7))))

(define-test find-if-list.4
  (assert-equal '6
                (find-if #'evenp '(1 2 4 8 3 1 6 7) :from-end t)))

(define-test find-if-list.5
  (assert-equal '(2 2 4 8 6 6 6 nil)
                (loop for i from 0 to 7 collect
                  (find-if #'evenp '(1 2 4 8 3 1 6 7) :start i))))

(define-test find-if-list.6
  (assert-equal '(2 2 4 8 6 6 6 nil)
                (loop for i from 0 to 7 collect
                  (find-if #'evenp '(1 2 4 8 3 1 6 7) :start i :end nil))))

(define-test find-if-list.7
  (assert-equal '(6 6 6 6 6 6 6 nil)
                (loop for i from 0 to 7 collect
                  (find-if #'evenp '(1 2 4 8 3 1 6 7) :start i :from-end t))))

(define-test find-if-list.8
  (assert-equal '(6 6 6 6 6 6 6 nil)
                (loop for i from 0 to 7 collect
                  (find-if #'evenp '(1 2 4 8 3 1 6 7) :start i :end nil :from-end t))))

(define-test find-if-list.9
  (assert-equal '(nil nil 2 2 2 2 2 2 2)
                (loop for i from 0 to 8 collect
                  (find-if #'evenp '(1 2 4 8 3 1 6 7) :end i))))

(define-test find-if-list.10
  (assert-equal '(nil nil 2 4 8 8 8 6 6)
                (loop for i from 0 to 8 collect
                  (find-if #'evenp '(1 2 4 8 3 1 6 7) :end i :from-end t))))

(define-test find-if-list.11
  (assert-equal '((nil 2 2 2 2 2 2 2)
                  (2 2 2 2 2 2 2)
                  (4 4 4 4 4 4)
                  (8 8 8 8 8)
                  (nil nil 6 6)
                  (nil 6 6)
                  (6 6)
                  (nil))
                (loop for j from 0 to 7
                      collect
                   (loop for i from (1+ j) to 8 collect
                     (find-if #'evenp '(1 2 4 8 3 1 6 7) :start j :end i)))))

(define-test find-if-list.12
  (assert-equal '((nil 2 4 8 8 8 6 6)
                  (2 4 8 8 8 6 6)
                  (4 8 8 8 6 6)
                  (8 8 8 6 6)
                  (nil nil 6 6)
                  (nil 6 6)
                  (6 6)
                  (nil))
                (loop for j from 0 to 7
                      collect
                   (loop for i from (1+ j) to 8 collect
                     (find-if #'evenp '(1 2 4 8 3 1 6 7) :start j :end i
                          :from-end t)))))

(define-test find-if-list.13
  (assert-equal '(1 11 11 45 45 71 nil)
                (loop for i from 0 to 6
                      collect
                   (find-if #'evenp '(1 6 11 32 45 71 100) :key #'1+ :start i))))

(define-test find-if-list.14
  (assert-equal '(71 71 71 71 71 71 nil)
                (loop for i from 0 to 6
                      collect
                   (find-if #'evenp '(1 6 11 32 45 71 100) :key '1+ :start i :from-end t))))

(define-test find-if-list.15
  (assert-equal '(nil 1 1 1 1 1 1 1)
                (loop for i from 0 to 7
                      collect
                   (find-if #'evenp '(1 6 11 32 45 71 100) :key #'1+ :end i))))

(define-test find-if-list.16
  (assert-equal '(nil 1 1 11 11 45 71 71)
                (loop for i from 0 to 7
                      collect
                   (find-if #'evenp '(1 6 11 32 45 71 100) :key '1+ :end i :from-end t))))

(define-test find-if-list.17
  (assert-equal '((nil 2 2 2 2 2 2 2)
                  (2 2 2 2 2 2 2)
                  (4 4 4 4 4 4)
                  (8 8 8 8 8)
                  (nil nil 6 6)
                  (nil 6 6)
                  (6 6)
                  (nil))
                (loop for j from 0 to 7
                      collect
                   (loop for i from (1+ j) to 8 collect
                     (find-if #'oddp '(1 2 4 8 3 1 6 7) :start j :end i :key #'1-)))))

(define-test find-if-list.18
  (assert-equal '((nil 2 4 8 8 8 6 6)
                  (2 4 8 8 8 6 6)
                  (4 8 8 8 6 6)
                  (8 8 8 6 6)
                  (nil nil 6 6)
                  (nil 6 6)
                  (6 6)
                  (nil))
                (loop for j from 0 to 7
                      collect
                   (loop for i from (1+ j) to 8 collect
                     (find-if #'oddp '(1 2 4 8 3 1 6 7) :start j :end i
                          :from-end t :key #'1+)))))

;;; tests for vectors

(define-test find-if-vector.1
  (assert-equal 'nil
                (find-if #'identity #())))

(define-test find-if-vector.2
  (assert-equal 'a
                (find-if #'identity #(a))))

(define-test find-if-vector.2a
  (assert-equal 'a
                (find-if 'identity #(a))))

(define-test find-if-vector.3
  (assert-equal '2
                (find-if #'evenp #(1 2 4 8 3 1 6 7))))

(define-test find-if-vector.4
  (assert-equal '6
                (find-if #'evenp #(1 2 4 8 3 1 6 7) :from-end t)))

(define-test find-if-vector.5
  (assert-equal '(2 2 4 8 6 6 6 nil)
                (loop for i from 0 to 7 collect
                  (find-if #'evenp #(1 2 4 8 3 1 6 7) :start i))))

(define-test find-if-vector.6
  (assert-equal '(2 2 4 8 6 6 6 nil)
                (loop for i from 0 to 7 collect
                  (find-if #'evenp #(1 2 4 8 3 1 6 7) :start i :end nil))))

(define-test find-if-vector.7
  (assert-equal '(6 6 6 6 6 6 6 nil)
                (loop for i from 0 to 7 collect
                  (find-if #'evenp #(1 2 4 8 3 1 6 7) :start i :from-end t))))

(define-test find-if-vector.8
  (assert-equal '(6 6 6 6 6 6 6 nil)
                (loop for i from 0 to 7 collect
                  (find-if #'evenp #(1 2 4 8 3 1 6 7) :start i :end nil :from-end t))))

(define-test find-if-vector.9
  (assert-equal '(nil nil 2 2 2 2 2 2 2)
                (loop for i from 0 to 8 collect
                  (find-if #'evenp #(1 2 4 8 3 1 6 7) :end i))))

(define-test find-if-vector.10
  (assert-equal '(nil nil 2 4 8 8 8 6 6)
                (loop for i from 0 to 8 collect
                  (find-if #'evenp #(1 2 4 8 3 1 6 7) :end i :from-end t))))

(define-test find-if-vector.11
  (assert-equal '((nil 2 2 2 2 2 2 2)
                  (2 2 2 2 2 2 2)
                  (4 4 4 4 4 4)
                  (8 8 8 8 8)
                  (nil nil 6 6)
                  (nil 6 6)
                  (6 6)
                  (nil))
                (loop for j from 0 to 7
                      collect
                   (loop for i from (1+ j) to 8 collect
                     (find-if #'evenp #(1 2 4 8 3 1 6 7) :start j :end i)))))

(define-test find-if-vector.12
  (assert-equal '((nil 2 4 8 8 8 6 6)
                  (2 4 8 8 8 6 6)
                  (4 8 8 8 6 6)
                  (8 8 8 6 6)
                  (nil nil 6 6)
                  (nil 6 6)
                  (6 6)
                  (nil))
                (loop for j from 0 to 7
                      collect
                   (loop for i from (1+ j) to 8 collect
                     (find-if #'evenp #(1 2 4 8 3 1 6 7) :start j :end i
                          :from-end t)))))

(define-test find-if-vector.13
  (assert-equal '(1 11 11 45 45 71 nil)
                (loop for i from 0 to 6
                      collect
                   (find-if #'evenp #(1 6 11 32 45 71 100) :key #'1+ :start i))))

(define-test find-if-vector.14
  (assert-equal '(71 71 71 71 71 71 nil)
                (loop for i from 0 to 6
                      collect
                   (find-if #'evenp #(1 6 11 32 45 71 100) :key '1+ :start i :from-end t))))

(define-test find-if-vector.15
  (assert-equal '(nil 1 1 1 1 1 1 1)
                (loop for i from 0 to 7
                      collect
                   (find-if #'evenp #(1 6 11 32 45 71 100) :key #'1+ :end i))))

(define-test find-if-vector.16
  (assert-equal '(nil 1 1 11 11 45 71 71)
                (loop for i from 0 to 7
                      collect
                   (find-if #'evenp #(1 6 11 32 45 71 100) :key '1+ :end i :from-end t))))

(define-test find-if-vector.17
  (assert-equal '((nil 2 2 2 2 2 2 2)
                  (2 2 2 2 2 2 2)
                  (4 4 4 4 4 4)
                  (8 8 8 8 8)
                  (nil nil 6 6)
                  (nil 6 6)
                  (6 6)
                  (nil))
                (loop for j from 0 to 7
                      collect
                   (loop for i from (1+ j) to 8 collect
                     (find-if #'oddp #(1 2 4 8 3 1 6 7) :start j :end i :key #'1-)))))

(define-test find-if-vector.18
  (assert-equal '((nil 2 4 8 8 8 6 6)
                  (2 4 8 8 8 6 6)
                  (4 8 8 8 6 6)
                  (8 8 8 6 6)
                  (nil nil 6 6)
                  (nil 6 6)
                  (6 6)
                  (nil))
                (loop for j from 0 to 7
                      collect
                   (loop for i from (1+ j) to 8 collect
                     (find-if #'oddp #(1 2 4 8 3 1 6 7) :start j :end i
                          :from-end t :key #'1+)))))

(define-test find-if-vector.19
  (let ((a (make-array '(10) :initial-contents '(1 2 3 4 5 6 7 8 9 10)
                       :fill-pointer 5)))
     (assert-equal 2 (find-if #'evenp a))
     (assert-equal 4 (find-if #'evenp a :from-end t))
     (assert-equal 1 (find-if #'oddp a))
     (assert-equal 5 (find-if #'oddp a :from-end t))))

;;; Tests for bit vectors

(define-test find-if-bit-vector.1
  (assert-equal 'nil
                (find-if #'identity #*)))

(define-test find-if-bit-vector.2
  (assert-equal '1
                (find-if #'identity #*1)))

(define-test find-if-bit-vector.3
  (assert-equal '0
                (find-if #'identity #*0)))

(define-test find-if-bit-vector.4
  (assert-equal '((nil 0 0 0 0 0 0 0)
                  (nil nil nil 0 0 0 0)
                  (nil nil 0 0 0 0)
                  (nil 0 0 0 0)
                  (nil nil nil 0)
                  (nil nil 0)
                  (nil 0))
                (loop for i from 0 to 6
                      collect (loop for j from i to 7
                                    collect (find-if #'evenp #*0110110 :start i :end j)))))

(define-test find-if-bit-vector.5
  (assert-equal '((nil 0 0 0 0 0 0 0)
                  (nil nil nil 0 0 0 0)
                  (nil nil 0 0 0 0)
                  (nil 0 0 0 0)
                  (nil nil nil 0)
                  (nil nil 0)
                  (nil 0))
                (loop for i from 0 to 6
                      collect (loop for j from i to 7
                                    collect (find-if #'evenp #*0110110 :start i :end j
                                                 :from-end t)))))

(define-test find-if-bit-vector.6
  (assert-equal '((nil 0 0 0 0 0 0 0)
                  (nil nil nil 0 0 0 0)
                  (nil nil 0 0 0 0)
                  (nil 0 0 0 0)
                  (nil nil nil 0)
                  (nil nil 0)
                  (nil 0))
                (loop for i from 0 to 6
                      collect (loop for j from i to 7
                                    collect (find-if #'oddp #*0110110 :start i :end j
                                                 :from-end t :key #'1+)))))

(define-test find-if-bit-vector.7
  (assert-equal '((nil 0 0 0 0 0 0 0)
                  (nil nil nil 0 0 0 0)
                  (nil nil 0 0 0 0)
                  (nil 0 0 0 0)
                  (nil nil nil 0)
                  (nil nil 0)
                  (nil 0))
                (loop for i from 0 to 6
                      collect (loop for j from i to 7
                                    collect (find-if #'oddp #*0110110 :start i :end j
                                                 :key '1-)))))

;;; Tests for strings

(define-test find-if-string.1
  (assert-equal 'nil
                (find-if #'identity "")))

(define-test find-if-string.2
  (assert-equal '#\a
                (find-if #'identity "a")))

(define-test find-if-string.2a
  (assert-equal '#\a
                (find-if 'identity "a")))

(defun evendigitp (digit)
  (member digit '(#\0 #\2 #\4 #\6 #\8)))

(defun odddigitp (digit)
  (member digit '(#\1 #\3 #\5 #\7 #\9)))

(define-test find-if-string.3
  (assert-equal '#\2
                (find-if #'evendigitp "12483167")))

(define-test find-if-string.3a
  (assert-equal '#\2
                (find-if #'evenp "12483167" :key #'(lambda (c) (read-from-string (string c))))))

(define-test find-if-string.4
  (assert-equal '#\6
                (find-if #'evendigitp "12483167" :from-end t)))

(define-test find-if-string.5
  (assert-equal '(#\2 #\2 #\4 #\8 #\6 #\6 #\6 nil)
                (loop for i from 0 to 7 collect
                  (find-if #'evendigitp "12483167" :start i))))

(define-test find-if-string.6
  (assert-equal '(#\2 #\2 #\4 #\8 #\6 #\6 #\6 nil)
                (loop for i from 0 to 7 collect
                  (find-if #'evendigitp "12483167" :start i :end nil))))

(define-test find-if-string.7
  (assert-equal '(#\6 #\6 #\6 #\6 #\6 #\6 #\6 nil)
                (loop for i from 0 to 7 collect
                  (find-if #'evendigitp "12483167" :start i :from-end t))))

(define-test find-if-string.8
  (assert-equal '(#\6 #\6 #\6 #\6 #\6 #\6 #\6 nil)
                (loop for i from 0 to 7 collect
                  (find-if #'evendigitp "12483167" :start i :end nil :from-end t))))

(define-test find-if-string.9
  (assert-equal '(nil nil #\2 #\2 #\2 #\2 #\2 #\2 #\2)
                (loop for i from 0 to 8 collect
                  (find-if #'evendigitp "12483167" :end i))))

(define-test find-if-string.10
  (assert-equal '(nil nil #\2 #\4 #\8 #\8 #\8 #\6 #\6)
                (loop for i from 0 to 8 collect
                  (find-if #'evendigitp "12483167" :end i :from-end t))))

(define-test find-if-string.11
  (assert-equal '((nil #\2 #\2 #\2 #\2 #\2 #\2 #\2)
                  (#\2 #\2 #\2 #\2 #\2 #\2 #\2)
                  (#\4 #\4 #\4 #\4 #\4 #\4)
                  (#\8 #\8 #\8 #\8 #\8)
                  (nil nil #\6 #\6)
                  (nil #\6 #\6)
                  (#\6 #\6)
                  (nil))
                (loop for j from 0 to 7
                      collect
                   (loop for i from (1+ j) to 8 collect
                     (find-if #'evendigitp "12483167" :start j :end i)))))

(define-test find-if-string.12
  (assert-equal '((nil #\2 #\4 #\8 #\8 #\8 #\6 #\6)
                  (#\2 #\4 #\8 #\8 #\8 #\6 #\6)
                  (#\4 #\8 #\8 #\8 #\6 #\6)
                  (#\8 #\8 #\8 #\6 #\6)
                  (nil nil #\6 #\6)
                  (nil #\6 #\6)
                  (#\6 #\6)
                  (nil))
                (loop for j from 0 to 7
                      collect
                   (loop for i from (1+ j) to 8 collect
                     (find-if #'evendigitp "12483167" :start j :end i
                          :from-end t)))))

(defun compose (fun1 fun2)
  (lambda (x)
    (funcall fun1 (funcall fun2 x))))

(define-test find-if-string.13
  (assert-equal '(#\4 #\4 #\8 #\8 #\8 #\6 #\6)
                (loop for i from 0 to 6
                      collect
                   (find-if #'evenp "1473816"
                        :key (compose #'read-from-string #'string)
                        :start i))))

(define-test find-if-string.14
  (assert-equal '(#\6 #\6 #\6 #\6 #\6 #\6 #\6)
                (loop for i from 0 to 6
                      collect
                   (find-if #'evenp "1473816"
                        :key (compose #'read-from-string #'string)
                        :start i :from-end t))))

(define-test find-if-string.15
  (assert-equal '(nil nil #\4 #\4 #\4 #\4 #\4 #\4)
                (loop for i from 0 to 7
                      collect
                   (find-if #'evenp "1473816"
                        :key (compose #'read-from-string #'string)
                        :end i))))

(define-test find-if-string.16
  (assert-equal '(nil nil #\4 #\4 #\4 #\8 #\8 #\6)
                (loop for i from 0 to 7
                      collect
                   (find-if #'evenp "1473816"
                        :key (compose #'read-from-string #'string)
                        :end i :from-end t))))

(define-test find-if-string.17
  (assert-equal '((nil #\4 #\4 #\4 #\4 #\4 #\4)
                  (#\4 #\4 #\4 #\4 #\4 #\4)
                  (nil nil #\8 #\8 #\8)
                  (nil #\8 #\8 #\8)
                  (#\8 #\8 #\8)
                  (nil #\6)
                  (#\6))
                (loop for j from 0 to 6
                      collect
                   (loop for i from (1+ j) to 7 collect
                     (find-if #'evenp "1473816"
                          :key (compose #'read-from-string #'string)
                          :start j :end i)))))

(define-test find-if-string.18
  (assert-equal '((nil #\4 #\4 #\4 #\8 #\8 #\6)
                  (#\4 #\4 #\4 #\8 #\8 #\6)
                  (nil nil #\8 #\8 #\6)
                  (nil #\8 #\8 #\6)
                  (#\8 #\8 #\6)
                  (nil #\6)
                  (#\6))
                (loop for j from 0 to 6
                      collect
                   (loop for i from (1+ j) to 7 collect
                     (find-if #'evenp "1473816"
                          :key (compose #'read-from-string #'string)
                          :start j :end i
                          :from-end t)))))

(define-test find-if-string.19
  (let ((a (make-array '(10) :initial-contents "123456789a"
                       :fill-pointer 5
                       :element-type 'character)))
    (assert-equal #\2 (find-if #'evendigitp a))
    (assert-equal #\4 (find-if #'evendigitp a :from-end t))
    (assert-equal #\1 (find-if #'odddigitp a))
    (assert-equal #\5 (find-if #'odddigitp a :from-end t))))

;;; Keyword tests

(define-test find-if.allow-other-keys.1
  (assert-equal '2
                (find-if #'evenp '(1 2 3 4 5) :bad t :allow-other-keys t)))

(define-test find-if.allow-other-keys.2
  (assert-equal '2
                (find-if #'evenp '(1 2 3 4 5) :allow-other-keys t :also-bad t)))

;;; The leftmost of two :allow-other-keys arguments is the one that  matters.
(define-test find-if.allow-other-keys.3
  (assert-equal '2
                (find-if #'evenp '(1 2 3 4 5)
                         :allow-other-keys t
                         :allow-other-keys nil
                         :bad t)))

(define-test find-if.keywords.4
  (assert-equal '2
                (find-if #'evenp '(1 2 3 4 5) :key #'identity :key #'1+)))

(define-test find-if.allow-other-keys.5
  (assert-equal 'a
                (find-if #'identity '(nil a b c nil) :allow-other-keys nil)))


;;; Error tests

(define-test find-if.error.4
  (assert-error 'type-error (find-if 'null '(a b c . d))))

(define-test find-if.error.5
  (let ((fun nil)
        (warned nil))
    (handler-bind ((warning (lambda (condition)
                              (setf warned t)
                              (muffle-warning condition))))
      (setf fun
            (compile nil '(lambda () (find-if)))))
    (assert-true warned)
    (assert-error 'program-error (funcall fun))))

(define-test find-if.error.6
  (let ((fun nil)
        (warned nil))
    (handler-bind ((warning (lambda (condition)
                              (setf warned t)
                              (muffle-warning condition))))
      (setf fun
            (compile nil '(lambda () (find-if #'null)))))
    (assert-true warned)
    (assert-error 'program-error (funcall fun))))

(define-test find-if.error.7
  (let ((fun nil)
        (warned nil))
    (handler-bind ((warning (lambda (condition)
                              (setf warned t)
                              (muffle-warning condition))))
      (setf fun
            (compile nil '(lambda () (find-if #'null nil :bad t)))))
    (assert-true warned)
    (assert-error 'program-error (funcall fun))))

(define-test find-if.error.8
  (let ((fun nil)
        (warned nil))
    (handler-bind ((warning (lambda (condition)
                              (setf warned t)
                              (muffle-warning condition))))
      (setf fun
            (compile nil '(lambda ()
                           (find-if #'null nil :bad t :allow-other-keys nil)))))
    (assert-true warned)
    (assert-error 'program-error (funcall fun))))

(define-test find-if.error.9
  (let ((fun nil)
        (warned nil))
    (handler-bind ((warning (lambda (condition)
                              (setf warned t)
                              (muffle-warning condition))))
      (setf fun
            (compile nil '(lambda ()
                           (find-if #'null nil 1 1)))))
    (assert-true warned)
    (assert-error 'program-error (funcall fun))))

(define-test find-if.error.10
  (let ((fun nil)
        (warned nil))
    (handler-bind ((warning (lambda (condition)
                              (setf warned t)
                              (muffle-warning condition))))
      (setf fun
            (compile nil '(lambda ()
                           (find-if #'null nil :key)))))
    (assert-true warned)
    (assert-error 'program-error (funcall fun))))

(define-test find-if.error.11
  (let ((fun nil)
        (warned nil))
    (handler-bind ((warning (lambda (condition)
                              (setf warned t)
                              (muffle-warning condition))))
      (setf fun
            (compile nil '(lambda () (find-if)))))
    (assert-true warned)
    (assert-error 'program-error (funcall fun))))

(define-test find-if.error.11
  (assert-error 'type-error (locally (find-if #'null 'b) t)))

(define-test find-if.error.12
  (assert-error 'program-error (find-if #'cons '(a b c))))

(define-test find-if.error.13
  (assert-error 'type-error (find-if #'car '(a b c))))

(define-test find-if.error.14
  (assert-error 'program-error (find-if #'identity '(a b c) :key #'cons)))

(define-test find-if.error.15
  (assert-error 'type-error
                 (find-if #'identity '(a b c) :key #'car)))

;;; Order of evaluation tests

(define-test find-if.order.1
  (let ((i 0) x y)
    (assert-equal
     '(a 2 1 2)
     (list
      (find-if (progn (setf x (incf i)) #'identity)
               (progn (setf y (incf i)) '(nil nil nil a nil nil)))
      i x y))))

(define-test find-if.order.2
  (let ((i 0) a b c d e f)
    (assert-equal
     '(a 6 1 2 3 4 5 6)
     (list
      (find-if (progn (setf a (incf i)) #'null)
               (progn (setf b (incf i)) '(nil nil nil a nil nil))
               :start (progn (setf c (incf i)) 1)
               :end   (progn (setf d (incf i)) 4)
               :from-end (setf e (incf i))
               :key   (progn (setf f (incf i)) #'null)
               )
      i a b c d e f))))

(define-test find-if.order.3
  (let ((i 0) a b c d e f)
    (assert-equal
     '(a 6 1 2 3 4 5 6)
     (list
      (find-if (progn (setf a (incf i)) #'null)
               (progn (setf b (incf i)) '(nil nil nil a nil nil))
               :key   (progn (setf c (incf i)) #'null)
               :from-end (setf d (incf i))
               :end   (progn (setf e (incf i)) 4)
               :start (progn (setf f (incf i)) 1)
               )
      i a b c d e f))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Tests for function find-if-not

(define-test find-if-not-list.1
  (assert-equal
   nil
   (find-if-not #'identity ())))

(define-test find-if-not-list.2
  (assert-equal
   'a
   (find-if-not #'null '(a))))

(define-test find-if-not-list.2a
  (assert-equal
   'a
   (find-if-not 'null '(a))))

(define-test find-if-not-list.3
  (assert-equal
   2
   (find-if-not #'oddp '(1 2 4 8 3 1 6 7))))

(define-test find-if-not-list.4
  (assert-equal
   6
   (find-if-not #'oddp '(1 2 4 8 3 1 6 7) :from-end t)))

(define-test find-if-not-list.5
  (assert-equal
   '(2 2 4 8 6 6 6 nil)
   (loop for i from 0 to 7 collect
     (find-if-not #'oddp '(1 2 4 8 3 1 6 7) :start i))))

(define-test find-if-not-list.6
  (assert-equal
   '(2 2 4 8 6 6 6 nil)
   (loop for i from 0 to 7 collect
     (find-if-not #'oddp '(1 2 4 8 3 1 6 7) :start i :end nil))))

(define-test find-if-not-list.7
  (assert-equal
   '(6 6 6 6 6 6 6 nil)
   (loop for i from 0 to 7 collect
     (find-if-not #'oddp '(1 2 4 8 3 1 6 7) :start i :from-end t))))

(define-test find-if-not-list.8
  (assert-equal
   '(6 6 6 6 6 6 6 nil)
   (loop for i from 0 to 7 collect
     (find-if-not #'oddp '(1 2 4 8 3 1 6 7) :start i :end nil :from-end t))))

(define-test find-if-not-list.9
  (assert-equal
   '(nil nil 2 2 2 2 2 2 2)
   (loop for i from 0 to 8 collect
     (find-if-not #'oddp '(1 2 4 8 3 1 6 7) :end i))))

(define-test find-if-not-list.10
  (assert-equal
   '(nil nil 2 4 8 8 8 6 6)
   (loop for i from 0 to 8 collect
     (find-if-not #'oddp '(1 2 4 8 3 1 6 7) :end i :from-end t))))

(define-test find-if-not-list.11
  (assert-equal
   '((nil 2 2 2 2 2 2 2)
     (2 2 2 2 2 2 2)
     (4 4 4 4 4 4)
     (8 8 8 8 8)
     (nil nil 6 6)
     (nil 6 6)
     (6 6)
     (nil))
   (loop for j from 0 to 7
         collect
      (loop for i from (1+ j) to 8 collect
        (find-if-not #'oddp '(1 2 4 8 3 1 6 7) :start j :end i)))))

(define-test find-if-not-list.12
  (assert-equal
   '((nil 2 4 8 8 8 6 6)
     (2 4 8 8 8 6 6)
     (4 8 8 8 6 6)
     (8 8 8 6 6)
     (nil nil 6 6)
     (nil 6 6)
     (6 6)
     (nil))
   (loop for j from 0 to 7
         collect
      (loop for i from (1+ j) to 8 collect
        (find-if-not #'oddp '(1 2 4 8 3 1 6 7) :start j :end i
             :from-end t)))))

(define-test find-if-not-list.13
  (assert-equal
   '(1 11 11 45 45 71 nil)
   (loop for i from 0 to 6
         collect
      (find-if-not #'oddp '(1 6 11 32 45 71 100) :key #'1+ :start i))))

(define-test find-if-not-list.14
  (assert-equal
   '(71 71 71 71 71 71 nil)
   (loop for i from 0 to 6
         collect
      (find-if-not #'oddp '(1 6 11 32 45 71 100) :key '1+ :start i :from-end t))))

(define-test find-if-not-list.15
  (assert-equal
   '(nil 1 1 1 1 1 1 1)
   (loop for i from 0 to 7
         collect
      (find-if-not #'oddp '(1 6 11 32 45 71 100) :key #'1+ :end i))))

(define-test find-if-not-list.16
  (assert-equal
   '(nil 1 1 11 11 45 71 71)
   (loop for i from 0 to 7
         collect
      (find-if-not #'oddp '(1 6 11 32 45 71 100) :key '1+ :end i :from-end t))))

(define-test find-if-not-list.17
  (assert-equal
   '((nil 2 2 2 2 2 2 2)
     (2 2 2 2 2 2 2)
     (4 4 4 4 4 4)
     (8 8 8 8 8)
     (nil nil 6 6)
     (nil 6 6)
     (6 6)
     (nil))
   (loop for j from 0 to 7
         collect
      (loop for i from (1+ j) to 8 collect
        (find-if-not #'evenp '(1 2 4 8 3 1 6 7) :start j :end i :key #'1-)))))

(define-test find-if-not-list.18
  (assert-equal
   '((nil 2 4 8 8 8 6 6)
     (2 4 8 8 8 6 6)
     (4 8 8 8 6 6)
     (8 8 8 6 6)
     (nil nil 6 6)
     (nil 6 6)
     (6 6)
     (nil))
   (loop for j from 0 to 7
         collect
      (loop for i from (1+ j) to 8 collect
        (find-if-not #'evenp '(1 2 4 8 3 1 6 7) :start j :end i
             :from-end t :key #'1+)))))

;;; tests for vectors

(define-test find-if-not-vector.1
  (assert-equal
   nil
   (find-if-not #'identity #())))

(define-test find-if-not-vector.2
  (assert-equal
   'a
   (find-if-not #'not #(a))))

(define-test find-if-not-vector.2a
  (assert-equal
   'a
   (find-if-not 'null #(a))))

(define-test find-if-not-vector.3
  (assert-equal
   2
   (find-if-not #'oddp #(1 2 4 8 3 1 6 7))))

(define-test find-if-not-vector.4
  (assert-equal
   6
   (find-if-not #'oddp #(1 2 4 8 3 1 6 7) :from-end t)))

(define-test find-if-not-vector.5
  (assert-equal
   '(2 2 4 8 6 6 6 nil)
   (loop for i from 0 to 7 collect
     (find-if-not #'oddp #(1 2 4 8 3 1 6 7) :start i))))

(define-test find-if-not-vector.6
  (assert-equal
   '(2 2 4 8 6 6 6 nil)
   (loop for i from 0 to 7 collect
     (find-if-not #'oddp #(1 2 4 8 3 1 6 7) :start i :end nil))))

(define-test find-if-not-vector.7
  (assert-equal
   '(6 6 6 6 6 6 6 nil)
   (loop for i from 0 to 7 collect
     (find-if-not #'oddp #(1 2 4 8 3 1 6 7) :start i :from-end t))))

(define-test find-if-not-vector.8
  (assert-equal
   '(6 6 6 6 6 6 6 nil)
   (loop for i from 0 to 7 collect
     (find-if-not #'oddp #(1 2 4 8 3 1 6 7) :start i :end nil :from-end t))))

(define-test find-if-not-vector.9
  (assert-equal
   '(nil nil 2 2 2 2 2 2 2)
   (loop for i from 0 to 8 collect
     (find-if-not #'oddp #(1 2 4 8 3 1 6 7) :end i))))

(define-test find-if-not-vector.10
  (assert-equal
   '(nil nil 2 4 8 8 8 6 6)
   (loop for i from 0 to 8 collect
     (find-if-not #'oddp #(1 2 4 8 3 1 6 7) :end i :from-end t))))

(define-test find-if-not-vector.11
  (assert-equal
   '((nil 2 2 2 2 2 2 2)
     (2 2 2 2 2 2 2)
     (4 4 4 4 4 4)
     (8 8 8 8 8)
     (nil nil 6 6)
     (nil 6 6)
     (6 6)
     (nil))
   (loop for j from 0 to 7
         collect
      (loop for i from (1+ j) to 8 collect
        (find-if-not #'oddp #(1 2 4 8 3 1 6 7) :start j :end i)))))

(define-test find-if-not-vector.12
  (assert-equal
   '((nil 2 4 8 8 8 6 6)
     (2 4 8 8 8 6 6)
     (4 8 8 8 6 6)
     (8 8 8 6 6)
     (nil nil 6 6)
     (nil 6 6)
     (6 6)
     (nil))
   (loop for j from 0 to 7
         collect
      (loop for i from (1+ j) to 8 collect
        (find-if-not #'oddp #(1 2 4 8 3 1 6 7) :start j :end i
             :from-end t)))))

(define-test find-if-not-vector.13
  (assert-equal
   '(1 11 11 45 45 71 nil)
   (loop for i from 0 to 6
         collect
      (find-if-not #'oddp #(1 6 11 32 45 71 100) :key #'1+ :start i))))

(define-test find-if-not-vector.14
  (assert-equal
   '(71 71 71 71 71 71 nil)
   (loop for i from 0 to 6
         collect
      (find-if-not #'oddp #(1 6 11 32 45 71 100) :key '1+ :start i :from-end t))))

(define-test find-if-not-vector.15
  (assert-equal
   '(nil 1 1 1 1 1 1 1)
   (loop for i from 0 to 7
         collect
      (find-if-not #'oddp #(1 6 11 32 45 71 100) :key #'1+ :end i))))

(define-test find-if-not-vector.16
  (assert-equal
   '(nil 1 1 11 11 45 71 71)
   (loop for i from 0 to 7
         collect
      (find-if-not #'oddp #(1 6 11 32 45 71 100) :key '1+ :end i :from-end t))))

(define-test find-if-not-vector.17
  (assert-equal
   '((nil 2 2 2 2 2 2 2)
     (2 2 2 2 2 2 2)
     (4 4 4 4 4 4)
     (8 8 8 8 8)
     (nil nil 6 6)
     (nil 6 6)
     (6 6)
     (nil))
   (loop for j from 0 to 7
         collect
      (loop for i from (1+ j) to 8 collect
        (find-if-not #'evenp #(1 2 4 8 3 1 6 7) :start j :end i :key #'1-)))))

(define-test find-if-not-vector.18
  (assert-equal
   '((nil 2 4 8 8 8 6 6)
     (2 4 8 8 8 6 6)
     (4 8 8 8 6 6)
     (8 8 8 6 6)
     (nil nil 6 6)
     (nil 6 6)
     (6 6)
     (nil))
   (loop for j from 0 to 7
         collect
      (loop for i from (1+ j) to 8 collect
        (find-if-not #'evenp #(1 2 4 8 3 1 6 7) :start j :end i
             :from-end t :key #'1+)))))

;;; Tests for bit vectors

(define-test find-if-not-bit-vector.1
  (assert-equal
   nil
   (find-if-not #'identity #*)))

(define-test find-if-not-bit-vector.2
  (assert-equal
   1
   (find-if-not #'null #*1)))

(define-test find-if-not-bit-vector.3
  (assert-equal
   0
   (find-if-not #'not #*0)))

(define-test find-if-not-bit-vector.4
  (assert-equal
   '((nil 0 0 0 0 0 0 0)
     (nil nil nil 0 0 0 0)
     (nil nil 0 0 0 0)
     (nil 0 0 0 0)
     (nil nil nil 0)
     (nil nil 0)
     (nil 0))
   (loop for i from 0 to 6
         collect (loop for j from i to 7
                       collect (find-if-not #'oddp #*0110110 :start i :end j)))))

(define-test find-if-not-bit-vector.5
  (assert-equal
   '((nil 0 0 0 0 0 0 0)
     (nil nil nil 0 0 0 0)
     (nil nil 0 0 0 0)
     (nil 0 0 0 0)
     (nil nil nil 0)
     (nil nil 0)
     (nil 0))
   (loop for i from 0 to 6
         collect (loop for j from i to 7
                       collect (find-if-not #'oddp #*0110110 :start i :end j
                                    :from-end t)))))

(define-test find-if-not-bit-vector.6
  (assert-equal
   '((nil 0 0 0 0 0 0 0)
     (nil nil nil 0 0 0 0)
     (nil nil 0 0 0 0)
     (nil 0 0 0 0)
     (nil nil nil 0)
     (nil nil 0)
     (nil 0))
   (loop for i from 0 to 6
         collect (loop for j from i to 7
                       collect (find-if-not #'evenp #*0110110 :start i :end j
                                    :from-end t :key #'1+)))))

(define-test find-if-not-bit-vector.7
  (assert-equal
   '((nil 0 0 0 0 0 0 0)
     (nil nil nil 0 0 0 0)
     (nil nil 0 0 0 0)
     (nil 0 0 0 0)
     (nil nil nil 0)
     (nil nil 0)
     (nil 0))
   (loop for i from 0 to 6
         collect (loop for j from i to 7
                       collect (find-if-not #'evenp #*0110110 :start i :end j
                                    :key '1-)))))

;;; Tests for strings

(define-test find-if-not-string.1
  (assert-equal
   nil
   (find-if-not #'identity "")))

(define-test find-if-not-string.2
  (assert-equal
   #\a
   (find-if-not #'null "a")))

(define-test find-if-not-string.2a
  (assert-equal
   #\a
   (find-if-not 'null "a")))

(define-test find-if-not-string.3
  (assert-equal
   #\2
   (find-if-not #'odddigitp "12483167")))

(define-test find-if-not-string.3a
  (assert-equal
   #\2
   (find-if-not #'oddp "12483167" :key #'(lambda (c) (read-from-string (string c))))))

(define-test find-if-not-string.4
  (assert-equal
   #\6
   (find-if-not #'odddigitp "12483167" :from-end t)))

(define-test find-if-not-string.5
  (assert-equal
   '(#\2 #\2 #\4 #\8 #\6 #\6 #\6 nil)
   (loop for i from 0 to 7 collect
     (find-if-not #'odddigitp "12483167" :start i))))

(define-test find-if-not-string.6
  (assert-equal
   '(#\2 #\2 #\4 #\8 #\6 #\6 #\6 nil)
   (loop for i from 0 to 7 collect
     (find-if-not #'odddigitp "12483167" :start i :end nil))))

(define-test find-if-not-string.7
  (assert-equal
   '(#\6 #\6 #\6 #\6 #\6 #\6 #\6 nil)
   (loop for i from 0 to 7 collect
     (find-if-not #'odddigitp "12483167" :start i :from-end t))))

(define-test find-if-not-string.8
  (assert-equal
   '(#\6 #\6 #\6 #\6 #\6 #\6 #\6 nil)
   (loop for i from 0 to 7 collect
     (find-if-not #'odddigitp "12483167" :start i :end nil :from-end t))))

(define-test find-if-not-string.9
  (assert-equal
   '(nil nil #\2 #\2 #\2 #\2 #\2 #\2 #\2)
   (loop for i from 0 to 8 collect
     (find-if-not #'odddigitp "12483167" :end i))))

(define-test find-if-not-string.10
  (assert-equal
   '(nil nil #\2 #\4 #\8 #\8 #\8 #\6 #\6)
   (loop for i from 0 to 8 collect
     (find-if-not #'odddigitp "12483167" :end i :from-end t))))

(define-test find-if-not-string.11
  (assert-equal
   '((nil #\2 #\2 #\2 #\2 #\2 #\2 #\2)
     (#\2 #\2 #\2 #\2 #\2 #\2 #\2)
     (#\4 #\4 #\4 #\4 #\4 #\4)
     (#\8 #\8 #\8 #\8 #\8)
     (nil nil #\6 #\6)
     (nil #\6 #\6)
     (#\6 #\6)
     (nil))
   (loop for j from 0 to 7
         collect
      (loop for i from (1+ j) to 8 collect
        (find-if-not #'odddigitp "12483167" :start j :end i)))))

(define-test find-if-not-string.12
  (assert-equal
  '((nil #\2 #\4 #\8 #\8 #\8 #\6 #\6)
    (#\2 #\4 #\8 #\8 #\8 #\6 #\6)
    (#\4 #\8 #\8 #\8 #\6 #\6)
    (#\8 #\8 #\8 #\6 #\6)
    (nil nil #\6 #\6)
    (nil #\6 #\6)
    (#\6 #\6)
    (nil))
  (loop for j from 0 to 7
        collect
     (loop for i from (1+ j) to 8 collect
       (find-if-not #'odddigitp "12483167" :start j :end i
            :from-end t)))))

(define-test find-if-not-string.13
  (assert-equal
  '(#\4 #\4 #\8 #\8 #\8 #\6 #\6)
  (loop for i from 0 to 6
        collect
     (find-if-not #'oddp "1473816"
          :key (compose #'read-from-string #'string)
          :start i))))

(define-test find-if-not-string.14
  (assert-equal
  '(#\6 #\6 #\6 #\6 #\6 #\6 #\6)
  (loop for i from 0 to 6
        collect
     (find-if-not #'oddp "1473816"
          :key (compose #'read-from-string #'string)
          :start i :from-end t))))

(define-test find-if-not-string.15
  (assert-equal
  '(nil nil #\4 #\4 #\4 #\4 #\4 #\4)
  (loop for i from 0 to 7
        collect
     (find-if-not #'oddp "1473816"
          :key (compose #'read-from-string #'string)
          :end i))))

(define-test find-if-not-string.16
  (assert-equal
  '(nil nil #\4 #\4 #\4 #\8 #\8 #\6)
  (loop for i from 0 to 7
        collect
     (find-if-not #'oddp "1473816"
          :key (compose #'read-from-string #'string)
          :end i :from-end t))))

(define-test find-if-not-string.17
  (assert-equal
  '((nil #\4 #\4 #\4 #\4 #\4 #\4)
    (#\4 #\4 #\4 #\4 #\4 #\4)
    (nil nil #\8 #\8 #\8)
    (nil #\8 #\8 #\8)
    (#\8 #\8 #\8)
    (nil #\6)
    (#\6))
  (loop for j from 0 to 6
        collect
     (loop for i from (1+ j) to 7 collect
       (find-if-not #'oddp "1473816"
            :key (compose #'read-from-string #'string)
            :start j :end i))))  )

(define-test find-if-not-string.18
  (assert-equal
  '((nil #\4 #\4 #\4 #\8 #\8 #\6)
    (#\4 #\4 #\4 #\8 #\8 #\6)
    (nil nil #\8 #\8 #\6)
    (nil #\8 #\8 #\6)
    (#\8 #\8 #\6)
    (nil #\6)
    (#\6))
   (loop for j from 0 to 6
         collect
      (loop for i from (1+ j) to 7 collect
        (find-if-not #'oddp "1473816"
             :key (compose #'read-from-string #'string)
             :start j :end i
             :from-end t)))))

;;; Keyword tests

(define-test find-if-not.allow-other-keys.1
  (assert-equal
   2
   (find-if-not #'oddp '(1 2 3 4 5) :bad t :allow-other-keys t)))

(define-test find-if-not.allow-other-keys.2
  (assert-equal
   2
   (find-if-not #'oddp '(1 2 3 4 5) :allow-other-keys t :also-bad t)))

;;; The leftmost of two :allow-other-keys arguments is the one that  matters.
(define-test find-if-not.allow-other-keys.3
  (assert-equal
   2
   (find-if-not #'oddp '(1 2 3 4 5)
                :allow-other-keys t
                :allow-other-keys nil
                :bad t)))

(define-test find-if-not.keywords.4
  (assert-equal
   2
   (find-if-not #'oddp '(1 2 3 4 5) :key #'identity :key #'1+)))

(define-test find-if-not.allow-other-keys.5
  (assert-equal
   'a
   (find-if-not #'null '(nil a b c nil) :allow-other-keys nil)))

;;; Error tests

(define-test find-if-not.error.4
  (assert-error 'type-error
                (find-if-not 'identity '(a b c . d))))

(define-test find-if-not.error.5
  (let ((fun nil)
        (warned nil))
    (handler-bind ((warning (lambda (condition)
                              (setf warned t)
                              (muffle-warning condition))))
      (setf fun
            (compile nil '(lambda () (find-if-not)))))
    (assert-true warned)
    (assert-error 'program-error (funcall fun))))

(define-test find-if-not.error.6
  (let ((fun nil)
        (warned nil))
    (handler-bind ((warning (lambda (condition)
                              (setf warned t)
                              (muffle-warning condition))))
      (setf fun
            (compile nil '(lambda () (find-if-not #'null)))))
    (assert-true warned)
    (assert-error 'program-error (funcall fun))))

(define-test find-if-not.error.7
  (let ((fun nil)
        (warned nil))
    (handler-bind ((warning (lambda (condition)
                              (setf warned t)
                              (muffle-warning condition))))
      (setf fun
            (compile nil '(lambda () (find-if-not #'null nil :bad t)))))
    (assert-true warned)
    (assert-error 'program-error (funcall fun))))

(define-test find-if-not.error.8
  (let ((fun nil)
        (warned nil))
    (handler-bind ((warning (lambda (condition)
                              (setf warned t)
                              (muffle-warning condition))))
      (setf fun
            (compile nil '(lambda ()
                           (find-if-not #'null nil :bad t :allow-other-keys nil)))))
    (assert-true warned)
    (assert-error 'program-error (funcall fun))))

(define-test find-if-not.error.9
  (let ((fun nil)
        (warned nil))
    (handler-bind ((warning (lambda (condition)
                              (setf warned t)
                              (muffle-warning condition))))
      (setf fun
            (compile nil '(lambda ()
                           (find-if-not #'null nil 1 1)))))
    (assert-true warned)
    (assert-error 'program-error (funcall fun))))

(define-test find-if-not.error.10
  (let ((fun nil)
        (warned nil))
    (handler-bind ((warning (lambda (condition)
                              (setf warned t)
                              (muffle-warning condition))))
      (setf fun
            (compile nil '(lambda ()
                           (find-if-not #'null nil :key)))))
    (assert-true warned)
    (assert-error 'program-error (funcall fun))))

(define-test find-if-not.error.11
  (assert-error 'type-error (locally (find-if-not #'null 'b) t)))

(define-test find-if-not.error.12
  (assert-error 'program-error (find-if-not #'cons '(a b c))))

(define-test find-if-not.error.13
  (assert-error 'type-error (find-if-not #'car '(a b c))))

(define-test find-if-not.error.14
  (assert-error 'program-error (find-if-not #'identity '(a b c) :key #'cons)))

(define-test find-if-not.error.15
  (assert-error 'type-error
                (find-if-not #'identity '(a b c) :key #'car)))

;;; Order of evaluation tests

(define-test find-if-not.order.1
  (let ((i 0) x y)
    (assert-equal
     '(a 2 1 2)
     (list
      (find-if-not (progn (setf x (incf i)) #'null)
                   (progn (setf y (incf i)) '(nil nil nil a nil nil)))
      i x y))))

(define-test find-if-not.order.2
  (let ((i 0) a b c d e f)
    (assert-equal
     '(a 6 1 2 3 4 5 6)
     (list
      (find-if-not (progn (setf a (incf i)) #'identity)
                   (progn (setf b (incf i)) '(nil nil nil a nil nil))
                   :start (progn (setf c (incf i)) 1)
                   :end   (progn (setf d (incf i)) 4)
                   :from-end (setf e (incf i))
                   :key   (progn (setf f (incf i)) #'null)
                   )
      i a b c d e f))))

(define-test find-if-not.order.3
  (let ((i 0) a b c d e f)
    (assert-equal
     '(a 6 1 2 3 4 5 6)
     (list
      (find-if-not (progn (setf a (incf i)) #'identity)
                   (progn (setf b (incf i)) '(nil nil nil a nil nil))
                   :key   (progn (setf c (incf i)) #'null)
                   :from-end (setf d (incf i))
                   :end   (progn (setf e (incf i)) 4)
                   :start (progn (setf f (incf i)) 1)
                   )
      i a b c d e f))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Tests for function length

(define-test length.list.1
  (assert-equal
   0
   (length nil)))

(define-test length.list.2
  (assert-equal
   5
   (length '(a b c d e))))

(define-test length.list.3
  (assert-equal
   200000
   (length (make-list 200000))))

(defun length.list-4-body ()
  (let ((x ()))
    (loop
     for i from 0 to 999 do
     (progn
       (unless (eql (length x) i) (return nil))
       (push i x))
     finally (return t))))

(define-test length.list-4
  (assert-equal
   t
   (length.list-4-body)))

(define-test length.vector.1
  (assert-equal
   0
   (length #())))

(define-test length.vector.2
  (assert-equal
   1
   (length #(a))))

(define-test length.vector.3
  (assert-equal
   2
   (length #(a b))))

(define-test length.vector.4
  (assert-equal
   3
   (length #(a b c))))

(define-test length.nonsimple-vector.1
  (assert-equal
   10
   (length (make-array 10 :fill-pointer t :adjustable t))))

(define-test length.nonsimple-vector.2
  (assert-equal
   5
   (let ((a (make-array 10 :fill-pointer t :adjustable t)))
     (setf (fill-pointer a) 5)
     (length a))))

(define-test length.bit-vector.1
  (assert-equal
   0
   (length #*)))

(define-test length.bit-vector.2
  (assert-equal
   1
   (length #*1)))

(define-test length.bit-vector.3
  (assert-equal
   1
   (length #*0)))

(define-test length.bit-vector.4
  (assert-equal
   6
   (length #*010101)))

(define-test length.bit-vector.5
  (assert-equal
   '(5 1)
   (let ((i 0))
     (flet ((%f () (incf i)
              (make-array 5 :element-type 'bit
                          :initial-contents '(0 0 1 1 0))))
       (list
        (length (the (simple-bit-vector 5) (%f)))
        i)))))

(define-test length.string.1
  (assert-equal
   0
   (length "")))

(define-test length.string.2
  (assert-equal
   1
   (length "a")))

(define-test length.string.3
  (assert-equal
   13
   (length "abcdefghijklm")))

(define-test length.string.4
  (assert-equal
   1
   (length "\ ")))

(define-test length.string.5
  (assert-equal
   '(5 1)
   (let ((i 0))
     (flet ((%f () (incf i)
              (make-string 5 :initial-element #\a)))
       (list (length (the (simple-string 5) (%f))) i)))))


(define-test length.string.6
  (assert-equal
   '(5 1)
   (let ((i 0))
     (flet ((%f () (incf i)
              (make-array 5 :element-type 'base-char
                          :initial-element #\a)))
       (list (length (the (simple-base-string 5) (%f))) i)))))

(define-test length.error.6
  (let ((fun nil)
        (warned nil))
    (handler-bind ((warning (lambda (condition)
                              (setf warned t)
                              (muffle-warning condition))))
      (setf fun
            (compile nil '(lambda ()
                           (length)))))
    (assert-true warned)
    (assert-error 'program-error (funcall fun))))

(define-test length.error.7
  (let ((fun nil)
        (warned nil))
    (handler-bind ((warning (lambda (condition)
                              (setf warned t)
                              (muffle-warning condition))))
      (setf fun
            (compile nil '(lambda ()
                           (length nil nil)))))
    (assert-true warned)
    (assert-error 'program-error (funcall fun))))

(define-test length.error.8
  (assert-error 'type-error (locally (length 'a) t)))

;;; Length on vectors created with make-array

(define-test length.array.1
  (assert-equal
   20
   (length (make-array '(20)))))

(define-test length.array.2
  (assert-equal
   100001
   (length (make-array '(100001)))))

(define-test length.array.3
  (assert-equal
   0
   (length (make-array '(0)))))

(define-test length.array.4
  (assert-equal
   10
   (let ((x (make-array '(100) :fill-pointer 10)))
     (length x))))

(define-test length.array.5
  (assert-equal
   20
   (let ((x (make-array '(100) :fill-pointer 10)))
     (setf (fill-pointer x) 20)
     (length x))))

;;; Unusual vectors

(define-test length.array.6
  (assert-equal
   nil
   (loop for i from 1 to 40
         for etype = `(unsigned-byte ,i)
         for vec = (make-array 7 :element-type etype :initial-element 0)
         for len = (length vec)
         unless (eql len 7)
           collect (list i vec len))))

(define-test length.array.7
  (assert-equal
   nil
   (loop for i from 1 to 40
         for etype = `(signed-byte ,i)
         for vec = (make-array 13 :element-type etype :initial-element 0)
         for len = (length vec)
         unless (eql len 13)
           collect (list i vec len))))

(define-test length.array.8
  (assert-equal
   nil
   (loop for etype in '(short-float single-float double-float long-float rational)
         for vec = (make-array 5 :element-type etype :initial-element (coerce 0 etype))
         for len = (length vec)
         unless (eql len 5)
           collect (list etype vec len))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Tests for function remove

(defun stupid-remove (item sequence
                      &key
                      key
                      (test nil test-p)
                      (test-not nil test-not-p)
                      (start 0)
                      end
                      from-end
                      count)
  (let ((length (length sequence)))
    (if (null end)
        (setf end length)
        (assert (<= end length)))
    (assert (<= 0 start length))
    (assert (not (and test-p test-not-p)))
    (if from-end
        (reverse (apply #'stupid-remove
                        item (reverse sequence)
                        :key key
                        :start (- length end)
                        :end (- length start)
                        :from-end nil
                        :count count
                        (append (if test-p (list :test test) '())
                                (if test-not-p (list :test-not test-not) '()))))
        (progn (when (null key)
                 (setf key #'identity))
               (if test-not-p
                   (setf test (lambda (x y) (not (funcall test-not x y))))
                   (when (not test-p)
                     (setf test #'eql)))
               (unless count
                 (setf count length))
               (if (<= end start)
                   sequence
                   (coerce (append (coerce (subseq sequence 0 start) 'list)
                                   (loop for element in (coerce (subseq sequence start end) 'list)
                                         unless (and (plusp count)
                                                     (funcall test item (funcall key element)))
                                           collect element
                                         else
                                           do (decf count))
                                   (coerce (subseq sequence end length) 'list))
                           (cond ((listp sequence) 'list)
                                 ((stringp sequence) 'string)
                                 (t 'vector))))))))

(defun test-remove (item sequence key test test-not start end from-end count)
  (flet ((make-arg (name thing)
           (if (eql thing -1) '() (list name thing))))
    (let ((arguments (append (make-arg :key key)
                             (make-arg :test test)
                             (make-arg :test-not test-not)
                             (make-arg :start start)
                             (make-arg :end end)
                             (make-arg :from-end from-end)
                             (make-arg :count count))))
      (let ((v1 (apply #'stupid-remove item sequence arguments))
            (v2 (apply #'remove item sequence arguments)))
        (if (equalp v1 v2)
            (format *trace-output* "*")
            (format *trace-output*
                    "for test: REMOVE ~s ~s ~s~@
                     expected: ~s~%     but saw:  ~s~%"
                    item sequence arguments v1 v2))))))

(defun test-remove-1 (item sequence test test-not start end from-end count)
  (test-remove item sequence
               -1 test test-not start end from-end count)
  (test-remove item sequence
               nil test test-not start end from-end count)
  (test-remove item sequence
               #'identity test test-not start end from-end count)
  (test-remove item sequence
               'identity test test-not start end from-end count)

  (test-remove item (coerce sequence 'vector)
               -1 test test-not start end from-end count)
  (test-remove item (coerce sequence 'vector)
               nil test test-not start end from-end count)
  (test-remove item (coerce sequence 'vector)
               #'identity test test-not start end from-end count)

  (test-remove item (mapcar #'list sequence)
               #'car test test-not start end from-end count)
  (test-remove item (mapcar #'list sequence)
               'car test test-not start end from-end count)

  (test-remove item (map 'vector #'list sequence)
               #'car test test-not start end from-end count)
  (test-remove item (map 'vector #'list sequence)
               'car test test-not start end from-end count))

(defun test-remove-2 (item sequence test start end from-end count)
  (if (eq test 'eql)
      (progn (test-remove-1 item sequence -1 -1 start end from-end count)
             (test-remove-1 item sequence #'eql -1 start end from-end count)
             (test-remove-1 item sequence 'eql -1 start end from-end count)
             (test-remove-1 item sequence -1 #'eql start end from-end count)
             (test-remove-1 item sequence -1 'eql start end from-end count))
      (progn (test-remove-1 item sequence test -1 start end from-end count)
             (test-remove-1 item sequence (symbol-function test) -1 start end from-end count)
             (test-remove-1 item sequence -1 test start end from-end count)
             (test-remove-1 item sequence -1 (symbol-function test) start end from-end count))))

(defun test-remove-3 (item sequence test start end count)
  (test-remove-2 item sequence test start end -1 count)
  (test-remove-2 item sequence test start end nil count)
  (test-remove-2 item sequence test start end t count))

(defun test-remove-4 (item sequence test)
  (let ((length (length sequence)))
    (test-remove-3 item sequence test -1 -1 -1)
    (loop for index from 0 to length
          do (test-remove-3 item sequence test -1 -1 index)
             (test-remove-3 item sequence test -1 index -1)
             (test-remove-3 item sequence test index -1 -1))
    (loop for index1 from 0 to length
          do (loop for index2 from 0 to length
                   do (test-remove-3 item sequence test -1 index1 index2)
                      (test-remove-3 item sequence test index1 -1 index2)))
    (loop for start from 0 to length
          do (loop for end from start to length
                   do (loop for count from 0 to length
                            do (test-remove-3 item sequence test start end -1)
                            do (test-remove-3 item sequence test start end count))))))

(define-test |remove normal|
  (assert-equal nil
                (test-remove-4 *i01* (list *i1* *i02* *i2* *i02* *i4*) 'eql)
                (test-remove-4 *i5* (list *i1* *i02* *i2* *i02* *i4*) 'eql)
                (test-remove-4 *l01* (list *l1* *l02* *l2* *l02* *l4*) 'eq)
                (test-remove-4 *l5* (list *l1* *l02* *l2* *l02* *l4*) 'eq)
                (test-remove-4 *s01* (list *s1* *s02* *s2* *s02* *s4*) 'string-equal)
                (test-remove-4 *s5* (list *s1* *s02* *s2* *s02* *s4*) 'string-equal)))

(define-test test.remove.list.1
  (assert-equal
   '()
   (remove 0 '())))

(define-test test.remove.list.2
  (assert-equal
   '()
   (remove 0 '(0))))

(define-test test.remove.list.3
  (assert-equal
   '()
   (remove 0 '(0 0))))

(define-test test.remove.list.4
  (assert-equal
   '(1)
   (remove 0 '(1))))

(define-test test.remove.list.5
  (assert-equal
   '(1)
   (remove 0 '(0 1 0))))

(define-test test.remove.list.6
  (assert-equal
   '()
   (remove '(a) '() :test #'equal)))

(define-test test.remove.list.7
  (assert-equal
   '()
   (remove '(a) '((a)) :test #'equal)))

(define-test test.remove.list.8
  (assert-equal
   '((b) (b))
   (remove '(a) '((b) (a) (b)) :test #'equal)))

(define-test test.remove.list.9
  (assert-equal
   '()
   (remove '(a) '() :test #'equal :key #'car)))

(define-test test.remove.list.10
  (assert-equal
   '()
   (remove '(a) '(((a))) :test #'equal :key #'car)))

(define-test test.remove.list.11
  (assert-equal
   '(((b)) ((b)))
   (remove '(a) '(((b)) ((a)) ((b))) :test #'equal :key #'car)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Tests for function substitute

(define-test |substitute seq-type=list start=0 end=nil test=eql count=nil key=identity 1|
  (assert-equal
   '(0 0)
   (substitute 1 2 '(0 0))))

(define-test |substitute seq-type=list start=0 end=nil test=eql count=nil key=identity 2|
  (assert-equal
   '(1)
   (substitute 1 2 '(2))))

(define-test |substitute seq-type=list start=0 end=nil test=eql count=nil key=identity 3|
  (assert-equal
   '(1)
   (substitute 1 2 '(1))))

(define-test |substitute seq-type=list start=0 end=nil test=eql count=nil key=identity 4|
  (assert-equal
   '(0 1)
   (substitute 1 2 '(0 2))))

(define-test |substitute seq-type=list start=0 end=nil test=eql count=nil key=identity 5|
  (assert-equal
   '(0 1 3 1)
   (substitute 1 2 '(0 2 3 2))))

(define-test |substitute seq-type=list start=0 end=nil test=eql count=nil key=identity 6|
  (assert-error
   'type-error
   (substitute 1 2 'a)))

(define-test |substitute seq-type=list start=0 end=nil test=eql count=nil key=identity 7|
  (assert-error
   'type-error
   (substitute 1 2 '(2 0 . 3))))

(define-test |substitute seq-type=list start=0 end=nil test=eql count=nil key=identity 8|
  (assert-equal
   '(0 0)
   (substitute 1 2 '(0 0) :test #'eql)))

(define-test |substitute seq-type=list start=0 end=nil test=eql count=nil key=identity 9|
  (assert-equal
   '(1)
   (substitute 1 2 '(2) :test #'eql)))

(define-test |substitute seq-type=list start=0 end=nil test=eql count=nil key=identity 10|
  (assert-equal
   '(1)
   (substitute 1 2 '(1) :test #'eql)))

(define-test |substitute seq-type=list start=0 end=nil test=eql count=nil key=identity 11|
  (assert-equal
   '(0 1)
   (substitute 1 2 '(0 2) :test #'eql)))

(define-test |substitute seq-type=list start=0 end=nil test=eql count=nil key=identity 12|
  (assert-equal
   '(0 1 3 1)
   (substitute 1 2 '(0 2 3 2) :test #'eql)))

(define-test |substitute seq-type=list start=0 end=nil test=eql count=nil key=identity 13|
  (assert-error
   'type-error
   (substitute 1 2 'a :test #'eql)))

(define-test |substitute seq-type=list start=0 end=nil test=eql count=nil key=identity 14|
  (assert-error
   'type-error
   (substitute 1 2 '(2 0 . 3) :test #'eql)))

(define-test |substitute seq-type=list start=other end=nil test=eql count=nil key=identity 1|
  (assert-equal
   '(2 2)
   (substitute 1 2 '(2 2) :start 2)))

(define-test |substitute seq-type=list start=other end=nil test=eql count=nil key=identity 2|
  (assert-equal
   '(2 1)
   (substitute 1 2 '(2 2) :start 1)))

(define-test |substitute seq-type=list start=other end=nil test=eql count=nil key=identity 3|
  (assert-error
   'type-error
   (substitute 1 2 '(2 2) :start 3)))

(define-test |substitute seq-type=list start=0 end=nil test=eql count=nil key=other 1|
  (assert-equal
   '((0) (0))
   (substitute 1 2 '((0) (0)) :key #'car)))

(define-test |substitute seq-type=list start=0 end=nil test=eql count=nil key=other 2|
  (assert-equal
   '(1)
   (substitute 1 2 '((2)) :key #'car)))

(define-test |substitute seq-type=list start=0 end=nil test=eql count=nil key=other 3|
  (assert-equal
   '((1))
   (substitute 1 2 '((1)) :key #'car)))

(define-test |substitute seq-type=list start=0 end=nil test=eql count=nil key=other 4|
  (assert-equal
   '((0) 1)
   (substitute 1 2 '((0) (2)) :key #'car)))

(define-test |substitute seq-type=list start=0 end=nil test=eql count=nil key=other 5|
  (assert-equal
   '((0) 1 (3) 1)
   (substitute 1 2 '((0) (2) (3) (2)) :key #'car)))

(define-test |substitute seq-type=list start=0 end=nil test=eql count=nil key=other 6|
  (assert-error
   'type-error
   (substitute 1 2 'a :key #'car)))

(define-test |substitute seq-type=list start=0 end=nil test=eql count=nil key=other 7|
  (assert-error
   'type-error
   (substitute 1 2 '((2) (0) . 3) :key #'car)))

(define-test |substitute seq-type=list start=0 end=nil test=eql count=nil key=other 8|
  (assert-equal
   '((0) (0))
   (substitute 1 2 '((0) (0)) :test #'eql :key #'car)))

(define-test |substitute seq-type=list start=0 end=nil test=eql count=nil key=other 9|
  (assert-equal
   '(1)
   (substitute 1 2 '((2)) :test #'eql :key #'car)))

(define-test |substitute seq-type=list start=0 end=nil test=eql count=nil key=other 10|
  (assert-equal
   '((1))
   (substitute 1 2 '((1)) :test #'eql :key #'car)))

(define-test |substitute seq-type=list start=0 end=nil test=eql count=nil key=other 11|
  (assert-equal
   '((0) 1)
   (substitute 1 2 '((0) (2)) :test #'eql :key #'car)))

(define-test |substitute seq-type=list start=0 end=nil test=eql count=nil key=other 12|
  (assert-equal
   '((0) 1 (3) 1)
   (substitute 1 2 '((0) (2) (3) (2)) :test #'eql :key #'car)))

(define-test |substitute seq-type=list start=0 end=nil test=eql count=nil key=other 13|
  (assert-error
   'type-error
   (substitute 1 2 'a :test #'eql :key #'car)))

(define-test |substitute seq-type=list start=0 end=nil test=eql count=nil key=other 14|
  (assert-error
   'type-error
   (substitute 1 2 '((2) (0) . 3) :test #'eql :key #'car)))

(define-test |substitute seq-type=list start=other end=nil test=eql count=nil key=other 1|
  (assert-equal
   '((2) (2))
   (substitute 1 2 '((2) (2)) :start 2 :key #'car)))

(define-test |substitute seq-type=list start=other end=nil test=eql count=nil key=other 2|
  (assert-equal
   '((2) 1)
   (substitute 1 2 '((2) (2)) :start 1 :key #'car)))

(define-test |substitute seq-type=list start=other end=nil test=eql count=nil key=other 3|
  (assert-error
   'type-error
   (substitute 'b 'c '((c) (c)) :start 3 :key #'car)))

(define-test |substitute seq-type=list start=0 end=nil test=eq count=nil key=identity 1|
  (assert-equal
   '(a a)
   (substitute 'b 'c '(a a) :test #'eq)))

(define-test |substitute seq-type=list start=0 end=nil test=eq count=nil key=identity 2|
  (assert-equal
   '(b)
   (substitute 'b 'c '(c) :test #'eq)))

(define-test |substitute seq-type=list start=0 end=nil test=eq count=nil key=identity 3|
  (assert-equal
   '(b)
   (substitute 'b 'c '(b) :test #'eq)))

(define-test |substitute seq-type=list start=0 end=nil test=eq count=nil key=identity 4|
  (assert-equal
   '(a b)
   (substitute 'b 'c '(a c) :test #'eq)))

(define-test |substitute seq-type=list start=0 end=nil test=eq count=nil key=identity 5|
  (assert-equal
   '(a b d b)
   (substitute 'b 'c '(a c d c) :test #'eq)))

(define-test |substitute seq-type=list start=0 end=nil test=eq count=nil key=identity 6|
  (assert-error
   'type-error
   (substitute 'b 'c 'a :test #'eq)))

(define-test |substitute seq-type=list start=0 end=nil test=eq count=nil key=identity 7|
  (assert-error
   'type-error
   (substitute 'b 'c '(c a . d) :test #'eq)))

(define-test |substitute seq-type=list start=other end=nil test=eq count=nil key=identity 1|
  (assert-equal
   '(c c)
   (substitute 'b 'c '(c c) :start 2 :test #'eq)))

(define-test |substitute seq-type=list start=other end=nil test=eq count=nil key=identity 2|
  (assert-equal
   '(c b)
   (substitute 'b 'c '(c c) :start 1 :test #'eq)))

(define-test |substitute seq-type=list start=other end=nil test=eq count=nil key=identity 3|
  (assert-error
   'type-error
   (substitute 'b 'c '(c c) :start 3 :test #'eq)))

(define-test |substitute seq-type=list start=0 end=nil test=eq count=nil key=other 1|
  (assert-equal
   '((a) (a))
   (substitute 'b 'c '((a) (a)) :test #'eq :key #'car)))

(define-test |substitute seq-type=list start=0 end=nil test=eq count=nil key=other 2|
  (assert-equal
   '(b)
   (substitute 'b 'c '((c)) :test #'eq :key #'car)))

(define-test |substitute seq-type=list start=0 end=nil test=eq count=nil key=other 3|
  (assert-equal
   '((b))
   (substitute 'b 'c '((b)) :test #'eq :key #'car)))

(define-test |substitute seq-type=list start=0 end=nil test=eq count=nil key=other 4|
  (assert-equal
   '((a) b)
   (substitute 'b 'c '((a) (c)) :test #'eq :key #'car)))

(define-test |substitute seq-type=list start=0 end=nil test=eq count=nil key=other 5|
  (assert-equal
   '((a) b (3) b)
   (substitute 'b 'c '((a) (c) (3) (c)) :test #'eq :key #'car)))

(define-test |substitute seq-type=list start=0 end=nil test=eq count=nil key=other 6|
  (assert-error
   'type-error
   (substitute 'b 'c 'a :test #'eq :key #'car)))

(define-test |substitute seq-type=list start=0 end=nil test=eq count=nil key=other 7|
  (assert-error
   'type-error
   (substitute 'b 'c '((c) (a) . 3) :test #'eq :key #'car)))

(define-test |substitute seq-type=list start=other end=nil test=eq count=nil key=other 1|
  (assert-equal
   '((c) (c))
   (substitute 'b 'c '((c) (c)) :start 2 :test #'eq :key #'car)))

(define-test |substitute seq-type=list start=other end=nil test=eq count=nil key=other 2|
  (assert-equal
   '((c) b)
   (substitute 'b 'c '((c) (c)) :start 1 :test #'eq :key #'car)))

(define-test |substitute seq-type=list start=other end=nil test=eq count=nil key=other 3|
  (assert-error
   'type-error
   (substitute 'b 'c '((c) (c)) :start 3 :test #'eq :key #'car)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Tests for function position

(defun stupid-position (item sequence
                        &key
                        key
                        (test nil test-p)
                        (test-not nil test-not-p)
                        (start 0)
                        end
                        from-end)
  (assert (not (and test-p test-not-p)))
  (when test-not-p
    (setf test (lambda (x y) (not (funcall test-not x y)))))
  (unless test
    (setf test #'eql))
  (unless key
    (setf key #'identity))
  (unless (vectorp sequence)
    (setf sequence (coerce sequence 'vector)))
  (when (null end)
    (setf end (length sequence)))
  (assert (<= 0 start (length sequence)))
  (assert (<= 0 end (length sequence)))
  (if from-end
      (loop for index downfrom (1- end) to start
            when (funcall test item (funcall key (aref sequence index)))
              return index)
      (loop for index from start below end
            when (funcall test item (funcall key (aref sequence index)))
              return index)))

(defun test-position (item sequence key test test-not start end from-end)
  (flet ((make-arg (name thing)
           (if (eql thing -1) '() (list name thing))))
    (let ((arguments (append (make-arg :key key)
                             (make-arg :test test)
                             (make-arg :test-not test-not)
                             (make-arg :start start)
                             (make-arg :end end)
                             (make-arg :from-end from-end))))
      (let ((v1 (apply #'stupid-position item sequence arguments))
            (v2 (apply #'position item sequence arguments)))
      (if (eql v1 v2)
          (format *trace-output* "*")
          (format *trace-output*
                  "for test: POSITION ~s ~s ~s~@
                   expected: ~s~%     but saw:  ~s~%"
                  item sequence arguments v1 v2))))))

(defun test-position-1 (item sequence test test-not start end from-end)
  (test-position item sequence
                 -1 test test-not start end from-end)
  (test-position item sequence
                 nil test test-not start end from-end)
  (test-position item sequence
                 #'identity test test-not start end from-end)
  (test-position item sequence
                 'identity test test-not start end from-end)

  (test-position item (coerce sequence 'vector)
                 -1 test test-not start end from-end)
  (test-position item (coerce sequence 'vector)
                 nil test test-not start end from-end)
  (test-position item (coerce sequence 'vector)
                 #'identity test test-not start end from-end)

  (test-position item (mapcar #'list sequence)
                 #'car test test-not start end from-end)
  (test-position item (mapcar #'list sequence)
                 'car test test-not start end from-end)

  (test-position item (map 'vector #'list sequence)
                 #'car test test-not start end from-end)
  (test-position item (map 'vector #'list sequence)
                 'car test test-not start end from-end))

(defun test-position-2 (item sequence test start end from-end)
  (if (eq test 'eql)
      (progn (test-position-1 item sequence -1 -1 start end from-end)
             (test-position-1 item sequence #'eql -1 start end from-end)
             (test-position-1 item sequence 'eql -1 start end from-end)
             (test-position-1 item sequence -1 #'eql start end from-end)
             (test-position-1 item sequence -1 'eql start end from-end))
      (progn (test-position-1 item sequence test -1 start end from-end)
             (test-position-1 item sequence (symbol-function test) -1 start end from-end)
             (test-position-1 item sequence -1 test start end from-end)
             (test-position-1 item sequence -1 (symbol-function test) start end from-end))))

(defun test-position-3 (item sequence test start end)
  (test-position-2 item sequence test start end -1)
  (test-position-2 item sequence test start end nil)
  (test-position-2 item sequence test start end t))

(defun test-position-4 (item sequence test)
  (let ((length (length sequence)))
    (test-position-3 item sequence test -1 -1)
    (loop for index from 0 to length
          do (test-position-3 item sequence test -1 index)
             (test-position-3 item sequence test index -1))
    (loop for start from 0 to length
          do (loop for end from start to length
                   do (test-position-3 item sequence test start end)))))

(define-test |position normal|
  (assert-equal nil
                (test-position-4 *i01* (list *i1* *i02* *i2* *i02* *i4*) 'eql)
                (test-position-4 *i5* (list *i1* *i02* *i2* *i02* *i4*) 'eql)
                (test-position-4 *l01* (list *l1* *l02* *l2* *l02* *l4*) 'eq)
                (test-position-4 *l5* (list *l1* *l02* *l2* *l02* *l4*) 'eq)
                (test-position-4 *s01* (list *s1* *s02* *s2* *s02* *s4*) 'string-equal)
                (test-position-4 *s5* (list *s1* *s02* *s2* *s02* *s4*) 'string-equal)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Tests for function position-if

(defun stupid-position-if (predicate sequence
                           &key
                           key
                           (start 0)
                           end
                           from-end)
  (unless key
    (setf key #'identity))
  (unless (vectorp sequence)
    (setf sequence (coerce sequence 'vector)))
  (when (null end)
    (setf end (length sequence)))
  (assert (<= 0 start (length sequence)))
  (assert (<= 0 end (length sequence)))
  (if from-end
      (loop for index downfrom (1- end) to start
            when (funcall predicate (funcall key (aref sequence index)))
              return index)
      (loop for index from start below end
            when (funcall predicate (funcall key (aref sequence index)))
              return index)))

(defun test-position-if (predicate sequence key start end from-end)
  (flet ((make-arg (name thing)
           (if (eql thing -1) '() (list name thing))))
    (let ((arguments (append (make-arg :key key)
                             (make-arg :start start)
                             (make-arg :end end)
                             (make-arg :from-end from-end))))
      (let ((v1 (apply #'stupid-position-if predicate sequence arguments))
            (v2 (apply #'position-if predicate sequence arguments)))
      (if (eql v1 v2)
          (format *trace-output* "*")
          (format *trace-output*
                  "for test: POSITION-IF ~s ~s ~s~@
                   expected: ~s~%     but saw:  ~s~%"
                  predicate sequence arguments v1 v2))))))

(defun test-position-if-1 (predicate sequence start end from-end)
  (test-position-if predicate sequence
             -1 start end from-end)
  (test-position-if predicate sequence
             nil start end from-end)
  (test-position-if predicate sequence
             #'identity start end from-end)
  (test-position-if predicate sequence
             'identity start end from-end)

  (test-position-if predicate (coerce sequence 'vector)
             -1 start end from-end)
  (test-position-if predicate (coerce sequence 'vector)
             nil start end from-end)
  (test-position-if predicate (coerce sequence 'vector)
             #'identity start end from-end)

  (test-position-if predicate (mapcar #'list sequence)
             #'car start end from-end)
  (test-position-if predicate (mapcar #'list sequence)
             'car start end from-end)

  (test-position-if predicate (map 'vector #'list sequence)
             #'car start end from-end)
  (test-position-if predicate (map 'vector #'list sequence)
             'car start end from-end))

(defun test-position-if-2 (predicate sequence start end)
  (test-position-if-1 predicate sequence start end -1)
  (test-position-if-1 predicate sequence start end nil)
  (test-position-if-1 predicate sequence start end t))

(defun test-position-if-3 (predicate sequence)
  (let ((length (length sequence)))
    (test-position-if-2 predicate sequence -1 -1)
    (loop for index from 0 to length
          do (test-position-if-2 predicate sequence -1 index)
             (test-position-if-2 predicate sequence index -1))
    (loop for start from 0 to length
          do (loop for end from start to length
                   do (test-position-if-2 predicate sequence start end)))))

(define-test |position-if normal|
  (assert-equal nil
                (test-position-if-3 (lambda (x) (eql x *i01*))
                                    (list *i1* *i02* *i2* *i02* *i4*))
                (test-position-if-3 (lambda (x) (eql x *i5*))
                                    (list *i1* *i02* *i2* *i02* *i4*))
                (test-position-if-3 (lambda (x) (eq x *l01*))
                                    (list *l1* *l02* *l2* *l02* *l4*))
                (test-position-if-3 (lambda (x) (eq x *l5* ))
                                    (list *l1* *l02* *l2* *l02* *l4*))
                (test-position-if-3 (lambda (x) (string-equal x *s01* ))
                                    (list *s1* *s02* *s2* *s02* *s4*))
                (test-position-if-3 (lambda (x) (string-equal x *s5*))
                                    (list *s1* *s02* *s2* *s02* *s4*))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Tests for function position-if-not

(defun stupid-position-if-not (predicate sequence
                               &key
                               key
                               (start 0)
                               end
                               from-end)
  (unless key
    (setf key #'identity))
  (unless (vectorp sequence)
    (setf sequence (coerce sequence 'vector)))
  (when (null end)
    (setf end (length sequence)))
  (assert (<= 0 start (length sequence)))
  (assert (<= 0 end (length sequence)))
  (if from-end
      (loop for index downfrom (1- end) to start
            unless (funcall predicate (funcall key (aref sequence index)))
              return index)
      (loop for index from start below end
            unless (funcall predicate (funcall key (aref sequence index)))
              return index)))

(defun test-position-if-not (predicate sequence key start end from-end)
  (flet ((make-arg (name thing)
           (if (eql thing -1) '() (list name thing))))
    (let ((arguments (append (make-arg :key key)
                             (make-arg :start start)
                             (make-arg :end end)
                             (make-arg :from-end from-end))))
      (let ((v1 (apply #'stupid-position-if-not predicate sequence arguments))
            (v2 (apply #'position-if-not predicate sequence arguments)))
      (if (eql v1 v2)
          (format *trace-output* "*")
          (format *trace-output*
                  "for test: POSITION-IF-NOT ~s ~s ~s~@
                   expected: ~s~%     but saw:  ~s~%"
                  predicate sequence arguments v1 v2))))))

(defun test-position-if-not-1 (predicate sequence start end from-end)
  (test-position-if-not predicate sequence
                        -1 start end from-end)
  (test-position-if-not predicate sequence
                        nil start end from-end)
  (test-position-if-not predicate sequence
                        #'identity start end from-end)
  (test-position-if-not predicate sequence
                        'identity start end from-end)

  (test-position-if-not predicate (coerce sequence 'vector)
                        -1 start end from-end)
  (test-position-if-not predicate (coerce sequence 'vector)
                        nil start end from-end)
  (test-position-if-not predicate (coerce sequence 'vector)
                        #'identity start end from-end)

  (test-position-if-not predicate (mapcar #'list sequence)
                        #'car start end from-end)
  (test-position-if-not predicate (mapcar #'list sequence)
                        'car start end from-end)

  (test-position-if-not predicate (map 'vector #'list sequence)
                        #'car start end from-end)
  (test-position-if-not predicate (map 'vector #'list sequence)
                        'car start end from-end))

(defun test-position-if-not-2 (predicate sequence start end)
  (test-position-if-not-1 predicate sequence start end -1)
  (test-position-if-not-1 predicate sequence start end nil)
  (test-position-if-not-1 predicate sequence start end t))

(defun test-position-if-not-3 (predicate sequence)
  (let ((length (length sequence)))
    (test-position-if-not-2 predicate sequence -1 -1)
    (loop for index from 0 to length
          do (test-position-if-not-2 predicate sequence -1 index)
             (test-position-if-not-2 predicate sequence index -1))
    (loop for start from 0 to length
          do (loop for end from start to length
                   do (test-position-if-not-2 predicate sequence start end)))))

(define-test |position-if-not normal|
  (assert-equal nil
                (test-position-if-not-3 (lambda (x) (eql x *i01*))
                                        (list *i1* *i02* *i2* *i02* *i4*))
                (test-position-if-not-3 (lambda (x) (eql x *i5*))
                                        (list *i1* *i02* *i2* *i02* *i4*))
                (test-position-if-not-3 (lambda (x) (eq x *l01*))
                                        (list *l1* *l02* *l2* *l02* *l4*))
                (test-position-if-not-3 (lambda (x) (eq x *l5* ))
                                        (list *l1* *l02* *l2* *l02* *l4*))
                (test-position-if-not-3 (lambda (x) (string-equal x *s01* ))
                                        (list *s1* *s02* *s2* *s02* *s4*))
                (test-position-if-not-3 (lambda (x) (string-equal x *s5*))
                                        (list *s1* *s02* *s2* *s02* *s4*))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Tests for function count

(defun stupid-count (item sequence
                     &key
                     key
                     (test nil test-p)
                     (test-not nil test-not-p)
                     (start 0)
                     end
                     from-end)
  (assert (not (and test-p test-not-p)))
  (when test-not-p
    (setf test (lambda (x y) (not (funcall test-not x y)))))
  (unless test
    (setf test #'eql))
  (unless key
    (setf key #'identity))
  (unless (vectorp sequence)
    (setf sequence (coerce sequence 'vector)))
  (when (null end)
    (setf end (length sequence)))
  (assert (<= 0 start (length sequence)))
  (assert (<= 0 end (length sequence)))
  (if from-end
      (loop for index downfrom (1- end) to start
            count (funcall test item (funcall key (aref sequence index))))
      (loop for index from start below end
            count (funcall test item (funcall key (aref sequence index))))))

(defun test-count (item sequence key test test-not start end from-end)
  (flet ((make-arg (name thing)
           (if (eql thing -1) '() (list name thing))))
    (let ((arguments (append (make-arg :key key)
                             (make-arg :test test)
                             (make-arg :test-not test-not)
                             (make-arg :start start)
                             (make-arg :end end)
                             (make-arg :from-end from-end))))
      (let ((v1 (apply #'stupid-count item sequence arguments))
            (v2 (apply #'count item sequence arguments)))
        (if (eql v1 v2)
            (format *trace-output* "*")
            (format *trace-output*
                    "for test: COUNT ~s ~s ~s~@
                     expected: ~s~%     but saw:  ~s~%"
                    item sequence arguments v1 v2))))))

(defun test-count-1 (item sequence test test-not start end from-end)
  (test-count item sequence
              -1 test test-not start end from-end)
  (test-count item sequence
              nil test test-not start end from-end)
  (test-count item sequence
              #'identity test test-not start end from-end)
  (test-count item sequence
              'identity test test-not start end from-end)

  (test-count item (coerce sequence 'vector)
              -1 test test-not start end from-end)
  (test-count item (coerce sequence 'vector)
              nil test test-not start end from-end)
  (test-count item (coerce sequence 'vector)
              #'identity test test-not start end from-end)

  (test-count item (mapcar #'list sequence)
              #'car test test-not start end from-end)
  (test-count item (mapcar #'list sequence)
              'car test test-not start end from-end)

  (test-count item (map 'vector #'list sequence)
              #'car test test-not start end from-end)
  (test-count item (map 'vector #'list sequence)
              'car test test-not start end from-end))

(defun test-count-2 (item sequence test start end from-end)
  (if (eq test 'eql)
      (progn (test-count-1 item sequence -1 -1 start end from-end)
             (test-count-1 item sequence #'eql -1 start end from-end)
             (test-count-1 item sequence 'eql -1 start end from-end)
             (test-count-1 item sequence -1 #'eql start end from-end)
             (test-count-1 item sequence -1 'eql start end from-end))
      (progn (test-count-1 item sequence test -1 start end from-end)
             (test-count-1 item sequence (symbol-function test) -1 start end from-end)
             (test-count-1 item sequence -1 test start end from-end)
             (test-count-1 item sequence -1 (symbol-function test) start end from-end))))

(defun test-count-3 (item sequence test start end)
  (test-count-2 item sequence test start end -1)
  (test-count-2 item sequence test start end nil)
  (test-count-2 item sequence test start end t))

(defun test-count-4 (item sequence test)
  (let ((length (length sequence)))
    (test-count-3 item sequence test -1 -1)
    (loop for index from 0 to length
          do (test-count-3 item sequence test -1 index)
             (test-count-3 item sequence test index -1))
    (loop for start from 0 to length
          do (loop for end from start to length
                   do (test-count-3 item sequence test start end)))))

(define-test |count normal|
  (assert-equal nil
                (test-count-4 *i01* (list *i1* *i02* *i2* *i02* *i4*) 'eql)
                (test-count-4 *i5* (list *i1* *i02* *i2* *i02* *i4*) 'eql)
                (test-count-4 *l01* (list *l1* *l02* *l2* *l02* *l4*) 'eq)
                (test-count-4 *l5* (list *l1* *l02* *l2* *l02* *l4*) 'eq)
                (test-count-4 *s01* (list *s1* *s02* *s2* *s02* *s4*) 'string-equal)
                (test-count-4 *s5* (list *s1* *s02* *s2* *s02* *s4*) 'string-equal)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Tests for function count-if

(defun stupid-count-if (predicate sequence
                        &key
                        key
                        (start 0)
                        end
                        from-end)
  (unless key
    (setf key #'identity))
  (unless (vectorp sequence)
    (setf sequence (coerce sequence 'vector)))
  (when (null end)
    (setf end (length sequence)))
  (assert (<= 0 start (length sequence)))
  (assert (<= 0 end (length sequence)))
  (if from-end
      (loop for index downfrom (1- end) to start
            count (funcall predicate (funcall key (aref sequence index))))
      (loop for index from start below end
            count (funcall predicate (funcall key (aref sequence index))))))

(defun test-count-if (predicate sequence key start end from-end)
  (flet ((make-arg (name thing)
           (if (eql thing -1) '() (list name thing))))
    (let ((arguments (append (make-arg :key key)
                             (make-arg :start start)
                             (make-arg :end end)
                             (make-arg :from-end from-end))))
      (let ((v1 (apply #'stupid-count-if predicate sequence arguments))
            (v2 (apply #'count-if predicate sequence arguments)))
        (if (eql v1 v2)
            (format *trace-output* "*")
            (format *trace-output*
                    "for test: COUNT-IF ~s ~s ~s~@
                     expected: ~s~%     but saw:  ~s~%"
                    predicate sequence arguments v1 v2))))))

(defun test-count-if-1 (predicate sequence start end from-end)
  (test-count-if predicate sequence
                 -1 start end from-end)
  (test-count-if predicate sequence
                 nil start end from-end)
  (test-count-if predicate sequence
                 #'identity start end from-end)
  (test-count-if predicate sequence
                 'identity start end from-end)

  (test-count-if predicate (coerce sequence 'vector)
                 -1 start end from-end)
  (test-count-if predicate (coerce sequence 'vector)
                 nil start end from-end)
  (test-count-if predicate (coerce sequence 'vector)
                 #'identity start end from-end)

  (test-count-if predicate (mapcar #'list sequence)
                 #'car start end from-end)
  (test-count-if predicate (mapcar #'list sequence)
                 'car start end from-end)

  (test-count-if predicate (map 'vector #'list sequence)
                 #'car start end from-end)
  (test-count-if predicate (map 'vector #'list sequence)
                 'car start end from-end))

(defun test-count-if-2 (predicate sequence start end)
  (test-count-if-1 predicate sequence start end -1)
  (test-count-if-1 predicate sequence start end nil)
  (test-count-if-1 predicate sequence start end t))

(defun test-count-if-3 (predicate sequence)
  (let ((length (length sequence)))
    (test-count-if-2 predicate sequence -1 -1)
    (loop for index from 0 to length
          do (test-count-if-2 predicate sequence -1 index)
             (test-count-if-2 predicate sequence index -1))
    (loop for start from 0 to length
          do (loop for end from start to length
                   do (test-count-if-2 predicate sequence start end)))))

(define-test |count-if normal|
  (assert-equal nil
                (test-count-if-3 (lambda (x) (eql x *i01*))
                                 (list *i1* *i02* *i2* *i02* *i4*))
                (test-count-if-3 (lambda (x) (eql x *i5*))
                                 (list *i1* *i02* *i2* *i02* *i4*))
                (test-count-if-3 (lambda (x) (eq x *l01*))
                                 (list *l1* *l02* *l2* *l02* *l4*))
                (test-count-if-3 (lambda (x) (eq x *l5* ))
                                 (list *l1* *l02* *l2* *l02* *l4*))
                (test-count-if-3 (lambda (x) (string-equal x *s01* ))
                                 (list *s1* *s02* *s2* *s02* *s4*))
                (test-count-if-3 (lambda (x) (string-equal x *s5*))
                                 (list *s1* *s02* *s2* *s02* *s4*))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Tests for function count-if-not

(defun stupid-count-if-not (predicate sequence
                            &key
                            key
                            (start 0)
                            end
                            from-end)
  (unless key
    (setf key #'identity))
  (unless (vectorp sequence)
    (setf sequence (coerce sequence 'vector)))
  (when (null end)
    (setf end (length sequence)))
  (assert (<= 0 start (length sequence)))
  (assert (<= 0 end (length sequence)))
  (if from-end
      (loop for index downfrom (1- end) to start
            count (not (funcall predicate (funcall key (aref sequence index)))))
      (loop for index from start below end
            count (not (funcall predicate (funcall key (aref sequence index)))))))

(defun test-count-if-not (predicate sequence key start end from-end)
  (flet ((make-arg (name thing)
           (if (eql thing -1) '() (list name thing))))
    (let ((arguments (append (make-arg :key key)
                             (make-arg :start start)
                             (make-arg :end end)
                             (make-arg :from-end from-end))))
      (let ((v1 (apply #'stupid-count-if-not predicate sequence arguments))
            (v2 (apply #'count-if-not predicate sequence arguments)))
        (if (eql v1 v2)
            (format *trace-output* "*")
            (format *trace-output*
                    "for test: COUNT-IF-NOT ~s ~s ~s~@
                     expected: ~s~%     but saw:  ~s~%"
                    predicate sequence arguments v1 v2))))))

(defun test-count-if-not-1 (predicate sequence start end from-end)
  (test-count-if-not predicate sequence
                     -1 start end from-end)
  (test-count-if-not predicate sequence
                     nil start end from-end)
  (test-count-if-not predicate sequence
                     #'identity start end from-end)
  (test-count-if-not predicate sequence
                     'identity start end from-end)

  (test-count-if-not predicate (coerce sequence 'vector)
                     -1 start end from-end)
  (test-count-if-not predicate (coerce sequence 'vector)
                     nil start end from-end)
  (test-count-if-not predicate (coerce sequence 'vector)
                     #'identity start end from-end)

  (test-count-if-not predicate (mapcar #'list sequence)
                     #'car start end from-end)
  (test-count-if-not predicate (mapcar #'list sequence)
                     'car start end from-end)

  (test-count-if-not predicate (map 'vector #'list sequence)
                     #'car start end from-end)
  (test-count-if-not predicate (map 'vector #'list sequence)
                     'car start end from-end))

(defun test-count-if-not-2 (predicate sequence start end)
  (test-count-if-not-1 predicate sequence start end -1)
  (test-count-if-not-1 predicate sequence start end nil)
  (test-count-if-not-1 predicate sequence start end t))

(defun test-count-if-not-3 (predicate sequence)
  (let ((length (length sequence)))
    (test-count-if-not-2 predicate sequence -1 -1)
    (loop for index from 0 to length
          do (test-count-if-not-2 predicate sequence -1 index)
             (test-count-if-not-2 predicate sequence index -1))
    (loop for start from 0 to length
          do (loop for end from start to length
                   do (test-count-if-not-2 predicate sequence start end)))))

(define-test |count-if-not normal|
  (assert-equal nil
                (test-count-if-not-3 (lambda (x) (eql x *i01*))
                                     (list *i1* *i02* *i2* *i02* *i4*))
                (test-count-if-not-3 (lambda (x) (eql x *i5*))
                                     (list *i1* *i02* *i2* *i02* *i4*))
                (test-count-if-not-3 (lambda (x) (eq x *l01*))
                                     (list *l1* *l02* *l2* *l02* *l4*))
                (test-count-if-not-3 (lambda (x) (eq x *l5* ))
                                     (list *l1* *l02* *l2* *l02* *l4*))
                (test-count-if-not-3 (lambda (x) (string-equal x *s01* ))
                                     (list *s1* *s02* *s2* *s02* *s4*))
                (test-count-if-not-3 (lambda (x) (string-equal x *s5*))
                                     (list *s1* *s02* *s2* *s02* *s4*))))


