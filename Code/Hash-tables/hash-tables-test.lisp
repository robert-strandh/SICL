(defpackage #:sicl-hash-tables-test
  (:use #:common-lisp #:lisp-unit)
  (:shadowing-import-from #:sicl-hash-tables
                          #:hash-table
                          #:make-hash-table #:hash-table-p
                          #:hash-table-count #:hash-table-rehash-threshold #:hash-table-rehash-size
                          #:hash-table-size #:hash-table-test
                          #:puthash #:gethash #:remhash #:clrhash
                          #:with-hash-table-iterator
                          #:maphash))

(in-package #:sicl-hash-tables-test)

(define-test construction
  (let ((ht1 (make-hash-table))
        (ht2 (make-hash-table :test #'equal))
        (ht3 (make-hash-table :size 123)))
    (assert-true (hash-table-p ht1))
    (assert-false (hash-table-p "foobar"))
    (assert-equal #'eql (hash-table-test ht1))
    (assert-equal #'equal (hash-table-test ht2))
    (assert-equal 123 (hash-table-size ht3))
    (assert-equal 0 (hash-table-count ht1))))

(define-test gethash-puthash
  (let ((ht-eql (make-hash-table))
        (ht-equal (make-hash-table :test #'equal)))
    (assert-equal 0 (hash-table-count ht-eql))
    (puthash "foobar" ht-eql 100)
    (assert-equal 1 (hash-table-count ht-eql))
    (puthash "foobar" ht-equal 100)
    (multiple-value-bind (value found-p)
        (gethash "foobar" ht-eql)
      (assert-equal nil value)
      (assert-equal nil found-p))
    (multiple-value-bind (value found-p)
        (gethash "foobar" ht-equal)
      (assert-equal 100 value)
      (assert-equal t found-p))))

(define-test setf-gethash
  (let ((ht (make-hash-table)))
    (setf (gethash 'foobar ht) 100)
    (multiple-value-bind (value found-p)
        (gethash 'foobar ht)
      (assert-equal 100 value)
      (assert-equal t found-p))))

(define-test setf-remhash
  (let ((ht (make-hash-table)))
    (setf (gethash 'foobar ht) 100)
    (remhash 'foobar ht)
    (multiple-value-bind (value found-p)
        (gethash 'foobar ht)
      (assert-equal nil value)
      (assert-equal nil found-p))))
    
(define-test clrhash
  (let ((ht (make-hash-table)))
    (assert-equal 0 (hash-table-count ht))
    (setf (gethash 'foo ht) 100)
    (setf (gethash 'bar ht) 200)
    (setf (gethash 'baz ht) 300)
    (assert-equal 3 (hash-table-count ht))
    (clrhash ht)
    (assert-equal 0 (hash-table-count ht))
    (loop for key in '(foo bar baz)
          do (multiple-value-bind (value found-p)
                 (gethash key ht)
               (assert-equal nil value)
               (assert-equal nil found-p)))))

(define-test with-hash-table-iterator
  (let ((ht (make-hash-table)))
    (setf (gethash 'foo ht) 100)
    (setf (gethash 'bar ht) 200)
    (let ((results))
      (with-hash-table-iterator (next-entry ht)
        (loop as result = (multiple-value-list (next-entry))
              until (not (first result))
              do (push result results)))
      (assert-true (or (equal results '((t foo 100) (t bar 200)))
                       (equal results '((t bar 200) (t foo 100))))))))

(define-test rehashing
  (let ((ht (make-hash-table :size 2 :rehash-size 3)))
    (setf (gethash 'foo ht) 100)
    (setf (gethash 'bar ht) 200)
    (setf (gethash 'baz ht) 300)
    (assert-equal 5 (hash-table-size ht))
    (assert-equal 100 (gethash 'foo ht))
    (assert-equal 200 (gethash 'bar ht))
    (assert-equal 300 (gethash 'baz ht)))
  (let ((ht (make-hash-table :size 2 :rehash-size 3.0)))
    (setf (gethash 'foo ht) 100)
    (setf (gethash 'bar ht) 200)
    (setf (gethash 'baz ht) 300)
    (assert-equal 6 (hash-table-size ht))
    (assert-equal 100 (gethash 'foo ht))
    (assert-equal 200 (gethash 'bar ht))
    (assert-equal 300 (gethash 'baz ht))))


