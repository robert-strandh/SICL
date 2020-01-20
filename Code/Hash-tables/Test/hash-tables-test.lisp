(defpackage #:sicl-hash-table-test
  (:use #:common-lisp #:parachute)
  (:export #:sicl-hash-table-test)
  (:shadowing-import-from #:sicl-hash-table
                          #:hash-table
                          #:make-hash-table #:hash-table-p
                          #:hash-table-count #:hash-table-rehash-threshold #:hash-table-rehash-size
                          #:hash-table-size #:hash-table-test
                          #:puthash #:gethash #:remhash #:clrhash
                          #:with-hash-table-iterator
                          #:maphash))

(in-package #:sicl-hash-table-test)

(define-test sicl-hash-table-test)

(define-test construction
  :parent sicl-hash-table-test
  (let ((ht1 (make-hash-table))
        (ht2 (make-hash-table :test #'equal))
        (ht3 (make-hash-table :size 123)))
    (true (hash-table-p ht1))
    (false (hash-table-p "foobar"))
    (is eql 'eql (hash-table-test ht1))
    (is eql 'equal (hash-table-test ht2))
    (is = 123 (hash-table-size ht3))
    (is = 0 (hash-table-count ht1))))

(define-test setf-gethash
  :parent sicl-hash-table-test
  (let ((ht-eql (make-hash-table))
        (ht-equal (make-hash-table :test #'equal)))
    (is = 0 (hash-table-count ht-eql))
    (setf (gethash "foobar" ht-eql) 100)
    (is = 1 (hash-table-count ht-eql))
    (setf (gethash "foobar" ht-equal) 100)
    #+(or)
    (multiple-value-bind (value found-p)
        (gethash "foobar" ht-eql)
      (is eql nil value)
      (is eql nil found-p))
    (multiple-value-bind (value found-p)
        (gethash "foobar" ht-equal)
      (is eql 100 value)
      (is eql t found-p))))

(define-test setf-gethash
  :parent sicl-hash-table-test
  (let ((ht (make-hash-table)))
    (setf (gethash 'foobar ht) 100)
    (multiple-value-bind (value found-p)
        (gethash 'foobar ht)
      (is eql 100 value)
      (is eql t found-p))))

(define-test setf-remhash
  :parent sicl-hash-table-test
  (let ((ht (make-hash-table)))
    (setf (gethash 'foobar ht) 100)
    (remhash 'foobar ht)
    (multiple-value-bind (value found-p)
        (gethash 'foobar ht)
      (is eql nil value)
      (is eql nil found-p))))
    
(define-test clrhash
  :parent sicl-hash-table-test
  (let ((ht (make-hash-table)))
    (is = 0 (hash-table-count ht))
    (setf (gethash 'foo ht) 100)
    (setf (gethash 'bar ht) 200)
    (setf (gethash 'baz ht) 300)
    (is = 3 (hash-table-count ht))
    (clrhash ht)
    (is = 0 (hash-table-count ht))
    (loop for key in '(foo bar baz)
          do (multiple-value-bind (value found-p)
                 (gethash key ht)
               (is eql nil value)
               (is eql nil found-p)))))

(define-test with-hash-table-iterator
  (let ((ht (make-hash-table)))
    (setf (gethash 'foo ht) 100)
    (setf (gethash 'bar ht) 200)
    (let ((results))
      (with-hash-table-iterator (next-entry ht)
        (loop as result = (multiple-value-list (next-entry))
              until (not (first result))
              do (push result results)))
      (true (or (equal results '((t foo 100) (t bar 200)))
                (equal results '((t bar 200) (t foo 100))))))))

(define-test rehashing
  (let ((ht (make-hash-table :size 2 :rehash-size 3)))
    (setf (gethash 'foo ht) 100)
    (setf (gethash 'bar ht) 200)
    (setf (gethash 'baz ht) 300)
    (is = 5 (hash-table-size ht))
    (is equal 100 (gethash 'foo ht))
    (is equal 200 (gethash 'bar ht))
    (is equal 300 (gethash 'baz ht)))
  (let ((ht (make-hash-table :size 2 :rehash-size 3.0)))
    (setf (gethash 'foo ht) 100)
    (setf (gethash 'bar ht) 200)
    (setf (gethash 'baz ht) 300)
    (is = 6 (hash-table-size ht))
    (is equal 100 (gethash 'foo ht))
    (is equal 200 (gethash 'bar ht))
    (is equal 300 (gethash 'baz ht))))


