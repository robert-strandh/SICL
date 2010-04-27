(in-package #:sicl-sequences-test)

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
  (assert-error 'program-error (find-if)))

(define-test find-if.error.6
  (assert-error 'program-error (find-if #'null)))

(define-test find-if.error.7
  (assert-error 'program-error (find-if #'null nil :bad t)))

(define-test find-if.error.8
  (assert-error 'program-error
		 (find-if #'null nil :bad t :allow-other-keys nil))
  t)

(define-test find-if.error.9
  (assert-error 'program-error (find-if #'null nil 1 1)))

(define-test find-if.error.10
  (assert-error 'program-error (find-if #'null nil :key)))

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

;; (define-test find-if.order.1
;;   (let ((i 0) x y)
;;     (values
;;      (find-if (progn (setf x (incf i)) #'identity)
;; 	      (progn (setf y (incf i)) '(nil nil nil a nil nil)))
;;      i x y))
;;   a 2 1 2)

;; (define-test find-if.order.2
;;   (let ((i 0) a b c d e f)
;;     (values
;;      (find-if (progn (setf a (incf i)) #'null)
;; 	      (progn (setf b (incf i)) '(nil nil nil a nil nil))
;; 	      :start (progn (setf c (incf i)) 1)
;; 	      :end   (progn (setf d (incf i)) 4)
;; 	      :from-end (setf e (incf i))
;; 	      :key   (progn (setf f (incf i)) #'null)
;; 	      )
;;      i a b c d e f))
;;   a 6 1 2 3 4 5 6)


;; (define-test find-if.order.3
;;   (let ((i 0) a b c d e f)
;;     (values
;;      (find-if (progn (setf a (incf i)) #'null)
;; 	      (progn (setf b (incf i)) '(nil nil nil a nil nil))
;; 	      :key   (progn (setf c (incf i)) #'null)
;; 	      :from-end (setf d (incf i))
;; 	      :end   (progn (setf e (incf i)) 4)
;; 	      :start (progn (setf f (incf i)) 1)
;; 	      )
;;      i a b c d e f))
;;   a 6 1 2 3 4 5 6)
