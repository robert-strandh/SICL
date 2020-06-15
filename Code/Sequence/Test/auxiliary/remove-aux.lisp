;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sun Sep 15 07:42:36 2002
;;;; Contains: Auxiliary functions for testing REMOVE and related functions

(in-package #:sicl-sequence-test)

(defun make-random-element (type)
  (cond
   ((subtypep* 'fixnum type)
    (random most-positive-fixnum))
   ((and (listp type)
         (eql (car type) 'integer)
         (integerp (cadr type))
         (integerp (caddr type))
         (null (cdddr type)))
    (+ (cadr type) (random (- (1+ (caddr type)) (cadr type)))))
   ((subtypep* '(integer 0 255) type)
    (random 255))
   ((subtypep* '(integer 0 7) type)
    (random 8))
   ((subtypep* 'bit type)
    (random 2))
   ((subtypep* 'symbol type)
    (elt '(a b c d e f g h) (random 8)))
   ((subtypep* '(member #\a #\b #\c #\d #\e #\f #\g #\h) type)
    (elt "abcdefgh" (random 8)))
   (t (error "Can't get random element of type ~A~%." type))))

(defun make-random-remove-input (len type element-type)

  "Randomly generate a test case for REMOVE.  Given a length
   a sequence type, and an element type, produce a random
   sequence of length LEN of sequence type TYPE, and either
   generate a random member of the sequence or a random
   element of the element type to delete from the sequence."

  (let* ((seq (if (subtypep* type 'list)
                  (loop for i from 1 to len collect
                        (make-random-element element-type))
                (let ((seq (if (and (subtypep type 'vector)
                                    (coin 3))
                               (make-array
                                (list (+ len (random (1+ len))))
                                :initial-element (make-random-element element-type)
                                :fill-pointer len
                                :element-type element-type)
                               (make-sequence type len))))
                  (dotimes (i len)
                    (setf (elt seq i) (make-random-element element-type)))
                  seq)))
         (e (if (and (> len 0) (coin))
                (elt seq (random len))
              (make-random-element element-type)))
         )
    (values len seq e)))

(defun my-remove (element
                  sequence
                  &key
                  (start 0)
                  (end nil)
                  (test #'eql test-p)
                  (test-not nil test-not-p)
                  (key nil)
                  (from-end nil)
                  (count nil))
  (assert (not (and test-p test-not-p)))
  (my-remove-if
   (cond (test-p
          (setf test (coerce test 'function))
          #'(lambda (x) (funcall (the function test) element x)))
         (test-not-p
          (setf test-not (coerce test-not 'function))
          #'(lambda (x) (not (funcall (the function test-not) element x))))
         (t #'(lambda (x) (eql element x))))
   sequence :start start :end end :key key :from-end from-end :count count))

(defun my-remove-if (predicate
                     original-sequence
                     &key (from-end nil)
                     (start 0)
                     (end nil)
                     (count nil)
                     (key #'identity))
  (let ((len (length original-sequence))
        (sequence (copy-seq original-sequence)))
    (unless end (setq end len))
    (unless key (setq key #'identity))
    (unless count (setq count len))

    ;; Check that everything's kosher
    (assert (<= 0 start end len))
    (assert (typep sequence 'sequence))
    (assert (integerp count))
    (assert (or (symbolp predicate) (functionp predicate)))
    (assert (or (symbolp key) (functionp key)))

    (setf predicate (coerce predicate 'function))
    (setf key (coerce key 'function))

    ;; If FROM-END, reverse the sequence and flip
    ;; start, end
    (when from-end
      (psetq sequence (nreverse sequence)
             start (- len end)
             end (- len start)))

    ;; Accumulate a list of elements for the result
    (let ((pos 0)
          (result nil)) ;; accumulate in reverse order
      (map nil
           #'(lambda (e)
               (if (and (> count 0)
                        (>= pos start)
                        (< pos end)
                        (funcall (the function predicate)
                                 (funcall (the function key) e)))
                   (decf count)
                 (push e result))
               (incf pos))
           sequence)
      (unless from-end
        (setq result (nreverse result)))
      ;; Convert to the correct type
      (if (listp sequence)
          result
        (let ((element-type (array-element-type original-sequence)))
          (make-array (length result) :element-type element-type
                      :initial-contents result))))))

(defun my-remove-if-not (pred &rest args)
  (when (symbolp pred)
    (setq pred (coerce pred 'function)))
  (assert (typep pred 'function))
  (apply #'my-remove-if (complement pred) args))

(defun make-random-rd-params (maxlen)
  "Generate random paramaters for remove/delete/etc. functions."
  (let* ((element-type
          (rcase
           (2 t)
           (1 'bit)
           (1 '(integer 0 2))
           (1 'symbol)))
         (type-select (random 7))
         (type
          (case type-select
            (0 'list)
            (1 'vector)
            (2 (setq element-type 'character) 'string)
            (3 (setq element-type 'bit) 'bit-vector)
            (4 'simple-vector)
            (5 (setq element-type '(integer 0 255))
               '(vector (integer 0 255)))
            (6 (setq element-type 'fixnum) '(vector fixnum))
            (t (error "Can't happen?!~%"))))
         (len (random maxlen))
         (start (and (coin) (> len 0)
                     (random len)))
         (end (and (coin)
                   (if start (+ start (random (- len start)))
                     (random (1+ len)))))
         (from-end (coin))
         (count (case (random 5)
                  ((0 1) nil)
                  ((2 3) (random (1+ len)))
                  (t (if (coin) -1 -10000000000000))))
         (seq (multiple-value-bind (x y z) (make-random-remove-input len type element-type)
                (declare (ignore x z))
                y))
         (key (and (coin)
                   (case type-select
                     (2 (random-case
                         #'char-upcase 'char-upcase
                         #'char-downcase 'char-downcase))
                     (3 #'(lambda (x) (- 1 x)))
                     ((5 6) (random-case #'1+ '1+ #'1- '1-))
                     (t (random-case 'identity #'identity)))))
         (test (and (eql (random 3) 0)
                    (random-case 'eq 'eql 'equal
                                 #'eq #'eql #'equal)))
         (test-not (and (not test)
                        (coin)
                        (random-case 'eq 'eql 'equal
                                     #'eq #'eql #'equal)))
         )
    ;; Return parameters
    (values
     element-type type len start end from-end count seq key test test-not)))

(defun random-test-remove-args (maxlen)
  (multiple-value-bind (element-type type len start end from-end count seq key test test-not)
      (make-random-rd-params maxlen)
    (declare (ignore type))
    (let ((element (if (and (coin) (> len 0))
                       (random-from-seq seq)
                     (make-random-element element-type)))
          (arg-list
           (reduce #'nconc
                   (random-permute
                    (list
                     (when start (list :start start))
                     (cond (end (list :end end))
                           ((coin) (list :end nil)))
                     (cond (from-end (list :from-end from-end))
                           ((coin) (list :from-end nil)))
                     (cond (count (list :count count))
                           ((coin) (list :count nil)))
                     (cond (key (list :key key))
                           ;; ((coin) (list :key nil))
                           )
                     (when test (list :test test))
                     (when test-not (list :test test-not)))))))
      (values element seq arg-list))))

(defparameter *remove-fail-args* nil)

(defun random-test-remove (maxlen &key (tested-fn #'remove)
                                  (check-fn #'my-remove)
                                  (pure t))
  (setf tested-fn (coerce tested-fn 'function))
  (setf check-fn (coerce check-fn 'function))
  (multiple-value-bind (element seq arg-list)
      (random-test-remove-args maxlen)
    (let* ((seq1 (copy-seq seq))
           (seq2 (copy-seq seq))
           (seq1r (apply (the function tested-fn) element seq1 arg-list))
           (seq2r (apply (the function check-fn) element seq2 arg-list)))
      (setq *remove-fail-args* (list* element seq arg-list))
      (cond
       ((and pure (not (equalp seq seq1))) :fail1)
       ((and pure (not (equalp seq seq2))) :fail2)
       ((not (equalp seq1r seq2r)) :fail3)
       (t t)))))

(defun random-test-remove-if (maxlen &optional (negate nil))
  (multiple-value-bind (element seq arg-list)
      (random-test-remove-args maxlen)
    (let ((fn (getf arg-list :key))
          (test (getf arg-list :test)))
      (remf arg-list :key)
      (remf arg-list :test)
      (remf arg-list :test-not)
      (unless test (setq test #'eql))
      (setf test (coerce test 'function))
      (if fn
          (case (random 3)
            (0 (setf arg-list (list* :key 'identity arg-list)))
            (1 (setf arg-list (list* :key #'identity arg-list)))
            (t nil))
        (setf fn (if (coin) 'identity
                   #'(lambda (x) (funcall (the function test)
                                          element x)))))
      (let* ((seq1 (copy-seq seq))
             (seq2 (copy-seq seq))
             (seq1r (apply (if negate #'remove-if-not #'remove-if)
                           fn seq1 arg-list))
             (seq2r (apply (if negate #'my-remove-if-not #'my-remove-if)
                           fn seq2 arg-list)))
        (setq *remove-fail-args* (cons seq1 arg-list))
        (cond
         ((not (equalp seq seq1)) :fail1)
         ((not (equalp seq seq2)) :fail2)
         ((not (equalp seq1r seq2r)) :fail3)
         (t t))))))

(defun random-test-delete (maxlen)
  (random-test-remove maxlen :tested-fn #'delete :pure nil))

(defun random-test-delete-if (maxlen &optional (negate nil))
  (multiple-value-bind (element seq arg-list)
      (random-test-remove-args maxlen)
    (let ((fn (getf arg-list :key))
          (test (getf arg-list :test)))
      (remf arg-list :key)
      (remf arg-list :test)
      (remf arg-list :test-not)
      (unless test (setq test #'eql))
      (setf test (coerce test 'function))
      (if fn
          (case (random 3)
            (0 (setf arg-list (list* :key 'identity arg-list)))
            (1 (setf arg-list (list* :key #'identity arg-list)))
            (t nil))
        (setf fn (if (coin) 'identity
                   #'(lambda (x) (funcall (the function test) element x)))))
      (setq *remove-fail-args* (list* seq arg-list))
      (let* ((seq1 (copy-seq seq))
             (seq2 (copy-seq seq))
             (seq1r (apply (if negate #'delete-if-not #'delete-if)
                           fn seq1 arg-list))
             (seq2r (apply (if negate #'my-remove-if-not #'my-remove-if)
                           fn seq2 arg-list)))
        (cond
         ((not (equalp seq1r seq2r)) :fail3)
         (t t))))))
