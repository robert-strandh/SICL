;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Mon Sep 23 20:59:10 2002
;;;; Contains: Aux. functions for testing REMOVE-DUPLICATES/DELETE-DUPLICATES



(defun my-remove-duplicates (orig-sequence
                             &key from-end test test-not (start 0) end key)
  (assert (typep orig-sequence 'sequence))
  (let* ((sequence orig-sequence)
         (len (length sequence)))
    (unless end (setq end len))
    (unless key (setq key #'identity))
    (setf key (coerce key 'function))
    (cond
      (test (setf test (coerce test 'function))
            (assert (not test-not)))
      (test-not (setf test-not (coerce test-not 'function))
                (setq test #'(lambda (x y)
                               (not (funcall (the function test) x y)))))
      (t (setq test #'eql)))
    (assert (integerp start))
    (assert (integerp end))
    (assert (<= 0 start end len))
    ;; (format t "start = ~A, end = ~A, len = ~A~%" start end len)
    (if from-end
        (psetq start (- len end)
               end (- len start)
               sequence (reverse sequence))
        (setq sequence (copy-seq sequence)))
    ;; (format t "start = ~A, end = ~A, len = ~A~%" start end len)
    (assert (<= 0 start end len) (start end len))
    (let ((result nil))
      (loop for i from 0 below start
            do (push (elt sequence i) result))
      (loop for i from start below end
            for x = (elt sequence i)
            for kx = (funcall (the function key) x)
            unless (position kx
                             sequence
                             :start (1+ i)
                             :end end
                             :test (the function test)
                             :key (the function key))
            do (push x result))
      (loop for i from end below len
            do (push (elt sequence i) result))
      (unless from-end (setq result (reverse result)))
      (cond
        ((listp orig-sequence) result)
        ((arrayp orig-sequence)
         (make-array (length result) :initial-contents result
                     :element-type (array-element-type orig-sequence)))
        (t (assert nil))))))

(defun make-random-rdup-params (maxlen)
  "Make random input parameters for REMOVE-DUPLICATES."
  (multiple-value-bind (element-type type len start end from-end
                                     count seq key test test-not)
      (make-random-rd-params maxlen)
    (declare (ignore count element-type len type))
    (let ((arg-list
           (reduce #'nconc
                   (random-permute
                    (list
                     (when start (list :start start))
                     (cond (end (list :end end))
                           ((coin) (list :end nil)))
                     (cond (from-end (list :from-end from-end))
                           ((coin) (list :from-end nil)))
                     (cond (key (list :key key))
                           ;; ((coin) (list :key nil))
                           )
                     (when test (list :test test))
                     (when test-not (list :test test-not)))))))
      (values seq arg-list))))

(defun random-test-remove-dups (maxlen &optional (pure t))
  (multiple-value-bind (seq arg-list)
      (make-random-rdup-params maxlen)
    (let* ((seq1 (copy-seq seq))
           (seq2 (copy-seq seq))
           (seq1r (apply (if pure #'remove-duplicates
                           #'delete-duplicates)
                         seq1 arg-list))
           (seq2r (apply #'my-remove-duplicates seq2 arg-list)))
      (cond
       ((and pure (not (equalp seq seq1))) (list :fail1 seq seq1r seq2r arg-list))
       ((and pure (not (equalp seq seq2))) (list :fail2 seq seq1r seq2r arg-list))
       ((not (equalp seq1r seq2r)) (list :fail3 seq seq1r seq2r arg-list))
       (t t)))))
