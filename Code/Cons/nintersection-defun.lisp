(cl:in-package #:sicl-cons)

;;; We take advantage of the fact that the standard doesn't require
;;; this function to have any side effects.

(defun nintersection (list1 list2
                      &key key (test nil test-given) (test-not nil test-not-given))
  (when (and test-given test-not-given)
    (error 'both-test-and-test-not-given :name 'nintersection))
  (let ((use-hash (> (* (length list1) (length list2)) 1000)))
    (if key
        (if test-given
            (cond ((or (eq test #'eq) (eq test 'eq))
                   (if use-hash
                       (|intersection key=other test=eq hash| 'nintersection list1 list2 key)
                       (|intersection key=other test=eq| 'nintersection list1 list2 key)))
                  ((or (eq test #'eql) (eq test 'eql))
                   (if use-hash
                       (|intersection key=other test=eql hash| 'nintersection list1 list2 key)
                       (|intersection key=other test=eql| 'nintersection list1 list2 key)))
                  ((or (eq test #'equal) (eq test 'equal))
                   (if use-hash
                       (|intersection key=other test=equal hash| 'nintersection list1 list2 key)
                       (|intersection key=other test=other| 'nintersection list1 list2 key #'equal)))
                  ((or (eq test #'equalp) (eq test 'equalp))
                   (if use-hash
                       (|intersection key=other test=equalp hash| 'nintersection list1 list2 key)
                       (|intersection key=other test=other| 'nintersection list1 list2 key #'equalp)))
                  (t
                   (|intersection key=other test=other| 'nintersection list1 list2 key test)))
            (if test-not-given
                (|intersection key=other test-not=other| 'nintersection list1 list2 key test-not)
                (if use-hash
                    (|intersection key=other test=eql hash| 'nintersection list1 list2 key)
                    (|intersection key=other test=eql| 'nintersection list1 list2 key))))
        (if test-given
            (cond ((or (eq test #'eq) (eq test 'eq))
                   (if use-hash
                       (|intersection key=identity test=eq hash| 'nintersection list1 list2)
                       (|intersection key=identity test=eq| 'nintersection list1 list2)))
                  ((or (eq test #'eql) (eq test 'eql))
                   (if use-hash
                       (|intersection key=identity test=eql hash| 'nintersection list1 list2)
                       (|intersection key=identity test=eql| 'nintersection list1 list2)))
                  ((or (eq test #'equal) (eq test 'equal))
                   (if use-hash
                       (|intersection key=identity test=equal hash| 'nintersection list1 list2)
                       (|intersection key=identity test=other| 'nintersection list1 list2 #'equal)))
                  ((or (eq test #'equalp) (eq test 'equalp))
                   (if use-hash
                       (|intersection key=identity test=equalp hash| 'nintersection list1 list2)
                       (|intersection key=identity test=other| 'nintersection list1 list2 #'equalp)))
                  (t
                   (|intersection key=identity test=other| 'nintersection list1 list2 test)))
            (if test-not-given
                (|intersection key=identity test-not=other| 'nintersection list1 list2 test-not)
                (if use-hash
                    (|intersection key=identity test=eql hash| 'nintersection list1 list2)
                    (|intersection key=identity test=eql| 'nintersection list1 list2)))))))
