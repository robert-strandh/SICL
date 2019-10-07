(cl:in-package #:sicl-cons)

;;; We take advantage of the fact that the standard doesn't require
;;; this function to have any side effects.

(defun nset-difference (list1 list2
                        &key key (test nil test-given) (test-not nil test-not-given))
  (when (and test-given test-not-given)
    (error 'both-test-and-test-not-given :name 'nset-difference))
  (let ((use-hash (> (* (length list1) (length list2)) 1000)))
    (if key
        (if test-given
            (cond ((or (eq test #'eq) (eq test 'eq))
                   (if use-hash
                       (|set-difference key=other test=eq hash| 'nset-difference list1 list2 key)
                       (|set-difference key=other test=eq| 'nset-difference list1 list2 key)))
                  ((or (eq test #'eql) (eq test 'eql))
                   (if use-hash
                       (|set-difference key=other test=eql hash| 'nset-difference list1 list2 key)
                       (|set-difference key=other test=eql| 'nset-difference list1 list2 key)))
                  ((or (eq test #'equal) (eq test 'equal))
                   (if use-hash
                       (|set-difference key=other test=equal hash| 'nset-difference list1 list2 key)
                       (|set-difference key=other test=other| 'nset-difference list1 list2 key #'equal)))
                  ((or (eq test #'equalp) (eq test 'equalp))
                   (if use-hash
                       (|set-difference key=other test=equalp hash| 'nset-difference list1 list2 key)
                       (|set-difference key=other test=other| 'nset-difference list1 list2 key #'equalp)))
                  (t
                   (|set-difference key=other test=other| 'nset-difference list1 list2 key test)))
            (if test-not-given
                (|set-difference key=other test-not=other| 'nset-difference list1 list2 key test-not)
                (if use-hash
                    (|set-difference key=other test=eql hash| 'nset-difference list1 list2 key)
                    (|set-difference key=other test=eql| 'nset-difference list1 list2 key))))
        (if test-given
            (cond ((or (eq test #'eq) (eq test 'eq))
                   (if use-hash
                       (|set-difference key=identity test=eq hash| 'nset-difference list1 list2)
                       (|set-difference key=identity test=eq| 'nset-difference list1 list2)))
                  ((or (eq test #'eql) (eq test 'eql))
                   (if use-hash
                       (|set-difference key=identity test=eql hash| 'nset-difference list1 list2)
                       (|set-difference key=identity test=eql| 'nset-difference list1 list2)))
                  ((or (eq test #'equal) (eq test 'equal))
                   (if use-hash
                       (|set-difference key=identity test=equal hash| 'nset-difference list1 list2)
                       (|set-difference key=identity test=other| 'nset-difference list1 list2 #'equal)))
                  ((or (eq test #'equalp) (eq test 'equalp))
                   (if use-hash
                       (|set-difference key=identity test=equalp hash| 'nset-difference list1 list2)
                       (|set-difference key=identity test=other| 'nset-difference list1 list2 #'equalp)))
                  (t
                   (|set-difference key=identity test=other| 'nset-difference list1 list2 test)))
            (if test-not-given
                (|set-difference key=identity test-not=other| 'nset-difference list1 list2 test-not)
                (if use-hash
                    (|set-difference key=identity test=eql hash| 'nset-difference list1 list2)
                    (|set-difference key=identity test=eql| 'nset-difference list1 list2)))))))
