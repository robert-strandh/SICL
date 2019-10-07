(cl:in-package #:sicl-cons)

;;; We take advantage of the fact that the standard doesn't require
;;; this function to have any side effects.

(defun nset-exclusive-or (list1 list2
                          &key key (test nil test-given) (test-not nil test-not-given))
  (when (and test-given test-not-given)
    (error 'both-test-and-test-not-given :name 'nset-exclusive-or))
  (let ((use-hash (> (* (length list1) (length list2)) 1000)))
    (if key
        (if test-given
            (cond ((or (eq test #'eq) (eq test 'eq))
                   (if use-hash
                       (|set-exclusive-or key=other test=eq hash| 'nset-exclusive-or list1 list2 key)
                       (|set-exclusive-or key=other test=eq| 'nset-exclusive-or list1 list2 key)))
                  ((or (eq test #'eql) (eq test 'eql))
                   (if use-hash
                       (|set-exclusive-or key=other test=eql hash| 'nset-exclusive-or list1 list2 key)
                       (|set-exclusive-or key=other test=eql| 'nset-exclusive-or list1 list2 key)))
                  ((or (eq test #'equal) (eq test 'equal))
                   (if use-hash
                       (|set-exclusive-or key=other test=equal hash| 'nset-exclusive-or list1 list2 key)
                       (|set-exclusive-or key=other test=other| 'nset-exclusive-or list1 list2 key #'equal)))
                  ((or (eq test #'equalp) (eq test 'equalp))
                   (if use-hash
                       (|set-exclusive-or key=other test=equalp hash| 'nset-exclusive-or list1 list2 key)
                       (|set-exclusive-or key=other test=other| 'nset-exclusive-or list1 list2 key #'equalp)))
                  (t
                   (|set-exclusive-or key=other test=other| 'nset-exclusive-or list1 list2 key test)))
            (if test-not-given
                (|set-exclusive-or key=other test-not=other| 'nset-exclusive-or list1 list2 key test-not)
                (if use-hash
                    (|set-exclusive-or key=other test=eql hash| 'nset-exclusive-or list1 list2 key)
                    (|set-exclusive-or key=other test=eql| 'nset-exclusive-or list1 list2 key))))
        (if test-given
            (cond ((or (eq test #'eq) (eq test 'eq))
                   (if use-hash
                       (|set-exclusive-or key=identity test=eq hash| 'nset-exclusive-or list1 list2)
                       (|set-exclusive-or key=identity test=eq| 'nset-exclusive-or list1 list2)))
                  ((or (eq test #'eql) (eq test 'eql))
                   (if use-hash
                       (|set-exclusive-or key=identity test=eql hash| 'nset-exclusive-or list1 list2)
                       (|set-exclusive-or key=identity test=eql| 'nset-exclusive-or list1 list2)))
                  ((or (eq test #'equal) (eq test 'equal))
                   (if use-hash
                       (|set-exclusive-or key=identity test=equal hash| 'nset-exclusive-or list1 list2)
                       (|set-exclusive-or key=identity test=other| 'nset-exclusive-or list1 list2 #'equal)))
                  ((or (eq test #'equalp) (eq test 'equalp))
                   (if use-hash
                       (|set-exclusive-or key=identity test=equalp hash| 'nset-exclusive-or list1 list2)
                       (|set-exclusive-or key=identity test=other| 'nset-exclusive-or list1 list2 #'equalp)))
                  (t
                   (|set-exclusive-or key=identity test=other| 'nset-exclusive-or list1 list2 test)))
            (if test-not-given
                (|set-exclusive-or key=identity test-not=other| 'nset-exclusive-or list1 list2 test-not)
                (if use-hash
                    (|set-exclusive-or key=identity test=eql hash| 'nset-exclusive-or list1 list2)
                    (|set-exclusive-or key=identity test=eql| 'nset-exclusive-or list1 list2)))))))
