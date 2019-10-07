(cl:in-package #:sicl-cons)

;;; We take advantage of the fact that the standard doesn't require
;;; this function to have any side effects.

(defun nunion (list1 list2
               &key key (test nil test-given) (test-not nil test-not-given))
  (when (and test-given test-not-given)
    (error 'both-test-and-test-not-given :name 'nunion))
  (let ((use-hash (> (* (length list1) (length list2)) 1000)))
    (if key
        (if test-given
            (cond ((or (eq test #'eq) (eq test 'eq))
                   (if use-hash
                       (|union key=other test=eq hash| 'nunion list1 list2 key)
                       (|union key=other test=eq| 'nunion list1 list2 key)))
                  ((or (eq test #'eql) (eq test 'eql))
                   (if use-hash
                       (|union key=other test=eql hash| 'nunion list1 list2 key)
                       (|union key=other test=eql| 'nunion list1 list2 key)))
                  ((or (eq test #'equal) (eq test 'equal))
                   (if use-hash
                       (|union key=other test=equal hash| 'nunion list1 list2 key)
                       (|union key=other test=other| 'nunion list1 list2 key #'equal)))
                  ((or (eq test #'equalp) (eq test 'equalp))
                   (if use-hash
                       (|union key=other test=equalp hash| 'nunion list1 list2 key)
                       (|union key=other test=other| 'nunion list1 list2 key #'equalp)))
                  (t
                   (|union key=other test=other| 'nunion list1 list2 key test)))
            (if test-not-given
                (|union key=other test-not=other| 'nunion list1 list2 key test-not)
                (if use-hash
                    (|union key=other test=eql hash| 'nunion list1 list2 key)
                    (|union key=other test=eql| 'nunion list1 list2 key))))
        (if test-given
            (cond ((or (eq test #'eq) (eq test 'eq))
                   (if use-hash
                       (|union key=identity test=eq hash| 'nunion list1 list2)
                       (|union key=identity test=eq| 'nunion list1 list2)))
                  ((or (eq test #'eql) (eq test 'eql))
                   (if use-hash
                       (|union key=identity test=eql hash| 'nunion list1 list2)
                       (|union key=identity test=eql| 'nunion list1 list2)))
                  ((or (eq test #'equal) (eq test 'equal))
                   (if use-hash
                       (|union key=identity test=equal hash| 'nunion list1 list2)
                       (|union key=identity test=other| 'nunion list1 list2 #'equal)))
                  ((or (eq test #'equalp) (eq test 'equalp))
                   (if use-hash
                       (|union key=identity test=equalp hash| 'nunion list1 list2)
                       (|union key=identity test=other| 'nunion list1 list2 #'equalp)))
                  (t
                   (|union key=identity test=other| 'nunion list1 list2 test)))
            (if test-not-given
                (|union key=identity test-not=other| 'nunion list1 list2 test-not)
                (if use-hash
                    (|union key=identity test=eql hash| 'nunion list1 list2)
                    (|union key=identity test=eql| 'nunion list1 list2)))))))
