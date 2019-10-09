(cl:in-package #:sicl-cons)

;;; The hyperspec says the consequences are undefined if the two lists
;;; do not have the same length.  We check for this situation and
;;; signal an error in that case.

(defun pairlis (keys data &optional alist)
  (loop with result = alist
        with remaining-keys = keys
        with remaining-data = data
        until (or (atom remaining-keys) (atom remaining-data))
        do (push (cons (pop remaining-keys) (pop remaining-data)) result)
        finally (unless (and (null remaining-keys) (null remaining-data))
                  (cond ((and (atom remaining-keys) (not (null remaining-keys)))
                         (error 'must-be-proper-list
                                :datum keys
                                :name 'pairlis))
                        ((and (atom remaining-data) (not (null remaining-data)))
                         (error 'must-be-proper-list
                                :datum data
                                :name 'pairlis))
                        (t
                         (error 'lists-must-have-the-same-length
                                :list1 keys
                                :list2 data
                                :name 'pairlis))))
                (return result)))
