(cl:in-package #:sicl-cons)

;;; FIXME: rewrite this function to use the same techniqe
;;; as is used by the sequence functions.

(defun |subsetp key=identity test=eql| (name list1 list2)
  (with-proper-list-elements (element list1 name)
    (unless (|member test=eql key=identity| name element list2)
      (return-from |subsetp key=identity test=eql| nil)))
  t)

(defun |subsetp key=identity test=eq| (name list1 list2)
  (with-proper-list-elements (element list1 name)
    (unless (|member test=eq key=identity| name element list2)
      (return-from |subsetp key=identity test=eq| nil)))
  t)

(defun |subsetp key=other test=eql| (name list1 list2 key)
  (with-proper-list-elements (element list1 name)
    (unless (|member test=eql key=other| name (funcall key element) list2 key)
      (return-from |subsetp key=other test=eql| nil)))
  t)

(defun |subsetp key=other test=eq| (name list1 list2 key)
  (with-proper-list-elements (element list1 name)
    (unless (|member test=eq key=other| name (funcall key element) list2 key)
      (return-from |subsetp key=other test=eq| nil)))
  t)

(defun |subsetp key=identity test=other| (name list1 list2 test)
  (with-proper-list-elements (element list1 name)
    (unless (|member test=other key=identity| name element list2 test)
      (return-from |subsetp key=identity test=other| nil)))
  t)

(defun |subsetp key=other test=other| (name list1 list2 key test)
  (with-proper-list-elements (element list1 name)
    (unless (|member test=other key=other| name (funcall key element) list2 test key)
      (return-from |subsetp key=other test=other| nil)))
  t)

(defun |subsetp key=identity test-not=other| (name list1 list2 test-not)
  (with-proper-list-elements (element list1 name)
    (unless (|member test-not=other key=identity| name  element list2 test-not)
      (return-from |subsetp key=identity test-not=other| nil)))
  t)

(defun |subsetp key=other test-not=other| (name list1 list2 key test-not)
  (with-proper-list-elements (element list1 name)
    (unless (|member test-not=other key=other| name (funcall key element) list2 test-not key)
      (return-from |subsetp key=other test-not=other| nil)))
  t)

(defun |subsetp key=identity test=eq hash| (name list1 list2)
  (let ((table (make-hash-table :test #'eq)))
    (with-proper-list-elements (element list2 name)
      (setf (gethash element table) t))
    (with-proper-list-elements (element list1 name)
      (unless (gethash element table)
        (return-from |subsetp key=identity test=eq hash| nil))))
  t)

(defun |subsetp key=identity test=eql hash| (name list1 list2)
  (let ((table (make-hash-table :test #'eql)))
    (with-proper-list-elements (element list2 name)
      (setf (gethash element table) t))
    (with-proper-list-elements (element list1 name)
      (unless (gethash element table)
        (return-from |subsetp key=identity test=eql hash| nil))))
  t)

(defun |subsetp key=identity test=equal hash| (name list1 list2)
  (let ((table (make-hash-table :test #'equal)))
    (with-proper-list-elements (element list2 name)
      (setf (gethash element table) t))
    (with-proper-list-elements (element list1 name)
      (unless (gethash element table)
        (return-from |subsetp key=identity test=equal hash| nil))))
  t)

(defun |subsetp key=identity test=equalp hash| (name list1 list2)
  (let ((table (make-hash-table :test #'equalp)))
    (with-proper-list-elements (element list2 name)
      (setf (gethash element table) t))
    (with-proper-list-elements (element list1 name)
      (unless (gethash element table)
        (return-from |subsetp key=identity test=equalp hash| nil))))
  t)

(defun |subsetp key=other test=eq hash| (name list1 list2 key)
  (let ((table (make-hash-table :test #'eq)))
    (with-proper-list-elements (element list2 name)
      (setf (gethash (funcall key element) table) t))
    (with-proper-list-elements (element list1 name)
      (unless (gethash (funcall key element) table)
        (return-from |subsetp key=other test=eq hash| nil))))
  t)

(defun |subsetp key=other test=eql hash| (name list1 list2 key)
  (let ((table (make-hash-table :test #'eql)))
    (with-proper-list-elements (element list2 name)
      (setf (gethash (funcall key element) table) t))
    (with-proper-list-elements (element list1 name)
      (unless (gethash (funcall key element) table)
        (return-from |subsetp key=other test=eql hash| nil))))
  t)

(defun |subsetp key=other test=equal hash| (name list1 list2 key)
  (let ((table (make-hash-table :test #'equal)))
    (with-proper-list-elements (element list2 name)
      (setf (gethash (funcall key element) table) t))
    (with-proper-list-elements (element list1 name)
      (unless (gethash (funcall key element) table)
        (return-from |subsetp key=other test=equal hash| nil))))
  t)

(defun |subsetp key=other test=equalp hash| (name list1 list2 key)
  (let ((table (make-hash-table :test #'equalp)))
    (with-proper-list-elements (element list2 name)
      (setf (gethash (funcall key element) table) t))
    (with-proper-list-elements (element list1 name)
      (unless (gethash (funcall key element) table)
        (return-from |subsetp key=other test=equalp hash| nil))))
  t)

(defun subsetp (list1 list2
                &key key (test nil test-given) (test-not nil test-not-given))
  (when (and test-given test-not-given)
    (error 'both-test-and-test-not-given :name 'subsetp))
  (let ((use-hash (> (* (length list1) (length list2)) 1000)))
    (if key
        (if test-given
            (cond ((or (eq test #'eq) (eq test 'eq))
                   (if use-hash
                       (|subsetp key=other test=eq hash| 'subsetp list1 list2 key)
                       (|subsetp key=other test=eq| 'subsetp list1 list2 key)))
                  ((or (eq test #'eql) (eq test 'eql))
                   (if use-hash
                       (|subsetp key=other test=eql hash| 'subsetp list1 list2 key)
                       (|subsetp key=other test=eql| 'subsetp list1 list2 key)))
                  ((or (eq test #'equal) (eq test 'equal))
                   (if use-hash
                       (|subsetp key=other test=equal hash| 'subsetp list1 list2 key)
                       (|subsetp key=other test=other| 'subsetp list1 list2 key #'equal)))
                  ((or (eq test #'equalp) (eq test 'equalp))
                   (if use-hash
                       (|subsetp key=other test=equalp hash| 'subsetp list1 list2 key)
                       (|subsetp key=other test=other| 'subsetp list1 list2 key #'equalp)))
                  (t
                   (|subsetp key=other test=other| 'subsetp list1 list2 key test)))
            (if test-not-given
                (|subsetp key=other test-not=other| 'subsetp list1 list2 key test-not)
                (if use-hash
                    (|subsetp key=other test=eql hash| 'subsetp list1 list2 key)
                    (|subsetp key=other test=eql| 'subsetp list1 list2 key))))
        (if test-given
            (cond ((or (eq test #'eq) (eq test 'eq))
                   (if use-hash
                       (|subsetp key=identity test=eq hash| 'subsetp list1 list2)
                       (|subsetp key=identity test=eq| 'subsetp list1 list2)))
                  ((or (eq test #'eql) (eq test 'eql))
                   (if use-hash
                       (|subsetp key=identity test=eql hash| 'subsetp list1 list2)
                       (|subsetp key=identity test=eql| 'subsetp list1 list2)))
                  ((or (eq test #'equal) (eq test 'equal))
                   (if use-hash
                       (|subsetp key=identity test=equal hash| 'subsetp list1 list2)
                       (|subsetp key=identity test=other| 'subsetp list1 list2 #'equal)))
                  ((or (eq test #'equalp) (eq test 'equalp))
                   (if use-hash
                       (|subsetp key=identity test=equalp hash| 'subsetp list1 list2)
                       (|subsetp key=identity test=other| 'subsetp list1 list2 #'equalp)))
                  (t
                   (|subsetp key=identity test=other| 'subsetp list1 list2 test)))
            (if test-not-given
                (|subsetp key=identity test-not=other| 'subsetp list1 list2 test-not)
                (if use-hash
                    (|subsetp key=identity test=eql hash| 'subsetp list1 list2)
                    (|subsetp key=identity test=eql| 'subsetp list1 list2)))))))
