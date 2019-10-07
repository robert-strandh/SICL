(cl:in-package #:sicl-cons)

(defun |union key=identity test=eql| (name list1 list2)
  (let ((result list2))
    (with-proper-list-elements (element1 list1 name)
      (unless (|member test=eql key=identity| name element1 list2)
        (push element1 result)))
    result))

(defun |union key=identity test=eq| (name list1 list2)
  (let ((result list2))
    (with-proper-list-elements (element1 list1 name)
      (unless (|member test=eq key=identity| name element1 list2)
        (push element1 result)))
    result))

(defun |union key=other test=eql| (name list1 list2 key)
  (let ((result list2))
    (with-proper-list-elements (element1 list1 name)
      (unless (|member test=eql key=other| name (funcall key element1) list2 key)
        (push element1 result)))
    result))

(defun |union key=other test=eq| (name list1 list2 key)
  (let ((result list2))
    (with-proper-list-elements (element1 list1 name)
      (unless (|member test=eq key=other| name (funcall key element1) list2 key)
        (push element1 result)))
    result))

(defun |union key=identity test=other| (name list1 list2 test)
  (let ((result list2))
    (with-proper-list-elements (element1 list1 name)
      (unless (|member test=other key=identity| name element1 list2 test)
        (push element1 result)))
    result))

(defun |union key=other test=other| (name list1 list2 key test)
  (let ((result list2))
    (with-proper-list-elements (element1 list1 name)
      (unless (|member test=other key=other| name (funcall key element1 ) list2 test key)
        (push element1 result)))
    result))

(defun |union key=identity test-not=other| (name list1 list2 test-not)
  (let ((result list2))
    (with-proper-list-elements (element1 list1 name)
      (unless (|member test-not=other key=identity| name element1 list2 test-not)
        (push element1 result)))
    result))

(defun |union key=other test-not=other| (name list1 list2 key test-not)
  (let ((result list2))
    (with-proper-list-elements (element1 list1 name)
      (unless (|member test-not=other key=other| name element1 list2 test-not key)
        (push element1 result)))
    result))

(defun |union key=identity test=eq hash| (name list1 list2)
  (let ((table (make-hash-table :test #'eq)))
    (with-proper-list-elements (element list1 name)
      (setf (gethash element table) element))
    (with-proper-list-elements (element list2 name)
      (setf (gethash element table) element))
    (loop for element being the hash-values of table
          collect element)))

(defun |union key=identity test=eql hash| (name list1 list2)
  (let ((table (make-hash-table :test #'eql)))
    (with-proper-list-elements (element list1 name)
      (setf (gethash element table) element))
    (with-proper-list-elements (element list2 name)
      (setf (gethash element table) element))
    (loop for element being the hash-values of table
          collect element)))

(defun |union key=identity test=equal hash| (name list1 list2)
  (let ((table (make-hash-table :test #'equal)))
    (with-proper-list-elements (element list1 name)
      (setf (gethash element table) element))
    (with-proper-list-elements (element list2 name)
      (setf (gethash element table) element))
    (loop for element being the hash-values of table
          collect element)))

(defun |union key=identity test=equalp hash| (name list1 list2)
  (let ((table (make-hash-table :test #'equalp)))
    (with-proper-list-elements (element list1 name)
      (setf (gethash element table) element))
    (with-proper-list-elements (element list2 name)
      (setf (gethash element table) element))
    (loop for element being the hash-values of table
          collect element)))

(defun |union key=other test=eq hash| (name list1 list2 key)
  (let ((table (make-hash-table :test #'eq)))
    (with-proper-list-elements (element list1 name)
      (setf (gethash (funcall key element) table) element))
    (with-proper-list-elements (element list2 name)
      (setf (gethash (funcall key element) table) element))
    (loop for element being the hash-values of table
          collect element)))

(defun |union key=other test=eql hash| (name list1 list2 key)
  (let ((table (make-hash-table :test #'eql)))
    (with-proper-list-elements (element list1 name)
      (setf (gethash (funcall key element) table) element))
    (with-proper-list-elements (element list2 name)
      (setf (gethash (funcall key element) table) element))
    (loop for element being the hash-values of table
          collect element)))

(defun |union key=other test=equal hash| (name list1 list2 key)
  (let ((table (make-hash-table :test #'equal)))
    (with-proper-list-elements (element list1 name)
      (setf (gethash (funcall key element) table) element))
    (with-proper-list-elements (element list2 name)
      (setf (gethash (funcall key element) table) element))
    (loop for element being the hash-values of table
          collect element)))

(defun |union key=other test=equalp hash| (name list1 list2 key)
  (let ((table (make-hash-table :test #'equalp)))
    (with-proper-list-elements (element list1 name)
      (setf (gethash (funcall key element) table) element))
    (with-proper-list-elements (element list2 name)
      (setf (gethash (funcall key element) table) element))
    (loop for element being the hash-values of table
          collect element)))

(defun union (list1 list2
              &key key (test nil test-given) (test-not nil test-not-given))
  (when (and test-given test-not-given)
    (error 'both-test-and-test-not-given :name 'union))
  (let ((use-hash (> (* (length list1) (length list2)) 1000)))
    (if key
        (if test-given
            (cond ((or (eq test #'eq) (eq test 'eq))
                   (if use-hash
                       (|union key=other test=eq hash| 'union list1 list2 key)
                       (|union key=other test=eq| 'union list1 list2 key)))
                  ((or (eq test #'eql) (eq test 'eql))
                   (if use-hash
                       (|union key=other test=eql hash| 'union list1 list2 key)
                       (|union key=other test=eql| 'union list1 list2 key)))
                  ((or (eq test #'equal) (eq test 'equal))
                   (if use-hash
                       (|union key=other test=equal hash| 'union list1 list2 key)
                       (|union key=other test=other| 'union list1 list2 key #'equal)))
                  ((or (eq test #'equalp) (eq test 'equalp))
                   (if use-hash
                       (|union key=other test=equalp hash| 'union list1 list2 key)
                       (|union key=other test=other| 'union list1 list2 key #'equalp)))
                  (t
                   (|union key=other test=other| 'union list1 list2 key test)))
            (if test-not-given
                (|union key=other test-not=other| 'union list1 list2 key test-not)
                (if use-hash
                    (|union key=other test=eql hash| 'union list1 list2 key)
                    (|union key=other test=eql| 'union list1 list2 key))))
        (if test-given
            (cond ((or (eq test #'eq) (eq test 'eq))
                   (if use-hash
                       (|union key=identity test=eq hash| 'union list1 list2)
                       (|union key=identity test=eq| 'union list1 list2)))
                  ((or (eq test #'eql) (eq test 'eql))
                   (if use-hash
                       (|union key=identity test=eql hash| 'union list1 list2)
                       (|union key=identity test=eql| 'union list1 list2)))
                  ((or (eq test #'equal) (eq test 'equal))
                   (if use-hash
                       (|union key=identity test=equal hash| 'union list1 list2)
                       (|union key=identity test=other| 'union list1 list2 #'equal)))
                  ((or (eq test #'equalp) (eq test 'equalp))
                   (if use-hash
                       (|union key=identity test=equalp hash| 'union list1 list2)
                       (|union key=identity test=other| 'union list1 list2 #'equalp)))
                  (t
                   (|union key=identity test=other| 'union list1 list2 test)))
            (if test-not-given
                (|union key=identity test-not=other| 'union list1 list2 test-not)
                (if use-hash
                    (|union key=identity test=eql hash| 'union list1 list2)
                    (|union key=identity test=eql| 'union list1 list2)))))))
