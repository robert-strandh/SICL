(cl:in-package #:sicl-cons)

(defun |rassoc key=identity test=eq| (item alist)
  (with-alist-elements (element alist rassoc)
    (when (eq item (cdr element))
      (return element))))

(defun |rassoc key=identity test=eql| (item alist)
  (with-alist-elements (element alist rassoc)
    (when (eql item (cdr element))
      (return element))))
        
(defun |rassoc key=other test=eq| (item alist key)
  (with-alist-elements (element alist rassoc)
    (when (eq item (funcall key (cdr element)))
      (return element))))
  
(defun |rassoc key=other test=eql| (item alist key)
  (with-alist-elements (element alist rassoc)
    (when (eql item (funcall key (cdr element)))
      (return element))))

(defun |rassoc key=identity test=other| (item alist test)
  (with-alist-elements (element alist rassoc)
    (when (funcall test item (cdr element))
      (return element))))

(defun |rassoc key=other-test-other| (item alist test key)
  (with-alist-elements (element alist rassoc)
    (when (funcall test item (funcall key (cdr element)))
      (return element))))
  
(defun |rassoc key=identity test-not=eq| (item alist)
  (with-alist-elements (element alist rassoc)
    (when (not (eq item (cdr element)))
      (return element))))

(defun |rassoc key=identity test-not=eql| (item alist)
  (with-alist-elements (element alist rassoc)
    (when (not (eql item (cdr element)))
      (return element))))
        
(defun |rassoc key=other test-not=eq| (item alist key)
  (with-alist-elements (element alist rassoc)
    (when (not (eq item (funcall key (cdr element))))
      (return element))))
  
(defun |rassoc key=other test-not=eql| (item alist key)
  (with-alist-elements (element alist rassoc)
    (when (not (eql item (funcall key (cdr element))))
      (return element))))

(defun |rassoc key=identity test-not=other| (item alist test)
  (with-alist-elements (element alist rassoc)
    (when (not (funcall test item (cdr element)))
      (return element))))

(defun |rassoc key=other test-not=other| (item alist test key)
  (with-alist-elements (element alist rassoc)
    (when (not (funcall test item (funcall key (cdr element))))
      (return element))))
  
(defun rassoc
    (item alist &key key (test nil test-given) (test-not nil test-not-given))
  (when (and test-given test-not-given)
    (error 'both-test-and-test-not-given :name 'rassoc))
  (if key
      (if test-given
          (if (or (eq test #'eq) (eq test 'eq))
              (|rassoc key=other test=eq| item alist key)
              (if (or (eq test #'eql) (eq test 'eql))
                  (|rassoc key=other test=eql| item alist key)
                  (|rassoc key=other-test-other| item alist test key)))
          (if test-not-given
              (if (or (eq test-not #'eq) (eq test-not 'eq))
                  (|rassoc key=other test-not=eq| item alist key)
                  (if (or (eq test-not #'eql) (eq test-not 'eql))
                      (|rassoc key=other test-not=eql| item alist key)
                      (|rassoc key=other test-not=other| item alist test-not key)))
              (|rassoc key=other test=eql| item alist key)))
      (if test-given
          (if (or (eq test #'eq) (eq test 'eq))
              (|rassoc key=identity test=eq| item alist)
              (if (or (eq test #'eql) (eq test 'eql))
                  (|rassoc key=identity test=eql| item alist)
                  (|rassoc key=identity test=other| item alist test)))
          (if test-not-given
              (if (or (eq test-not #'eq) (eq test-not 'eq))
                  (|rassoc key=identity test-not=eq| item alist)
                  (if (or (eq test-not #'eql) (eq test-not 'eql))
                      (|rassoc key=identity test-not=eql| item alist)
                      (|rassoc key=identity test-not=other| item alist test-not)))
              (|rassoc key=identity test=eql| item alist)))))
