(cl:in-package #:sicl-cons)

(defun |assoc key=identity test=eq| (item alist)
  (with-alist-elements (element alist assoc)
    (when (eq item (car element))
      (return element))))

(defun |assoc key=identity test=eql| (item alist)
  (with-alist-elements (element alist assoc)
    (when (eql item (car element))
      (return element))))
        
(defun |assoc key=other test=eq| (item alist key)
  (with-alist-elements (element alist assoc)
    (when (eq item (funcall key (car element)))
      (return element))))
  
(defun |assoc key=other test=eql| (item alist key)
  (with-alist-elements (element alist assoc)
    (when (eql item (funcall key (car element)))
      (return element))))

(defun |assoc key=identity test=other| (item alist test)
  (with-alist-elements (element alist assoc)
    (when (funcall test item (car element))
      (return element))))

(defun |assoc key=other-test-other| (item alist test key)
  (with-alist-elements (element alist assoc)
    (when (funcall test item (funcall key (car element)))
      (return element))))
  
(defun |assoc key=identity test-not=eq| (item alist)
  (with-alist-elements (element alist assoc)
    (when (not (eq item (car element)))
      (return element))))

(defun |assoc key=identity test-not=eql| (item alist)
  (with-alist-elements (element alist assoc)
    (when (not (eql item (car element)))
      (return element))))
        
(defun |assoc key=other test-not=eq| (item alist key)
  (with-alist-elements (element alist assoc)
    (when (not (eq item (funcall key (car element))))
      (return element))))
  
(defun |assoc key=other test-not=eql| (item alist key)
  (with-alist-elements (element alist assoc)
    (when (not (eql item (funcall key (car element))))
      (return element))))

(defun |assoc key=identity test-not=other| (item alist test)
  (with-alist-elements (element alist assoc)
    (when (not (funcall test item (car element)))
      (return element))))

(defun |assoc key=other test-not=other| (item alist test key)
  (with-alist-elements (element alist assoc)
    (when (not (funcall test item (funcall key (car element))))
      (return element))))
  
(defun assoc
    (item alist &key key (test nil test-given) (test-not nil test-not-given))
  (when (and test-given test-not-given)
    (error 'both-test-and-test-not-given :name 'assoc))
  (if key
      (if test-given
          (if (or (eq test #'eq) (eq test 'eq))
              (|assoc key=other test=eq| item alist key)
              (if (or (eq test #'eql) (eq test 'eql))
                  (|assoc key=other test=eql| item alist key)
                  (|assoc key=other-test-other| item alist test key)))
          (if test-not-given
              (if (or (eq test-not #'eq) (eq test-not 'eq))
                  (|assoc key=other test-not=eq| item alist key)
                  (if (or (eq test-not #'eql) (eq test-not 'eql))
                      (|assoc key=other test-not=eql| item alist key)
                      (|assoc key=other test-not=other| item alist test-not key)))
              (|assoc key=other test=eql| item alist key)))
      (if test-given
          (if (or (eq test #'eq) (eq test 'eq))
              (|assoc key=identity test=eq| item alist)
              (if (or (eq test #'eql) (eq test 'eql))
                  (|assoc key=identity test=eql| item alist)
                  (|assoc key=identity test=other| item alist test)))
          (if test-not-given
              (if (or (eq test-not #'eq) (eq test-not 'eq))
                  (|assoc key=identity test-not=eq| item alist)
                  (if (or (eq test-not #'eql) (eq test-not 'eql))
                      (|assoc key=identity test-not=eql| item alist)
                      (|assoc key=identity test-not=other| item alist test-not)))
              (|assoc key=identity test=eql| item alist)))))
