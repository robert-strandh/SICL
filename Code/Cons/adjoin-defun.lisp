(cl:in-package #:sicl-cons)

(defun |adjoin key=identity test=eq| (name item list)
  (if (|member test=eq key=identity| name item list)
      list
      (cons item list)))

(defun |adjoin key=identity test=eql| (name item list)
  (if (|member test=eql key=identity| name item list)
      list
      (cons item list)))

(defun |adjoin key=identity test=other| (name item list test)
  (if (|member test=other key=identity| name item list test)
      list
      (cons item list)))

(defun |adjoin key=identity test-not=other| (name item list test-not)
  (if (|member test-not=other key=identity| name item list test-not)
      list
      (cons item list)))

(defun |adjoin key=other test=eq| (name item list key)
  (if (|member test=eq key=other| name (funcall key item) list key)
      list
      (cons item list)))

(defun |adjoin key=other test=eql| (name item list key)
  (if (|member test=eql key=other| name (funcall key item) list key)
      list
      (cons item list)))

(defun |adjoin key=other test=other| (name item list key test)
  (if (|member test=other key=other| name (funcall key item) list test key)
      list
      (cons item list)))

(defun |adjoin key=other test-not=other| (name item list key test-not)
  (if (|member test-not=other key=other| name (funcall key item) list test-not key)
      list
      (cons item list)))

(defun adjoin (item list 
               &key key (test nil test-given) (test-not nil test-not-given))
  (when (and test-given test-not-given)
    (error 'both-test-and-test-not-given :name 'adjoin))
  (if key
      (if test-given
          (if (or (eq test #'eq) (eq test 'eq))
              (|adjoin key=other test=eq| 'adjoin item list key)
              (if (or (eq test #'eql) (eq test 'eql))
                  (|adjoin key=other test=eql| 'adjoin item list key)
                  (|adjoin key=other test=other| 'adjoin item list key test)))
          (if test-not-given
              (|adjoin key=other test-not=other| 'adjoin item list key test-not)
              (|adjoin key=other test=eql| 'adjoin item list key)))
      (if test-given
          (if (or (eq test #'eq) (eq test 'eq))
              (|adjoin key=identity test=eq| 'adjoin item list)
              (if (or (eq test #'eql) (eq test 'eql))
                  (|adjoin key=identity test=eql| 'adjoin item list)
                  (|adjoin key=identity test=other| 'adjoin item list test)))
          (if test-not-given
              (|adjoin key=identity test-not=other| 'adjoin item list test-not)
              (|adjoin key=identity test=eql| 'adjoin item list)))))
