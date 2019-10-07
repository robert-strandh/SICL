(cl:in-package #:sicl-cons)

(defun |member test=eq key=identity| (name item list)
  (with-proper-list-rests (rest list name)
    (when (eq item (car rest))
      (return rest))))

(defun |member test=eq key=other| (name item list key)
  (with-proper-list-rests (rest list name)
    (when (eq item (funcall key (car rest)))
      (return rest))))

(defun |member test-not=eq key=identity| (name item list)
  (with-proper-list-rests (rest list name)
    (when (not (eq item (car rest)))
      (return rest))))

(defun |member test-not=eq key=other| (name item list key)
  (with-proper-list-rests (rest list name)
    (when (not (eq item (funcall key (car rest))))
      (return rest))))

(defun |member test=eql key=identity| (name item list)
  (with-proper-list-rests (rest list name)
    (when (eql item (car rest))
      (return rest))))

(defun |member test=eql key=other| (name item list key)
  (with-proper-list-rests (rest list name)
    (when (eql item (funcall key (car rest)))
      (return rest))))

(defun |member test-not=eql key=identity| (name item list)
  (with-proper-list-rests (rest list name)
    (when (not (eql item (car rest)))
      (return rest))))

(defun |member test-not=eql key=other| (name item list key)
  (with-proper-list-rests (rest list name)
    (when (not (eql item (funcall key (car rest))))
      (return rest))))

(defun |member test=other key=identity| (name item list test)
  (with-proper-list-rests (rest list name)
    (when (funcall test item (car rest))
      (return rest))))

(defun |member test=other key=other| (name item list test key)
  (with-proper-list-rests (rest list name)
    (when (funcall test item (funcall key (car rest)))
      (return rest))))

(defun |member test-not=other key=identity| (name item list test)
  (with-proper-list-rests (rest list name)
    (when (not (funcall test item (car rest)))
      (return rest))))

(defun |member test-not=other key=other| (name item list test key)
  (with-proper-list-rests (rest list name)
    (when (not (funcall test item (funcall key (car rest))))
      (return rest))))

(defun member (item list
               &key key (test nil test-given) (test-not nil test-not-given))
  (when (and test-given test-not-given)
    (error 'both-test-and-test-not-given :name 'member))
  (if key
      (if test-given
          (if (or (eq test #'eq) (eq test 'eq))
              (|member test=eq key=other| 'member item list key)
              (if (or (eq test #'eql) (eq test 'eql))
                  (|member test=eql key=other| 'member item list key)
                  (|member test=other key=other| 'member item list test key)))
          (if test-not-given
              (if (or (eq test-not #'eq) (eq test-not 'eq))
                  (|member test-not=eq key=other| 'member item list key)
                  (if (or (eq test-not #'eql) (eq test-not 'eql))
                      (|member test-not=eql key=other| 'member item list key)
                      (|member test-not=other key=other| 'member item list test-not key)))
              (|member test=eql key=other| 'member item list key)))
      (if test-given
          (if (or (eq test #'eq) (eq test 'eq))
              (|member test=eq key=identity| 'member item list)
              (if (or (eq test #'eql) (eq test 'eql))
                  (|member test=eql key=identity| 'member item list)
                  (|member test=other key=identity| 'member item list test)))
          (if test-not-given
              (if (or (eq test-not #'eq) (eq test-not 'eq))
                  (|member test-not=eq key=identity| 'member item list)
                  (if (or (eq test-not #'eql) (eq test-not 'eql))
                      (|member test-not=eql key=identity| 'member item list)
                      (|member test-not=other key=identity| 'member item list test-not)))
              (|member test=eql key=identity| 'member item list)))))

;;; Special versions of the member function with the arguments
;;; of the test reversed.

(defun |member reversed test=other key=identity| (name item list test)
  (with-proper-list-rests (rest list name)
    (when (funcall test (car rest) item)
      (return rest))))

(defun |member reversed test=other key=other| (name item list test key)
  (with-proper-list-rests (rest list name)
    (when (funcall test (funcall key (car rest)) item)
      (return rest))))

(defun |member reversed test-not=other key=identity| (name item list test)
  (with-proper-list-rests (rest list name)
    (when (not (funcall test (car rest) item))
      (return rest))))

(defun |member reversed test-not=other key=other| (name item list test key)
  (with-proper-list-rests (rest list name)
    (when (not (funcall test (funcall key (car rest)) item))
      (return rest))))
