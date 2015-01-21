(cl:in-package #:sicl-cons)

(defun |rassoc-if-not key=identity| (predicate alist)
  (with-alist-elements (element alist rassoc-if-not)
    (when (not (funcall predicate (cdr element)))
      (return element))))

(defun |rassoc-if-not key=other| (predicate alist key)
  (with-alist-elements (element alist rassoc-if-not)
    (when (not (funcall predicate (funcall key (cdr element))))
      (return element))))

(defun rassoc-if-not (predicate alist &key key)
  (if key
      (|rassoc-if-not key=other| predicate alist key)
      (|rassoc-if-not key=identity| predicate alist)))
