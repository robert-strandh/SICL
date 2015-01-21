(cl:in-package #:sicl-cons)

(defun |rassoc-if key=other=identity| (predicate alist)
  (with-alist-elements (element alist rassoc-if)
    (when (funcall predicate (cdr element))
      (return element))))

(defun |rassoc-if key=other| (predicate alist key)
  (with-alist-elements (element alist rassoc-if)
    (when (funcall predicate (funcall key (cdr element)))
      (return element))))

(defun rassoc-if (predicate alist &key key)
  (if key
      (|rassoc-if key=other| predicate alist key)
      (|rassoc-if key=other=identity| predicate alist)))
