(cl:in-package #:sicl-cons)

(defun |assoc-if-not key=identity| (predicate alist)
  (with-alist-elements (element alist assoc-if-not)
    (when (not (funcall predicate (car element)))
      (return element))))

(defun |assoc-if-not key=other| (predicate alist key)
  (with-alist-elements (element alist assoc-if-not)
    (when (not (funcall predicate (funcall key (car element))))
      (return element))))

(defun assoc-if-not (predicate alist &key key)
  (if key
      (|assoc-if-not key=other| predicate alist key)
      (|assoc-if-not key=identity| predicate alist)))
