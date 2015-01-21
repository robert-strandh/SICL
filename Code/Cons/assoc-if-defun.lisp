(cl:in-package #:sicl-cons)

(defun |assoc-if key=other=identity| (predicate alist)
  (with-alist-elements (element alist assoc-if)
    (when (funcall predicate (car element))
      (return element))))

(defun |assoc-if key=other| (predicate alist key)
  (with-alist-elements (element alist assoc-if)
    (when (funcall predicate (funcall key (car element)))
      (return element))))

(defun assoc-if (predicate alist &key key)
  (if key
      (|assoc-if key=other| predicate alist key)
      (|assoc-if key=other=identity| predicate alist)))
