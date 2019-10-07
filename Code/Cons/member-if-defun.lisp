(cl:in-package #:sicl-cons)

(defun |member-if key=identity| (predicate list)
  (with-proper-list-rests (rest list 'member-if)
    (when (funcall predicate (car rest))
      (return rest))))

(defun |member-if key=other| (predicate list key)
  (with-proper-list-rests (rest list 'member-if)
    (when (funcall predicate (funcall key (car rest)))
      (return rest))))

(defun member-if (predicate list &key key)
  (if key
      (|member-if key=other| predicate list key)
      (|member-if key=identity| predicate list)))
