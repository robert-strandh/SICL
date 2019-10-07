(cl:in-package #:sicl-cons)

(defun |member-if-not key=identity| (predicate list)
  (with-proper-list-rests (rest list 'member-if-not)
    (unless (funcall predicate (car rest))
      (return rest))))

(defun |member-if-not key=other| (predicate list key)
  (with-proper-list-rests (rest list 'member-if-not)
    (unless (funcall predicate (funcall key (car rest)))
      (return rest))))

(defun member-if-not (predicate list &key key)
  (if key
      (|member-if-not key=other| predicate list key)
      (|member-if-not key=identity| predicate list)))
