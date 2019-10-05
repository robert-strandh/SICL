(cl:in-package #:sicl-cons)

(defun copy-list (list)
  (unless (typep list 'list)
    (error 'must-be-list
           :datum list
           :name 'copy-list))
  (if (null list)
      nil
      (let* ((result (list (car list)))
             (trailer result))
        (loop do (setf list (cdr list))
              until (atom list)
              ;; list is not an atom, allocate a new cell
              ;; at the end of the result list
              do (setf (cdr trailer) (list (car list)))
              do (setf trailer (cdr trailer)))
        ;; when we come here, list is an atom,
        ;; either NIL because the list was a proper list
        ;; or some other atom because it was not a proper list
        (setf (cdr trailer) list)
        result)))
