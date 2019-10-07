(cl:in-package #:sicl-cons)

;;; Special version for n = 1.  This version avoids that the list be
;;; traversed several times, and since list traversal dominates the
;;; time here, this is a win.

(defun nbutlast-1 (list)
  (unless (typep list 'list)
    (error 'must-be-list
           :datum list
           :name 'nbutlast))
  (if (atom (cdr list))
      nil
      ;; The compiler probably isn't smart enough to eliminate common
      ;; subexpressions such as (cdr x), so we do it manually by only
      ;; doing a single cdr in each iteration, and by storing pointers
      ;; to the result in local variables.
      (let ((a list)
            (b (cdr list))
            (c (cddr list)))
        (loop for slow on list
              until (atom c)
              do (setf a b
                       b c
                       c (cdr c))
              until (atom c)
              do (setf a b
                       b c
                       c (cdr c))
              do (when (eq slow c)
                   ;; we have a circular list
                   (error 'must-be-proper-or-dotted-list
                          :datum list
                          :name 'nbutlast)))
        (setf (cdr a) nil)
        list)))

(defun nbutlast (list &optional (n 1))
  (unless (typep list 'list)
    (error 'must-be-list
           :datum list
           :name 'nbutlast))
  (unless (typep n '(integer 0))
    (error 'must-be-nonnegative-integer
           :datum n
           :name 'nbutlast))
  (if (= n 1)
      (nbutlast-1 list)
      (let ((length (loop for slow on list
                          with conses = list
                          until (atom conses)
                          count t
                          do (setf conses (cdr conses))
                          until (atom conses)
                          count t
                          do (setf conses (cdr conses))
                          do (when (eq slow conses)
                               ;; we have a circular list
                               (error 'must-be-proper-or-dotted-list
                                      :datum list
                                      :name 'nbutlast)))))
        (if (<= length n)
            nil
            (progn (setf (cdr (nthcdr (- length (1+ n)) list)) nil)
                   list)))))
