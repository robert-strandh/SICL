(cl:in-package #:sicl-cons)

;;; The HyperSpec doesn't specify what happens if n is 0.  There is a
;;; note that says:
;;;
;;;     (butlast list n) ==  (ldiff list (last list n))
;;;
;;; but notes are not normative.  If you do believe this note, then
;;; (butlast '(1 2 3 . 4) 0) should return (1 2 3), i.e., it doesn't
;;; return the unmodified list.
;;;
;;; This implementation works according to the HyperSpec note when n
;;; is 0.

(defun butlast (list &optional (n 1))
  (unless (typep list 'list)
    (error 'must-be-list
           :datum list
           :name 'butlast))
  (unless (typep n '(integer 0))
    (error 'must-be-nonnegative-integer
           :datum n
           :name 'butlast))
  (let ((remaining list))
    (loop repeat n
          until (atom remaining)
          do (setf remaining (cdr remaining)))
    (loop for slow on remaining
          until (atom remaining)
          do (setf remaining (cdr remaining))
          collect (prog1 (car list) (setf list (cdr list)))
          until (atom remaining)
          do (setf remaining (cdr remaining))
          collect (prog1 (car list) (setf list (cdr list)))
          do (when (eq slow remaining)
               ;; we have a circular list
               (error 'must-be-proper-or-dotted-list
                      :datum list
                      :name 'butlast)))))

;;; There is probably no point in making a special version of butlast
;;; for n = 1, because the time is going to be dominated by consing up
;;; the new list anyawy.
