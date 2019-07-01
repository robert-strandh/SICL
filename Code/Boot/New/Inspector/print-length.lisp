(cl:in-package #:sicl-boot-inspector)

(defun circular-list-p (object)
  (loop with table = (make-hash-table :test #'eq)
        for remaining = object then (cdr remaining)
        when (atom remaining)
          return nil
        when (gethash remaining table)
          return t
        do (setf (gethash remaining table) t)))

;;; Return true if and only if the number of characters required to
;;; print OBJECT is known to be at most N.  We allow ourselves some
;;; approximation, in that we only consider certain types of objects,
;;; such as lists, symbols, and strings.
(defun print-length-at-most-p (object n)
  (cond ((not (plusp n))
         nil)
        ((or (stringp object)
             (symbolp object)
             (characterp object)
             (numberp object))
         (<= (print-length object) n))
        ((consp object)
         (and (not (circular-list-p object))
              (print-length-at-most-p (car object) (- n 2))
              (print-length-at-most-p
               (cdr object)
               (- n 2 (print-length (car object))))))
        (t
         nil)))

(defun print-length (object)
  (cond ((or (stringp object) (symbolp object) (characterp object))
         (length (format nil "~s" object)))
        ((consp object)
         (+ 2 (print-length (car object)) (print-length (cdr object))))
        (t
         nil)))

