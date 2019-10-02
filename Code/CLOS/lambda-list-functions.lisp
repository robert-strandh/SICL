(cl:in-package #:sicl-clos)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Lambda list functions.
;;;
;;; FIXME: check syntax of lambda list (but not of specializers) in
;;; both these functions according to the MOP.

(defun extract-lambda-list (specialized-lambda-list)
  (loop for rest = specialized-lambda-list then (cdr rest)
        until (or (atom rest)
                  (member (car rest) lambda-list-keywords))
        collect (if (consp (car rest))
                    (caar rest)
                    (car rest))
          into required
        finally (return (append required rest))))

(defun extract-specializer-names (specialized-lambda-list)
  (loop for rest = specialized-lambda-list then (cdr rest)
        until (or (atom rest)
                  (member (car rest) lambda-list-keywords))
        collect (if (consp (car rest))
                    (cadar rest)
                    t)))
