(cl:in-package #:cleavir-code-utilities)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Separate an ordinary body such as a let or let* body that may
;;; contain declarations (but no documentation) into the declarations
;;; and the executable forms.
;;;
;;; If there are declarations after the first executable form (which
;;; is a syntax error), then those declarations will be considered
;;; part of the executable forms.

(defun separate-ordinary-body (body)
  (unless (proper-list-p body)
    (error 'ordinary-body-must-be-proper-list
           :body body))
  (let ((pos (position-if-not (lambda (item)
                                (and (consp item)
                                     (eq (car item) 'declare)))
                              body)))
    (if (null pos)
        (values body '())
        (values (subseq body 0 pos) (subseq body pos)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Separate a body such as a defun, flet, or lables that may contain
;;; both declarations and a documentation string into the
;;; declarations, the documentation, and the executable forms.
;;;
;;; Return three values.  The first value is a list of declarations.
;;; Each element of the list is a complete declaration, including the
;;; symbol DECLARE.  The second value is a the documentation as a
;;; string, or NIL if no documentation was found.  The last value is a
;;; list of forms.

(defun separate-function-body (body)
  (unless (proper-list-p body)
    (error 'function-body-must-be-proper-list
           :body body))
  (let ((declarations '())
        (documentation nil)
        (forms '()))
    (loop for (expr . rest) on body
          do (cond ((not (null forms))
                    (push expr forms))
                   ((and (consp expr) (eq (car expr) 'declare))
                    (push expr declarations))
                   ((stringp expr)
                    (if (or (null rest)
                            (not (null documentation))
                            (not (null forms)))
                        (push expr forms)
                        (setf documentation expr)))
                   (t
                    (push expr forms))))
    (values (nreverse declarations) documentation (nreverse forms))))
