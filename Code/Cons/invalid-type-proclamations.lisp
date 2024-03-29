(cl:in-package #:sicl-cons)

;;; The idea behind most of the type proclamations here was to link
;;; the types of the arguments to the types of the return values for
;;; several different cases.  But I am now convinced that this idea
;;; can not be expressed as valid Common Lisp code.  So I want to keep
;;; the code here for later, and express it using a formalism that is
;;; different from that of type proclamations.

(declaim (ftype (or
                 (function (list) t)
                 (function ((not list)) nil))
                endp))

(declaim (ftype (or
                 (function () null)
                 (function (t &rest t) cons))
                list))

(declaim (ftype (function (&rest t) t)
                append))

(declaim (ftype (or
                 (function (null) null)
                 (function (cons) t))
                car cdr first rest))

(declaim (ftype (function (t) (member t nil))
                atom consp null listp))

(deftype cxrn (operations)
  (loop with result = 'null
        for operation in (cdr operations)
        do (setf result
                 (if (eq operation 'a)
                     `(or null (cons ,result))
                     `(or null (cons t ,result))))
        finally (return result)))

(deftype cxrt (operations)
  (loop with result = 'cons
        for operation in (cdr operations)
        do (setf result
                 (if (eq operation 'a)
                     `(cons ,result)
                     `(cons t ,result)))
        finally (return result)))

(declaim (ftype (or
                 (function ((cxrn x d)) null)
                 (function ((cxrt x d)) t))
                cadr cddr second))

(declaim (ftype (or
                 (function ((or null (cons null))) null)
                 (function ((cons cons)) t))
                caar cdar))

(declaim (ftype (or
                 (function ((cxrn x d d)) null)
                 (function ((cxrt x d d)) t))
                caddr cdddr third))

(declaim (ftype (or
                 (function ((cxrn x a d)) null)
                 (function ((cxrt x a d)) t))
                caadr cdadr))

(declaim (ftype (or
                 (function ((cxrn (x d a))) null)
                 (function ((cxrt (x d a))) t))
                cadar cddar))

(declaim (ftype (or
                 (function ((cxrn (x a a))) null)
                 (function ((cxrt (x a a))) t))
                caaar cdaar))

(declaim (ftype (or
                 (function ((cxrn (x d d d))) null)
                 (function ((cxrt (x d d d))) t))
                cadddr cddddr))

(declaim (ftype (or
                 (function ((cxrn (x a d d))) null)
                 (function ((cxrt (x a d d))) t))
                caaddr cdaddr))

(declaim (ftype (or
                 (function ((cxrn (x d a d))) null)
                 (function ((cxrt (x d a d))) t))
                cadadr cddadr))

(declaim (ftype (or
                 (function ((cxrn (x a a d))) null)
                 (function ((cxrt (x a a d))) t))
                caaadr cdaadr))

(declaim (ftype (or
                 (function ((cxrn (x d d a))) null)
                 (function ((cxrt (x d d a))) t))
                caddar cdddar))

(declaim (ftype (or
                 (function ((cxrn (x a d a))) null)
                 (function ((cxrt (x a d a))) t))
                caadar cdadar))

(declaim (ftype (or
                 (function ((cxrn (x d a a))) null)
                 (function ((cxrt (x d a a))) t))
                cadaar cddaar))

(declaim (ftype (or
                 (function ((cxrn (x a a a))) null)
                 (function ((cxrt (x a a a))) t))
                caaaar cdaaar))

(declaim (ftype (or
                 (function ((cxrn (x d d d))) null)
                 (function ((cxrt (x d d d))) t))
                fourth))

(declaim (ftype (or
                 (function ((cxrn (x d d d d))) null)
                 (function ((cxrt (x d d d d))) t))
                fifth))

(declaim (ftype (or
                 (function ((cxrn (x d d d d d))) null)
                 (function ((cxrt (x d d d d d))) t))
                sixth))

(declaim (ftype (or
                 (function ((cxrn (x d d d d d d))) null)
                 (function ((cxrt (x d d d d d d))) t))
                seventh))

(declaim (ftype (or
                 (function ((cxrn (x d d d d d d d))) null)
                 (function ((cxrt (x d d d d d d d))) t))
                eighth))

(declaim (ftype (or
                 (function ((cxrn (x d d d d d d d d))) null)
                 (function ((cxrt (x d d d d d d d d))) t))
                ninth))

(declaim (ftype (or
                 (function ((cxrn (x d d d d d d d d d))) null)
                 (function ((cxrt (x d d d d d d d d d))) t))
                tenth))

(declaim (ftype (or
                 (function (list) t)
                 (function ((not list)) nil))
                endp))

(declaim (ftype (function (t t) cons)
                cons))

(declaim (ftype (function (t list
                             &key
                             (:key (or symbol function))
                             (:test (or symbol function))
                             (:test-no (or symbol function)))
                          list)
                member))

(declaim (ftype (function (t cons) t)
                (setf car) (setf cdr)))

(declaim (ftype (function (t (cxrt (x d))) t)
                (setf cadr) (setf cddr)))

(declaim (ftype (function ((integer 0) list) t) nth))

(declaim (ftype (function (function list &rest list) list)
                mapc mapcar mapcan mapl maplist mapcon))
