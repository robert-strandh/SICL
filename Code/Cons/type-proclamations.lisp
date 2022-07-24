(cl:in-package #:sicl-cons)

(declaim (ftype (or
                 (function (null) null)
                 (function (cons) t))
                car cdr first rest))

(declaim (ftype (or
                 (function ((or null (cons t null))) null)
                 (function ((cons t cons)) t))
                cadr cddr second))

(declaim (ftype (or
                 (function ((or null (cons null))) null)
                 (function ((cons cons)) t))
                caar cdar))

(declaim (ftype (or
                 (function ((or null
                                (cons t
                                      (or null
                                          (cons t null)))))
                           null)
                 (function ((cons t (cons t cons)))
                           t))
                caddr cdddr third))

(deftype cxrn (operations)
  (if (= (length operations) 1)
      'null
      (ecase (car (last operations))
        (a `(or null (cons (cxrn ,(butlast operations)) *)))
        (d `(or null (cons * (cxrn ,(butlast operations))))))))

(deftype cxrt (operations)
  (if (= (length operations) 1)
      'cons
      (ecase (car (last operations))
        (a `(cons (cxrt ,(butlast operations)) *))
        (d `(cons * (cxrt ,(butlast operations)))))))

(declaim (ftype (or
                 (function ((cxrn (x a d))) null)
                 (function ((cxrt (x a d))) t))
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

(declaim (ftype (or
                 (function () null)
                 (function (t &rest t) cons))
                list))

(declaim (ftype (function (&rest t) t)
                append))

(declaim (ftype (function (t) (member t nil))
                atom consp null listp))

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

(declaim (ftype (function (t (cons t (cons t))) t)
                (setf cadr) (setf cddr)))

(declaim (ftype (function ((integer 0) list) t) nth))

(declaim (ftype (function (function list &rest list) list)
                mapc mapcar mapcan mapl maplist mapcon))
