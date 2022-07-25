(cl:in-package #:sicl-cons)

(declaim (ftype (function (t) t)
                car cdr caar cadr cdar cddr
                caaar caadr cadar caddr cdaar cdadr cddar cdddr
                caaaar caaadr caadar caaddr cadaar cadadr caddar cadddr
                cdaaar cdaadr cdadar cdaddr cddaar cddadr cdddar cddddr
                first second third fourth fifth
                sixth seventh eighth ninth tenth
                rest))

(declaim (ftype (function (t) t)
                endp))

(declaim (ftype (function (&rest t) list)
                list list*))

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

(declaim (ftype (function (t (cxrt (x d))) t)
                (setf cadr) (setf cddr)))

(declaim (ftype (function ((integer 0) list) t) nth))

(declaim (ftype (function (function list &rest list) list)
                mapc mapcar mapcan mapl maplist mapcon))

(declaim (ftype (function (t list) (or cons null))
                assoc))
