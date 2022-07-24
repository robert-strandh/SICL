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

(declaim (ftype (or
                 (function ((or null
                                (cons t
                                      (or null
                                          (cons null)))))
                           null)
                 (function ((cons t (cons cons)))
                           t))
                caadr cdadr))

(declaim (ftype (or
                 (function ((or null
                                (cons (or null
                                          (cons t null)))))
                           null)
                 (function ((cons (cons t cons)))
                           t))
                cadar cddar))

(declaim (ftype (or
                 (function ((or null
                                (cons (or null
                                          (cons null)))))
                           null)
                 (function ((cons (cons cons)))
                           t))
                caaar cdaar))

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
