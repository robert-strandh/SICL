(cl:in-package #:sicl-cons)

(deftype cxrn (operations)
  (if (= (length operations) 1)
      'null
      (ecase (car (last (operations)))
        (a `(or null (cons (cxrn ,(butlast operations)) *)))
        (d `(or null (cons * (cxrn ,(butlast operations))))))))

(deftype cxrt (operations)
  (if (= (length operations) 1)
      'cons
      (ecase (car (last (operations)))
        (a `(cons (cxrt ,(butlast operations)) *))
        (d `(cons * (cxrt ,(butlast operations)))))))

(proclaim '(ftype (or
                   (function ((cxrn (x))) null)
                   (function ((cxrt (x))) t))
            car cdr))

(proclaim '(ftype (or
                   (function ((cxrn (x d))) null)
                   (function ((cxrt (x d))) t))
            cadr cddr))

(proclaim '(ftype (or
                   (function ((cxrn (x a))) null)
                   (function ((cxrt (x a))) t))
            caar cdar))

(proclaim '(ftype (or
                   (function ((cxrn (x d d))) null)
                   (function ((cxrt (x d d))) t))
            caddr cdddr))

(proclaim '(ftype (or
                   (function ((cxrn (x a d))) null)
                   (function ((cxrt (x a d))) t))
            caadr cdadr))

(proclaim '(ftype (or
                   (function ((cxrn (x d a))) null)
                   (function ((cxrt (x d a))) t))
            cadar cddar))

(proclaim '(ftype (or
                   (function ((cxrn (x a a))) null)
                   (function ((cxrt (x a a))) t))
            caaar cdaar))

(proclaim '(ftype (or
                   (function ((cxrn (x d d d))) null)
                   (function ((cxrt (x d d d))) t))
            cadddr cddddr))

(proclaim '(ftype (or
                   (function ((cxrn (x a d d))) null)
                   (function ((cxrt (x a d d))) t))
            caaddr cdaddr))

(proclaim '(ftype (or
                   (function ((cxrn (x d a d))) null)
                   (function ((cxrt (x d a d))) t))
            cadadr cddadr))

(proclaim '(ftype (or
                   (function ((cxrn (x a a d))) null)
                   (function ((cxrt (x a a d))) t))
            caaadr cdaadr))

(proclaim '(ftype (or
                   (function ((cxrn (x d d a))) null)
                   (function ((cxrt (x d d a))) t))
            caddar cdddar))

(proclaim '(ftype (or
                   (function ((cxrn (x a d a))) null)
                   (function ((cxrt (x a d a))) t))
            caadar cdadar))

(proclaim '(ftype (or
                   (function ((cxrn (x d a a))) null)
                   (function ((cxrt (x d a a))) t))
            cadaar cddaar))

(proclaim '(ftype (or
                   (function ((cxrn (x a a a))) null)
                   (function ((cxrt (x a a a))) t))
            caaaar cdaaar))

(proclaim '(ftype (or
                   (function ((cxrn (x))) null)
                   (function ((cxrt (x))) t))
            first))

(proclaim '(ftype (or
                   (function ((cxrn (x d))) null)
                   (function ((cxrt (x d))) t))
            second))

(proclaim '(ftype (or
                   (function ((cxrn (x d d))) null)
                   (function ((cxrt (x d d))) t))
            third))

(proclaim '(ftype (or
                   (function ((cxrn (x d d d))) null)
                   (function ((cxrt (x d d d))) t))
            fourth))

(proclaim '(ftype (or
                   (function ((cxrn (x d d d d))) null)
                   (function ((cxrt (x d d d d))) t))
            fifth))

(proclaim '(ftype (or
                   (function ((cxrn (x d d d d d))) null)
                   (function ((cxrt (x d d d d d))) t))
            sixth))

(proclaim '(ftype (or
                   (function ((cxrn (x d d d d d d))) null)
                   (function ((cxrt (x d d d d d d))) t))
            seventh))

(proclaim '(ftype (or
                   (function ((cxrn (x d d d d d d d))) null)
                   (function ((cxrt (x d d d d d d d))) t))
            eighth))

(proclaim '(ftype (or
                   (function ((cxrn (x d d d d d d d d))) null)
                   (function ((cxrt (x d d d d d d d d))) t))
            ninth))

(proclaim '(ftype (or
                   (function ((cxrn (x d d d d d d d d d))) null)
                   (function ((cxrt (x d d d d d d d d d))) t))
            tenth))

(proclaim '(ftype (or
                   (function ((cxrn (x))) null)
                   (function ((cxrt (x))) t))
            rest))
