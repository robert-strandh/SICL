(in-package :sicl.documentation)

(defun fmt (&rest args)
  (apply #'format nil args))

(setf (documentation 'car 'function)
      (fmt "Lambda list: (OBJECT)~@
            When OBJECT is a CONS cell, return the CAR of that cell.~@
            When OBJECT is NIL, return NIL."))

(setf (documentation #'car 'function)
      (documentation 'car 'function))

(setf (documentation 'cdr 'function)
      (fmt "Lambda list: (OBJECT)~@
            When OBJECT is a CONS cell, return the CDR of that cell.~@
            When OBJECT is NIL, return NIL."))

(setf (documentation #'cdr 'function)
      (documentation 'cdr 'function))

(defmacro make-c*r-documentation (function-name)
  (let* ((name (symbol-name function-name))
         (letters (reverse (cdr (butlast (coerce name 'list))))))
    (flet ((primitive (letter)
             (if (eql letter #\A) 'car 'cdr)))
      `(progn (setf (documentation ',function-name 'function)
                    ,(fmt "Lambda list: (OBJECT)~@
                           Equivalent to ~a"
                          (loop with form = 'object
                                for letter in letters
                                do (setf form
                                         (list (primitive letter) form))
                                finally (return form))))
              (setf (documentation (fdefinition ',function-name) 'function)
                    (documentation ',function-name 'function))))))
             
(make-c*r-documentation caar)
(make-c*r-documentation cadr)
(make-c*r-documentation cdar)
(make-c*r-documentation cddr)
(make-c*r-documentation caaar)
(make-c*r-documentation caadr)
(make-c*r-documentation cadar)
(make-c*r-documentation caddr)
(make-c*r-documentation cdaar)
(make-c*r-documentation cdadr)
(make-c*r-documentation cddar)
(make-c*r-documentation cdddr)
(make-c*r-documentation caaaar)
(make-c*r-documentation caaadr)
(make-c*r-documentation caadar)
(make-c*r-documentation caaddr)
(make-c*r-documentation cadaar)
(make-c*r-documentation cadadr)
(make-c*r-documentation caddar)
(make-c*r-documentation cadddr)
(make-c*r-documentation cdaaar)
(make-c*r-documentation cdaadr)
(make-c*r-documentation cdadar)
(make-c*r-documentation cdaddr)
(make-c*r-documentation cddaar)
(make-c*r-documentation cddadr)
(make-c*r-documentation cdddar)
(make-c*r-documentation cddddr)

(setf (documentation 'list 'function)
      (fmt "Lambda list: (&rest OBJECTS)~@
            Return a list containing the objects in OBJECTS.~@"))

(setf (documentation #'list 'function)
      (documentation 'list 'function))

(setf (documentation 'list* 'function)
      (fmt "Lambda list: (&rest OBJECTS)~@
            At least one argument must be given.~@
            Return a list containing the objects in OBJECTS,~@
            except that the last object in OBJECTS becomes the~@
            CDR of the last CONS cell created.~@
            When given a single argument, retun that argument."))

(setf (documentation #'list* 'function)
      (documentation 'list* 'function))

(setf (documentation 'first 'function)
      (fmt "Lambda list: (LIST)~@
            Return the first element of the list LIST.~@
            When LIST is neither a list nor NIL,~@
            an error is signaled."))

(setf (documentation #'first 'function)
      (documentation 'first 'function))

(defmacro make-nth-documentation (function-name number)
  `(progn (setf (documentation ',function-name 'function)
                ,(fmt "Lambda list: (LIST)~@
                       Return the ~a element of the list LIST.~@
                       When LIST is a proper list with fewer than ~a element,~@
                       NIL is returned.~@
                       When LIST is not a proper list, and it has fewer than~@
                       ~a elements, an error is signaled.~@
                       In particular, when LIST is neither a list nor NIL,~@
                       an error is signaled."
                      (string-downcase (symbol-name function-name))
                      number
                      number))
          (setf (documentation (fdefinition ',function-name) 'function)
                (documentation ',function-name 'function))))

(make-nth-documentation 'second  "two")
(make-nth-documentation 'third   "three")
(make-nth-documentation 'fourth  "four")
(make-nth-documentation 'fifth   "five")
(make-nth-documentation 'sixth   "six")
(make-nth-documentation 'seventh "seven")
(make-nth-documentation 'eighth  "eight")
(make-nth-documentation 'ninth   "nine")
(make-nth-documentation 'tenth   "ten")
