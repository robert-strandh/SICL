(cl:in-package #:sicl-arithmetic)

(macrolet ((define-binary-reducer (name binary type default)
             `(define-compiler-macro ,name (&rest args)
                (cond ((null args) ,default)
                      ((null (cdr args)) (let ((sym (gensym)))
                                           `(let ((,sym ,(car args)))
                                              (check-type ,sym ,',type)
                                              ,sym)))
                      (t `(,',binary ,(car args) (,',name ,@(cdr args))))))))
  (define-binary-reducer + binary-add number 0)
  (define-binary-reducer * binary-multiply number 1)
  (define-binary-reducer gcd binary-gcd integer 0)
  (define-binary-reducer lcm binary-lcm integer 1)
  (define-binary-reducer logand binary-logand integer -1)
  (define-binary-reducer logior binary-logior integer 0)
  (define-binary-reducer logxor binary-logxor integer 0))

;;; FIXME: do this better by not having a required argument
;;; and instead reporting a compilation error when no
;;; arguments are given
(define-compiler-macro - (x &rest args)
  (cond ((null args) `(negate ,x))
        ((null (cdr args)) `(binary-subtract ,x ,(car args)))
        (t `(binary-subtract ,x (+ ,@args)))))

;;; FIXME: do this better by not having a required argument
;;; and instead reporting a compilation error when no
;;; arguments are given
(define-compiler-macro / (x &rest args)
  (cond ((null args) `(invert ,x))
        ((null (cdr args)) `(binary-divide ,x ,(car args)))
        (t `(/ (binary-divide ,x ,(car args)) ,@(cdr args)))))


(macrolet ((define-all-binary (name binary)
             `(define-compiler-macro ,name (x &rest args)
                (if (null args)
                    t
                    (loop for form in (cons x args)
                          for previous-var = nil then var
                          for var = (gensym)
                          collect `(,var ,form) into bindings
                          when previous-var collect `(,',binary ,previous-var ,var) into conditions
                          finally (return
                                    `(let ,bindings
                                       (and ,@conditions))))))))
  (define-all-binary < binary-less)
  (define-all-binary <= binary-not-greater)
  (define-all-binary > binary-greater)
  (define-all-binary >= binary-not-less)
  (define-all-binary = binary-equal))

(define-compiler-macro /= (&whole form &rest args)
  (if (or (null args) (> (length args) 5)) ; 5 is arbitrary; should perhaps be tuned
    form
    (loop for arg in args
          for var = (gensym)
          collect var into vars
          collect `(,var ,arg) into bindings
          finally (return
                    `(let ,bindings
                       (not (or
                              ,@(loop for x on vars
                                      append (loop for y on (cdr x)
                                                   collect `(binary-equal ,(car x) ,(car y)))))))))))
