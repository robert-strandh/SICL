(cl:in-package #:sicl-arithmetic)

(define-compiler-macro + (&rest args)
  (cond ((null args) 0)
        ;; FIXME: check that we have a number
        ((null (cdr args)) (car args))
        (t `(binary-add ,(car args) (+ ,@(cdr args))))))

;;; FIXME: do this better by not having a required argument
;;; and instead reporting a compilation error when no
;;; arguments are given
(define-compiler-macro - (x &rest args)
  (cond ((null args) `(negate ,x))
        ((null (cdr args)) `(binary-subtract ,x ,(car args)))
        (t `(binary-subtract ,x (+ ,@args)))))

(define-compiler-macro * (&rest args)
  (cond ((null args) 1)
        ;; FIXME: check that we have a number
        ((null (cdr args)) (car args))
        (t `(* (binary-multiply ,(car args) ,(cadr args)) ,@(cddr args)))))

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
