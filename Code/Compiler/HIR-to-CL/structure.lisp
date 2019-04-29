;;; Suppose we have the following original code:

(let* ((x 10) (y 20)) (+ x y 30))

;;; At the HIR level, it will generate two nested functions, defined
;;; by ENTER-INSTRUCTIONs.  Let us call the outermost one f1 and the
;;; innermost one f2.
;;; 
;;; In addition, we add a "wrapper" function with a single parameter,
;;; namely a function with single parameter.  Let us call that
;;; parameter FUNCTION-CELL-IN-ENVIRONMENT.  The wrapper function is
;;; called in order to "tie" the code to a particular global
;;; environment.  When the wrapper function is called, the argument it
;;; receives is a function with a single parameter, namely a function
;;; name.  It returns a CONS cell where the CAR is the function with
;;; that name in the environment into which this code will be loaded.
;;;
;;; So for the example, we have three nested functions the wrapper, f1
;;; nested inside the wrapper, and f2 nested inside f1.  The wrapper
;;; function must enclose f1 and then call it.  And f1 must enclose f2
;;; and call it.
;;;
;;; The question is: If we translate this HIR code to Common Lisp code
;;; for execution in a host, how does that Common Lisp code look?
;;;
;;; I am skipping details about assignments, multiple values, etc.

(lambda (function-cell-in-environment)
  (let* ((enclose-function (car (funcall function-cell-in-environment...)))
         (make-cell-function (car (funcall function-cell-in-environment...)))
         (fetch-function (car (funcall function-cell-in-environment...)))
         (static-env-function (car (funcall function-cell-in-environment...)))
         (f2 (lambda (args static-env dynamic-env)
               (let ((x (car (funcall fetch-function static-env 0)))
                     (y (first args)))
                 ...))) ; (+ x y 30)
         (f1 (lambda (args static-env dynamic-env)
               (let* ((cell (funcall make-cell-function))
                      ;; Create a closure with f2 as the entry point
                      ;; and CELL as the only element of the static
                      ;; environment.  The closure is a
                      ;; funcallable-standard-object in the host.
                      (c2 (funcall enclose-function f2 cell)))
                 ;; Put X in the cell.
                 (rplaca cell (first args))
                 (set-funcallable-instance-function
                  c2
                  (lambda (&rest args)
                    (funcall f2 args (funcall static-env-function c2))))
                 ...)))) ; (funcall c2 20)
    (let (c1 (funcall enclose-function f1))
      (set-funcallable-instance-function
       c1
       (lambda (&rest args)
         (funcall f1 args (funcall static-env-function c1))))
      ...))) ; (funcall c1 10)
