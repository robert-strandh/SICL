(cl:in-package #:sicl-cons)

;;; The compiler macro for mapcar generates code to loop individually
;;; over each list given, and thus avoids having a list of lists
;;; (which implies consing).  Since the number of lists given is
;;; known, we can use funcall instead of apply when we call the
;;; function.

(define-compiler-macro mapcar (&whole form function &rest lists)
  (if (null lists)
      form
      (let ((funvar (gensym))
            (listvars (loop for var in lists collect (gensym)))
            (vars (loop for var in lists collect (gensym))))
        `(loop with ,funvar = ,function
               ,@(apply #'append (loop for listvar in listvars
                                       for list in lists
                                       collect `(with ,listvar = ,list)))
               ,@(apply #'append (loop for var in vars
                                       for listvar in listvars
                                       collect `(for ,var = ,listvar then (cdr ,var))))
               ,@(apply #'append (loop for var in vars
                                       collect `(until (atom ,var))))
               collect (funcall ,funvar ,@(loop for var in vars
                                                collect `(car ,var)))
               finally (progn ,@(loop for var in vars
                                      for listvar in listvars
                                      collect `(unless (listp ,var)
                                                 (error 'must-be-proper-list
                                                        :datum ,listvar
                                                        :name 'mapcar))))))))
