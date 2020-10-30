(cl:in-package #:sicl-clos)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; A few definitions:
;;;
;;; A METHOD FUNCTION is a function that represents a single method
;;; metaobject of a generic function.  By default, when the generic
;;; function is an instance of STANDARD-GENERIC-FUNCTION and the
;;; method is an instance of STANDARD-METHOD, then the method function
;;; takes two arguments: a list of all the arguments to the generic
;;; function and a list of NEXT METHODS that the method may call using
;;; CALL-NEXT-METHOD.  These next methods are METHOD METAOBJECTS, so
;;; that calling them involves using the generic function
;;; METHOD-FUNCTION to get the method function of the method
;;; metaobject and use FUNCALL or APPLY to call it.
;;;
;;; A METHOD LAMBDA is a lambda expression that must be converted into
;;; a METHOD FUNCTION.  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; MAKE-METHOD-LAMBDA.

;;; This generic function takes four arguments: a generic function, a
;;; method, a lambda expression, and an environment.  We are told from
;;; the specification that "The generic function and method the method
;;; function will be used with are not required to be the given
;;; ones. Moreover, the method metaobject may be uninitialized."  This
;;; all means that MAKE-METHOD-LAMBDA must do its job without
;;; inspecting those arguments, and using only their classes to
;;; determine what to do.  Presumably, the method argument will just
;;; be a prototype instance of some class.
;;;
;;; The third argument, the lambda expression, is the lambda
;;; expression resulting from some minor transformations of the
;;; DEFMETHOD form.  In other words, MAKE-METHOD-LAMBDA must transform
;;; the unspecialized lambda list and body of an invocation of
;;; DEFMETHOD to a lambda expression.
;;;
;;; We are also told that the result of a call to MAKE-METHOD-LAMBDA
;;; must be converted to a function an passed as the :function
;;; initialization argument to MAKE-INSTANCE when an instance of the
;;; method metaobject is created.  This means that we can not use any
;;; information about the generic function (other than its class) to
;;; which the method will eventually belong.  
;;;
;;; This function returns two values, the first is a lambda expression
;;; and the second a list of initialization arguments and values.  As
;;; indicated above, the lambda expression must be converted to a
;;; function.  The initialization arguments and values are also passed
;;; to MAKE-INSTANCE when the method metaobject is created. 

(defun make-method-lambda-default
    (generic-function method lambda-expression environment)
  (declare (ignore generic-function method environment))
  (let ((args (gensym))
        (next-methods (gensym)))
    (values `(lambda (,args ,next-methods)
               (flet ((next-method-p ()
                        (not (null ,next-methods)))
                      (call-next-method (&rest args)
                        (when (null ,next-methods)
                          ;; FIXME: do this better.
                          (error "no next method"))
                        (funcall (method-function (car ,next-methods))
                                 (or args ,args)
                                 (cdr ,next-methods))))
                 (declare (ignorable #'next-method-p #'call-next-method))
                 (apply ,lambda-expression
                        ,args)))
            '())))
