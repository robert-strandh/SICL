(cl:in-package #:sicl-clos)

;;;; This file contains the support code for the generic function
;;;; COMPUTE-EFFECTIVE-METHOD.
;;;;
;;;; In this file, there are no definitions of generic functions, nor
;;;; of any methods.  

;;;; This file contains an ordinary function that, given a list of
;;;; methods, computes an effective method using the standard method
;;;; combination.  This function is named
;;;; COMPUTE-EFFECTIVE-METHOD-DEFAULT.

;;; For the specification of this generic function, see
;;; http://metamodular.com/CLOS-MOP/compute-effective-method.html
;;;
;;; The specification includes a single method, specialized only for
;;; STANDARD-GENERIC-FUNCTION, independently of the method
;;; combination.  The default actions below is valid for
;;; STANDARD-GENERIC-FUNCTION and the STANDARD method combination. 

;;; In this version of COMPUTE-EFFECTIVE-METHOD-DEFAULT, we do not
;;; use the compiler to produce the effective method.  As a
;;; consequence, the effective method will contain loops that iterate
;;; over the different methods in each role. 
;;;
;;; For a version that DOES use the compiler, see the file
;;; compute-effective-method-support-a.lisp

;;; Notice that when the list of primary methods is empty, we do not
;;; signal an error.  Instead, we generate an effective method that
;;; signals an error.  The reason for doing it this way is that we
;;; might call COMPUTE-EFFECTIVE-METHOD in a situation other than when
;;; the generic function was invoked.  When COMPUTE-EFFECTIVE-METHOD
;;; is called as a result of an invocation of a generic function, it
;;; does not matter much whether we signal an error immediately, or
;;; generate an effective method that signals an error; the error will
;;; be signaled either way.  However, we sometimes employ a trick that
;;; we call SATIATING a generic function.  Satiating a generic
;;; function means simulating calls to it in order to fill up the
;;; cache and to create a discriminating function that does not have
;;; to call the full dispatch machinery very often.  For some
;;; simulated calls, it might be the case that there are only
;;; non-primary methods applicable.  But whether that is the case
;;; depends on the method combination of the generic function, so we
;;; can not make any assumptions about it; we can only call
;;; COMPUTE-EFFECTIVE-METHOD and use what it returns.  We certainly do
;;; not want to have an error signaled whenever there is a
;;; hypothetical future invocation of a generic function that might
;;; not have a primary method, so we delay the error signaling until
;;; this invocation actually happens.

;;; FIXME: The AMOP says that an effective method is a form, rather
;;; than a function.  This function should therefore be renamed to
;;; (say) COMPUTE-EFFECTIVE-METHOD-FUNCTION-DEFAULT, so that it is
;;; clear that it does not return an effective method.  But we do need
;;; this function in the initial image of the system, because the
;;; initial image does not have an evaluator, so we can not call
;;; COMPUTE-EFFECTIVE-METHOD to get a form and then convert that form
;;; to a function.

(defun compute-effective-method-default (methods)
  (let ((primary-methods (remove-if-not #'primary-method-p methods))
        (before-methods (remove-if-not #'before-method-p methods))
        (after-methods (reverse (remove-if-not  #'after-method-p methods)))
        (around-methods (remove-if-not  #'around-method-p methods)))
    (if (null primary-methods)
        (lambda (args)
          (declare (ignore args))
          (error 'option-or-method-must-be-non-empty-list
                 :expression methods))
        (let ((primary-chain
                (lambda (args)
                  (funcall (method-function (car primary-methods))
                           args
                           (mapcar #'method-function (cdr primary-methods)))))
              (before-chain
                (lambda (args)
                  (loop for method in before-methods
                        do (funcall (method-function method) args '()))))
              (after-chain
                (lambda (args)
                  (loop for method in after-methods
                        do (funcall (method-function method) args '())))))
          (lambda (args)
            (if (null around-methods)
                (progn (funcall before-chain args)
                       (multiple-value-prog1
                           (funcall primary-chain args)
                         (funcall after-chain args)))
                (funcall (method-function (car around-methods))
                         args
                         (append (mapcar #'method-function (cdr around-methods))
                                 (list (lambda (args next-methods)
                                         (declare (ignore next-methods))
                                         (funcall before-chain args)
                                         (multiple-value-prog1
                                             (funcall primary-chain args)
                                           (funcall after-chain args))))))))))))
