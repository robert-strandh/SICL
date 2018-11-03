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

;;; In this version of the default action, we use the compiler to
;;; produce the effective method so that the effective method
;;; contains only straight-line code and no loops.
;;;
;;; For a version that does not use the compiler, see the file
;;; compute-effective-method-support-b.lisp

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
;;; than a function.  This function should therefore be redefined to
;;; return a form, and that form should be compiled by the caller of
;;; COMPUTE-EFFECTIVE-METHOD.

(defun compute-effective-method-default (methods)
  (let ((primary-methods (remove-if-not #'primary-method-p methods))
        (before-methods (remove-if-not #'before-method-p methods))
        (after-methods (remove-if-not  #'after-method-p methods))
        (around-methods (remove-if-not  #'around-method-p methods)))
    (if (null primary-methods)
        (compile nil '(lambda (args)
                       (declare (ignore args))
                       (error 'option-or-method-must-be-non-empty-list
                        :expression methods)))
        (let ((primary-chain
                `(funcall ,(method-function (car primary-methods))
                          args
                          '(,@(loop for method in (cdr primary-methods)
                                    collect (method-function method)))))
              (before-chain
                (loop for method in before-methods
                      collect `(funcall ,(method-function method)
                                        args
                                        '())))
              (after-chain
                (loop for method in (reverse after-methods)
                      collect `(funcall ,(method-function method)
                                        args
                                        '()))))
          (compile
           nil
           (if (null around-methods)
               `(lambda (args)
                  ,@before-chain
                  (multiple-value-prog1
                      ,primary-chain
                    ,@after-chain))
               `(lambda (args)
                  (funcall ,(method-function (car around-methods))
                           args
                           (list ,@(loop for method in (cdr around-methods)
                                         collect (method-function method))
                                 (lambda (args next-methods)
                                   (declare (ignore next-methods))
                                   ,@before-chain
                                   (multiple-value-prog1
                                       ,primary-chain
                                     ,@after-chain)))))))))))
