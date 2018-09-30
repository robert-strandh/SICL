(cl:in-package #:cleavir-load-time-value-hoisting-test)

;;; In this file, we define an example client for the load-time-value
;;; hoisting machinery.  We use this client for testing, but it is also an
;;; illustration of how to design a custom client.
;;;
(defvar *environment*
  (make-instance 'sicl-extrinsic-environment:environment))

(defclass client ()
  ((%environment
    :initarg :environment
    :reader environment)))

(defmethod cleavir-load-time-value-hoisting:hir-from-form
    (form (client client))
  (cleavir-ast-to-hir:ast-to-hir
   (cleavir-generate-ast:generate-ast
    form
    (environment client)
    client)))

(defmethod cleavir-load-time-value-hoisting:make-load-form-using-client
    (object (client client))
  (funcall
   (sicl-genv:fdefinition 'cl:make-load-form (environment client))
   object
   (environment client)))

(defmethod cleavir-load-time-value-hoisting:make-load-form-using-client
    ((symbol symbol) (client client))
  42)

(defmethod cleavir-load-time-value-hoisting:make-load-form-using-client
    ((string string) (client client))
  1337)

(defmethod cleavir-load-time-value-hoisting:simplify-datum
    ((constant-input cleavir-ir:constant-input) (client client))
  (flet ((change-to-immediate-input (value)
           (change-class constant-input 'cleavir-ir:immediate-input
             :value value)))
    (let ((value (cleavir-ir:value constant-input)))
      (typecase value
        (fixnum
         (change-to-immediate-input (ash value 2)))
        (character
         (change-to-immediate-input (+ (ash (char-int value) 2) 2)))
        (t nil)))))

;;; This client uses a SICL extrinsic environment for compilation.  In this
;;; environment, references to functions and variables are replaced by
;;; something like
;;;
;;; (car (load-time-value (sicl-genv:function-cell 'NAME environment)))
;;;
;;; Now we have a problem, because we cannot use sicl-genv:function-cell to
;;; obtain its own function cell.

(defparameter *car-lexical-location*
  (cleavir-ir:make-lexical-location 'car))

(defparameter *function-cell-lexical-location*
  (cleavir-ir:make-lexical-location 'sicl-genv:function-cell))

(defparameter *global-environment-lexical-location*
  (cleavir-ir:make-lexical-location 'sicl-genv:function-cell))

(defun make-lexical-location-thunk (lexical-location)
  (cleavir-ir:make-enter-instruction
   '()
   :successor
   (cleavir-ir:make-return-instruction
    (list lexical-location))))

(defmethod cleavir-load-time-value-hoisting:scan-hir :around
    ((enter-instruction cleavir-ir:enter-instruction) client)
  (let ((cleavir-ir:*policy* (cleavir-ir:policy enter-instruction)))
    (call-next-method)))

(defmethod cleavir-load-time-value-hoisting:hir-from-form :around
    (form (client client))
  (cond
    ;; FIXME: This cascade of equal tests is neither fast, nor pretty.  But
    ;; it will do for testing.
    ((equal form '(sicl-global-environment:function-cell
                   'car
                   (sicl-global-environment:global-environment)))
     (make-lexical-location-thunk *car-lexical-location*))
    ((equal form '(sicl-global-environment:function-cell
                   'sicl-global-environment:function-cell
                   (sicl-global-environment:global-environment)))
     (make-lexical-location-thunk *function-cell-lexical-location*))
    ((equal form '(sicl-global-environment:function-cell
                   'sicl-global-environment:global-environment
                   (sicl-global-environment:global-environment)))
     (make-lexical-location-thunk *global-environment-lexical-location*))
    (t
     (call-next-method))))
