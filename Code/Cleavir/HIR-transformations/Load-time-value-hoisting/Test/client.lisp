(cl:in-package #:cleavir-load-time-value-hoisting-test)

;;; In this file, we define an example client for the load-time-value
;;; hoisting machinery.  We use this client for testing, but it is also an
;;; illustration of how to design a custom client.

(defvar *environment*
  (make-instance 'sicl-extrinsic-environment:environment))

(defclass client ()
  ((%environment
    :initform *environment*
    :initarg :environment
    :reader environment)))

;;; This client uses a SICL extrinsic environment for compilation.  To
;;; avoid circular definitions, we must ensure that calls to
;;; SICL-GENV:FUNCTION-CELL of the symbols CL:INTERN, CL:MAKE-STRING and
;;; SICL-GENV:FUNCTION-CELL itself are handled specially.  We do this by
;;; detecting these cases in HIR-FROM-FROM and by returning a custom
;;; expansion for each such case.

(defvar *make-string*)

(defvar *cons*)

(defvar *find-package*)

(defvar *intern*)

(defvar *function-cell*)

;; Create a HIR thunk that, when called, will call the function stored in
;; LEXICAL-LOCATION with the given input data ARGUMENTS.
(defun hir-funcall (lexical-location &rest arguments)
  (let ((values-location (cleavir-ir:make-values-location)))
    (cleavir-ir:make-enter-instruction
     '()
     :successor
     (cleavir-ir:make-funcall-instruction
      (list* lexical-location arguments)
      (list values-location)
      (cleavir-ir:make-return-instruction
       (list values-location))))))

(defmethod cleavir-load-time-value-hoisting:hir-from-form :around
    (form (client client))
  (cond
    ((eq (first form) '%make-string)
     (apply #'hir-funcall
            *make-string*
            (loop for character in (rest form)
                  collect
                  (cleavir-ir:make-constant-input character))))
    ((eq (first form) '%cons)
     (hir-funcall *cons*))
    ((eq (first form) '%find-package)
     (hir-funcall
      *find-package*
      (cleavir-ir:make-constant-input (second form))))
    ((eq (first form) '%intern)
     (hir-funcall
      *intern*
      (cleavir-ir:make-constant-input (second form))
      (cleavir-ir:make-constant-input (third form))))
    ((eq (first form) 'sicl-genv:function-cell)
     (hir-funcall
      *function-cell*
      (cleavir-ir:make-constant-input (second (second form)))))
    (t
     (call-next-method))))

(defmethod cleavir-load-time-value-hoisting:hir-from-form
    (form (client client))
  (cleavir-ast-to-hir:compile-toplevel-unhoisted
   (cleavir-generate-ast:generate-ast
    `(lambda () ,form)
    (environment client)
    client)))

(defmethod cleavir-load-time-value-hoisting:make-load-form-using-client
    (object (client client))
  (funcall
   (sicl-genv:fdefinition 'cl:make-load-form (environment client))
   object
   (environment client)))

(defmethod cleavir-load-time-value-hoisting:make-load-form-using-client
    ((string string) (client client))
  (values
   `(%make-string
     ,@(loop for index below (length string)
             collect (row-major-aref string index)))))

(defmethod cleavir-load-time-value-hoisting:make-load-form-using-client
    ((package package) (client client))
  `(%find-package ,(package-name package)))

(defmethod cleavir-load-time-value-hoisting:make-load-form-using-client
    ((symbol symbol) (client client))
  `(%intern ,(symbol-name symbol)
            ,(symbol-package symbol)))

(defmethod cleavir-load-time-value-hoisting:make-load-form-using-client
    ((cons cons) (client client))
  (values
   `(%cons)
   `(setf (car ,cons) ',(car cons)
          (cdr ,cons) ',(cdr cons))))

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

(defmethod cleavir-load-time-value-hoisting:scan-hir :around
    ((enter-instruction cleavir-ir:enter-instruction) client)
  (let ((cleavir-ir:*policy* (cleavir-ir:policy enter-instruction)))
    (call-next-method)))

