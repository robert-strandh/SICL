(cl:in-package #:sicl-boot)

;;; When we are asked to compile the name of a global function, by
;;; default Cleavir generates an FDEFINITION-AST taking the function
;;; name as an input.  For SICL, we do not want that.  Instead we want
;;; it to generate an access to the CAR of the global function cell
;;; that contains the function.  And we want the function cell to be
;;; accessed at load time.
(defmethod cleavir-cst-to-ast:convert-global-function-reference
    (cst (info cleavir-env:global-function-info) (env environment) system)
  (declare (ignore system))
  (cleavir-ast:make-car-ast
   (cleavir-ast:make-load-time-value-ast
    `(sicl-genv:function-cell
      ',(cleavir-env:name info)
      (sicl-genv:global-environment))
    ;; The cell is not read-only.
    nil
    :origin (cst:source cst))))

;;; FIXME: This one should be removed.
(defmethod cleavir-generate-ast:convert-global-function
    ((info cleavir-env:global-function-info) (env environment) system)
  (declare (ignore system))
  (cleavir-ast:make-car-ast
   (cleavir-ast:make-load-time-value-ast
    `(sicl-genv:function-cell
      ',(cleavir-env:name info)
      (sicl-genv:global-environment))
    ;; The cell is not read-only.
    nil)))

;;; When we are asked to compile the name of a special variable, by
;;; default Cleavir generates a SYMBOL-VALUE-AST taking the variable
;;; name as an input.  For SICL, we do not want that.  Instead we want
;;; it to generate a call to the function
;;; SICL-EXTRINSIC-ENVIRONMENT:SYMBOL-VALUE, passing it the symbol
;;; naming the variable and the GLOBAL ENVIRONMENT.
;;;
;;; The global environment is used by the function
;;; SICL-EXTRINSIC-ENVIRONMENT:SYMBOL-VALUE to retrieve the value
;;; representing UNBOUND in that environment, and to retrieve the
;;; global value cell in case the variable is not bound in the dynamic
;;; run-time environment.
(defmethod cleavir-cst-to-ast:convert-special-variable
    (cst (info cleavir-env:special-variable-info) (env environment) system)
  (declare (ignore system))
  (cleavir-ast:make-call-ast
   (cleavir-ast:make-car-ast
    (cleavir-ast:make-load-time-value-ast
     '(sicl-genv:function-cell
       'sicl-minimal-extrinsic-environment:symbol-value
       (sicl-genv:global-environment))
     nil
     :origin (cst:source cst))
    :origin (cst:source cst))
   (list (cleavir-ast:make-load-time-value-ast
	  `',(cleavir-env:name info)
	  t
          :origin (cst:source cst))
	 (cleavir-ast:make-load-time-value-ast
	  'sicl-genv:*global-environment*
	  t))
   :origin (cst:source cst)))

;;; FIXME: this one should be removed
(defmethod cleavir-generate-ast:convert-special-variable
    ((info cleavir-env:special-variable-info) (env environment) system)
  (declare (ignore system))
  (cleavir-ast:make-call-ast
   (cleavir-ast:make-car-ast
    (cleavir-ast:make-load-time-value-ast
     '(sicl-genv:function-cell
       'sicl-minimal-extrinsic-environment:symbol-value
       (sicl-genv:global-environment))
     nil))
   (list (cleavir-ast:make-load-time-value-ast
	  `',(cleavir-env:name info)
	  t)
	 (cleavir-ast:make-load-time-value-ast
	  'sicl-genv:*global-environment*
	  t))))

;;; When we are asked to compile an assignment to a special variable,
;;; by default Cleavir generates a SET-SYMBOL-VALUE-AST taking the
;;; variable name and the value as an input.  For SICL, we do not want
;;; that.  Instead we want it to generate a call to (SETF
;;; SICL-EXTRINSIC-ENVIRONMENT:SYMBOL-VALUE), passing it the new
;;; value, the symbol naming the variable, and the environment.
(defmethod cleavir-cst-to-ast:convert-setq-special-variable
    (var-cst
     form-ast
     (info cleavir-env:special-variable-info)
     (env environment)
     system)
  (declare (ignore system))
  (cleavir-ast:make-call-ast
   (cleavir-ast:make-car-ast
    (cleavir-ast:make-load-time-value-ast
     '(sicl-genv:function-cell
       '(setf sicl-minimal-extrinsic-environment:symbol-value)
       sicl-genv:*global-environment*)
     nil
     :origin (cst:source var-cst))
    :origin (cst:source var-cst))
   (list form-ast
	 (cleavir-ast:make-load-time-value-ast
	  `',(cleavir-env:name info)
	  t
          :origin (cst:source var-cst))
	 (cleavir-ast:make-load-time-value-ast
	  'sicl-genv:*global-environment*
	  nil))
   :origin (cst:source var-cst)))

(defmethod cleavir-generate-ast:convert-setq-special-variable
    ((info cleavir-env:special-variable-info)
     var
     form-ast
     (env environment)
     system)
  (declare (ignore var system))
  (cleavir-ast:make-call-ast
   (cleavir-ast:make-car-ast
    (cleavir-ast:make-load-time-value-ast
     '(sicl-genv:function-cell
       '(setf sicl-minimal-extrinsic-environment:symbol-value)
       sicl-genv:*global-environment*)
     nil))
   (list form-ast
	 (cleavir-ast:make-load-time-value-ast
	  `',(cleavir-env:name info)
	  t)
	 (cleavir-ast:make-load-time-value-ast
	  'sicl-genv:*global-environment*
	  nil))))

(defclass environment (sicl-minimal-extrinsic-environment:environment)
  ())

(defmethod initialize-instance :after ((object environment) &key)
  (import-functions-from-host
   '(format
     error
     cleavir-code-utilities:proper-list-p)
   object)
  (setf (sicl-genv:special-variable '*trace-output* object t) *trace-output*)
  (import-package-from-host 'sicl-clos object)
  (import-package-from-host 'sicl-cons object)
  (import-package-from-host 'sicl-method-combination object)
  (import-package-from-host 'sicl-package object)
  (import-package-from-host 'sicl-symbol object)
  (import-package-from-host 'sicl-sequence object)
  (import-package-from-host 'sicl-arithmetic object))

(defclass boot ()
  ((%e0 :initarg :e0 :accessor e0)
   (%e1 :initarg :e1 :accessor e1)
   (%e2 :initarg :e2 :accessor e2)
   (%e3 :initarg :e3 :accessor e3)
   (%e4 :initarg :e4 :accessor e4)
   (%e5 :initarg :e5 :accessor e5)
   (%e6 :initarg :e6 :accessor e6)))

(defclass boot-client () ())

(defun boot ()
  (let* ((client (make-instance 'boot-client))
         (boot
           (let ((sicl-minimal-extrinsic-environment::*cache-p* t))
             (make-instance 'boot
               :e0 (make-instance 'environment :system client)
               :e1 (make-instance 'environment :system client)
               :e2 (make-instance 'environment :system client)
               :e3 (make-instance 'environment :system client)
               :e4 (make-instance 'environment :system client)
               :e5 (make-instance 'environment :system client)
               :e6 (make-instance 'environment :system client)))))
    (sicl-boot-phase-0:boot-phase-0 boot)
    (sicl-boot-phase-1:boot-phase-1 boot)
    (sicl-boot-phase-2:boot-phase-2 boot)
    (sicl-boot-phase-3:boot-phase-3 boot)
    (sicl-boot-phase-4:boot-phase-4 boot)
    (sicl-boot-phase-5:boot-phase-5 boot)
    ;; (sicl-boot-phase-6:boot-phase-6 boot)
    boot))
