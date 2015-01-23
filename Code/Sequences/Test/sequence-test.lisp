(in-package #:sicl-sequence-test)

(defclass test ()
  ((%name :initarg :name :reader name)
   (%form :initarg :form :reader form)
   (%results :initarg :results :reader results)))

(defparameter *tests* '())

(defmacro deftest (name form &rest results)
  `(push (make-instance 'test
	   :name ',name
	   :form ',form
	   :results ',results)
	 *tests*))

(defmacro signals-error (form error-type)
  `(handler-case (eval ,form)
     (,error-type () t)
     (condition () nil)
     (:no-error (&rest values) (declare (ignore values)) nil)))

(defmacro expand-in-current-env (macro-form &environment env)
  (macroexpand macro-form env))

(defun run-test (test)
  (assert (equal (multiple-value-list (eval (form test)))
		 (results test))))

(defun sequence-test ()
  (mapc #'run-test *tests*)
  nil)
