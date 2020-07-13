;;;; src/conditions.lisp

(in-package #:portable-condition-system)

;;; HANDLER-BIND

(defmacro handler-bind (bindings &body forms)
  (flet ((make-binding (binding)
           (destructuring-bind (condition-type function) binding
             `(cons ',condition-type ,function))))
    (let ((cluster (mapcar #'make-binding bindings)))
      `(let ((*handler-clusters* (cons (list ,@cluster) *handler-clusters*)))
         ,@forms))))

;;; HANDLER-CASE - main macro body

(defmacro handler-case (form &rest cases)
  (let ((no-error-case-count (count :no-error cases :key #'car)))
    (case no-error-case-count
      (0 (make-handler-case-without-no-error-case form cases))
      (1 (make-handler-case-with-no-error-case form cases))
      (t (error "Multiple :NO-ERROR cases found in HANDLER-CASE.")))))

;;; IGNORE-ERRORS

(defmacro ignore-errors (&rest forms)
  `(handler-case (progn ,@forms)
     (error (condition) (values nil condition))))
