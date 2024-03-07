;;;; src/conditions.lisp

(in-package #:portable-condition-system)

;;; Case assertions - common

(defun case-failure (datum complex-type operator-name keys)
  (error 'case-failure :datum datum
                       :expected-type `(,complex-type ,@keys)
                       :name operator-name
                       :possibilities keys))

(defun case-transform-t-otherwise-cases (cases)
  (loop for (key . forms) in cases
        if (member key '(t otherwise))
          collect `((,key) ,@forms)
        else collect
        `(,key ,@forms)))

(defun case-accumulate-keys (cases)
  (loop for case in cases
        for key-or-keys = (first case)
        if (listp key-or-keys)
          append key-or-keys
        else
          collect key-or-keys))

;;; Correctable assertions - utilities

(defun store-value-read-evaluated-form ()
  (format *query-io* "~&;; Type a form to be evaluated:~%")
  (list (eval (read *query-io*))))

(defmacro with-store-value-restart ((temp-var place tag) &body forms)
  (let ((report-var (gensym "STORE-VALUE-REPORT"))
        (new-value-var (gensym "NEW-VALUE"))
        (form-or-forms (if (= 1 (length forms)) (first forms) `(progn ,@forms))))
    `(flet ((,report-var (stream)
              (format stream "Supply a new value of ~S." ',place)))
       (restart-case ,form-or-forms
         (store-value (,new-value-var)
           :report ,report-var
           :interactive store-value-read-evaluated-form
           (setf ,temp-var ,new-value-var
                 ,place ,new-value-var)
           (go ,tag))))))
