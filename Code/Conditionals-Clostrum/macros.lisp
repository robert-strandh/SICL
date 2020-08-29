(cl:in-package #:sicl-conditionals)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Macro OR.

(defmacro or (&rest forms)
  (or-expander forms))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Macro AND.

(defmacro and (&rest forms)
  (and-expander forms))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Macro WHEN.

(defmacro when (test &body body)
  `(if ,test
       (progn ,@body)
       nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Macro UNLESS.

(defmacro unless (test &body body)
  `(if ,test
       nil
       (progn ,@body)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Macro COND.

(defmacro cond (&rest clauses)
  (cond-expander clauses))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Macros CASE, ECASE, CCASE.

(defmacro case (keyform &rest clauses)
  (case-expander keyform clauses))

(defmacro ecase (keyform &rest clauses)
  (ecase-expander keyform clauses))

(defmacro ccase (keyplace &rest clauses &environment environment)
  (let* ((global-environment (sicl-environment:global-environment environment))
         (client (sicl-environment:client global-environment)))
    (ccase-expander client environment keyplace clauses)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Macros TYPECASE, ETYPECASE, CTYPECASE.

(defmacro typecase (keyform &rest clauses)
  (typecase-expander keyform clauses))

(defmacro etypecase (keyform &rest clauses)
  (etypecase-expander keyform clauses))

(defmacro ctypecase (keyplace &rest clauses &environment environment)
  (let* ((global-environment (sicl-environment:global-environment environment))
         (client (sicl-environment:client global-environment)))
    (ctypecase-expander client environment keyplace clauses)))
