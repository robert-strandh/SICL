(cl:in-package #:sicl-boot-phase-6)

(defun load-eclector (e5)
  (load-source-file "Array/make-array.lisp" e5)
  ;; Eclector uses EVAL in some compiler macros to evaluate some
  ;; Boolean arguments, but it is applied only to contstants
  ;; so we can use the host EVAL.
  (import-functions-from-host '(eval) e5)
  (let ((*features* '(:sicl)))
    (load-asdf-system '#:eclector e5)))
