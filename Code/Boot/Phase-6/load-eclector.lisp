(cl:in-package #:sicl-boot-phase-6)

(defun load-eclector (e5)
  (load-source-file "Array/make-array-defun.lisp" e5)
  ;; Eclector calls MAKE-ARRAY to create a vector, and then it calls
  ;; LENGTH to determine its length.  But at this point, LENGTH is the
  ;; host function by that name, so it won't work on SICL vectors.
  ;; For that reason, we temporarily redefine LENGTH here so that it
  ;; works both on lists and SICL vectors.
  (setf (env:fdefinition (env:client e5) e5 'length)
        (lambda (sequence)
          (if (listp sequence)
              (length sequence)
              (first (funcall (env:fdefinition (env:client e5) e5 'array-dimensions)
                              sequence)))))
  ;; Eclector uses EVAL in some compiler macros to evaluate some
  ;; Boolean arguments, but it is applied only to contstants
  ;; so we can use the host EVAL.
  (import-functions-from-host '(eval) e5)
  (let ((*features* '(:sicl)))
    (load-asdf-system '#:eclector e5)))
