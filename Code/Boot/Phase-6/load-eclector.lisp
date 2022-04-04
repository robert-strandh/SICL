(cl:in-package #:sicl-boot-phase-6)

(defun load-eclector (e5)
  (load-source-file "Array/make-array-defun.lisp" e5)
  (load-source-file "Symbol/make-symbol-defun.lisp" e5)
  (setf (env:special-operator (env:client e5) e5 'sicl-primop:rack)
        '(:special-operator t))
  (setf (env:special-operator (env:client e5) e5 'sicl-primop:set-rack)
        '(:special-operator t))
  ;; Eclector calls ALEXANDRIA:SYMBOLICATE, but we can't use that
  ;; version right now, because it creates a SICL string and then
  ;; tries to call the host REPLACE with that string as the first
  ;; argument.  So we define it here, just for Eclector:
  (setf (env:fdefinition
         (env:client e5) e5 (intern (symbol-name '#:symbolicate) '#:alexandria))
        (lambda (s1 s2 s3)
          (intern (concatenate 'string (string s1) (string s2) (string s3)))))
  (load-source-file "Array/adjust-array-defun.lisp" e5)
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
  (let ((*features* '(:sicl :eclector-define-cl-variables)))
    (ensure-asdf-system '#:eclector e5)))
