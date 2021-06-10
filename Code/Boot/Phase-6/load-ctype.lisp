(cl:in-package #:sicl-boot-phase-6)

(defun load-ctype (e5)
  (import-functions-from-host
   '(map substitute functionp lognot ceiling logcount subsetp member-if intersection)
   e5)
  (load-source-file "Arithmetic/realp-defgeneric.lisp" e5)
  (load-source-file "Arithmetic/rationalp-defgeneric.lisp" e5)
  (load-source-file "Arithmetic/floatp-defgeneric.lisp" e5)
  (load-source-file "Arithmetic/upgraded-complex-part-type-defun.lisp" e5)
  (load-source-file "Character/char-code-limit-defconstant.lisp" e5)
  (let ((*features* '(:sicl))) (load-asdf-system '#:ctype e5)))
