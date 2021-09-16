(cl:in-package #:sicl-boot-phase-6)

(defun load-ctype (e5)
  (load-source-file "Types/typexpand-defun.lisp" e5)
  (load-source-file "Arithmetic/type-definitions.lisp" e5)
  (sicl-boot:import-functions-from-host
   '(realp rationalp)
   e5)
  (load-source-file "Arithmetic/floatp-defgeneric.lisp" e5)
  (load-source-file "Arithmetic/upgraded-complex-part-type-defun.lisp" e5)
  (load-source-file "Character/char-code-limit-defconstant.lisp" e5)
  (load-source-file "Cons/ldiff-defun.lisp" e5)
  (let ((*features* '(:sicl))) (load-asdf-system '#:ctype e5)))
