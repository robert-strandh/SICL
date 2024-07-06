(cl:in-package #:asdf-user)

(defsystem #:sicl-evaluation-and-compilation-compile-time
  :depends-on ()
  :serial t
  :components
  ((:file "defmacro-defmacro")
   (:file "define-compiler-macro-defmacro")))
