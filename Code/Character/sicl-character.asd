(cl:in-package #:asdf-user)

(defsystem #:sicl-character
  :depends-on (#:cl-unicode
               #:cleavir-code-utilities)
  :serial t
  :components
  ((:file "packages")
   (:file "binary-char-equal-1-defun")
   (:file "char-equal-1-defun")
   (:file "char-equal-1-define-compiler-macro")))

