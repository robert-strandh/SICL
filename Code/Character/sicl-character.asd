(cl:in-package #:asdf-user)

(defsystem #:sicl-character
  :depends-on (#:cl-unicode
               #:cleavir-code-utilities)
  :serial t
  :components
  ((:file "packages")
   (:file "binary-char-equal-1-defun")
   (:file "char-equal-1-defun")
   (:file "char-equal-1-define-compiler-macro")
   (:file "binary-char-less-1-defun")
   (:file "char-less-1-defun")
   (:file "char-less-1-define-compiler-macro")
   (:file "binary-char-not-greaterp-1-defun")
   (:file "char-not-greaterp-1-defun")
   (:file "char-not-greaterp-1-define-compiler-macro")
   (:file "binary-char-greaterp-1-defun")
   (:file "char-greaterp-1-defun")
   (:file "char-greaterp-1-define-compiler-macro")
   (:file "binary-char-not-lessp-1-defun")
   (:file "char-not-lessp-1-defun")
   (:file "char-not-lessp-1-define-compiler-macro")))

