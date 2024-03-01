(cl:in-package #:sicl-new-boot)

(defclass boot ()
  (;; This slot contains a hash table of extrinsic Parcl packages plus
   ;; two host packages, namely COMMON-LISP-USER and KEYWORD.
   ;; Packages created as a result of loading code into a Clostrum
   ;; environment are contained in this table.
   (%packages
    :initform (make-hash-table :test #'equal)
    :reader packages)
   ;; When a target symbol is created in a package other than KEYWORD,
   ;; we instead create an uninterned host symbol, and we use this
   ;; table to determine to which extrinsic Parcl package the target
   ;; symbol belongs.
   (%symbol-package
    :initform (make-hash-table :test #'eq)
    :reader symbol-package)
   (%e1 :accessor e1)
   (%e2 :accessor e2)
   (%e3 :accessor e3)))

;;; This variable contains an instance of the class BOOT during
;;; bootstrapping.
(defvar *boot*)
