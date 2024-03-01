(cl:in-package #:sicl-new-boot)

(defclass boot ()
  (;; This slot contains a hash table of extrinsic Parcl packages plus
   ;; two host packages, namely COMMON-LISP-USER and KEYWORD.
   ;; Packages created as a result of loading code into a Clostrum
   ;; environment are contained in this table.
   (%packages
    :initform (make-hash-table :test #'equal)
    :reader packages)
   (%e1 :accessor e1)
   (%e2 :accessor e2)
   (%e3 :accessor e3)))
