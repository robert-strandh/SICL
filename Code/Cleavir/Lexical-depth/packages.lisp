(cl:in-package #:common-lisp-user)

(defpackage #:cleavir-lexical-depth
  (:use #:common-lisp)
  (:export
   #:compute-ownerships
   #:lexical-depths
   #:lexical-depth))
