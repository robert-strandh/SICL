(cl:in-package #:common-lisp-user)

(defpackage #:cleavir-lexical
  (:use #:common-lisp)
  (:shadowing-import-from
   #:trucler
   #:name
   #:identity
   #:type
   #:inline
   #:ignore
   #:dynamic-extent
   #:compiler-macro
   #:expander
   #:value
   #:expansion)
  (:export))

