(cl:in-package #:common-lisp-user)

(defpackage #:sicl-string-test
  (:use #:common-lisp)
  (:shadow
   #:string-upcase
   #:string-downcase
   #:string-capitalize
   #:nstring-upcase
   #:nstring-downcase
   #:nstring-capitalize
   #:string-trim
   #:string-left-trim
   #:string-right-trim
   #:string=
   #:string/=
   #:string<
   #:string>
   #:string<=
   #:string>=
   #:string-equal
   #:string-not-equal
   #:string-lessp
   #:string-greaterp
   #:string-not-greaterp
   #:string-not-lessp))
