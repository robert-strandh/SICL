(cl:in-package #:common-lisp-user)

;;; This package is going to contain symbols related to functionality
;;; that is now in SICL-GLOBAL-ENVIRONMENT and that is not going to
;;; exist in Clostrum.  The idea is to eliminate the package
;;; SICL-GLOBAL-ENVIRONMENT entirely.  Furthermore, the source files
;;; that are now in this directory should be moved to places that
;;; correspond to the dictionary entries in the standard.  And the
;;; file that is now called packages.lisp will then disappear, and
;;; this file will be renamed.

(defpackage #:sicl-environment
  (:use #:common-lisp)
  (:export #:global-environment))
