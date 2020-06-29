(cl:in-package #:asdf-user)

;;; This system is empty, at least for now.  It is used with
;;; ASDF:SYSTEM-RELATIVE-PATHNAME to refer to various files in the
;;; SICL hierarchy without having to refer to them using either
;;; absolute or directory-relative pathnames.

(defsystem #:sicl)
