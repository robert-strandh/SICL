(cl:in-package #:sicl-printer)

(defvar *print-array* t)

(defvar *print-base* 10)

(defvar *print-case* :upcase)

(defvar *print-radix* nil)

(defvar *print-circle* nil)

(defvar *print-escape* t)

(defvar *print-gensym* t)

(defvar *print-length* nil)

(defvar *print-level* nil)

(defvar *print-lines* nil)

(defvar *print-miser-width* nil)

(defvar *print-pretty* t)

(defvar *print-radix* nil)

(defvar *print-readably* nil)

(defvar *print-right-margin* nil)

;;; FIXME: The value of this variable should be a pprint dispatch
;;; table.
(defvar *print-pprint-dispatch* nil)
