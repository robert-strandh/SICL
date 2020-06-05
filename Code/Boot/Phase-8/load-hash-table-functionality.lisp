(cl:in-package #:sicl-boot-phase-8)

(defun load-hash-table-functionality (e5)
  (load-source "Hash-tables/hash-table-defclass.lisp" e5)
  (load-source "Hash-tables/List/list-hash-table-defclass.lisp" e5)
  (load-source "Data-and-control-flow/equalp-defgeneric.lisp" e5)
  (load-source "Hash-tables/generic-functions.lisp" e5))
