(cl:in-package #:asdf-user)

(defsystem :sicl-cons-defuns
  :serial t
  :components
  ((:file "null-defun")
   (:file "endp-defun")
   ;; Currently, CONSP is defined to be (TYPEP ... 'CONS) and TYPEP
   ;; starts by calling CONSP to determine whether the type specifier
   ;; is atomic or compound.  So if we define CONSP that way, we get
   ;; an infinite recursion.
   ;; consp-defun)
   (:file "listp-defun")
   (:file "list-defun")
   (:file "list-star-defun")
   (:file "with-proper-list-elements-defmacro")
   (:file "with-proper-list-rests-defmacro")
   (:file "set-difference-defun")
   (:file "nset-difference-defun")
   (:file "member-defun")
   (:file "member-if-defun")
   (:file "member-if-not-defun")
   (:file "adjoin-defun")
   (:file "append-defun")
   (:file "nconc-defun")
   (:file "nth-defun")
   (:file "nthcdr-defun")
   (:file "copy-list-defun")
   (:file "with-alist-elements-defmacro")
   (:file "assoc-defun")
   (:file "assoc-if-defun")
   (:file "assoc-if-not-defun")
   (:file "rassoc-defun")
   (:file "rassoc-if-defun")
   (:file "rassoc-if-not-defun")
   (:file "make-list-defun")
   (:file "last-defun")
   (:file "butlast-defun")
   (:file "union-defun")
   (:file "set-exclusive-or-defun")
   (:file "mapcar-defun")
   (:file "mapc-defun")
   (:file "mapcan-defun")
   (:file "mapcon-defun")))
