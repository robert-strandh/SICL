(cl:in-package #:asdf-user)

(defsystem :sicl-cons-defuns
  :serial t
  :components
  ((:file "acons-defun")
   (:file "null-defun")
   (:file "endp-defun")
   (:file "consp-defun")
   (:file "getf-defun")
   (:file "atom-defun")
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
   (:file "setf-nth-defun")
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
   (:file "mapcon-defun")
   (:file "nreconc-defun")
   (:file "subst-defun")
   (:file "cxr")
   (:file "setf-cxr")))
