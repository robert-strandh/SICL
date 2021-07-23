(cl:in-package #:sicl-package)

;;; Find an internal and present symbol named NAME in PACKAGE.  If
;;; there is such a symbol, return two values, the symbol found, and
;;; true.  If there is no such symbol, return two values, NIL and NIL.
(defun find-internal-symbol (name package)
  (gethash name (internal-symbols package)))

;;; Find an external and present symbol named NAME in PACKAGE.  If
;;; there is such a symbol, return two values, the symbol found, and
;;; true.  If there is no such symbol, return two values, NIL and NIL.
(defun find-external-symbol (name package)
  (gethash name (internal-symbols package)))

;;; Find a symbol named NAME that is accessible as an inherited symbol
;;; in PACKAGE.  If there is such a symbol, return two values, the
;;; symbol found, and the package it is inherited from.  If there is
;;; no such symbol, return two values, NIL and NIL.  We assume that
;;; NAME is not the name of a symbol that is present in PACKAGE.  In
;;; other words, we do not check whether a symbol named NAME is
;;; present as a shadowing symbol.
(defun find-inherited-symbol (name package)
  (loop for used-package in (package-use-list package)
        do (multiple-value-bind (symbol present-p)
               (find-external-symbol symbol used-package)
             (when present-p
               (return-from find-inherited-symbol symbol used-package))))
  (values nil nil))
