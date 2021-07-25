(cl:in-package #:sicl-package)

;;; Find an internal and present symbol named NAME in PACKAGE.  If
;;; there is such a symbol, return two values, the symbol found, and
;;; true.  If there is no such symbol, return two values, NIL and NIL.
(defun find-internal-symbol (name package)
  (gethash name (internal-symbols package)))

;;; Find an external (and thus present) symbol named NAME in PACKAGE.
;;; If there is such a symbol, return two values, the symbol found,
;;; and true.  If there is no such symbol, return two values, NIL and
;;; NIL.
(defun find-external-symbol (name package)
  (gethash name (internal-symbols package)))

;;; Find a shadowing (and thus present) symbol named NAME in PACKAGE.
;;; If there is such a symbol, return two values, the symbol found,
;;; and true.  If there is no such symbol, return two values, NIL and
;;; NIL.
(defun find-shadowing-symbol (name package)
  (let ((result (member name (shadowing-symbols package)
                        :key #'symbol-name
                        :test #'equal)))
    (if (null result)
        (values nil nil)
        (values (first result) t))))

;;; Call FUNCTION for each symbol in PACKAGE that is both present and
;;; internal.
(defun map-internal-symbols (function package)
  (maphash (lambda (key value)
             (declare (ignore key))
             (funcall function value))
           (internal-symbols package)))

;;; Call FUNCTION for each external symbol in PACKAGE.
(defun map-external-symbols (function package)
  (maphash (lambda (key value)
             (declare (ignore key))
             (funcall function value))
           (external-symbols package)))

;;; Call FUNCTION for each shadowing symbol in PACKAGE.
(defun map-shadowing-symbols (function package)
  (mapc function (shadowing-symbols package)))

;;; Return true if and only if SYMBOL is present and is internal
;;; symbol of PACKAGE.
(defun symbol-is-internal-p (symbol package)
  (multiple-value-bind (result present-p)
      (gethash (symbol-name symbol) (internal-symbols package))
    (and present-p (eq symbol result))))

;;; Return true if and only if SYMBOL is an external symbol of
;;; PACKAGE.
(defun symbol-is-external-p (symbol package)
  (multiple-value-bind (result present-p)
      (gethash (symbol-name symbol) (external-symbols package))
    (and present-p (eq symbol result))))

(defun symbol-is-present-p (symbol package)
  (multiple-value-bind (result present-p)
      (find-external-symbol (symbol-name symbol) package)
    (or (and present-p (eq symbol result))
        (multiple-value-bind (result present-p)
            (find-internal-symbol (symbol-name symbol) package)
          (and present-p (eq symbol result))))))

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
