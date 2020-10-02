(cl:in-package #:sicl-loop)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Parser for d-var-spec.

;;; A d-var-spec is a is a destructuring variable specifier: 
;;; 
;;;    d-var-spec ::= simple-var | nil | (d-var-spec . d-var-spec)
;;;
;;; where simple-var is a symbol (a name of a variable). 
;;;

;;; Return true if and only if the argument is a valid d-var-spec, in
;;; other words if it is a tree of CONS cells where the leaves are
;;; symbols.
(defun d-var-spec-p (object)
  (or (and (symbolp object)
           (not (constantp object)))
      (and (consp object)
           (d-var-spec-p (car object))
           (d-var-spec-p (cdr object)))))
