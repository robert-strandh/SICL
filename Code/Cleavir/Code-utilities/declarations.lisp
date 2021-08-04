(cl:in-package #:cleavir-code-utilities)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Take a list of declaration specifiers and return a list of
;;; canonicalized declaration specifiers.
;;;
;;; A canonicalized declaration specifier is one of following:
;;;
;;;   * (declaration name)
;;;   * (dynamic-extent var)
;;;   * (dynamic-extent (function function-name))
;;;   * (ftype type function-name)
;;;   * (ignore var)
;;;   * (ignore (function function-name))
;;;   * (ignorable var)
;;;   * (ignorable (function function-name))
;;;   * (inline function-name)
;;;   * (notinline function-name)
;;;   * (optimize (quality value))
;;;   * (special var)
;;;   * (type typespec var)
;;;
;;; In other words, the following work is done here:
;;;
;;;   * Each declaration specifier is divided up so that each one
;;;     concerns a single entity (name, variable, function name,
;;;     quality).
;;;
;;;   * Abbreviations for TYPE declaration specifiers are expanded so
;;;     that they contain the word TYPE.
;;;
;;;   * A declaration specifier with an empty list of entities is
;;;     removed.
;;;
;;; We do NOT check whether the type in a TYPE declaration specifier
;;; is a valid type.
;;;
;;; FIXME:
;;; At the moment, we assume that a declaration specifier where the
;;; CAR is not one of the symbols DECLARATION, DYNAMIC-EXTENT, FTYPE,
;;; IGNORE, IGNORABLE, INLINE, NOTINLINE, OPTIMIZE, SPECIAL, or TYPE
;;; is an abbreviated type specifier.  This is not quite correct,
;;; because it could be a symbol introduced by DECLARATION.

;;; FIXME: use specific conditions
(defun check-declaration-specifier (declaration-specifier)
  (unless (proper-list-p declaration-specifier)
    (error 'declaration-specifier-must-be-proper-list
           :declaration-specifier declaration-specifier))
  (when (null declaration-specifier)
    (error 'declaration-specifier-can-not-be-nil))
  (when (eq (car declaration-specifier) 'type)
    (unless (>= (length declaration-specifier) 2)
      (error "TYPE declaration specifier must have a type specifier"))))

;;; Take a single declaration specifier and return a list of
;;; canonicalized declaration specifiers.
(defun canonicalize-declaration-specifier (declaration-specifier
                                           alien-identifiers)
  (cond ((member (car declaration-specifier)
                 '(declaration dynamic-extent ignore ignorable
                   inline notinline special))
         (loop for entity in (cdr declaration-specifier)
               collect `(,(car declaration-specifier) ,entity)))
        ((eq (car declaration-specifier) 'optimize)
         (loop for entity in (cdr declaration-specifier)
               collect `(optimize ,entity)))
        ((member (car declaration-specifier) '(type ftype))
         (loop for entity in (cddr declaration-specifier)
               collect `(,(car declaration-specifier)
                         ,(cadr declaration-specifier) ,entity)))
        ((member (car declaration-specifier) alien-identifiers)
         ;; This means that the declaration is one specified by
         ;; (declaim (declaration ...))
         ;; It has some user- or implementation- specified meaning
         ;; that we don't care about, so we ignore it.
         nil)
        (t
         (loop for entity in (cdr declaration-specifier)
               collect `(type ,(car declaration-specifier) ,entity)))))

(defun canonicalize-declaration-specifiers (declaration-specifiers
                                            alien-identifiers)
  (unless (proper-list-p declaration-specifiers)
    (error "declaration specifiers must be a proper list"))
  (reduce #'append
          (mapcar (lambda (spec)
                    (canonicalize-declaration-specifier
                     spec alien-identifiers))
                  declaration-specifiers)))
