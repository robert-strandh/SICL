(cl:in-package #:sicl-loop)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Conditions for parsing
;;;
;;; FIXME: Remove condition reporters from the DEFINE-CONDITION forms
;;; and put them in a separate (language-specific) file.  

(define-condition loop-parse-error (parse-error acclimation:condition)
  ())

;;; Root class for loop parse errors that report something that was
;;; found, but should not be there.
(define-condition loop-parse-error-found (parse-error acclimation:condition)
  ((%found :initarg :found :reader found)))

(define-condition expected-var-spec-but-end (loop-parse-error)
  ())

(define-condition expected-var-spec-but-found (loop-parse-error-found)
  ())

(define-condition expected-simple-var-but-end (loop-parse-error)
  ())

(define-condition expected-simple-var-but-found (loop-parse-error-found)
  ())

(define-condition expected-type-spec-but-end (loop-parse-error)
  ())

(define-condition expected-type-spec-but-found (loop-parse-error-found)
  ())

(define-condition expected-compound-form-but-end (loop-parse-error)
  ())

(define-condition expected-compound-form-but-found (loop-parse-error-found)
  ())

(define-condition expected-form-but-end (loop-parse-error)
  ())

(define-condition expected-symbol-but-end (loop-parse-error)
  ())

(define-condition expected-symbol-but-found (loop-parse-error-found)
  ())

(define-condition expected-keyword-but-found (loop-parse-error-found)
  ())

(define-condition expected-for/as-subclause-but-end (loop-parse-error)
  ())

(define-condition expected-symbol-but-found (loop-parse-error-found)
  ())

(define-condition expected-each/the-but-end (loop-parse-error)
  ())

(define-condition expected-each/the-but-found (loop-parse-error-found)
  ())

(define-condition expected-hash-or-package-but-end (loop-parse-error)
  ())

(define-condition expected-hash-or-package-but-found (loop-parse-error-found)
  ())

(define-condition expected-in/of-but-end (loop-parse-error)
  ())

(define-condition expected-in/of-but-found (loop-parse-error-found)
  ())

(define-condition expected-hash-key-but-end (loop-parse-error)
  ())

(define-condition expected-hash-value-but-end (loop-parse-error)
  ())

(define-condition expected-hash-key-but-found (loop-parse-error-found)
  ())

(define-condition expected-hash-value-but-found (loop-parse-error-found)
  ())

(define-condition expected-preposition-but-end (loop-parse-error)
  ())

(define-condition too-many-prepositions-from-one-group (loop-parse-error)
  ())

(define-condition conflicting-stepping-directions (loop-parse-error)
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Conditions for syntactic and semantic analysis
;;;
;;; FIXME: Define English language condition reporters for these
;;;        conditions.

;;; The root of all syntax errors.
(define-condition loop-syntax-error (program-error acclimation:condition)
  ())

;;; This condition is signaled when a name-clause is found
;;; and the first clause is not a name-clause. 
(define-condition name-clause-not-first (loop-syntax-error) ())

;;; This condition is signaled when the first clause is a name-clause
;;; but there are other name-clauses. 
(define-condition multiple-name-clauses (loop-syntax-error) ())

;;; This condition is signaled when a variable-clause (other than an
;;; initially-clause or a finally-clause) appears after a main-clause
;;; (other than an initially-clause or a finally-clause).  
(define-condition invalid-clause-order (loop-syntax-error) ())

;;; This condition is signaled when there are multiple occurrences of
;;; a variable to be bound by any loop clause.
(define-condition multiple-variable-occurrences (loop-syntax-error)
  ((%bound-variable :initarg :bound-variable :reader bound-variable)))

;;; The root of all semantic errors.
(define-condition loop-semantic-error (program-error acclimation:condition)
  ())
