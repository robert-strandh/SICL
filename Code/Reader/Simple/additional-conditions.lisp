(cl:in-package #:sicl-reader)

(define-condition backquote-condition (reader-error)
  ())

(define-condition invalid-context-for-backquote (backquote-condition)
  ())

(define-condition comma-not-inside-backquote (backquote-condition)
  ())

(define-condition undefined-use-of-backquote (backquote-condition)
  ())

(define-condition invalid-context-for-consing-dot (reader-error)
  ())

(define-condition consing-dot-most-be-followed-by-object (reader-error)
  ())

(define-condition multiple-objects-following-consing-dot (reader-error)
  ())

(define-condition invalid-context-for-right-parenthesis (reader-error)
  ())

(define-condition sub-char-must-not-be-a-decimal-digit (error)
  ((%disp-char :initarg :disp-char :reader disp-char)
   (%sub-char :initarg :sub-char :reader sub-char)))

(define-condition char-must-be-a-dispatching-character (error)
  ((%disp-char :initarg :disp-char :reader disp-char)))

(define-condition symbol-name-must-not-end-with-package-marker (reader-error)
  ((%desired-symbol
    :initarg :desired-symbol
    :reader desired-symbol)))

(define-condition symbol-does-not-exist (reader-error)
  ((%desired-symbol
    :initarg :desired-symbol
    :reader desired-symbol)))

(define-condition symbol-is-not-external (reader-error)
  ((%desired-symbol
    :initarg :desired-symbol
    :reader desired-symbol)))

(define-condition two-package-markers-must-be-adjacent (reader-error)
  ((%desired-symbol
    :initarg :desired-symbol
    :reader desired-symbol)))

(define-condition two-package-markers-must-not-be-first (reader-error)  
  ((%desired-symbol
    :initarg :desired-symbol
    :reader desired-symbol)))

(define-condition symbol-can-have-at-most-two-package-markers (reader-error)
  ((%desired-symbol
    :initarg :desired-symbol
    :reader desired-symbol)))
  
(define-condition unknown-macro-sub-character (reader-error)
  ((%sub-char :initarg :sub-char :reader sub-char)))

(define-condition numeric-parameter-supplied-but-ignored (warning)
  ((%parameter :initarg :parameter :reader parameter)
   (%macro-name :initarg :macro-name :reader macro-name)))

(define-condition extraneous-objects-ignored (warning)
  ((%parameter :initarg :parameter :reader parameter)
   (%macro-name :initarg :macro-name :reader macro-name)))

(define-condition no-objects-supplied (warning)
  ((%parameter :initarg :parameter :reader parameter)
   (%macro-name :initarg :macro-name :reader macro-name)))
  
(define-condition read-time-evaluation-inhibited (reader-error)
  ())

(define-condition unknown-character-name (reader-error)
  ((%name :initarg :name :reader name)))

(define-condition digit-expected (reader-error)
  ((%character-found :initarg :character-found :reader character-found)
   (%base :initarg :base :reader base)))

(define-condition invalid-radix (reader-error)
  ((%radix :initarg :radix :reader radix)))

(define-condition invalid-default-float-format (reader-error)
  ((%float-format :initarg :float-format :reader float-format)))

(define-condition too-many-elements (reader-error)
  ((%exptected-number :initarg :expected-number :reader expected-number)
   (%number-found :initarg :number-found :reader number-found)))

(define-condition no-elements-found (reader-error)
  ((%exptected-number :initarg :expected-number :reader expected-number)))

(define-condition incorrect-initialization-length (error)
  ((%expected-length :initarg :expected-length :reader expected-length )
   (%datum :initarg :datum :reader datum)))

(define-condition single-feature-expected (error)
  ((%features :initarg :features :reader features)))

(define-condition sharpsign-invalid (reader-error)
  ((%character-found :initarg :character-found :reader character-found)))
