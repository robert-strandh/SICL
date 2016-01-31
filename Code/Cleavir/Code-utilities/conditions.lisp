(cl:in-package #:cleavir-code-utilities)

(define-condition code-condition ()
  ((%code :initarg :code :reader code)))

(define-condition form-must-be-proper-list
    (program-error cleavir-i18n:condition)
  ((%form :initarg :form :reader form)))

;;; A max-argcount of NIL means no upper bound.
(define-condition invalid-number-of-arguments 
    (program-error cleavir-i18n:condition)
  ((%form :initarg :form :reader form)
   (%min-argcount :initarg :min-argcount :reader min-argcount)
   (%max-argcount :initarg :max-argcount :reader max-argcount)))

;;; FIXME: improve these conditions!

(define-condition too-few-arguments
    (program-error cleavir-i18n:condition)
  ())

(define-condition too-many-arguments
    (program-error cleavir-i18n:condition)
  ())

(define-condition odd-number-of-keyword-arguments
    (program-error cleavir-i18n:condition)
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Lambda list conditions.

(define-condition lambda-list-must-be-list
    (program-error cleavir-i18n:condition)
  ())

(define-condition lambda-list-must-not-be-circular
    (program-error cleavir-i18n:condition)
  ())

(define-condition lambda-list-must-be-proper-list
    (program-error cleavir-i18n:condition)
  ())

(define-condition lambda-list-keyword-not-allowed
    (program-error cleavir-i18n:condition)
  ((%keyword :initarg :keyword :reader lambda-list-keyword)))

(define-condition lambda-list-keyword-not-allowed-in-dotted-lambda-list
    (program-error cleavir-i18n:condition)
  ((%keyword :initarg :keyword :reader lambda-list-keyword)))

(define-condition multiple-occurrences-of-lambda-list-keyword
    (program-error cleavir-i18n:condition)
  ((%keyword :initarg :keyword :reader lambda-list-keyword)))

(define-condition incorrect-keyword-order
    (program-error cleavir-i18n:condition)
  ((%keyword1 :initarg :keyword1 :reader lambda-list-keyword1)
   (%keyword2 :initarg :keyword2 :reader lambda-list-keyword2)))

(define-condition both-rest-and-body-occur-in-lambda-list
    (program-error cleavir-i18n:condition)
  ())

(define-condition rest/body-must-be-followed-by-variable
    (program-error cleavir-i18n:condition)
  ())

(define-condition atomic-lambda-list-tail-must-be-variable
    (program-error cleavir-i18n:condition)
  ())

(define-condition whole-must-appear-first
    (program-error cleavir-i18n:condition)
  ())

(define-condition whole-must-be-followed-by-variable
    (program-error code-condition cleavir-i18n:condition)
  ())

(define-condition environment-must-be-followed-by-variable
    (program-error code-condition cleavir-i18n:condition)
  ())

(define-condition environment-can-appear-at-most-once
    (program-error code-condition cleavir-i18n:condition)
  ())

(define-condition malformed-specialized-required
    (program-error code-condition cleavir-i18n:condition)
  ())

;;; This condition is used to indicate that there is a malformed item
;;; following the &optional lambda-list keyword in an ordinary lambda
;;; list, a specialized lambda list, a boa lambda list, a defsetf
;;; lambda list, a define-modify-macro lambda list, or a
;;; define-method-combination lambda list.  These lambda lists allow
;;; the following form for such an item:
;;;
;;;   * var
;;;   * (var)
;;;   * (var init-form)
;;;   * (var init-form supplied-p-parameter)
;;;
;;; where var and supplied-p-parameter are symbols that are not names
;;; of constants.
(define-condition malformed-ordinary-optional
    (program-error code-condition cleavir-i18n:condition)
  ())

;;; This condition is used to indicate that there is a malformed item
;;; following the &optional lambda-list keyword in a defgeneric lambda
;;; list.  This lambda list allows the following form for such an item:
;;;
;;;   * var
;;;   * (var)
;;;
;;; where var is a symbol that is not a name of a constant.
(define-condition malformed-defgeneric-optional
    (program-error code-condition cleavir-i18n:condition)
  ())

;;; This condition is used to indicate that there is a malformed item
;;; following the &optional lambda-list keyword in a macro lambda
;;; list, a desstructuring lambda list, or a deftype lambda list.
;;; These lambda lists allow the following form for such an item:
;;;
;;;   * var
;;;   * (pattern)
;;;   * (pattern init-form)
;;;   * (pattern init-form supplied-p-parameter)
;;;
;;; where var and supplied-p-parameter are symbols that are not names
;;; of constants, and pattern is any destructuring pattern.
(define-condition malformed-destructuring-optional
    (program-error code-condition cleavir-i18n:condition)
  ())

(define-condition malformed-ordinary-key
    (program-error code-condition cleavir-i18n:condition)
  ())

(define-condition malformed-defgeneric-key
    (program-error code-condition cleavir-i18n:condition)
  ())

(define-condition malformed-destructuring-key
    (program-error code-condition cleavir-i18n:condition)
  ())

(define-condition malformed-aux
    (program-error code-condition cleavir-i18n:condition)
  ())

;;; This condition is used to indicate that a destructuring tree
;;; contains some item other than a CONS cell or a symbol which is not
;;; also the name of a constant. 
(define-condition malformed-destructuring-tree
    (program-error code-condition cleavir-i18n:condition)
  ())

;;; This condition is used to indicate that something that ought
;;; to be either a destructuring tree or a list with lambda-list
;;; keywords in fact is something else, such as a constant or some
;;; illegal atomic object.
(define-condition malformed-lambda-list-pattern
    (program-error code-condition cleavir-i18n:condition)
  ())

;;; This condition is used to indicate that in a (non destructuring)
;;; lambda list, the required parameter must be a variable
(define-condition required-must-be-variable
    (program-error code-condition cleavir-i18n:condition)
  ())

(define-condition suspect-lambda-list-keyword
    (style-warning cleavir-i18n:condition)
  ((%keyword :initarg :keyword :reader lambda-list-keyword)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Conditions for forms with BODY.

(define-condition ordinary-body-must-be-proper-list
    (style-warning cleavir-i18n:condition)
  ((%body :initarg :body :reader body)))

(define-condition function-body-must-be-proper-list
    (style-warning cleavir-i18n:condition)
  ((%body :initarg :body :reader body)))
