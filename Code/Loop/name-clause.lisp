(cl:in-package #:sicl-loop)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Clause NAME-CLAUSE.
;;;
;;; A NAME-CLAUSE is a clause that gives a name to the loop.  It
;;; translates to a block name, so that RETURN-FROM can be used to
;;; exit the loop.  By default, the name of the loop is nil.
;;;
;;; The name-clause is optional, and if present, must be the first one
;;; in the body.  The syntax is:
;;;
;;;    NAMED name
;;;
;;; where name is a symbol.

(defclass name-clause (clause)
  ((%name :initarg :name :reader name)))

(defmethod bound-variables ((clause name-clause))
  '())

(defmethod accumulation-variables ((clause name-clause))
  '())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Parser.

(define-parser name-clause-parser
  (consecutive (lambda (named name)
                 (declare (ignore named))
                 (make-instance 'name-clause
                   :name name))
               (keyword-parser 'named)
               (singleton #'identity #'symbolp)))

(add-clause-parser 'name-clause-parser)
