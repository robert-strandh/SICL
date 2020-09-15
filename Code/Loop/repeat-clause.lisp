(cl:in-package #:sicl-loop)

(defclass repeat-clause (termination-test-clause var-and-type-spec-mixin)
  ((%form :initarg :form :reader form))
  (:default-initargs :var-spec (gensym) :type-spec 'real))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Parsers.

(define-parser repeat-clause-parser
  (consecutive (lambda (repeat form)
                 (declare (ignore repeat))
                 (make-instance 'repeat-clause :form form))
               (keyword-parser 'repeat)
               'anything-parser))

(add-clause-parser 'repeat-clause-parser)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compute the bindings.

(defmethod initial-bindings ((clause repeat-clause))
  `((,(var-spec clause) ,(form clause))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compute the declarations.

(defmethod declarations ((clause repeat-clause))
  `((cl:type (integer 0) ,(var-spec clause))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compute the prologue-form.

(defmethod prologue-form ((clause repeat-clause) end-tag)
  `(when (<= ,(var-spec clause) 0)
     (go ,end-tag)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compute the termination-form.

(defmethod termination-form ((clause repeat-clause) end-tag)
  `(when (<= ,(var-spec clause) 1)
     (go ,end-tag)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compute the step-form.

(defmethod step-form ((clause repeat-clause))
  `(decf ,(var-spec clause)))
