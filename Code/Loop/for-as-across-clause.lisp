(cl:in-package #:sicl-loop)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Clause FOR-AS-ACROSS

(defclass for-as-across (for-as-subclause var-and-type-spec-mixin)
  (;; This slot contains a copy of the tree contained in the VAR-SPEC
   ;; slot except that the non-NIL leaves have been replaced by
   ;; GENSYMs.
   (%temp-vars :initarg :temp-vars :reader temp-vars)
   ;; This slot contains a list of pairs.  Each pair is a CONS cell
   ;; where the CAR is a variable in VAR-SPEC and the CDR is the
   ;; corresponding variable in TEMP-VARS.
   (%dictionary :initarg :dictionary :reader dictionary)
   (%vector-form :initarg :vector-form :reader vector-form)
   (%form-var :initform (gensym) :reader form-var)
   (%length-var :initform (gensym) :reader length-var)
   (%index-var :initform (gensym) :reader index-var)))

(defmethod initialize-instance :after
    ((clause for-as-across) &key &allow-other-keys)
  (multiple-value-bind (temp-vars dictionary)
      (fresh-variables (var-spec clause))
    (reinitialize-instance clause
                           :temp-vars temp-vars
                           :dictionary dictionary)))

;;; The FOR-AS-ACROSS clasue binds all the variables in the VAR-SPEC
;;; of the clause, so this method should return a list of all those
;;; variables.
(defmethod bound-variables ((clause for-as-across))
  (mapcar #'car
          (extract-variables (var-spec clause) nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Parser

(define-parser for-as-across-parser
  (consecutive (lambda (var type-spec across vector-form)
                 (declare (ignore across))
                 (make-instance 'for-as-across
                   :var-spec var
                   :type-spec type-spec
                   :vector-form vector-form))
               'anything-parser
               'optional-type-spec-parser
               (keyword-parser 'across)
               'anything-parser))

(add-for-as-subclause-parser 'for-as-across-parser)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compute bindings.

(defmethod initial-bindings ((clause for-as-across))
  `((,(form-var clause) ,(vector-form clause))
    (,(index-var clause) 0)))

(defmethod final-bindings ((clause for-as-across))
  `((,(length-var clause) (length ,(form-var clause)))
    ,@(loop for (real-var) in (dictionary clause)
            collect `(,real-var nil))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compute declarations.

(defmethod declarations ((clause for-as-across))
  (loop with d-var-spec = (var-spec clause)
        with d-type-spec = (type-spec clause)
        for (variable type) in (extract-variables d-var-spec d-type-spec)
        collect `(cl:type (or null ,type) ,variable)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compute prologue-form.

(defmethod prologue-form ((clause for-as-across) end-tag)
  `(progn ,(termination-form clause end-tag)
          ,(generate-assignments (var-spec clause)
                                 `(aref ,(form-var clause)
                                        ,(index-var clause)))
          (incf ,(index-var clause))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compute termination-form

(defmethod termination-form ((clause for-as-across) end-tag)
  `(when (>= ,(index-var clause) ,(length-var clause))
     (go ,end-tag)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compute step-form.

(defmethod step-form ((clause for-as-across))
  `(progn ,(generate-assignments (var-spec clause)
                                 `(aref ,(form-var clause)
                                        ,(index-var clause)))
          (incf ,(index-var clause))))
