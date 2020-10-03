(cl:in-package #:sicl-loop)

(defclass for-as-list (for-as-subclause)
  ((%list-form :initarg :list-form :reader list-form)
   (%list-var :initform (gensym) :reader list-var)
   (%by-form :initarg :by-form :reader by-form)
   (%by-var :initform (gensym) :reader by-var)
   (%rest-var :initform (gensym) :reader rest-var)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Clause FOR-AS-IN-LIST.

(defclass for-as-in-list (for-as-list) ())

(defmethod bound-variables ((subclause for-as-list))
  (mapcar #'car
          (extract-variables (var-spec subclause) nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Parsers.

(define-parser for-as-in-list-parser-1
  (consecutive (lambda (var type-spec in list-form by-form)
                 (declare (ignore in))
                 (make-instance 'for-as-in-list
                   :var-spec var
                   :type-spec type-spec
                   :list-form list-form
                   :by-form by-form))
               'anything-parser
               'optional-type-spec-parser
               (keyword-parser 'in)
               'anything-parser
               'by-parser))

(define-parser for-as-in-list-parser-2
  (consecutive (lambda (var type-spec in list-form)
                 (declare (ignore in))
                 (make-instance 'for-as-in-list
                   :var-spec var
                   :type-spec type-spec
                   :list-form list-form
                   :by-form '#'cdr))
               'anything-parser
               'optional-type-spec-parser
               (keyword-parser 'in)
               'anything-parser))

;;; Define a parser that tries the longer form first
(define-parser for-as-in-list-parser
  (alternative 'for-as-in-list-parser-1
               'for-as-in-list-parser-2))

(add-for-as-subclause-parser 'for-as-in-list-parser)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Clause FOR-AS-ON-LIST.

(defclass for-as-on-list (for-as-list) ())

(define-parser for-as-on-list-parser-1
  (consecutive (lambda (var type-spec on list-form by-form)
                 (declare (ignore on))
                 (make-instance 'for-as-on-list
                   :var-spec var
                   :type-spec type-spec
                   :list-form list-form
                   :by-form by-form))
               'anything-parser
               'optional-type-spec-parser
               (keyword-parser 'on)
               'anything-parser
               'by-parser))

(define-parser for-as-on-list-parser-2
  (consecutive (lambda (var type-spec on list-form)
                 (declare (ignore on))
                 (make-instance 'for-as-on-list
                   :var-spec var
                   :type-spec type-spec
                   :list-form list-form
                   :by-form '#'cdr))
               'anything-parser
               'optional-type-spec-parser
               (keyword-parser 'on)
               'anything-parser))

;;; Define a parser that tries the longer form first
(define-parser for-as-on-list-parser
  (alternative 'for-as-on-list-parser-1
               'for-as-on-list-parser-2))

(add-for-as-subclause-parser 'for-as-on-list-parser)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compute the bindings.

(defmethod initial-bindings ((clause for-as-list))
  `((,(list-var clause) ,(list-form clause))
    ,@(if (member (by-form clause) '(#'cdr #'cddr) :test #'equal)
          '()
          `((,(by-var clause) ,(by-form clause))))))

(defmethod final-bindings ((clause for-as-list))
  `((,(rest-var clause) ,(list-var clause))
    ,@(loop with d-var-spec = (var-spec clause)
            with d-type-spec = (type-spec clause)
            for (variable) in (extract-variables d-var-spec d-type-spec)
            collect `(,variable nil))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compute the declarations.

(defmethod declarations ((clause for-as-list))
  (loop with d-var-spec = (var-spec clause)
        with d-type-spec = (type-spec clause)
        for (variable type) in (extract-variables d-var-spec d-type-spec)
        collect `(cl:type (or null ,type) ,variable)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compute the prologue.

(defmethod prologue-form ((clause for-as-in-list) end-tag)
  `(progn ,(termination-form clause end-tag)
          ,(generate-assignments (var-spec clause) `(car ,(rest-var clause)))
          ,(if (member (by-form clause) '(#'cdr #'cddr) :test #'equal)
               `(setq ,(rest-var clause)
                      (,(cadr (by-form clause)) ,(rest-var clause)))
               `(setq ,(rest-var clause)
                      (funcall ,(by-var clause) ,(rest-var clause))))))

(defmethod prologue-form ((clause for-as-on-list) end-tag)
  `(progn ,(termination-form clause end-tag)
          ,(generate-assignments (var-spec clause) (rest-var clause))
          ,(if (member (by-form clause) '(#'cdr #'cddr) :test #'equal)
               `(setq ,(rest-var clause)
                      (,(cadr (by-form clause)) ,(rest-var clause)))
               `(setq ,(rest-var clause)
                      (funcall ,(by-var clause) ,(rest-var clause))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compute the termination-form.

(defmethod termination-form ((clause for-as-in-list) end-tag)
  `(when (endp ,(rest-var clause))
     (go ,end-tag)))

(defmethod termination-form ((clause for-as-on-list) end-tag)
  `(when (atom ,(rest-var clause))
     (go ,end-tag)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compute the step-form.

(defmethod step-form ((clause for-as-in-list))
  `(progn ,(generate-assignments (var-spec clause) `(car ,(rest-var clause)))
          ,(if (member (by-form clause) '(#'cdr #'cddr) :test #'equal)
               `(setq ,(rest-var clause)
                      (,(cadr (by-form clause)) ,(rest-var clause)))
               `(setq ,(rest-var clause)
                      (funcall ,(by-var clause) ,(rest-var clause))))))

(defmethod step-form ((clause for-as-on-list))
  `(progn ,(generate-assignments (var-spec clause) (rest-var clause))
          ,(if (member (by-form clause) '(#'cdr #'cddr) :test #'equal)
               `(setq ,(rest-var clause)
                      (,(cadr (by-form clause)) ,(rest-var clause)))
               `(setq ,(rest-var clause)
                      (funcall ,(by-var clause) ,(rest-var clause))))))
