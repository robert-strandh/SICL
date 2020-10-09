(cl:in-package #:sicl-loop)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Clause FOR-AS-PACKAGE

(defclass for-as-package (for-as-subclause)
  ((%package-form :initarg :package-form :reader package-form)
   (%package-var :initform (gensym) :reader package-var)
   (%temp-entry-p-var :initform (gensym) :reader temp-entry-p-var)
   (%temp-symbol-var :initform (gensym) :reader temp-symbol-var)
   (%iterator-var :initform (gensym) :reader iterator-var)
   (%iterator-keywords :initarg :iterator-keywords :reader iterator-keywords)))

(defmethod bound-variables ((subclause for-as-package))
  (mapcar #'car
          (extract-variables (var-spec subclause) nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Parsers

(define-parser symbol-parser
  (alternative (keyword-parser 'symbol)
               (keyword-parser 'symbols)))

(define-parser present-symbol-parser
  (alternative (keyword-parser 'present-symbol)
               (keyword-parser 'present-symbols)))

(define-parser external-symbol-parser
  (alternative (keyword-parser 'external-symbol)
               (keyword-parser 'external-symbols)))

;;; This parser recognizes IN/OF followed by any form, and returns
;;; that form, or, if there is no IN/OF, returns the symbol *PACKAGE*.
(define-parser package-form-parser
  (optional '*package*
            (consecutive (lambda (of form)
                           (declare (ignore of))
                           form)
                         'in-of-parser
                         'anything-parser)))

(define-parser package-symbol-parser
  (consecutive (lambda (var-spec
                        type-spec
                        being
                        each
                        symbol
                        package-form)
                 (declare (ignore being each symbol))
                 (make-instance 'for-as-package
                   :var-spec var-spec
                   :type-spec type-spec
                   :package-form package-form
                   :iterator-keywords '(:internal :external :inherited)))
               'anything-parser
               'optional-type-spec-parser
               'being-parser
               'each-the-parser
               'symbol-parser
               'package-form-parser))

(define-parser package-present-symbol-parser
  (consecutive (lambda (var-spec
                        type-spec
                        being
                        each
                        present-symbol
                        package-form)
                 (declare (ignore being each present-symbol))
                 (make-instance 'for-as-package
                   :var-spec var-spec
                   :type-spec type-spec
                   :package-form package-form
                   :iterator-keywords '(:internal :external)))
               'anything-parser
               'optional-type-spec-parser
               'being-parser
               'each-the-parser
               'present-symbol-parser
               'package-form-parser))


(define-parser package-external-symbol-parser
  (consecutive (lambda (var-spec
                        type-spec
                        being
                        each
                        external-symbol
                        package-form)
                 (declare (ignore being each external-symbol))
                 (make-instance 'for-as-package
                   :var-spec var-spec
                   :type-spec type-spec
                   :package-form package-form
                   :iterator-keywords '(:external)))
               'anything-parser
               'optional-type-spec-parser
               'being-parser
               'each-the-parser
               'external-symbol-parser
               'package-form-parser))

(define-parser for-as-package-parser
  (alternative 'package-symbol-parser
               'package-present-symbol-parser
               'package-external-symbol-parser))

(add-for-as-subclause-parser 'for-as-package-parser)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compute the initial bindings.

(defmethod initial-bindings ((clause for-as-package))
  `((,(package-var clause) ,(package-form clause))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compute the subclause wrapper.

(defmethod wrap-subclause ((subclause for-as-package) inner-form)
  `(let ((,(temp-entry-p-var subclause) nil)
         (,(temp-symbol-var subclause) nil)
         ,@(loop with d-var-spec = (var-spec subclause)
                 with d-type-spec = (type-spec subclause)
                 for (variable) in (extract-variables d-var-spec d-type-spec)
                 collect `(,variable nil)))
     (declare ,@(loop with d-var-spec = (var-spec subclause)
                      with d-type-spec = (type-spec subclause)
                      for (variable type)
                        in (extract-variables d-var-spec d-type-spec)
                      collect `(cl:type (or null ,type) ,variable)))
     (with-package-iterator
         (,(iterator-var subclause)
          ,(package-var subclause)
          ,@(iterator-keywords subclause))
       ,inner-form)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compute the prologue form.

(defmethod prologue-form ((subclause for-as-package) end-tag)
  `(progn (multiple-value-bind (entry-p symbol)
              (,(iterator-var subclause))
            (setq ,(temp-entry-p-var subclause) entry-p
                  ,(temp-symbol-var subclause) symbol))
          (unless ,(temp-entry-p-var subclause)
            (go ,end-tag))
          ,(generate-assignments (var-spec subclause)
                                 (temp-symbol-var subclause))
          (multiple-value-bind (entry-p symbol)
              (,(iterator-var subclause))
            (setq ,(temp-entry-p-var subclause) entry-p
                  ,(temp-symbol-var subclause) symbol))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compute the termination form.

(defmethod termination-form ((subclause for-as-package) end-tag)
  `(unless ,(temp-entry-p-var subclause)
     (go ,end-tag)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compute the step form.

(defmethod step-form ((subclause for-as-package))
  `(progn ,(generate-assignments (var-spec subclause)
                                 (temp-symbol-var subclause))
          (multiple-value-bind (entry-p symbol)
              (,(iterator-var subclause))
            (setq ,(temp-entry-p-var subclause) entry-p
                  ,(temp-symbol-var subclause) symbol))))
