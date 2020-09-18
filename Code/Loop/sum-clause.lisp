(cl:in-package #:sicl-loop)

(defclass sum-clause (count/sum-accumulation-clause) ())

(defclass sum-it-clause (sum-clause it-mixin)
  ())

(defclass sum-form-clause (sum-clause form-mixin)
  ())

(defclass sum-it-into-clause (into-mixin sum-clause it-mixin)
  ())

(defclass sum-form-into-clause (into-mixin sum-clause form-mixin)
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Parsers.

(define-parser sum-it-into-clause-parser
  (consecutive (lambda (sum it into var type-spec)
                 (declare (ignore sum it into))
                 (make-instance 'sum-it-into-clause
                   :into-var var
                   :type-spec type-spec))
               (alternative (keyword-parser 'sum)
                            (keyword-parser 'summing))
               (keyword-parser 'it)
               (keyword-parser 'into)
               (singleton #'identity
                          (lambda (x)
                            (and (symbolp x) (not (constantp x)))))
               'optional-type-spec-parser))

(define-parser sum-it-clause-parser
  (consecutive (lambda (sum it type-spec)
                 (declare (ignore sum it))
                 (make-instance 'sum-it-clause
                   :type-spec type-spec))
               (alternative (keyword-parser 'sum)
                            (keyword-parser 'summing))
               (keyword-parser 'it)
               'optional-type-spec-parser))

(define-parser sum-form-into-clause-parser
  (consecutive (lambda (sum form into var type-spec)
                 (declare (ignore sum into))
                 (make-instance 'sum-form-into-clause
                   :form form
                   :into-var var
                   :type-spec type-spec))
               (alternative (keyword-parser 'sum)
                            (keyword-parser 'summing))
               'anything-parser
               (keyword-parser 'into)
               (singleton #'identity
                          (lambda (x)
                            (and (symbolp x) (not (constantp x)))))
               'optional-type-spec-parser))

(define-parser sum-form-clause-parser
  (consecutive (lambda (sum form type-spec)
                 (declare (ignore sum))
                 (make-instance 'sum-form-clause
                   :form form
                   :type-spec type-spec))
               (alternative (keyword-parser 'sum)
                            (keyword-parser 'summing))
               'anything-parser
               'optional-type-spec-parser))

(define-parser sum-clause-parser
  (alternative 'sum-it-into-clause-parser
               'sum-it-clause-parser
               'sum-form-into-clause-parser
               'sum-form-clause-parser))

(add-clause-parser 'sum-clause-parser)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compute the BODY-FORM.

(defmethod body-form ((clause sum-form-clause) end-tag)
  (declare (ignore end-tag))
  `(setq ,*accumulation-variable*
         (+ ,*accumulation-variable* ,(form clause))))

(defmethod body-form ((clause sum-form-into-clause) end-tag)
  (declare (ignore end-tag))
  `(setq ,(into-var clause)
         (+ ,(into-var clause) ,(form clause))))

(defmethod body-form ((clause sum-it-clause) end-tag)
  (declare (ignore end-tag))
  `(setq ,*accumulation-variable*
         (+ ,*accumulation-variable* ,*it-var*)))

(defmethod body-form ((clause sum-it-into-clause) end-tag)
  (declare (ignore end-tag))
  `(setq ,(into-var clause)
         (+ ,(into-var clause) ,*it-var*)))
