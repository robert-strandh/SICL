(cl:in-package #:sicl-loop)

(defclass maximize-clause (max/min-accumulation-clause) ())

(defclass maximize-it-clause (maximize-clause it-mixin)
  ())

(defclass maximize-form-clause (maximize-clause form-mixin)
  ())

(defclass maximize-it-into-clause (into-mixin maximize-clause it-mixin)
  ())

(defclass maximize-form-into-clause (into-mixin maximize-clause form-mixin)
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Parsers.

(define-parser maximize-it-into-clause-parser
  (consecutive (lambda (maximize it into var type-spec)
                 (declare (ignore maximize it into))
                 (make-instance 'maximize-it-into-clause
                   :into-var var
                   :type-spec type-spec))
               (alternative (keyword-parser 'maximize)
                            (keyword-parser 'maximizing))
               (keyword-parser 'it)
               (keyword-parser 'into)
               (singleton #'identity
                          (lambda (x)
                            (and (symbolp x) (not (constantp x)))))
               'optional-type-spec-parser))

(define-parser maximize-it-clause-parser
  (consecutive (lambda (maximize it type-spec)
                 (declare (ignore maximize it))
                 (make-instance 'maximize-it-clause
                   :type-spec type-spec))
               (alternative (keyword-parser 'maximize)
                            (keyword-parser 'maximizing))
               (keyword-parser 'it)
               'optional-type-spec-parser))

(define-parser maximize-form-into-clause-parser
  (consecutive (lambda (maximize form into var type-spec)
                 (declare (ignore maximize into))
                 (make-instance 'maximize-form-into-clause
                   :form form
                   :into-var var
                   :type-spec type-spec))
               (alternative (keyword-parser 'maximize)
                            (keyword-parser 'maximizing))
               'anything-parser
               (keyword-parser 'into)
               (singleton #'identity
                          (lambda (x)
                            (and (symbolp x) (not (constantp x)))))
               'optional-type-spec-parser))

(define-parser maximize-form-clause-parser
  (consecutive (lambda (maximize form type-spec)
                 (declare (ignore maximize))
                 (make-instance 'maximize-form-clause
                   :form form
                   :type-spec type-spec))
               (alternative (keyword-parser 'maximize)
                            (keyword-parser 'maximizing))
               'anything-parser
               'optional-type-spec-parser))

(define-parser maximize-clause-parser
  (alternative 'maximize-it-into-clause-parser
               'maximize-it-clause-parser
               'maximize-form-into-clause-parser
               'maximize-form-clause-parser))

(add-clause-parser 'maximize-clause-parser)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compute the BODY-FORM.

(defmethod body-form ((clause maximize-form-clause) end-tag)
  (declare (ignore end-tag))
  `(if (null ,*accumulation-variable*)
       (setq ,*accumulation-variable*
             (ensure-real ,(form clause) 'max-argument-must-be-real))
       (setq ,*accumulation-variable*
             (maximize ,*accumulation-variable* ,(form clause)))))

(defmethod body-form ((clause maximize-form-into-clause) end-tag)
  (declare (ignore end-tag))
  `(if (null ,(into-var clause))
       (setq ,(into-var clause)
             (ensure-real ,(form clause) 'max-argument-must-be-real))
       (setq ,(into-var clause)
             (maximize ,(into-var clause) ,(form clause)))))

(defmethod body-form ((clause maximize-it-clause) end-tag)
  (declare (ignore end-tag))
  `(if (null ,*accumulation-variable*)
       (setq ,*accumulation-variable*
             (ensure-real ,*it-var* 'max-argument-must-be-real))
       (setq ,*accumulation-variable*
             (maximize ,*accumulation-variable* ,*it-var*))))

(defmethod body-form ((clause maximize-it-into-clause) end-tag)
  (declare (ignore end-tag))
  `(if (null ,(into-var clause))
       (setq ,(into-var clause)
             (ensure-real ,*it-var* 'max-argument-must-be-real))
       (setq ,(into-var clause)
             (maximize ,(into-var clause) ,*it-var*))))
