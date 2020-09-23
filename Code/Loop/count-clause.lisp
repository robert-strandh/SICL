(cl:in-package #:sicl-loop)

(defclass count-clause (count/sum-accumulation-clause) ())

(defclass count-it-clause (count-clause it-mixin)
  ())

(defclass count-form-clause (count-clause form-mixin)
  ())

(defclass count-it-into-clause (into-mixin count-clause it-mixin)
  ())

(defclass count-form-into-clause (into-mixin count-clause form-mixin)
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Parsers.

(define-parser count-it-into-clause-parser
  (consecutive (lambda (count it into var type-spec)
                 (declare (ignore count it into))
                 (make-instance 'count-it-into-clause
                   :into-var var
                   :type-spec type-spec))
               (alternative (keyword-parser 'count)
                            (keyword-parser 'counting))
               (keyword-parser 'it)
               (keyword-parser 'into)
               (singleton #'identity
                          (lambda (x)
                            (and (symbolp x) (not (constantp x)))))
               'optional-type-spec-parser))

(define-parser count-it-clause-parser
  (consecutive (lambda (count it type-spec)
                 (declare (ignore count it))
                 (make-instance 'count-it-clause
                   :type-spec type-spec))
               (alternative (keyword-parser 'count)
                            (keyword-parser 'counting))
               (keyword-parser 'it)
               'optional-type-spec-parser))

(define-parser count-form-into-clause-parser
  (consecutive (lambda (count form into var type-spec)
                 (declare (ignore count into))
                 (make-instance 'count-form-into-clause
                   :form form
                   :into-var var
                   :type-spec type-spec))
               (alternative (keyword-parser 'count)
                            (keyword-parser 'counting))
               'anything-parser
               (keyword-parser 'into)
               (singleton #'identity
                          (lambda (x)
                            (and (symbolp x) (not (constantp x)))))
               'optional-type-spec-parser))

(define-parser count-form-clause-parser
  (consecutive (lambda (count form type-spec)
                 (declare (ignore count))
                 (make-instance 'count-form-clause
                   :form form
                   :type-spec type-spec))
               (alternative (keyword-parser 'count)
                            (keyword-parser 'counting))
               'anything-parser
               'optional-type-spec-parser))

(define-parser count-clause-parser
  (alternative 'count-it-into-clause-parser
               'count-it-clause-parser
               'count-form-into-clause-parser
               'count-form-clause-parser))

(add-clause-parser 'count-clause-parser)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compute the BODY-FORM.

(defmethod body-form ((clause count-form-clause) end-tag)
  (declare (ignore end-tag))
  `(when ,(form clause)
     (setq ,*accumulation-variable*
           (1+ ,*accumulation-variable*))))

(defmethod body-form ((clause count-form-into-clause) end-tag)
  (declare (ignore end-tag))
  `(when ,(form clause)
     (setq ,(into-var clause)
           (1+ ,(into-var clause)))))

(defmethod body-form ((clause count-it-clause) end-tag)
  (declare (ignore end-tag))
  `(when ,*it-var*
     (setq ,*accumulation-variable*
           (1+ ,*accumulation-variable*))))

(defmethod body-form ((clause count-it-into-clause) end-tag)
  (declare (ignore end-tag))
  `(when ,*it-var*
     (setq ,(into-var clause)
           (1+ ,(into-var clause)))))
