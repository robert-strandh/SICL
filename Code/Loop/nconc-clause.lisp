(cl:in-package #:sicl-loop)

(defclass nconc-clause (list-accumulation-clause) ())

(defclass nconc-it-clause (nconc-clause it-mixin)
  ())

(defclass nconc-form-clause (nconc-clause form-mixin)
  ())

(defclass nconc-it-into-clause (into-mixin nconc-clause it-mixin)
  ())

(defclass nconc-form-into-clause (into-mixin nconc-clause form-mixin)
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Parsers.

(define-parser nconc-it-into-clause-parser
  (consecutive (lambda (nconc it into var)
                 (declare (ignore nconc it into))
                 (make-instance 'nconc-it-into-clause
                   :into-var var))
               (alternative (keyword-parser 'nconc)
                            (keyword-parser 'nconcing))
               (keyword-parser 'it)
               (keyword-parser 'into)
               (singleton #'identity
                          (lambda (x)
                            (and (symbolp x) (not (constantp x)))))))

(define-parser nconc-it-clause-parser
  (consecutive (lambda (nconc it)
                 (declare (ignore nconc it))
                 (make-instance 'nconc-it-clause))
               (alternative (keyword-parser 'nconc)
                            (keyword-parser 'nconcing))
               (keyword-parser 'it)))

(define-parser nconc-form-into-clause-parser
  (consecutive (lambda (nconc form into var)
                 (declare (ignore nconc into))
                 (make-instance 'nconc-form-into-clause
                   :form form
                   :into-var var))
               (alternative (keyword-parser 'nconc)
                            (keyword-parser 'nconcing))
               'anything-parser
               (keyword-parser 'into)
               (singleton #'identity
                          (lambda (x)
                            (and (symbolp x) (not (constantp x)))))))

(define-parser nconc-form-clause-parser
  (consecutive (lambda (nconc form)
                 (declare (ignore nconc))
                 (make-instance 'nconc-form-clause
                   :form form))
               (alternative (keyword-parser 'nconc)
                            (keyword-parser 'nconcing))
               'anything-parser))

(define-parser nconc-clause-parser
  (alternative 'nconc-it-into-clause-parser
               'nconc-it-clause-parser
               'nconc-form-into-clause-parser
               'nconc-form-clause-parser))

(add-clause-parser 'nconc-clause-parser)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compute body-form.

(defmethod body-form ((clause nconc-form-clause) end-tag)
  (declare (ignore end-tag))
  `(if (null ,*list-tail-accumulation-variable*)
       (progn (setq ,*accumulation-variable*
                    ,(form clause))
              (setq ,*list-tail-accumulation-variable*
                    (last ,*accumulation-variable*)))
       (progn (rplacd ,*list-tail-accumulation-variable*
                      ,(form clause))
              (setq ,*list-tail-accumulation-variable*
                    (last ,*list-tail-accumulation-variable*)))))

(defmethod body-form ((clause nconc-form-into-clause) end-tag)
  (declare (ignore end-tag))
  `(if (null ,(tail-variable (into-var clause)))
       (progn (setq ,(into-var clause)
                    ,(form clause))
              (setq ,(tail-variable (into-var clause))
                    (last ,(into-var clause))))
       (progn (rplacd ,(tail-variable (into-var clause))
                      ,(form clause))
              (setq ,(tail-variable (into-var clause))
                    (last ,(tail-variable (into-var clause)))))))

(defmethod body-form ((clause nconc-it-clause) end-tag)
  (declare (ignore end-tag))
  `(if (null ,*list-tail-accumulation-variable*)
       (progn (setq ,*accumulation-variable*
                    ,*it-var*)
              (setq ,*list-tail-accumulation-variable*
                    (last ,*accumulation-variable*)))
       (progn (rplacd ,*list-tail-accumulation-variable*
                      ,*it-var*)
              (setq ,*list-tail-accumulation-variable*
                    (last ,*list-tail-accumulation-variable*)))))

(defmethod body-form ((clause nconc-it-into-clause) end-tag)
  (declare (ignore end-tag))
  `(if (null ,(tail-variable (into-var clause)))
       (progn (setq ,(into-var clause)
                    ,*it-var*)
              (setq ,(tail-variable (into-var clause))
                    (last ,(into-var clause))))
       (progn (rplacd ,(tail-variable (into-var clause))
                      ,*it-var*)
              (setq ,(tail-variable (into-var clause))
                    (last ,(tail-variable (into-var clause)))))))
