(cl:in-package #:sicl-loop)

(defclass append-clause (list-accumulation-clause) ())

(defclass append-it-clause (append-clause it-mixin)
  ())

(defclass append-form-clause (append-clause form-mixin)
  ())

(defclass append-it-into-clause (into-mixin append-clause it-mixin)
  ())

(defclass append-form-into-clause (into-mixin append-clause form-mixin)
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Parsers.

(define-parser append-it-into-clause-parser
  (consecutive (lambda (append it into var)
                 (declare (ignore append it into))
                 (make-instance 'append-it-into-clause
                   :into-var var))
               (alternative (keyword-parser 'append)
                            (keyword-parser 'appending))
               (keyword-parser 'it)
               (keyword-parser 'into)
               (singleton #'identity
                          (lambda (x)
                            (and (symbolp x) (not (constantp x)))))))

(define-parser append-it-clause-parser
  (consecutive (lambda (append it)
                 (declare (ignore append it))
                 (make-instance 'append-it-clause))
               (alternative (keyword-parser 'append)
                            (keyword-parser 'appending))
               (keyword-parser 'it)))

(define-parser append-form-into-clause-parser
  (consecutive (lambda (append form into var)
                 (declare (ignore append into))
                 (make-instance 'append-form-into-clause
                   :form form
                   :into-var var))
               (alternative (keyword-parser 'append)
                            (keyword-parser 'appending))
               'anything-parser
               (keyword-parser 'into)
               (singleton #'identity
                          (lambda (x)
                            (and (symbolp x) (not (constantp x)))))))

(define-parser append-form-clause-parser
  (consecutive (lambda (append form)
                 (declare (ignore append))
                 (make-instance 'append-form-clause
                   :form form))
               (alternative (keyword-parser 'append)
                            (keyword-parser 'appending))
               'anything-parser))

(define-parser append-clause-parser
  (alternative 'append-it-into-clause-parser
               'append-it-clause-parser
               'append-form-into-clause-parser
               'append-form-clause-parser))

(add-clause-parser 'append-clause-parser)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compute body-form.

(defmethod body-form ((clause append-form-clause) end-tag)
  (declare (ignore end-tag))
  `(if (null ,*list-tail-accumulation-variable*)
       (progn (setq ,*accumulation-variable*
                    (copy-list ,(form clause)))
              (setq ,*list-tail-accumulation-variable*
                    (last ,*accumulation-variable*)))
       (progn (rplacd ,*list-tail-accumulation-variable*
                      (copy-list ,(form clause)))
              (setq ,*list-tail-accumulation-variable*
                    (last ,*list-tail-accumulation-variable*)))))

(defmethod body-form ((clause append-form-into-clause) end-tag)
  (declare (ignore end-tag))
  `(if (null ,(tail-variable (into-var clause)))
       (progn (setq ,(into-var clause)
                    (copy-list ,(form clause)))
              (setq ,(tail-variable (into-var clause))
                    (last ,(into-var clause))))
       (progn (rplacd ,(tail-variable (into-var clause))
                      (copy-list ,(form clause)))
              (setq ,(tail-variable (into-var clause))
                    (last ,(tail-variable (into-var clause)))))))

(defmethod body-form ((clause append-it-clause) end-tag)
  (declare (ignore end-tag))
  `(if (null ,*list-tail-accumulation-variable*)
       (progn (setq ,*accumulation-variable*
                    (copy-list ,*it-var*))
              (setq ,*list-tail-accumulation-variable*
                    (last ,*accumulation-variable*)))
       (progn (rplacd ,*list-tail-accumulation-variable*
                      (copy-list ,*it-var*))
              (setq ,*list-tail-accumulation-variable*
                    (last ,*list-tail-accumulation-variable*)))))

(defmethod body-form ((clause append-it-into-clause) end-tag)
  (declare (ignore end-tag))
  `(if (null ,(tail-variable (into-var clause)))
       (progn (setq ,(into-var clause)
                    (copy-list ,*it-var*))
              (setq ,(tail-variable (into-var clause))
                    (last ,(into-var clause))))
       (progn (rplacd ,(tail-variable (into-var clause))
                      (copy-list ,*it-var*))
              (setq ,(tail-variable (into-var clause))
                    (last ,(tail-variable (into-var clause)))))))
