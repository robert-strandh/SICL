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
  `(if (null ,*accumulation-variable*)
       ;; If the accumulation variable is NIL, then so is the tail
       ;; variable.  We first set the accumulation variable to the
       ;; value of the form.  Then we make the tail variable point to
       ;; the last cell of the list.
       (progn (setq ,*accumulation-variable* ,(form clause))
              (setq ,*list-tail-accumulation-variable*
                    (last ,*accumulation-variable*)))
       ;; If the accumulation variable is not NIL, then the tail
       ;; variable may or may not be NIL.
       (progn 
         ;; If the tail variable is NIL, then every CONS cell in the
         ;; list starting at the accumulation variable must be copied,
         ;; and we know that there is at least one.  So we can
         ;; eliminate this special case by copying the first CONS
         ;; cell, and setting the tail variable to point to it.  We
         ;; could call COPY-LIST and then LAST, but then we would
         ;; traverse the list twice, so we do it with a loop instead.
         (when (null ,*list-tail-accumulation-variable*)
           (setf ,*accumulation-variable*
                 (cons (car ,*accumulation-variable*)
                       (cdr ,*accumulation-variable*)))
           (setf ,*list-tail-accumulation-variable*
                 ,*accumulation-variable*))
         ;; Now, whether the tail variable was initially NIL or not,
         ;; now it no longer is.  And every CONS cell after the one
         ;; that the tail variable points to must be copied.
         (tagbody
          again
            (if (atom (cdr ,*list-tail-accumulation-variable*))
                ;; We have copied all the CONS cells that had to be
                ;; copied.
                (go out)
                ;; Otherwise, we copy the CONS cell pointed to by the
                ;; CDR of the tail variable and advance the tail
                ;; variable by one position.
                (progn (setf (cdr ,*list-tail-accumulation-variable*)
                             (cons (cadr ,*list-tail-accumulation-variable*)
                                   (cddr ,*list-tail-accumulation-variable*)))
                       (setf ,*list-tail-accumulation-variable*
                             (cdr ,*list-tail-accumulation-variable*))
                       (go again)))
          out)
         ;; When we come here, every CONS cell after the one that the
         ;; tail variable points to has been copied, and the tail
         ;; variable points to the last CONS cell in the list.  It
         ;; remains to attach the new list to the end, and to set the
         ;; tail variable to point to the last cell of the newly
         ;; attached list.
         (setf (cdr ,*list-tail-accumulation-variable*)
               ,(form clause))
         (setf ,*list-tail-accumulation-variable*
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
