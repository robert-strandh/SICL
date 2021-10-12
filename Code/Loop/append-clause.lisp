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
  `(if (null ,*accumulation-variable*)
       ;; If the accumulation variable is NIL, then so is the tail
       ;; variable.  We leave the tail variable as NIL so as to
       ;; indicate that every CONS cell in the list starting at the
       ;; accumulation variable must be copied whenever yet more CONS
       ;; cells are attached at the end.
       (setq ,*accumulation-variable* ,(form clause))
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
         ;; remains to attach the new list to the end.  And we leave
         ;; the tail variable where it is, indicating that the CONS
         ;; cells of the newly attached list must be copied whenever
         ;; yet more cells are attached at the end.
         (if (null (cdr ,*list-tail-accumulation-variable*))
             (setf (cdr ,*list-tail-accumulation-variable*)
                   ,(form clause))
             (error 'type-error
                    :datum (cdr ,*list-tail-accumulation-variable*)
                    :expected-type 'null)))))

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
