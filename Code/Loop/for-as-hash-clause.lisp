(cl:in-package #:sicl-loop)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Clause FOR-AS-HASH

(defclass for-as-hash (for-as-subclause)
  ((%hash-table-form :initarg :hash-table-form :reader hash-table-form)
   (%hash-table-var :initform (gensym) :reader hash-table-var)
   (%temp-entry-p-var :initform (gensym) :reader temp-entry-p-var)
   (%temp-key-var :initform (gensym) :reader temp-key-var)
   (%temp-value-var :initform (gensym) :reader temp-value-var)
   (%iterator-var :initform (gensym) :reader iterator-var)
   (%other-var-spec :initarg :other-var-spec :reader other-var-spec)))

(defclass for-as-hash-key (for-as-hash) ())

(defclass for-as-hash-value (for-as-hash) ())

(defmethod bound-variables ((subclause for-as-hash))
  (mapcar #'car
          (append (extract-variables (var-spec subclause) nil)
                  (extract-variables (other-var-spec subclause) nil))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Parsers

(define-parser hash-key-parser
  (alternative (keyword-parser 'hash-key)
               (keyword-parser 'hash-keys)))

(define-parser hash-value-parser
  (alternative (keyword-parser 'hash-value)
               (keyword-parser 'hash-values)))

(define-parser hash-value-other-parser
  (singleton #'second
             (lambda (token)
               (and (consp token)
                    (consp (cdr token))
                    (null (cddr token))
                    (symbolp (car token))
                    (equal (symbol-name (car token)) (string '#:hash-value))))))

(define-parser hash-key-other-parser
  (singleton #'second
             (lambda (token)
               (and (consp token)
                    (consp (cdr token))
                    (null (cddr token))
                    (symbolp (car token))
                    (equal (symbol-name (car token)) (string '#:hash-key))))))

(define-parser hash-key-using-parser
  (consecutive (lambda (var-spec
                        type-spec
                        being
                        each
                        hash-key
                        of
                        hash-table-form
                        using
                        other)
                 (declare (ignore being each hash-key of using))
                 (make-instance 'for-as-hash-key
                   :var-spec var-spec
                   :type-spec type-spec
                   :hash-table-form hash-table-form
                   :other-var-spec other))
               'anything-parser
               'optional-type-spec-parser
               'being-parser
               'each-the-parser
               'hash-key-parser
               'in-of-parser
               'anything-parser
               (keyword-parser 'using)
               'hash-value-other-parser))

(define-parser hash-key-no-using-parser
  (consecutive (lambda (var-spec
                        type-spec
                        being
                        each
                        hash-key
                        of
                        hash-table-form)
                 (declare (ignore being each hash-key of))
                 (make-instance 'for-as-hash-key
                   :var-spec var-spec
                   :type-spec type-spec
                   :hash-table-form hash-table-form
                   :other-var-spec nil))
               'anything-parser
               'optional-type-spec-parser
               'being-parser
               'each-the-parser
               'hash-key-parser
               'in-of-parser
               'anything-parser))

(define-parser hash-value-using-parser
  (consecutive (lambda (var-spec
                        type-spec
                        being
                        each
                        hash-key
                        of
                        hash-table-form
                        using
                        other)
                 (declare (ignore being each hash-key of using))
                 (make-instance 'for-as-hash-value
                   :var-spec var-spec
                   :type-spec type-spec
                   :hash-table-form hash-table-form
                   :other-var-spec other))
               'anything-parser
               'optional-type-spec-parser
               'being-parser
               'each-the-parser
               'hash-value-parser
               'in-of-parser
               'anything-parser
               (keyword-parser 'using)
               'hash-key-other-parser))

(define-parser hash-value-no-using-parser
  (consecutive (lambda (var-spec
                        type-spec
                        being
                        each
                        hash-key
                        of
                        hash-table-form)
                 (declare (ignore being each hash-key of))
                 (make-instance 'for-as-hash-value
                   :var-spec var-spec
                   :type-spec type-spec
                   :hash-table-form hash-table-form
                   :other-var-spec nil))
               'anything-parser
               'optional-type-spec-parser
               'being-parser
               'each-the-parser
               'hash-value-parser
               'in-of-parser
               'anything-parser))

(define-parser for-as-hash-parser
  (alternative 'hash-key-using-parser
               'hash-key-no-using-parser
               'hash-value-using-parser
               'hash-value-no-using-parser))

(add-for-as-subclause-parser 'for-as-hash-parser)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compute the initial bindings.

(defmethod initial-bindings ((clause for-as-hash))
  `((,(hash-table-var clause) ,(hash-table-form clause))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compute the subclause wrapper.

(defmethod wrap-subclause ((subclause for-as-hash) inner-form)
  `(let ((,(temp-entry-p-var subclause) nil)
         (,(temp-key-var subclause) nil)
         (,(temp-value-var subclause) nil)
         ,@(loop with d-var-spec = (var-spec subclause)
                 with d-type-spec = (type-spec subclause)
                 for (variable) in (extract-variables d-var-spec d-type-spec)
                 collect `(,variable nil))
         ,@(loop with other-var-spec = (other-var-spec subclause)
                 for (variable) in (extract-variables other-var-spec nil)
                 collect `(,variable nil)))
     (declare ,@(loop with d-var-spec = (var-spec subclause)
                      with d-type-spec = (type-spec subclause)
                      for (variable type)
                        in (extract-variables d-var-spec d-type-spec)
                      collect `(cl:type (or null ,type) ,variable)))
     (with-hash-table-iterator
         (,(iterator-var subclause) ,(hash-table-var subclause))
       ,inner-form)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compute the prologue form.

(defmethod prologue-form ((subclause for-as-hash-key) end-tag)
  `(progn (multiple-value-bind (entry-p key value)
              (,(iterator-var subclause))
            (setq ,(temp-entry-p-var subclause) entry-p
                  ,(temp-key-var subclause) key
                  ,(temp-value-var subclause) value))
          (unless ,(temp-entry-p-var subclause)
            (go ,end-tag))
          ,(generate-assignments (var-spec subclause)
                                 (temp-key-var subclause))
          ,(generate-assignments (other-var-spec subclause)
                                 (temp-value-var subclause))
          (multiple-value-bind (entry-p key value)
              (,(iterator-var subclause))
            (setq ,(temp-entry-p-var subclause) entry-p
                  ,(temp-key-var subclause) key
                  ,(temp-value-var subclause) value))))

(defmethod prologue-form ((subclause for-as-hash-value) end-tag)
  `(progn (multiple-value-bind (entry-p key value)
              (,(iterator-var subclause))
            (setq ,(temp-entry-p-var subclause) entry-p
                  ,(temp-key-var subclause) key
                  ,(temp-value-var subclause) value))
          (unless ,(temp-entry-p-var subclause)
            (go ,end-tag))
          ,(generate-assignments (var-spec subclause)
                                 (temp-value-var subclause))
          ,(generate-assignments (other-var-spec subclause)
                                 (temp-key-var subclause))
          (multiple-value-bind (entry-p key value)
              (,(iterator-var subclause))
            (setq ,(temp-entry-p-var subclause) entry-p
                  ,(temp-key-var subclause) key
                  ,(temp-value-var subclause) value))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compute the termination form.

(defmethod termination-form ((subclause for-as-hash) end-tag)
  `(unless ,(temp-entry-p-var subclause)
     (go ,end-tag)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compute the step form.

(defmethod step-form ((subclause for-as-hash-key))
  `(progn ,(generate-assignments (var-spec subclause)
                                 (temp-key-var subclause))
          ,(generate-assignments (other-var-spec subclause)
                                 (temp-value-var subclause))
          (multiple-value-bind (entry-p key value)
              (,(iterator-var subclause))
            (setq ,(temp-entry-p-var subclause) entry-p
                  ,(temp-key-var subclause) key
                  ,(temp-value-var subclause) value))))

(defmethod step-form ((subclause for-as-hash-value))
  `(progn ,(generate-assignments (var-spec subclause)
                                 (temp-value-var subclause))
          ,(generate-assignments (other-var-spec subclause)
                                 (temp-key-var subclause))
          (multiple-value-bind (entry-p key value)
              (,(iterator-var subclause))
            (setq ,(temp-entry-p-var subclause) entry-p
                  ,(temp-key-var subclause) key
                  ,(temp-value-var subclause) value))))
