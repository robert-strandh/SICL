(cl:in-package #:sicl-loop)

;;; Clause WITH-CLAUSE.
;;;
;;; A WITH-CLAUSE allows the creation of local variables.  It is
;;; executed once.
;;;
;;; The syntax of a with-clause is:
;;;
;;;    with-clause ::= WITH var1 [type-spec] [= form1]
;;;                    {AND var2 [type-spec] [= form2]}*
;;;
;;; where var1 and var2 are destructuring variable specifiers
;;; (d-var-spec) allowing multiple local variables to be created in a
;;; single with-clause by destructuring the value of the corresponding
;;; form.
;;;
;;; When there are several consecutive with-clause, the execution is
;;; done sequentially, so that variables created in one with-clause
;;; can be used in the forms of subsequent with-clauses.  If parallel
;;; creation of variables is wanted, then the with-clause can be
;;; followed by one or more and-clauses.
;;;
;;; The (destructuring) type specifier is optional.  If no type
;;; specifier is given, it is as if t was given.
;;;
;;; The initialization form is optional.  If there is a corresponding
;;; type specifier for a variable, but no initialization form, then
;;; the variable is initialized to a value that is appropriate for the
;;; type.  In particular, for the type t the value is nil, for the
;;; type number, the value is 0, and for the type float, the value is
;;; 0.0.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class WITH-CLAUSE.
;;;

(defclass with-clause (variable-clause subclauses-mixin) ())

(defclass with-subclause ()
  ((%var-spec :initarg :var-spec :reader var-spec)
   (%type-spec :initarg :type-spec :reader type-spec)
   ;; This slot contains a copy of the tree contained in the VAR-SPEC
   ;; slot except that the non-NIL leaves have been replaced by
   ;; GENSYMs.
   (%temp-vars :initarg :temp-vars :reader temp-vars)
   ;; This slot contains a list of pairs.  Each pair is a CONS cell
   ;; where the CAR is a variable in VAR-SPEC and the CDR is the
   ;; corresponding variable in TEMP-VARS.
   (%dictionary :initarg :dictionary :reader dictionary)))

(defmethod initialize-instance :after
    ((clause with-subclause) &key &allow-other-keys)
  (multiple-value-bind (temp-vars dictionary)
      (fresh-variables (var-spec clause))
    (reinitialize-instance clause
                           :temp-vars temp-vars
                           :dictionary dictionary)))

(defclass with-subclause-no-form (with-subclause) ())

(defclass with-subclause-with-form (with-subclause)
  ((%form :initarg :form :reader form)
   (%form-var :initform (gensym) :reader form-var)))

;;; The default form is NIL.
(defmethod form ((subclause with-subclause))
  nil)

(defmethod bound-variables ((clause with-clause))
  (reduce #'append
          (mapcar #'bound-variables (subclauses clause))
          :from-end t))

(defmethod bound-variables ((subclause with-subclause))
  (mapcar #'car
          (extract-variables (var-spec subclause) nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Parsers.

;;; Parser for var [type-spec] = form
;;; We try this parser first.
(define-parser with-subclause-type-1-parser
  (consecutive (lambda (var-spec type-spec = form)
                 (declare (ignore =))
                 (make-instance 'with-subclause-with-form
                   :var-spec var-spec
                   :type-spec type-spec
                   :form form))
               ;; Accept anything for now.  Analyze later.
               'anything-parser
               'optional-type-spec-parser
               (keyword-parser '=)
               'anything-parser))

;;; Parser for var [type-spec]
(define-parser with-subclause-type-2-parser
  (consecutive (lambda (var-spec type-spec)
                 (make-instance 'with-subclause-no-form
                   :var-spec var-spec
                   :type-spec type-spec))
               ;; Accept anything for now.  Analyze later.
               'anything-parser
               'optional-type-spec-parser))

;;; Parser for any type of with subclause without the leading keyword
(define-parser with-subclause-no-keyword-parser
  (alternative 'with-subclause-type-1-parser
               'with-subclause-type-2-parser))

;;; Parser for the with subclause starting with the AND keyword.
(define-parser with-subclause-and-parser
  (consecutive (lambda (and subclause)
                 (declare (ignore and))
                 subclause)
               (keyword-parser 'and)
               'with-subclause-no-keyword-parser))

;;; Parser for a with clause
(define-parser with-clause-parser
  (consecutive (lambda (with first rest)
                 (declare (ignore with))
                 (make-instance 'with-clause
                   :subclauses (cons first rest)))
               (keyword-parser 'with)
               'with-subclause-no-keyword-parser
               (repeat* #'list
                        'with-subclause-and-parser)))

(add-clause-parser 'with-clause-parser)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compute the bindings.

(defmethod initial-bindings ((clause with-clause))
  (reduce #'append (mapcar #'initial-bindings (subclauses clause))))

(defmethod initial-bindings ((clause with-subclause-with-form))
  `((,(form-var clause) ,(form clause))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compute the subclause wrapper.

(defmethod wrap-subclause ((subclause with-subclause-with-form) inner-form)
  `(let* ,(destructure-variables (var-spec subclause) (form-var subclause))
     ,inner-form))

(defmethod wrap-subclause ((subclause with-subclause-no-form) inner-form)
  (let* ((vars-and-types
           (extract-variables (var-spec subclause) (type-spec subclause)))
         (vars-and-values
           (loop for (var type) in vars-and-types
                 collect (list var
                               (case type
                                 (fixnum 0)
                                 (float 0.0)
                                 (t nil))))))
    `(let ,vars-and-values
       ,inner-form)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compute the declarations.

(defmethod declarations ((clause with-subclause))
  (reduce #'append (mapcar #'declarations (subclauses clause))
          :from-end t))
