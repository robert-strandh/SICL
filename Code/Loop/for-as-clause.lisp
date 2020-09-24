(cl:in-package #:sicl-loop)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Clause FOR-AS-CLAUSE.
;;;
;;; The HyperSpec says that a FOR-AS-CLAUSE has the following syntax:
;;;
;;;    for-as-clause ::= {for | as} for-as-subclause {and for-as-subclause}* 
;;;    for-as-subclause::= for-as-arithmetic | for-as-in-list | 
;;;                        for-as-on-list | for-as-equals-then | 
;;;                        for-as-across | for-as-hash | for-as-package 
;;;
;;; For the purpose of specialization, we need different names for the
;;; main clauses as well as for the subclauses, so we alter this
;;; grammar a bit and define it like this instead:
;;;
;;;    for-as-clause::= 
;;;      for-as-arithmetic-clause | for-as-in-list-clause | 
;;;      for-as-on-list-clause | for-as-equals-then-clause | 
;;;      for-as-across-clause | for-as-hash-clause | for-as-package-clause
;;;    
;;;    for-as-arithmetic-clause ::=
;;;      {for | as} for-as-arithmetic {and for-as-subclause}* 
;;;    
;;;    for-as-in-list-clause ::=
;;;      {for | as} for-as-in-list {and for-as-subclause}* 
;;;    
;;;    for-as-on-list-clause ::=
;;;      {for | as} for-as-on-list {and for-as-subclause}* 
;;;    
;;;    for-as-equals-then-clause ::=
;;;      {for | as} for-as-equals-then {and for-as-subclause}* 
;;;    
;;;    for-as-across-clause ::=
;;;      {for | as} for-as-across {and for-as-subclause}* 
;;;
;;;    for-as-hash-clause ::=
;;;      {for | as} for-as-hash {and for-as-subclause}* 
;;;
;;;    for-as-package-clause ::=
;;;      {for | as} for-as-package {and for-as-subclause}* 

(defclass for-as-clause (variable-clause subclauses-mixin) ())

(defclass for-as-subclause (var-and-type-spec-mixin)
  ())

(defmethod bound-variables ((clause for-as-clause))
  (reduce #'append
          (mapcar #'bound-variables (subclauses clause))
          :from-end t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Manage a list of FOR-AS subclause parsers. 

(defparameter *for-as-subclause-parsers* '())

(defun add-for-as-subclause-parser (parser)
  (push parser *for-as-subclause-parsers*))

;;; A parser that tries every parser in *FOR-AS-SUBCLAUSE-PARSERS* until one
;;; succeeds.

(defun for-as-subclause-parser (tokens)
  (loop for parser in *for-as-subclause-parsers*
        do (multiple-value-bind (successp result rest)
               (funcall parser tokens)
             (when successp
               (return (values t result rest))))
        finally (return (values nil nil tokens))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Parse a FOR-AS clause.

(define-parser for-as-clause-parser
  (consecutive (lambda (for subclause more-subclauses)
                 (declare (ignore for))
                 (make-instance 'for-as-clause
                   :subclauses (cons subclause more-subclauses)))
               (alternative (keyword-parser 'for)
                            (keyword-parser 'as))
               'for-as-subclause-parser
               (repeat* #'list
                        (consecutive (lambda (and subclause)
                                       (declare (ignore and))
                                       subclause)
                                     (keyword-parser 'and)
                                     'for-as-subclause-parser))))

(add-clause-parser 'for-as-clause-parser)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Expansion methods for FOR-AS clause.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compute the bindings.

(defmethod initial-bindings ((clause for-as-clause))
  (reduce #'append (mapcar #'initial-bindings (subclauses clause))
          :from-end t))

(defmethod final-bindings ((clause for-as-clause))
  (reduce #'append (mapcar #'final-bindings (subclauses clause))
          :from-end t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compute the declarations.

(defmethod declarations ((clause for-as-clause))
  (reduce #'append (mapcar #'declarations (subclauses clause))
          :from-end t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compute the prologue-form.

(defmethod prologue-form ((clause for-as-clause) end-tag)
  `(progn ,@(mapcar (lambda (subclause)
                      (prologue-form subclause end-tag))
                    (subclauses clause))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compute the termination-form.

(defmethod termination-form ((clause for-as-clause) end-tag)
  `(progn ,@(mapcar (lambda (subclause)
                      (termination-form subclause end-tag))
                    (subclauses clause))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compute the body-form.

(defmethod body-form ((clause for-as-clause) end-tag)
  `(progn ,@(mapcar (lambda (clause)
                      (body-form clause end-tag))
                    (subclauses clause))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Step a FOR-AS clause.

(defmethod step-form ((clause for-as-clause))
  `(progn ,@(mapcar #'step-form (subclauses clause))))
