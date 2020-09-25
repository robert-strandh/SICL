(cl:in-package #:sicl-loop)

;;; The purpose of this generic function is to generate a list of all
;;; bound variables in a clause.  The same variable occurs as many
;;; times in the list as the number of times it is bound in the
;;; clause.
(defgeneric bound-variables (clause))

;;; The purpose of this generic function is to generate a list of all
;;; the accumulation variables in a clause.  Each element of the list
;;; is itself a list of three elements.  The first element is the name
;;; of a variable used in an INTO clause, or NIL if the clause has no
;;; INTO.  The second element determines the kind of accumulation, and
;;; can be one of the symbols LIST, COUNT/SUM, or MAX/MIN.  The third
;;; element is a type specifier which can be T.
(defgeneric accumulation-variables (clause))

;;; The purpose of this generic function is to extract a list of
;;; declaration specifiers from the clause.  Notice that it is a list
;;; of declaration specifiers, not a list of declarations.  In other
;;; words, the symbol DECLARE is omitted.
(defgeneric declarations (clause)
  (:method (clause)
    (declare (ignore clause))
    '()))

(defgeneric initial-bindings (clause)
  (:method (clause)
    (declare (ignore clause))
    '()))

(defgeneric final-bindings (clause)
  (:method (clause)
    (declare (ignore clause))
    '()))

(defgeneric bindings (clause)
  (:method (clause)
    (append (initial-bindings clause) (final-bindings clause))))

;;; This generic function returns a form for CLAUSE that should go in
;;; the LOOP prologue.  The INITIALLY clause is an obvious candidate
;;; for such code.  But the stepping clauses also have code that goes
;;; in the prologue, namely an initial termination test to determine
;;; whether any iterations at all should be executed.  END-TAG is the
;;; tag to GO to when the initial termination test says that no
;;; iterations should be executed.
(defgeneric prologue-form (clause end-tag)
  (:method (clause end-tag)
    (declare (ignore clause end-tag))
    nil))

;;; This generic function returns a form for CLAUSE that should go
;;; between the body code and the stepping forms in the body of the
;;; expanded code.  Some of the FOR-AS clauses and also the REPEAT
;;; clause generate code here.  END-TAG is the tag to GO to when
;;; iteration should terminate.
(defgeneric termination-form (clause end-tag)
  (:method (clause end-tag)
    (declare (ignore clause end-tag))
    nil))

;;; This generic function returns a form for CLAUSE that should go in
;;; the main the body code, before the termination test and the
;;; stepping forms, in the body of the expanded code.  The DO clause
;;; and the accumulation clauses are obvious candidates for such code.
;;;
;;; FIXME: Currently, END-TAG is used only in the WHILE clause as a
;;; termination test.  Investigate whether the WHILE clause should use
;;; TERMINATION-TEST instead, so that we can eliminate this parameter.
(defgeneric body-form (clause end-tag)
  (:method (clause end-tag)
    (declare (ignore clause end-tag))
    nil))

;;; This generic function returns a form for CLAUSE that should go
;;; after the main body code and the termination tests in the body of
;;; the expanded code.  The FOR-AS clauses and also the REPEAT clause
;;; generate code here.
(defgeneric step-form (clause)
  (:method (clause)
    (declare (ignore clause))
    nil))

;;; This generic function returns a form for CLAUSE that should go in
;;; the LOOP epilogue.  Of the clause types defined by the Common Lisp
;;; standard, only the method specialized to the FINALLY clause
;;; returns a value other than NIL.
(defgeneric epilogue-form (clause)
  (:method (clause)
    (declare (ignore clause))
    nil))

;;; This variable is bound by the code generator for
;;; CONDITIONAL-CLAUSE before calling the code generators for the
;;; clauses in its THEN and ELSE branches.
(defvar *it-var*)

(defvar *accumulation-variable*)

(defvar *list-tail-accumulation-variable*)

(defvar *tail-variables*)

(defun tail-variable (head-variable)
  (let ((result (gethash head-variable *tail-variables*)))
    (when (null result)
      (setf result (gensym))
      (setf (gethash head-variable *tail-variables*) result))
    result))

(defun accumulation-bindings (clauses)
  (let* ((descriptors
           (reduce #'append
                   (mapcar #'accumulation-variables clauses)))
         (equal-fun (lambda (d1 d2)
                      (and (eq (first d1) (first d2))
                           (eq (second d1) (second d2)))))
         (unique (remove-duplicates descriptors :test equal-fun)))
    (loop for (name category type) in unique
          for initial-value = (cond  ((eq category 'count/sum)
                                      (coerce 0 type))
                                     ((eq category 'always/never)
                                      t)
                                     (t
                                      nil))
          collect (if (null name)
                      `(,*accumulation-variable* ,initial-value)
                      `(,name ,initial-value))
          when (eq category 'list)
            collect (if (null name)
                      `(,*list-tail-accumulation-variable* nil)
                      `(,(tail-variable name) nil)))))

(defvar *loop-name*)

(defun prologue-body-epilogue (clauses end-tag)
  (let ((start-tag (gensym)))
    `(tagbody
        (progn ,@(mapcar (lambda (clause)
                           (prologue-form clause end-tag))
                         clauses))
        ,start-tag
        (progn ,@(mapcar (lambda (clause)
                           (body-form clause end-tag))
                         clauses))
        (progn ,@(mapcar (lambda (clause)
                           (termination-form clause end-tag))
                         clauses))
        (progn ,@(mapcar #'step-form clauses))
        (go ,start-tag)
        ,end-tag
        (progn ,@(mapcar #'epilogue-form clauses)
               (return-from ,*loop-name*
                 ,*accumulation-variable*)))))

;;; Once the LOOP prologue, the LOOP body, and the LOOP epilogue have
;;; all been constructed, a bunch of successive WRAPPERS are applied
;;; so as to obtain the final expansion.  Each clause type defines how
;;; it needs to be wrapped.  Some clauses only require the
;;; establishment of variable bindings in the wrapper.  Other clauses
;;; might need to be wrapped in some iterator form.  The generic
;;; function WRAP-CLAUSE defines how each clause type is wrapped.
(defgeneric wrap-clause (clause inner-form))

;;; If a clause can have subclauses, then each subclause may need to
;;; be wrapped separately.  The generic function WRAP-SUBCLAUSE
;;; determines how this is done.
(defgeneric wrap-subclause (subclause inner-form))

;;; Default method for WRAP-SUBCLAUSE.  By default, the wrapper for
;;; each subclause contains only the final bindings, leaving the
;;; initial bindings to a single binding form of the entire clause.
(defmethod wrap-subclause (subclause inner-form)
  `(let ,(final-bindings subclause)
     ,inner-form))

;;; Default method for WRAP-CLAUSE.  This method is applicable only if
;;; the clause type does not admit any subclauses.  For this type of
;;; clause, the default implemented here is to wrap the clause in all
;;; the bindings, i.e., both the initial and the final bindings of
;;; both exist.
(defmethod wrap-clause (clause inner-form)
  `(let* ,(bindings clause)
     ,inner-form))

;;; Method on WRAP-CLAUSE specialized to clause types that admit
;;; subclauses.  This method overrides the default method above.  It
;;; wraps each subclause individually, and then wraps the result in
;;; the initial bindings for the entire clause.
(defmethod wrap-clause ((clause subclauses-mixin) inner-form)
  (let ((result inner-form))
    (mapc (lambda (subclause)
            (setf result (wrap-subclause subclause result)))
          (reverse (subclauses clause)))
    `(let ,(initial-bindings clause)
       ,result)))

;;; Process all clauses by first computing the prologue, the body, and
;;; the epilogue, and then applying the clause-specific wrapper for
;;; each clause to the result.
(defun do-clauses (all-clauses end-tag)
  (let ((result (prologue-body-epilogue all-clauses end-tag)))
    (mapc (lambda (clause)
            (setf result (wrap-clause clause result)))
          (reverse all-clauses))
    result))

(defun expand-clauses (all-clauses end-tag)
  (let ((acc (accumulation-bindings all-clauses)))
    `(let (,@(if (member *accumulation-variable* acc :key #'car)
                 '()
                 `((,*accumulation-variable* nil)))
           ,@acc)
       ,(do-clauses all-clauses end-tag))))

(defun expand-body (loop-body end-tag)
  (if (every #'consp loop-body)
      (let ((tag (gensym)))
        `(block nil
           (tagbody
              ,tag
              ,@loop-body
              (go ,tag))))
      (let ((clauses (parse-loop-body loop-body)))
        (analyze-clauses clauses)
        (let* ((name (if (typep (car clauses) 'name-clause)
                         (name (car clauses))
                         nil))
               (*loop-name* name)
               (*accumulation-variable* (gensym))
               (*list-tail-accumulation-variable* (gensym))
               (*tail-variables* (make-hash-table :test #'eq)))
          `(block ,name
             ,(expand-clauses clauses end-tag))))))
