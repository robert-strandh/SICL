(cl:in-package #:sicl-loop)

(defclass conditional-clause (selectable-clause)
  ((%condition :initarg :condition :reader condition)
   (%then-clauses :initarg :then-clauses :reader then-clauses)
   (%else-clauses :initarg :else-clauses :reader else-clauses)))

;;; A conditional clause does not introduce any bindings for any
;;; variables, so this method should return the empty list.
(defmethod bound-variables ((clause conditional-clause))
  '())

(defmethod accumulation-variables ((clause conditional-clause))
  (append (reduce #'append
                  (mapcar #'accumulation-variables (then-clauses clause)))
          (reduce #'append
                  (mapcar #'accumulation-variables (else-clauses clause)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Parsers.

(define-parser then-or-else-parser
  (consecutive #'cons
               'selectable-clause-parser
               (repeat* #'list
                        'and-selectable-clause-parser)))
(define-parser if-else-end-clause-parser
  (consecutive (lambda (if form then-clauses else else-clauses end)
                 (declare (ignore if else end))
                 (make-instance 'conditional-clause
                   :condition form
                   :then-clauses then-clauses
                   :else-clauses else-clauses))
               (alternative (keyword-parser 'if)
                            (keyword-parser 'when))
               'anything-parser
               'then-or-else-parser
               (keyword-parser 'else)
               'then-or-else-parser
               (keyword-parser 'end)))

(define-parser if-end-clause-parser
  (consecutive (lambda (if form then-clauses end)
                 (declare (ignore if end))
                 (make-instance 'conditional-clause
                   :condition form
                   :then-clauses then-clauses
                   :else-clauses nil))
               (alternative (keyword-parser 'if)
                            (keyword-parser 'when))
               'anything-parser
               'then-or-else-parser
               (keyword-parser 'end)))
               
(define-parser if-else-clause-parser
  (consecutive (lambda (if form then-clauses else else-clauses)
                 (declare (ignore if else))
                 (make-instance 'conditional-clause
                   :condition form
                   :then-clauses then-clauses
                   :else-clauses else-clauses))
               (alternative (keyword-parser 'if)
                            (keyword-parser 'when))
               'anything-parser
               'then-or-else-parser
               (keyword-parser 'else)
               'then-or-else-parser))

(define-parser if-clause-parser
  (consecutive (lambda (if form then-clauses)
                 (declare (ignore if))
                 (make-instance 'conditional-clause
                   :condition form
                   :then-clauses then-clauses
                   :else-clauses nil))
               (alternative (keyword-parser 'if)
                            (keyword-parser 'when))
               'anything-parser
               'then-or-else-parser))

(define-parser if-when-parser
  (alternative 'if-else-end-clause-parser
               'if-end-clause-parser
               'if-else-clause-parser
               'if-clause-parser))

(define-parser unless-else-end-clause-parser
  (consecutive (lambda (unless form else-clauses else then-clauses end)
                 (declare (ignore unless else end))
                 (make-instance 'conditional-clause
                   :condition form
                   :then-clauses then-clauses
                   :else-clauses else-clauses))
               (keyword-parser 'unless)
               'anything-parser
               'then-or-else-parser
               (keyword-parser 'else)
               'then-or-else-parser
               (keyword-parser 'end)))

(define-parser unless-end-clause-parser
  (consecutive (lambda (unless form else-clauses end)
                 (declare (ignore unless end))
                 (make-instance 'conditional-clause
                   :condition form
                   :then-clauses nil
                   :else-clauses else-clauses))
               (keyword-parser 'unless)
               'anything-parser
               'then-or-else-parser
               (keyword-parser 'end)))
               
(define-parser unless-else-clause-parser
  (consecutive (lambda (unless form else-clauses else then-clauses)
                 (declare (ignore unless else))
                 (make-instance 'conditional-clause
                   :condition form
                   :then-clauses then-clauses
                   :else-clauses else-clauses))
               (keyword-parser 'unless)
               'anything-parser
               'then-or-else-parser
               (keyword-parser 'else)
               'then-or-else-parser))

(define-parser unless-clause-parser
  (consecutive (lambda (unless form else-clauses)
                 (declare (ignore unless))
                 (make-instance 'conditional-clause
                   :condition form
                   :then-clauses nil
                   :else-clauses else-clauses))
               (alternative (keyword-parser 'unless)
                            (keyword-parser 'when))
               'anything-parser
               'then-or-else-parser))

(define-parser unless-parser
  (alternative 'unless-else-end-clause-parser
               'unless-end-clause-parser
               'unless-else-clause-parser
               'unless-clause-parser))

(define-parser conditional-clause-parser
  (alternative 'if-when-parser
               'unless-parser))

(add-clause-parser 'conditional-clause-parser)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compute body-form.

(defmethod body-form ((clause conditional-clause) end-tag)
  (let ((*it-var* (gensym)))
    `(let ((,*it-var* ,(condition clause)))
       (if ,*it-var*
           (progn
             ,@(mapcar (lambda (clause)
                         (body-form clause end-tag))
                       (then-clauses clause)))
           (progn
             ,@(mapcar (lambda (clause)
                         (body-form clause end-tag))
                       (else-clauses clause)))))))
