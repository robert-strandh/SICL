;;;; Parse and syntatically validate defstruct forms, converting them
;;;; into a more managable representation.

(cl:in-package #:sicl-structure)

(defparameter *valid-defstruct-option-names*
  '(:conc-name :constructor :copier :predicate :include
    :print-object :print-function :type :named :initial-offset))

(defun extract-option-name (option)
  (if (listp option)
      (first option)
      option))

(defun check-defstruct-option-names-valid (options)
  (dolist (option options)
    (let ((option-name (extract-option-name option)))
      (unless (member option-name *valid-defstruct-option-names*)
        (error 'invalid-option-name :option-name option-name)))))

(defun get-options (options option-name)
  (loop for option in options
        when (eql (extract-option-name option) option-name)
          collect option))

(defun get-singular-option (options option-name)
  (let ((opts (get-options options option-name)))
    (when (rest opts)
      (error 'duplicate-option :option-name option-name))
    (first opts)))

;;; ### Is this defined in a common place?
(deftype string-designator ()
  '(or string symbol character))

(defun parse-conc-name-option (options struct-name)
  (let ((option (get-singular-option options :conc-name)))
    (cond ((not option)
           ;; Not supplied
           (concatenate 'string (symbol-name struct-name) "-"))
          ((or (not (listp option))
               (endp (rest option))
               (and (endp (cddr option))
                    (not (second option))))
           ;; :conc-name, (:conc-name), or (:conc-name nil)
           nil)
          ((endp (cddr option))
           ;; (:conc-name string-designator)
           (unless (typep (second option) 'string-designator)
             (error 'conc-name-must-be-string-designator
                    :datum (second option)))
           (string (second option)))
          (t
           (error 'malformed-option :option option)))))

(defun symbolicate (&rest string-designators)
  (intern (apply #'concatenate 'string (mapcar #'string string-designators))))

(defun keywordify (string-designator)
  (intern (string string-designator) '#:keyword))

(defun parse-constructor-options (options struct-name)
  (let ((default-name (symbolicate "MAKE-" struct-name))
        (constructor-options (get-options options :constructor)))
    (cond (constructor-options
           (let ((constructors '()))
             (dolist (option constructor-options)
               (labels ((check-constructor-not-defined (name)
                          (when (member name constructors :key #'first)
                            (error 'duplicate-name :option option)))
                        (add-standard (name)
                          (check-constructor-not-defined name)
                          (push (list name) constructors))
                        (add-boa (name lambda-list)
                          (check-constructor-not-defined name)
                          (push (list name lambda-list) constructors)))
                 (cond ((or (not (listp option)) (null (rest option)))
                        ;; :constructor or (:constructor)
                        ;; Generate the default standard constructor.
                        (add-standard default-name))
                       ((null (cddr option))
                        ;; (:constructor <name>)
                        (unless (symbolp (second option))
                          (error 'name-must-be-symbol :option option :datum (second option)))
                        (when (second option)
                          (add-standard (second option))))
                       ((null (cdddr option))
                        ;; (:constructor <name> <arglist>)
                        (unless (symbolp (second option))
                          (error 'name-must-be-symbol :option option :datum (second option)))
                        (when (second option)
                          (add-boa (second option) (third option))))
                       (t
                        (error 'malformed-option :option option)))))
             (reverse constructors)))
          (t
           ;; A single default standard constructor
           (list (list default-name))))))

(defun parse-copier/predicate-options (options option-name default-name)
  (let ((applicable-options (get-options options option-name)))
    (cond (applicable-options
           (let ((result '()))
               (dolist (option applicable-options)
                 (flet ((add (name)
                          (when (member name result)
                            (error 'duplicate-name :option option))
                          (push name result)))
                 (cond ((or (not (listp option)) (endp (rest option)))
                        ;; <opt> or (<opt>)
                        (add default-name))
                       ((endp (cddr option))
                        ;; (<opt> name)
                        (unless (symbolp (second option))
                          (error 'name-must-be-symbol :option option :datum (second option)))
                        (when (second option)
                          (add (second option))))
                       (t
                        (error 'malformed-option :option option)))))
             (values (reverse result) t)))
          (t
           (values (list default-name) nil)))))

(defun parse-copier-options (options name)
  (parse-copier/predicate-options options :copier (symbolicate "COPY-" name)))

(defun parse-predicate-options (options name)
  (multiple-value-bind (predicates suppliedp)
      (parse-copier/predicate-options options :predicate (symbolicate name "-P"))
    (cond ((or (not (get-singular-option options :type))
               (get-singular-option options :named))
           ;; Structure is named, allow predicate options.
           predicates)
          (t
           ;; Structure is not named, predicates not permitted.
           (when (and suppliedp predicates)
             (error 'predicate-requires-named-structure))
           '()))))

(defun parse-named-option (options)
  (let ((option (get-singular-option options :named)))
    (cond ((not option)
           ;; "If no :type is supplied, then the structure is always named."
           (if (get-singular-option options :type)
               nil
               t))
          ((not (listp option))
           ;; :named
           t)
          (t
           (error 'malformed-option :option option)))))

(defun parse-type-option (options)
  (let ((option (get-singular-option options :type)))
    (cond ((not option)
           nil)
          ((and (listp option)
                (rest option)
                (endp (cddr option))
                (second option))
           ;; (:type <type>), where <type> is non-NIL.
           ;; This non-NIL requirement is required for :NAMED options to be
           ;; parsed properly. NIL is an invalid type to use here anyway.
           (second option))
          (t
           (error 'malformed-option :option option)))))

(defun parse-initial-offset-option (options)
  (let ((option (get-singular-option options :initial-offset)))
    (cond ((not option)
           nil)
          ((and (listp option)
                (rest option)
                (endp (cddr option))
                (integerp (second option))
                (not (minusp (second option))))
           ;; (:initial-offset <non-negative-integer>)
           (when (not (get-singular-option options :type))
             (error 'initial-offset-requires-typed-structure))
           (second option))
          (t
           (error 'malformed-option :option option)))))

(defun parse-print-object-option (options)
  (let ((print-object (get-singular-option options :print-object))
        (print-function (get-singular-option options :print-function)))
    (when (and (or print-object print-function)
               (get-singular-option options :type))
      (error 'print-object/function-requires-structure-object-structure))
    (when (and print-object print-function)
      (error 'print-object-and-print-function-mutually-exclusive))
    ;; Canonicalize :print-function down to :print-object
    (when print-function
      (cond ((endp (rest print-function))
             ;; (:print-function)
             (setf print-object '(:print-object)))
            ((and (endp (cddr print-function))
                  (second print-function))
             ;; (:print-function printer-name)
             ;; Portability note:
             ;; It is assumed that the <<current-print-depth>> is always zero
             ;; and that *print-level* is updated as printing descends recursively.
             (setf print-object
                   `(:print-object (lambda (object stream)
                                     (funcall (function ,(second print-function))
                                              object stream 0)))))
            (t
             (error 'malformed-option :option print-function))))
    (cond ((not print-object)
           ;; Not specified, no printing behaviour specified.
           nil)
          ((endp (rest print-object))
           ;; Use the default #S structure printer, overriding any inherited
           ;; printing behaviour.
           'print-structure)
          ((and (endp (cddr print-object))
                (second print-object))
           (second print-object))
          (t
           (error 'malformed-option :option print-object)))))

(defun check-duplicate-slot-option (slot-description slot-options option-name)
  (loop with seen = nil
        for (name value) on slot-options by #'cddr
        when (eql name option-name)
          do (if seen
                 (error 'duplicate-slot-option
                        :option-name option-name
                        :slot-description slot-description)
                 (setf seen t))))

(defun parse-slot-description (slot-description conc-name)
  (unless (or (symbolp slot-description)
              (listp slot-description))
    (error 'malformed-slot-description :slot-description slot-description))
  (when (symbolp slot-description)
    (setf slot-description (list slot-description)))
  (let ((slot-name (first slot-description))
        (slot-options (cddr slot-description)))
    (unless (symbolp slot-name)
      (error 'slot-name-must-be-symbol
             :datum slot-name))
    (unless (evenp (length slot-options))
      (error 'malformed-slot-description :slot-description slot-description))
    (check-duplicate-slot-option slot-description slot-options :type)
    (check-duplicate-slot-option slot-description slot-options :read-only)
    (make-instance 'slot-description
                   :name slot-name
                   :accessor-name (if conc-name
                                      (symbolicate conc-name slot-name)
                                      slot-name)
                   :initform-p (not (endp (rest slot-description)))
                   :initform (second slot-description)
                   :type (getf slot-options :type 't)
                   ;; Canonicalize this to T/NIL.
                   :read-only (if (getf slot-options :read-only 'nil) t nil))))

(defun parse-slot-descriptions (slot-descriptions conc-name)
  (loop for slot in slot-descriptions
        collect (parse-slot-description slot conc-name)))

(defun parse-include-option (options conc-name)
  (let ((option (get-singular-option options :include)))
    (cond ((not option)
           (values nil '()))
          ((and (listp option)
                (cdr option)
                (symbolp (second option)))
           ;; (:include included-structure-name {slot-description}*)
           (when (null (second option))
             (error 'included-structure-name-must-name-structure-type
                    :structure-name (second option)))
           (values (second option)
                   (parse-slot-descriptions (cddr option) conc-name)))
          (t
           (error 'malformed-option option)))))

(defun separate-documentation-and-slots (documentation-and-slots)
  (if (stringp (first documentation-and-slots))
      (values (first documentation-and-slots)
              (rest documentation-and-slots))
      (values nil documentation-and-slots)))

(defun parse-defstruct (name-and-options documentation-and-slots)
  (multiple-value-bind (documentation slots)
      (separate-documentation-and-slots documentation-and-slots)
    (multiple-value-bind (name options)
        (if (consp name-and-options)
            (values (first name-and-options) (rest name-and-options))
            (values name-and-options '()))
      (unless (symbolp name)
        (error 'structure-name-must-be-non-nil-symbol :datum name))
      (check-defstruct-option-names-valid options)
      (let* ((conc-name (parse-conc-name-option options name))
             (direct-slots (parse-slot-descriptions slots conc-name)))
        (multiple-value-bind (included-structure-name included-slots)
            (parse-include-option options conc-name)
          ;; Make sure there are no duplicate slot names
          (let ((slot-names '()))
            (dolist (slot included-slots)
              (when (find (slot-name slot) slot-names :test #'string=)
                (error 'duplicate-slot :slot-name (slot-name slot)))
              (push (slot-name slot) slot-names))
            ;; Also checks against the included names.
            (dolist (slot direct-slots)
              (when (find (slot-name slot) slot-names :test #'string=)
                (error 'duplicate-slot :slot-name (slot-name slot)))
              (push (slot-name slot) slot-names)))
          (let ((constructors (parse-constructor-options options name))
                (copiers (parse-copier-options options name))
                (predicates (parse-predicate-options options name))
                (named (parse-named-option options))
                (type (parse-type-option options))
                (initial-offset (parse-initial-offset-option options))
                (print-object (parse-print-object-option options)))
            (if type
                (make-instance 'defstruct-typed-description
                               :name name
                               :documentation documentation
                               :conc-name conc-name
                               :constructors constructors
                               :copiers copiers
                               :predicates predicates
                               :named named
                               :type type
                               :initial-offset initial-offset
                               :included-structure-name included-structure-name
                               :included-slots included-slots
                               :direct-slots direct-slots)
                (make-instance 'defstruct-object-description
                               :name name
                               :documentation documentation
                               :conc-name conc-name
                               :constructors constructors
                               :copiers copiers
                               :predicates predicates
                               :print-object print-object
                               :included-structure-name included-structure-name
                               :included-slots included-slots
                               :direct-slots direct-slots))))))))
