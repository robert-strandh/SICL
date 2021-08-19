(cl:in-package #:sicl-clos)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Functions to canonicalize certain parts of the defclass macro
;;;
;;; This file contains very simple code that can be executed in a host
;;; environment, which makes it easier to use during bootstrapping.
;;; There are no direct references to any SICL-specific features nor
;;; to any MOP features.

;;; The DEFCLASS macro.  The AMOP is inconsistent with respect to the
;;; CLHS.  For instance, it requires the arguments to ENSURE-CLASS to
;;; appear in the same order as they appear in the DEFCLASS form, but
;;; that should not matter since they are not evaluated.  The CLHS
;;; explicitly allows for DEFCLASS to support additional class
;;; options.

;;; The AMOP says that the NAME argument to DEFCLASS becomes the first
;;; argument to ENSURE-CLASS.  Nothing particular here.
;;;
;;; The AMOP says that the SUPERCLASS-NAMES argument to DEFCLASS
;;; becomes the value of the :DIRECT-SUPERCLASSES argument to
;;; ENSURE-CLASS.  The CLHS requires that the DIRECT-SUPERCLASSES
;;; argument to DEFCLASS be a proper list of non-NIL symbols.

(defun canonicalize-direct-superclass-name (class-name)
  (unless (and (symbolp class-name)
               (not (null class-name)))
    (error 'class-name-must-be-non-nil-symbol
           :name 'defclass
           :datum class-name))
  `',class-name)

(defun canonicalize-direct-superclass-names (direct-superclass-names)
  (unless (cleavir-code-utilities:proper-list-p direct-superclass-names)
    (error 'superclass-list-must-be-proper-list
           :name 'defclass
           :datum direct-superclass-names))
  `(list ,@(loop for name in direct-superclass-names
                 collect (canonicalize-direct-superclass-name name))))

(declaim (notinline make-initfunction))

(defun make-initfunction (form)
  `(lambda () ,form))

(defun check-slot-spec-non-empty-proper-list (direct-slot-spec)
  (unless (and (cleavir-code-utilities:proper-list-p direct-slot-spec)
               (consp direct-slot-spec))
    (error 'malformed-slot-spec
           :name 'defclass
           :datum direct-slot-spec)))

(defun check-slot-spec-name-is-symbol (direct-slot-spec)
  (unless (symbolp (car direct-slot-spec))
    (error 'illegal-slot-name
           :name 'defclass
           :datum (car direct-slot-spec))))

(defun check-slot-options-even-length (direct-slot-spec)
  (unless (evenp (length (cdr direct-slot-spec)))
    (error 'slot-options-must-be-even
           :name 'defclass
           :datum direct-slot-spec)))

(defun populate-table-with-slot-options (table slot-options)
  (loop for (name value) on slot-options by #'cddr
        do (unless (symbolp name)
             (error 'slot-option-name-must-be-symbol
                    :name 'defclass
                    :datum name))
           (push value (gethash name table '()))))

(defun process-initform-option (table direct-slot-spec)
  (multiple-value-bind (value flag) (gethash :initform table)
    (if flag
        (progn (unless (= (length value) 1)
                 (error 'multiple-initform-options-not-permitted
                        :datum direct-slot-spec))
               (remhash :initform table)
               `(:initform ',(car value)
                 :initfunction ,(make-initfunction (car value))))
        '())))

(defun process-initarg-options (table)
  (multiple-value-bind (value flag)
      (gethash :initarg table)
    (if flag
        (progn (remhash :initarg table)
               `(:initargs ',(reverse value)))
        '())))

(defun split-accessors (table)
  (multiple-value-bind (value flag) (gethash :accessor table)
    (when flag
      (loop for accessor in value
            do (push accessor (gethash :reader table '()))
               (push `(setf ,accessor) (gethash :writer table '())))
      (remhash :accessor table))))

(defun process-readers (table)
  (multiple-value-bind (value flag)
      (gethash :reader table)
    (if flag
        (progn (remhash :reader table)
               `(:readers ',(reverse value)))
        '())))

(defun process-writers (table)
  (multiple-value-bind (value flag)
      (gethash :writer table)
    (if flag
        (progn (remhash :writer table)
               `(:writers ',(reverse value)))
        '())))

(defun process-documentation (table direct-slot-spec)
  (multiple-value-bind (value flag) (gethash :documentation table)
    (if flag
        (progn (unless (= (length value) 1)
                 (error 'multiple-documentation-options-not-permitted
                        :datum direct-slot-spec))
               (unless (stringp (car value))
                 (error 'slot-documentation-option-must-be-string
                        :datum (car value)))
               (remhash :documentation table)
               `(:documentation ,(car value)))
        '())))

(defun process-allocation (table direct-slot-spec)
  (multiple-value-bind (value flag) (gethash :allocation table)
    (if flag
        (progn (unless (= (length value) 1)
                 (error 'multiple-allocation-options-not-permitted
                        :datum direct-slot-spec))
               (remhash :allocation table)
               `(:allocation ,(car value)))
        '())))

(defun process-type (table direct-slot-spec)
  (multiple-value-bind (value flag)
      (gethash :type table)
    (if flag
        (progn (unless (= (length value) 1)
                 (error 'multiple-type-options-not-permitted
                        :datum direct-slot-spec))
               (remhash :type table)
               `(:type ',(car value))))))

(defun canonicalize-direct-slot-spec (direct-slot-spec)
  ;; A direct-slot-spec can be a symbol which is then the
  ;; name of the slot.
  (if (symbolp direct-slot-spec)
      `(:name ',direct-slot-spec)
      (progn
        ;; If the direct-slot-spec is not a symbol, it must
        ;; be a non-empty proper list.
        (check-slot-spec-non-empty-proper-list direct-slot-spec)
        ;; In that case, the first element must be the name
        ;; of the slot, which must be a symbol.
        (check-slot-spec-name-is-symbol direct-slot-spec)
        ;; The slot options must be a list of even length
        ;; where every other element is the name of a slot
        ;; option and every other element is the value of
        ;; the slot option.
        (check-slot-options-even-length direct-slot-spec)
        (let ((ht (make-hash-table :test #'eq)))
          (populate-table-with-slot-options ht (cdr direct-slot-spec))
          (let ((result `(:name ',(car direct-slot-spec))))
            (flet ((add (option)
                     (setf result (append result option))))
              (add (process-initform-option ht direct-slot-spec))
              (add (process-initarg-options ht))
              (split-accessors ht)
              (add (process-readers ht))
              (add (process-writers ht))
              (add (process-documentation ht direct-slot-spec))
              (add (process-allocation ht direct-slot-spec))
              (add (process-type ht direct-slot-spec))
              ;; Add remaining options without checking.
              (maphash (lambda (name value)
                         (add (list name (reverse value))))
                       ht))
            `(list ,@result))))))

(defun canonicalize-direct-slot-specs (direct-slot-specs)
  (when (not (cleavir-code-utilities:proper-list-p direct-slot-specs))
    (error 'malformed-slots-list
           :name 'defclass
           :datum direct-slot-specs))
  `(list ,@(loop for spec in direct-slot-specs
                 collect (canonicalize-direct-slot-spec spec))))

;;; Canonicalize a single default initarg.  Recall that a
;;; canonicalized default initarg is a list of three elements: The
;;; symbol naming the initarg, the form to be used for to compute the
;;; initial value, and a thunk that, when called, returns the value of
;;; the form.
(defun canonicalize-default-initarg (name form)
  (unless (symbolp name)
    (error 'default-initarg-name-must-be-symbol
           :datum name))
  `(list ,name ',form (lambda () ,form)))

;;; Canonicalize the :DEFAULT-INITARGS class option.
(defun canonicalize-default-initargs (initargs)
  (unless (cleavir-code-utilities:proper-list-p initargs)
    (error 'malformed-default-initargs
           :datum `(:default-initargs ,@initargs)))
  (unless (evenp (length initargs))
    (error 'malformed-default-initargs
           :datum `(:default-initargs ,@initargs)))
  `(list ,@(loop for (name value) on initargs by #'cddr
                 collect (canonicalize-default-initarg name value))))

(defun check-options-non-empty (options)
  ;; Check that each option is a non-empty list
  (let ((potential-malformed-option (member-if-not #'consp options)))
    (unless (null potential-malformed-option)
      (error 'class-option-must-be-non-empty-list
             :name 'defclass
             :datum (car potential-malformed-option)))))

(defun check-option-names (options)
  ;; Check that the name of each option is a symbol
  (let ((potential-malformed-option (member-if-not #'symbolp options :key #'car)))
    (unless (null potential-malformed-option)
      (error 'class-option-name-must-be-symbol
             :name 'defclass
             :datum (car potential-malformed-option)))))

(defun check-no-duplicate-option-names (options)
  ;; Check that there are no duplicate option names
  (let ((reduced-options (remove-duplicates options :key #'car :test #'eq)))
    (when (< (length reduced-options) (length options))
      (loop for option in reduced-options
            do (when (> (count (car option) options
                               :key #'car :test #'eq) 1)
                 (error 'duplicate-class-option-not-allowed
                        :name 'defclass
                        :datum (car option)))))))

;;; Make sure each class options is well formed, and check that a
;;; class option appears at most once.  Return a list of class
;;; options, including the corresponding keyword argument, to be
;;; spliced into the call to ENSURE-CLASS.
(defun canonicalize-defclass-options (options)
  (check-options-non-empty options)
  (check-option-names options)
  (check-no-duplicate-option-names options)
  (let ((result '()))
    (loop for option in options
          do (case (car option)
               (:default-initargs
                (setf result
                      (append result
                              `(:direct-default-initargs
                                ,(canonicalize-default-initargs (cdr option))))))
               (:documentation
                (unless (null (cddr option))
                  (error 'malformed-documentation-option
                         :name 'defclass
                         :datum option))
                (setf result
                      (append result `(:documentation ,(cadr option)))))
               (:metaclass
                (unless (null (cddr option))
                  (error 'malformed-metaclass-option
                         :name 'defclass
                         :datum option))
                (setf result
                      (append result `(:metaclass ',(cadr option)))))
               (t
                (setf result
                      (append result `(,(car option) ,(cdr option)))))))
    result))

(defun defclass-expander
    (name superclass-names slot-specifiers options environment)
  (let* ((canonicalized-superclass-names
           (canonicalize-direct-superclass-names superclass-names))
         (options (canonicalize-defclass-options options))
         (quoted-metaclass-name (getf options :metaclass 'standard-class))
         (env-var (gensym)))
    `(progn
       (eval-when (:compile-toplevel)
         (let* ((,env-var (sicl-environment:global-environment ,environment)))
           (setf (sicl-environment:class-description ,env-var ',name)
                 (make-instance 'sicl-environment:class-description
                   :name ',name
                   :superclass-names ,canonicalized-superclass-names
                   :metaclass-name ,quoted-metaclass-name))))
       (eval-when (:load-toplevel :execute)
         (ensure-class ',name
                       :name ',name
                       :direct-superclasses
                       ,canonicalized-superclass-names
                       :direct-slots
                       ,(canonicalize-direct-slot-specs slot-specifiers)
                       ,@options)))))
