(cl:in-package #:sicl-clos)

(define-condition class-name-must-be-non-nil-symbol
    (program-error)
  ((%name :initarg :name :reader name))
  (:report (lambda (condition stream)
             (format stream
                     "A class name must be a non-nil symbol, but~@
                      ~s was found."
                     (name condition)))))

(define-condition attempt-to-add-existing-subclass
    (error)
  ((%subclass :initarg :subclass :reader subclass)
   (%superclass :initarg :superclass :reader superclass))
  (:report (lambda (condition stream)
             (format stream
                     "Attempt to add existing subclass ~s as a subclass of ~s."
                     (subclass condition)
                     (superclass condition)))))

(define-condition readers-must-be-proper-list
    (type-error)
  ((%slot-definition :initarg :slot-definition :reader slot-definition)
   (%readers :initarg :readers :reader readers))
  (:report (lambda (condition stream)
             (format stream
                     "The keyword argument :READERS when supplied as~@
                      initialization of a slot definition, must be~@
                      a proper list, but the following was found instead:~@
                      ~s."
                     (readers condition))))
  (:default-initargs :type 'list))

(define-condition malformed-documentation-option
    (program-error)
  ((%documentation-option
    :initarg :documentation-option :reader documentation-option))
  (:report (lambda (condition stream)
             (format stream
                     "A documentation option must have the form~@
                      (:documentation <name>), but~@
                      ~s was found."
                     (documentation-option condition)))))

(define-condition attempt-to-access-prototype-of-unfinalized-class
    (error)
  ((%offending-class :initarg :offending-class :reader offending-class))
  (:report (lambda (condition stream)
             (format stream
                     "Attempt to access the prototype of the class ~s~@
                      which has not yet been finalized."
                     (offending-class condition)))))

(define-condition attempt-to-access-precedence-list-of-unfinalized-class
    (error)
  ((%offending-class :initarg :offending-class :reader offending-class))
  (:report (lambda (condition stream)
             (format stream
                     "Attempt to access the precedence list of the class ~s~@
                      which has not yet been finalized."
                     (offending-class condition)))))

(define-condition attempt-to-access-default-initargs-of-unfinalized-class
    (error)
  ((%offending-class :initarg :offending-class :reader offending-class))
  (:report (lambda (condition stream)
             (format stream
                     "Attempt to access default initargs of the class ~s~@
                      which has not yet been finalized."
                     (offending-class condition)))))

(define-condition attempt-to-access-effective-slots-of-unfinalized-class
    (error)
  ((%offending-class :initarg :offending-class :reader offending-class))
  (:report (lambda (condition stream)
             (format stream
                     "Attempt to access effective slots of the class ~s~@
                      which has not yet been finalized."
                     (offending-class condition)))))

(define-condition attempt-to-access-precedence-list-of-forward-referenced-class
    (error)
  ((%offending-class :initarg :offending-class :reader offending-class)))

(define-condition attempt-to-access-default-initargs-of-forward-referenced-class
    (error)
  ((%offending-class :initarg :offending-class :reader offending-class)))

(define-condition attempt-to-access-effective-slots-of-forward-referenced-class
    (error)
  ((%offending-class :initarg :offending-class :reader offending-class)))

(define-condition malformed-specializer
    (error)
  ((%specializer :initarg :specializer :reader specializer))
  (:report (lambda (condition stream)
             (format stream
                     "A specializer must be either a class name~@
                      or an EQL specializer, but the following was found:~%
                      ~s"
                     (specializer condition)))))

(define-condition no-such-class-name
    (error)
  ((%name :initarg :name :reader name))
  (:report (lambda (condition stream)
             (format stream
                     "There is no class with the name ~s."
                     (name condition)))))

(define-condition slot-definition-argument-must-be-supplied
    (error)
  ()
  (:report (lambda (condition stream)
             (declare (ignore condition))
             (format stream "The slot-definition argument must be supplied."))))

(define-condition unable-to-compute-class-precedence-list
    (error)
  ((%offending-class :initarg :offending-class :reader offending-class))
  (:report (lambda (condition stream)
              (format stream
                      "Unable to compute the class precedence list of the class ~s"
                      (offending-class condition)))))

(define-condition option-or-method-must-be-non-empty-list
    (error)
  ((%expression :initarg :expression :reader expressions))
  (:report (lambda (condition stream)
             (format stream
                     "Option or method must be a non-empty list,~@
                      but the following expression was found instead~@
                      ~s"
                     (expression condition)))))

(define-condition method-already-associated-with-a-generic-function
    (error)
  ((%method-to-add :initarg :method-to-add :reader method-to-add)
   (%its-generic-function :initarg :its-generic-function
                          :reader its-generic-function))
  (:report (lambda (condition stream)
             (format stream
                     "An attempt was made to add the method:~@
                      ~s~@
                      to a generic function, but that method is~@
                      already associated with the generic function:~@
                      ~s"
                     (method-to-add conditon)
                     (its-generic-function condition)))))

(define-condition direct-default-initargs-must-be-a-proper-list
    (error)
  ((%initargs :initarg :initargs :reader initargs)))

(define-condition direct-default-initarg-must-be-a-proper-list
    (error)
  ((%initarg :initarg :initarg :reader initarg)))

(define-condition direct-default-initarg-must-be-a-list-of-three-elements
    (error)
  ((%initarg :initarg :initarg :reader initarg)))

(define-condition name-of-direct-default-initarg-must-be-a-symbol
    (error)
  ((%initarg :initarg :initarg :reader initarg)
   (%name :initarg :name :reader name)))

(define-condition third-element-of-direct-default-initarg-must-be-a-thunk
    (error)
  ((%initarg :initarg :initarg :reader initarg)
   (%initfunction :initarg :initfunction :reader initfunction)))

(define-condition direct-superclasses-must-be-proper-list
    (error)
  ((%superclasses :initarg :superclasses :reader superclasses)))

(define-condition superclass-must-be-a-class-metaobject
    (error)
  ((%superclass :initarg :superclass :reader superclass)))

(define-condition direct-superclass-must-be-a-class-metaobject-or-a-symbol
    (error)
  ((%superclass :initarg :superclass :reader superclass)))

(define-condition superclass-not-valid-for-class
    (error)
  ((%superclass :initarg :superclass :reader superclass)))

(define-condition direct-slots-must-be-proper-list
    (error)
  ((%direct-lost :initarg :direct-lost :reader direct-lost)))

(define-condition qualifier-must-be-proper-list
    (error)
  ((%qualifier :initarg :qualifier :reader qualifier)))

(define-condition argument-precedence-order-must-be-proper-list
    (error)
  ((%order :initarg :order :reader argument-precedence-order)))

(define-condition no-such-generic-function-class
    (error)
  ((%class-name :initarg :class-name :reader generic-function-class-name)))

(define-condition no-such-method-class
    (error)
  ((%class-name :initarg :class-name :reader generic-function-class-name)))

(define-condition generic-function-class-must-be-class-or-name
    (error)
  ((%object :initarg :object :reader object)))

(define-condition name-refers-to-a-special-operator
    (error)
  ((%name :initarg :name :reader name)))

(define-condition name-refers-to-a-macro
    (error)
  ((%name :initarg :name :reader name)))

(define-condition invalid-function-name
    (error)
  ((%name :initarg :name :reader name)))

(define-condition multiple-allocation-options-not-permitted (error)
  ((%slot-specifier :initarg :slot-specifier :reader slot-specifier))
  (:report (lambda (condition stream)
             (format stream
                     "A slot can not have multiple :ALLOCATION options.~@
                      ~s was found."
                     (slot-specifier condition)))))

(define-condition multiple-documentation-options-not-permitted (error)
  ((%slot-specifier :initarg :slot-specifier :reader slot-specifier))
  (:report (lambda (condition stream)
             (format stream
                     "A slot can not have multiple :DOCUMENTATION options.~@
                      ~s was found."
                     (slot-specifier condition)))))

(define-condition multiple-initform-options-not-permitted (error)
  ((%slot-specifier :initarg :slot-specifier :reader slot-specifier))
  (:report (lambda (condition stream)
             (format stream
                     "A slot can not have multiple :INITFORM options.~@
                      ~s was found."
                     (slot-specifier condition)))))

(define-condition multiple-type-options-not-permitted (error)
  ((%slot-specifier :initarg :slot-specifier :reader slot-specifier))
  (:report (lambda (condition stream)
             (format stream
                     "A slot can not have multiple :TYPE options.~@
                      ~s was found."
                     (slot-specifier condition)))))

(define-condition slot-documentation-option-must-be-string (type-error)
  ()
  (:report (lambda (condition stream)
             (format stream
                     "The :DOCUMENTATION option of a slot~@
                      must have a string argument, but~@
                      ~s was found."
                     (type-error-datum condition))))
  (:default-initargs :type 'string))

(define-condition superclass-list-must-be-proper-list (type-error)
  ()
  (:report (lambda (condition stream)
             (format stream
                     "The list of superclasses must be a proper list,~@
                      but ~s was found."
                     (type-error-datum condition))))
  (:default-initargs :type 'list))

(define-condition metaclass-option-once (program-error)
  ((%options :initarg :option :reader option))
  (:report (lambda (condition stream)
             (format stream
                     "The metaclass option can appear only once in the~@
                      list of class options, but a second such option:~@
                      ~s was found."
                     (option condition)))))

(define-condition slot-options-must-be-even (program-error)
  ((%options :initarg :options :reader options))
  (:report (lambda (condition stream)
             (format stream
                     "There must be an even number of elements in.~@
                      the list of slot options.~@
                      ~s was found."
                     (options condition)))))

(define-condition malformed-metaclass-option (program-error)
  ((%option :initarg :option :reader option))
  (:report (lambda (condition stream)
             (format stream
                     "Malformed :METACLASS option:~@
                      ~s was found."
                     (option condition)))))

(define-condition malformed-default-initargs-option (program-error)
  ((%option :initarg :option :reader option))
  (:report (lambda (condition stream)
             (format stream
                     "Malformed :DEFAULT-INITARGS option:~@
                      ~s was found."
                     (option condition)))))

(define-condition malformed-slot-spec (program-error)
  ((%slot-spec :initarg :slot-spec :reader slot-spec))
  (:report (lambda (conditoin stream)
             (format stream
                     "Malformed slot specification.~@
                      ~s was found."
                     (slot-spec condition)))))

(define-condition documentation-option-once (program-error)
  ((%option :initarg :option :reader option))
  (:report (lambda (condition stream)
             (format stream
                     "The documentation option can appear only once in the~@
                      list of class options, but a second such option:~@
                      ~s was found."
                     (option condition)))))

(define-condition unknown-class-option (program-error)
  ((%option :initarg :option :reader option))
  (:report (lambda (condition stream)
             (format stream
                     "A class option is either ~@
                     :DEFAULT-INITARGS, :DOCUMENTATION, or :METACLASS, but~@
                     ~s was found."
                     (option condition)))))

(define-condition default-initargs-option-once (program-error)
  ((%option :initarg :option :reader option))
  (:report (lambda (condition stream)
             (format stream
                     "The default-initargs option can appear only once in the~@
                      list of class options, but a second such option:~@
                      ~s was found."
                     (option condition)))))

(define-condition duplicate-class-option-not-allowed (program-error)
  ((%option :initarg :option :reader option))
  (:report (lambda (condition stream)
             (format stream
                     "This class option can occur at most once, in the~@
                      list of class options, but a second such option:~@
                      ~s was found."
                     (option condition)))))

(define-condition class-option-name-must-be-symbol (program-error)
  ((%option-name :initarg :option-name :reader option-name))
  (:report (lambda (condition stream)
             (format stream
                     "A class option name must be a symbol, but~@
                      ~s was found."
                     (option-name condition)))))

(define-condition :class-option-must-be-non-empty-list (program-error)
  ((%option :initarg :option :reader option))
  (:report (lambda (condition stream)
             (format stream
                     "A class option must be a a non-empty list, but~@
                      ~s was found."
                     (option condition)))))

(define-condition slot-option-name-must-be-symbol (program-error)
  ((%option-name :initarg :option-name :reader option-name))
  (:report (lambda (condition stream)
             (format stream
                     "A slot option name must be a symbol, but~@
                      ~s was found."
                     (option-name condition)))))

(define-condition illegal-slot-name (program-error)
  ((%slot-name :initarg :slot-name :reader slot-name))
  (:report (lambda (condition stream)
             (format stream
                     "Illegal slot name:~@
                      ~s was found."
                     (slot-name condition)))))

(define-condition malformed-slot-list (program-error)
  ((%slot-list :initarg :slot-list :reader slot-list))
  (:report (lambda (condition stream)
             (format stream
                     "The direct-slots must be a proper list of~@
                      slot specs, but~@
                      ~s was found."
                     (slot-list condition)))))
