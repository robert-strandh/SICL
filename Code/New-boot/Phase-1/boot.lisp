(cl:in-package #:sicl-new-boot-phase-1)

(eval-when (:compile-toplevel) (sb:enable-parcl-symbols c1))

(defun create-reader-generic-function (name)
  (let* ((sample-generic-function #'print-object)
         (method-combination
           (closer-mop:find-method-combination
            sample-generic-function 'standard '())))
    (make-instance 'standard-generic-function
      :lambda-list '(object)
      :method-combination method-combination
      :name name)))

(defun define-readers (c1 e1 slot-name readers class)
  (loop for reader in readers
        do (let* ((generic-function
                    (if (clo:fboundp c1 e1 reader)
                        (clo:fdefinition c1 e1 reader)
                        ;; We must create it.
                        (let ((generic-function
                                (create-reader-generic-function reader)))
                          (setf (clo:fdefinition c1 e1 reader)
                                generic-function)
                          generic-function)))
                  (method
                    (make-instance 'standard-method
                      :lambda-list '(object)
                      :specializers (list class)
                      :function (lambda (arguments next-methods)
                                  (declare (ignore next-methods))
                                  (slot-value
                                   (first arguments) slot-name)))))
             (add-method generic-function method))))

(defun create-writer-generic-function (name)
  (let* ((sample-generic-function #'print-object)
         (method-combination
           (closer-mop:find-method-combination
            sample-generic-function 'standard '())))
    (make-instance 'standard-generic-function
      :lambda-list '(new-value object)
      :method-combination method-combination
      :name name)))

(defun define-writers (c1 e1 slot-name writers class)
  (loop for writer in writers
        do (let* ((generic-function
                    (if (clo:fboundp c1 e1 writer)
                        (clo:fdefinition c1 e1 writer)
                        ;; We must create it.
                        (let ((generic-function
                                (create-writer-generic-function writer)))
                          (setf (clo:fdefinition c1 e1 writer)
                                generic-function)
                          generic-function)))
                  (method
                    (make-instance 'standard-method
                      :lambda-list '(new-value object)
                      :specializers (list (find-class 't) class)
                      :function (lambda (arguments next-methods)
                                  (declare (ignore next-methods))
                                  (setf (slot-value
                                         (second arguments) slot-name)
                                        (first arguments))))))
             (add-method generic-function method))))

(defun define-ensure-class (c1 e1)
  (setf (clo:fdefinition c1 e1 'ensure-class)
        (lambda (name
                 &key
                   direct-default-initargs
                   direct-superclasses
                   direct-slots
                 &allow-other-keys)
          (let* ((new-slots
                   ;; We must remove the :READERS and :WRITERS
                   ;; properties, because otherwise, they would be
                   ;; created as host methods on host generic
                   ;; functions.
                   (loop for direct-slot in direct-slots
                         for copy = (copy-list direct-slot)
                         collect (progn (remf copy :readers)
                                        (remf copy :writers)
                                        copy)))
                 (new-superclasses
                   (loop for direct-superclass in direct-superclasses
                         collect
                         (clo:find-class
                          c1 e1 direct-superclass)))
                 (result
                   (make-instance 'closer-mop:funcallable-standard-class
                     :name (make-symbol (symbol-name name))
                     :direct-default-initargs direct-default-initargs
                     :direct-slots new-slots
                     :direct-superclasses new-superclasses)))
            (loop for direct-slot in direct-slots
                  for slot-name = (getf direct-slot :name)
                  for readers = (getf direct-slot :readers)
                  for writers = (getf direct-slot :writers)
                  do (define-readers
                         c1 e1 slot-name readers result)
                     (define-writers
                         c1 e1 slot-name writers result))
            (setf (clo:find-class c1 e1 name)
                result)))))

(defun define-typep (c1 e1)
  (setf (clo:fdefinition c1 e1 'typep)
        (lambda (object type-specifier)
          (cond ((eq type-specifier 'class)
                 (typep object
                        (clo:find-class c1 e1 'class)))
                ((eq type-specifier 'method-combination)
                 (typep object
                        (clo:find-class c1 e1 'method-combination)))
                ((eq type-specifier @clostrophilia:direct-slot-definition)
                 (or (typep object
                            (clo:find-class
                             c1 e1
                             @clostrophilia:direct-slot-definition))
                     (progn
                       (format *trace-output*
                               "Assuming ~s is a direct slot-definition~%"
                               object)
                       t)))
                ((eq type-specifier @clostrophilia:standard-accessor-method)
                 (typep object
                        (clo:find-class
                         c1 e1
                         @clostrophilia:standard-accessor-method)))
                ((eq type-specifier 'null)
                 (null object))
                ((eq type-specifier 'string)
                 (stringp object))
                ((symbolp object)
                 (typep object type-specifier))
                ((typep object '(cons (eql setf) (cons symbol null)))
                 (typep object type-specifier))
                (t
                 (format *trace-output*
                         "Don't know whether ~s is of type ~s~%"
                         object type-specifier)
                 (break))))))

(defun boot (boot)
  (format *trace-output* "**************** Phase 1~%")
  (let* ((c1 (make-instance 'client))
         (w1 (create-environment c1))
         (e1 (trucler:global-environment c1 w1))
         (env:*client* c1)
         (env:*environment* e1))
    (sb:define-package-functions c1 e1)
    (setf (sb:e1 boot) e1)
    (reinitialize-instance c1
      :environment e1)
    (sb:define-backquote-macros c1 e1)
    (import-from-host c1 e1)
    (setf (clo:fdefinition c1 e1 'funcall)
          (lambda (function-or-name &rest arguments)
            (apply #'funcall
                   (if (functionp function-or-name)
                       function-or-name
                       (clo:fdefinition
                        c1 e1 function-or-name))
                   arguments)))
    (setf (clo:fdefinition c1 e1 'ensure-method)
          #'ensure-method)
    (setf (clo:fdefinition c1 e1 'closer-mop:method-function)
          #'closer-mop:method-function)
    (sb:import-khazern c1 e1)
    (clo:make-variable
     c1 e1 '*package* (find-package '#:common-lisp-user))
    (sb:fill-environment c1 e1)
    (sb:ensure-asdf-system c1 w1 "sicl-primop")
    (setf (clo:fdefinition c1 e1 @sicl-primop:primop)
          #'sb:primop)
    (sb:ensure-asdf-system c1 w1 "sicl-environment-clostrum-package")
    (sb:ensure-asdf-system c1 w1 "sicl-environment-run-time-package")
    (sb:ensure-asdf-system c1 w1 "sicl-environment-packages-intrinsic")
    (define-make-instance c1 e1)
    (setf (clo:find-class c1 e1 'package)
          (find-class 'parcl-low-class:package))
    (define-ensure-class c1 e1)
    (sb:define-client-and-environment-variables c1 e1)
    (sb:define-environment-functions c1 e1)
    (sb:ensure-asdf-system
     c1 w1 "clostrophilia-package")
    ;;; FIXME: Define these functions by loading SICL-specific code
    (setf (clo:fdefinition c1 e1 @clostrophilia:small-integer=)
          #'=)
    (setf (clo:fdefinition c1 e1 @clostrophilia:small-integer<)
          #'<)
    (sb:ensure-asdf-system c1 w1 "sicl-clos-package")
    (sb:ensure-asdf-system c1 w1 "clostrophilia-class-hierarchy")
    (sb:ensure-asdf-system c1 w1 "sicl-arithmetic-base")
    (sb:ensure-asdf-system c1 w1 "sicl-arithmetic-class-hierarchy")
    ;; Now, the class T is defined as a host standard class, but when
    ;; methods specialize to T, we must find the host class named T,
    ;; so we just replace the one we just loaded.
    (setf (clo:find-class c1 e1 'sb::sicl-t)
          (clo:find-class c1 e1 't))
    (setf (clo:find-class c1 e1 't)
          (find-class 't))
    ;; And, there are methods that specialize to the NULL class, like
    ;; ENSURE-CLASS-USING-CLASS, ENSURE-GENERIC-FUNCTION-USING-CLASS,
    ;; and they should be applicable when given NIL, so we need for
    ;; them to specialize on the host class named NULL.  For that
    ;; reason, we import that class from the host.
    (setf (clo:find-class c1 e1 'null)
          (find-class 'null))
    ;; We may also have methods that specialize to CONS or LIST, so we
    ;; import those as well.
    (setf (clo:find-class c1 e1 'cons)
          (find-class 'cons))
    (setf (clo:find-class c1 e1 'list)
          (find-class 'list))
    (define-typep c1 e1)
    (sb:ensure-asdf-system c1 w1 "sicl-asdf-packages")
    (setf (clo:macro-function c1 e1 @asdf-user:defsystem)
          (constantly nil))
    (setf (clo:macro-function c1 e1 @asdf:defsystem)
          (constantly nil))
    (sb:ensure-asdf-system
     c1 w1 "sicl-new-boot-phase-1-additional-classes")
    ;; We need to define HANDLER-BIND becuase it is used by Ecclesia.
    ;; The way we define it is that it just expands to a PROGN of the
    ;; forms in the body, with the bindings having no effect.
    (setf (clo:macro-function c1 e1 'handler-bind)
          (lambda (form environment)
            (declare (ignore environment))
            (cons 'progn (rest (rest form)))))
    (clo:make-variable c1 e1 'lambda-list-keywords lambda-list-keywords)
    ;; The ctype library calls the function SICL-TYPE:TYPE-EXPAND, so
    ;; we need to have the package SICL-TYPE defined.
    (sb:ensure-asdf-system c1 w1 "sicl-type-support")
    ;; FIXME: TYPEXPAND should be defined by code from SICL-TYPE being
    ;; loaded, rather than by defining it here. 
    (setf (clo:fdefinition c1 e1 @sicl-type:typexpand)
          (lambda (type-specifier &optional (environment e1))
            (clo:type-expand c1 environment type-specifier)))
    (sb:with-intercepted-function-cells
        ((make-instance (cons #'my-make-instance nil)))
      (load-predicament c1 w1 e1)
      (load-ctype c1 w1 e1)
      (sb:ensure-asdf-system c1 w1 "acclimation")
      (sb:ensure-asdf-system c1 w1 "ecclesia"))
    (sb:ensure-asdf-system c1 w1 "clostrophilia-dependent-maintenance")
    (setf (clo:fdefinition c1 e1 @sicl-clos:subtypep-1)
          #'subtypep)
    (setf (clo:fdefinition c1 e1 @clostrophilia:subtypep-1)
          #'subtypep)
    (sb:with-intercepted-function-cells
        ((make-instance (cons #'my-make-instance nil)))
      (sb:ensure-asdf-system
       c1 w1 "clostrophilia-generic-function-invocation"))
    (sb:ensure-asdf-system
     c1 w1 "clostrophilia-generic-function-initialization")
    ;; ENSURE-GENERIC-FUNCTION is called by the class initialization
    ;; protocol in order to put reader and writer methods on the
    ;; resulting function.  However, we remove the READER and WRITER
    ;; properties of the canonicalized slot specifiers, because we do
    ;; not want a host generic function to be created.  So then, the
    ;; class initialization protocol will have no readers or writers
    ;; to create, which means that ENSURE-GENERIC-FUNCTION will not be
    ;; called.  So we define it here to signal an error.
    (setf (clo:fdefinition c1 e1 'ensure-generic-function)
          (lambda (name &key &allow-other-keys)
            (error "Attempt to create generic function named ~s" name)))
    (setf (clo:fdefinition c1 e1 @clostrophilia:allocate-general-instance)
          #'sb:allocate-general-instance)
    (sb:with-intercepted-function-cells
        ((make-instance (cons #'my-make-instance nil)))
      (sb:ensure-asdf-system c1 w1 "clostrophilia-class-initialization"))
    (sb:ensure-asdf-system c1 w1 "clostrophilia-method-initialization")
    (sb:ensure-asdf-system
     c1 w1 "clostrophilia-slot-definition-initialization")
    (setf (clo:fdefinition c1 e1
                           @clostrophilia:set-funcallable-instance-function)
          (fdefinition 'closer-mop:set-funcallable-instance-function))
    (sb:with-intercepted-function-cells
        ((make-instance (cons #'my-make-instance nil)))
      (sb:ensure-asdf-system c1 w1 "clostrophilia-class-finalization")
      (sb:ensure-asdf-system c1 w1 "clostrophilia-method-combination-base"))
    (sb:ensure-asdf-system c1 w1 "sicl-new-boot-class-finalization")
    ;; The method on ENSURE-CLASS-USING-CLASS specialized to
    ;; FORWARD-REFERENCED-CLASS calls CHANGE-CLASS to turn the class
    ;; into something other than a FORWARD-REFERENCED-CLASS.  But in
    ;; this phase, we do not have any instances of
    ;; FORWARD-REFERENCED-CLASS, so we define CHANGE-CLASS to signal
    ;; an error.
    (setf (clo:fdefinition c1 e1 'change-class)
          (lambda (&rest arguments)
            (error "CHANGE-CLASS called with arguments ~s" arguments)))
    (sb:ensure-asdf-system
     c1 w1 "common-macro-definitions-packages-intrinsic")
    (let ((symbol @sicl-environment:type-expander))
      (setf (clo:fdefinition c1 e1 `(setf ,symbol))
            (lambda (expander c1 environment name)
              (setf (clo:type-expander c1 environment name)
                    expander))))
    ;; ENSURE-GENERIC-FUNCTION-USING-CLASS calls
    ;; FIND-METHOD-COMBINATION in the method specialized to NULL
    ;; (i.e., the generic function is being created) when the
    ;; METHOD-COMBINATION keyword argument is not supplied.  But
    ;; during bootstrapping we always call
    ;; ENSURE-GENERIC-FUNCTION-USING-CLASS from
    ;; ENSURE-GENERIC-FUNCTION, and the METHOD-COMBINATION keyword
    ;; argument must be supplied to ENSURE-GENERIC-FUNCTION, at least
    ;; as the standard version of ENSURE-GENERIC-FUNCTION is defined.
    ;; So to avoid that errors get propagated, we define
    ;; FIND-METHOD-COMBINATION to signal an error.
    (setf (clo:fdefinition c1 e1
                           @clostrophilia:find-method-combination)
          (lambda (generic-function
                   method-combination-type-name
                   method-combination-options)
            (declare (ignore generic-function
                             method-combination-type-name
                             method-combination-options))
            (error "FIND-METHOD-COMBINATION called in E1")))
    (sb:with-intercepted-function-cells
        ((make-instance (cons #'my-make-instance nil)))
      (sb:ensure-asdf-system c1 w1 "sicl-clos-ensure-metaobject-using"))
    (setf (clo:fdefinition c1 e1 @clostrophilia:make-method-instance)
          #'my-make-instance)
    (setf (clo:fdefinition c1 e1 'compile)
          (lambda (should-be-nil lambda-expression)
            (assert (null should-be-nil))
            (let ((cst (cst:cst-from-expression lambda-expression)))
              (sb:with-intercepted-function-cells
                  ((make-instance (cons #'my-make-instance nil)))
                (sb:eval-cst c1 cst w1)))))
    (sb:ensure-asdf-system
     c1 w1 "clostrophilia-slot-value-etc-using-class")
    ;;; During bootstrapping, we set the unbound slot value to
    ;;; something that is easier to manipulate during debugging.
    (setf (clo:symbol-value c1 e1 @clostrophilia:+unbound-slot-value+)
          99999)
    (sb:ensure-asdf-system
     c1 w1 "clostrophilia-standard-object-initialization-aux")
    (sb:ensure-asdf-system c1 w1 "sicl-clos-make-instance")
    e1))
