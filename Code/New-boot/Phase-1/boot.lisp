(cl:in-package #:sicl-new-boot-phase-1)

(eval-when (:compile-toplevel) (sb:enable-parcl-symbols client))

(defun create-reader-generic-function (name)
  (let* ((sample-generic-function #'print-object)
         (method-combination
           (closer-mop:find-method-combination
            sample-generic-function 'standard '())))
    (make-instance 'standard-generic-function
      :lambda-list '(object)
      :method-combination method-combination
      :name name)))

(defun define-readers (client environment slot-name readers class)
  (loop for reader in readers
        do (let* ((generic-function
                    (if (clo:fboundp client environment reader)
                        (clo:fdefinition client environment reader)
                        ;; We must create it.
                        (let ((generic-function
                                (create-reader-generic-function reader)))
                          (setf (clo:fdefinition client environment reader)
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

(defun define-writers (client environment slot-name writers class)
  (loop for writer in writers
        do (let* ((generic-function
                    (if (clo:fboundp client environment writer)
                        (clo:fdefinition client environment writer)
                        ;; We must create it.
                        (let ((generic-function
                                (create-writer-generic-function writer)))
                          (setf (clo:fdefinition client environment writer)
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

(defun define-ensure-class (client global-environment)
  (setf (clo:fdefinition client global-environment 'ensure-class)
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
                          client global-environment direct-superclass)))
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
                         client global-environment slot-name readers result)
                     (define-writers
                         client global-environment slot-name writers result))
            (setf (clo:find-class client global-environment name)
                result)))))

(defun define-typep (client global-environment)
  (setf (clo:fdefinition client global-environment 'typep)
        (lambda (object type-specifier)
          (cond ((eq type-specifier 'class)
                 (typep object
                        (clo:find-class
                         client global-environment
                         'class)))
                ((eq type-specifier 'method-combination)
                 (typep object
                        (clo:find-class
                         client global-environment
                         'method-combination)))
                ((eq type-specifier @clostrophilia:direct-slot-definition)
                 (or (typep object
                            (clo:find-class
                             client global-environment
                             @clostrophilia:direct-slot-definition))
                     (progn
                       (format *trace-output*
                               "Assuming ~s is a direct slot-definition~%"
                               object)
                       t)))
                ((eq type-specifier @clostrophilia:standard-accessor-method)
                 (typep object
                        (clo:find-class
                         client global-environment
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
  (let* ((client (make-instance 'client))
         (environment (create-environment client))
         (global-environment
           (trucler:global-environment client environment))
         (env:*client* client)
         (env:*environment* global-environment))
    (sb:define-package-functions client global-environment)
    (setf (sb:e1 boot) global-environment)
    (reinitialize-instance client
      :environment global-environment)
    (sb:define-backquote-macros client global-environment)
    (import-from-host client global-environment)
    (setf (clo:fdefinition client global-environment 'funcall)
          (lambda (function-or-name &rest arguments)
            (apply #'funcall
                   (if (functionp function-or-name)
                       function-or-name
                       (clo:fdefinition
                        client global-environment function-or-name))
                   arguments)))
    (setf (clo:fdefinition client global-environment 'ensure-method)
          #'ensure-method)
    (setf (clo:fdefinition client global-environment 'closer-mop:method-function)
          #'closer-mop:method-function)
    (sb:import-khazern client global-environment)
    (clo:make-variable
     client global-environment '*package* (find-package '#:common-lisp-user))
    (sb:ensure-asdf-system
     client environment "sicl-environment-clostrum-package")
    (sb:ensure-asdf-system
     client environment "sicl-environment-run-time-package")
    (sb:ensure-asdf-system
     client environment "sicl-environment-packages-intrinsic")
    (define-make-instance client global-environment)
    (setf (clo:find-class client global-environment 'package)
          (find-class 'parcl-low-class:package))
    (define-ensure-class client global-environment)
    (sb:define-client-and-environment-variables client global-environment)
    (sb:define-environment-functions client global-environment)
    (sb:ensure-asdf-system
     client environment "clostrophilia-package")
    (sb:ensure-asdf-system
     client environment "sicl-clos-package")
    (sb:ensure-asdf-system
     client environment "clostrophilia-class-hierarchy")
    ;; Now, the class T is defined as a host standard class, but when
    ;; methods specialize to T, we must find the host class named T,
    ;; so we just replace the one we just loaded.
    (setf (clo:find-class client global-environment 'sb::sicl-t)
          (clo:find-class client global-environment 't))
    (setf (clo:find-class client global-environment 't)
          (find-class 't))
    ;; And, there are methods that specialize to the NULL class, like
    ;; ENSURE-CLASS-USING-CLASS, ENSURE-GENERIC-FUNCTION-USING-CLASS,
    ;; and they should be applicable when given NIL, so we need for
    ;; them to specialize on the host class named NULL.  For that
    ;; reason, we import that class from the host.
    (setf (clo:find-class client global-environment 'null)
          (find-class 'null))
    ;; We may also have methods that specialize to CONS or LIST, so we
    ;; import those as well.
    (setf (clo:find-class client global-environment 'cons)
          (find-class 'cons))
    (setf (clo:find-class client global-environment 'list)
          (find-class 'list))
    (define-typep client global-environment)
    (sb:ensure-asdf-system
     client environment "sicl-asdf-packages")
    (setf (clo:macro-function
           client global-environment @asdf-user:defsystem)
          (constantly nil))
    (setf (clo:macro-function
           client global-environment @asdf:defsystem)
          (constantly nil))
    (sb:ensure-asdf-system
     client environment "sicl-new-boot-phase-1-additional-classes")
    ;; We need to define HANDLER-BIND becuase it is used by Ecclesia.
    ;; The way we define it is that it just expands to a PROGN of the
    ;; forms in the body, with the bindings having no effect.
    (setf (clo:macro-function client global-environment 'handler-bind)
          (lambda (form environment)
            (declare (ignore environment))
            (cons 'progn (rest (rest form)))))
    (clo:make-variable
     client global-environment 'lambda-list-keywords lambda-list-keywords)
    (sb:with-intercepted-function-cells
        ((make-instance (cons #'my-make-instance nil)))
      (load-predicament client environment global-environment)
      (load-ctype client environment global-environment)
      (sb:ensure-asdf-system client environment "acclimation")
      (sb:ensure-asdf-system client environment "ecclesia"))
    (sb:ensure-asdf-system
     client environment "clostrophilia-dependent-maintenance")
    (sb:ensure-asdf-system
     client environment "clostrophilia-generic-function-initialization")
    ;; ENSURE-GENERIC-FUNCTION is called by the class initialization
    ;; protocol in order to put reader and writer methods on the
    ;; resulting function.  However, we remove the READER and WRITER
    ;; properties of the canonicalized slot specifiers, because we do
    ;; not want a host generic function to be created.  So then, the
    ;; class initialization protocol will have no readers or writers
    ;; to create, which means that ENSURE-GENERIC-FUNCTION will not be
    ;; called.  So we define it here to signal an error.
    (setf (clo:fdefinition
           client global-environment 'ensure-generic-function)
          (lambda (name &key &allow-other-keys)
            (error "Attempt to create generic function named ~s" name)))
    (setf (clo:fdefinition
           client global-environment @clostrophilia:allocate-general-instance)
          #'sb:allocate-general-instance)
    (sb:with-intercepted-function-cells
        ((make-instance (cons #'my-make-instance nil)))
      (sb:ensure-asdf-system
       client environment "clostrophilia-class-initialization"))
    (sb:ensure-asdf-system
     client environment "clostrophilia-method-initialization")
    (sb:ensure-asdf-system
     client environment "clostrophilia-slot-definition-initialization")
    (setf (clo:fdefinition client global-environment
                           @clostrophilia:set-funcallable-instance-function)
          (fdefinition 'closer-mop:set-funcallable-instance-function))
    (sb:with-intercepted-function-cells
        ((make-instance (cons #'my-make-instance nil)))
      (sb:ensure-asdf-system
       client environment "clostrophilia-class-finalization")
      (sb:ensure-asdf-system
       client environment "clostrophilia-method-combination-base")
      (sb:ensure-asdf-system
       client environment "clostrophilia-generic-function-invocation"))
    ;; The method on ENSURE-CLASS-USING-CLASS specialized to
    ;; FORWARD-REFERENCED-CLASS calls CHANGE-CLASS to turn the class
    ;; into something other than a FORWARD-REFERENCED-CLASS.  But in
    ;; this phase, we do not have any instances of
    ;; FORWARD-REFERENCED-CLASS, so we define CHANGE-CLASS to signal
    ;; an error.
    (setf (clo:fdefinition client global-environment 'change-class)
          (lambda (&rest arguments)
            (error "CHANGE-CLASS called with arguments ~s" arguments)))
    (sb:ensure-asdf-system
     client environment "common-macro-definitions-packages-intrinsic")
    (let ((symbol @sicl-environment:type-expander))
      (setf (clo:fdefinition client global-environment `(setf ,symbol))
            (lambda (expander client environment name)
              (setf (clo:type-expander client environment name)
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
    (setf (clo:fdefinition client global-environment
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
      (sb:ensure-asdf-system
       client environment "sicl-clos-ensure-metaobject-using"))
    (setf (clo:fdefinition client global-environment 'compile)
          (lambda (should-be-nil lambda-expression)
            (assert (null should-be-nil))
            (let ((cst (cst:cst-from-expression lambda-expression)))
              (sb:eval-cst client cst environment))))
    (setf (clo:fdefinition client global-environment 'break)
          (lambda (&rest arguments)
            (declare (ignore arguments))
            (sicl-new-boot-backtrace-inspector:inspect
             common-boot-ast-evaluator::*stack*)))
    (sb:ensure-asdf-system
     client environment "clostrophilia-slot-value-etc-using-class")
    ;;; During bootstrapping, we set the unbound slot value to
    ;;; something that is easier to manipulate during debugging.
    (setf (clo:symbol-value
           client global-environment @clostrophilia:+unbound-slot-value+)
          99999)
    (sb:ensure-asdf-system
     client environment "clostrophilia-standard-object-initialization-aux")
    (sb:ensure-asdf-system client environment "sicl-clos-make-instance")
    global-environment))
