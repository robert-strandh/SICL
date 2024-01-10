(cl:in-package #:sicl-new-boot-phase-1)

(defgeneric my-make-instance (class &rest initargs))

(defun define-make-instance (client environment)

  (defmethod my-make-instance ((name symbol) &rest initargs)
    (let ((class (clostrum:find-class client environment name)))
      (apply #'make-instance class initargs)))

  (defmethod my-make-instance ((class class) &rest initargs)
    (apply #'make-instance class initargs))

  (setf (clostrum:fdefinition client environment 'make-instance)
        #'my-make-instance))

(defun define-ensure-class (client global-environment)
  (setf (clostrum:fdefinition client global-environment 'ensure-class)
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
                   (make-instance 'standard-class
                     :name name
                     :direct-default-initargs direct-default-initargs
                     :direct-slots new-slots
                     :direct-superclasses new-superclasses)))
          (setf (clo:find-class client global-environment name)
                result)))))

(defun define-typep (client global-environment)
  (setf (clostrum:fdefinition client global-environment 'typep)
        (lambda (object type-specifier)
          (cond ((eq type-specifier 'class)
                 (format *trace-output*
                         "Assuming ~s is a class~%" object)
                 t)
                ((eq type-specifier 'method)
                 (format *trace-output*
                         "Assuming ~s is a method~%" object)
                 t)
                ((eq type-specifier 'method-combination)
                 (format *trace-output*
                         "Assuming ~s is a method combination~%" object)
                 t)
                (t
                 (format *trace-output*
                         "Don't know whether ~s is of type ~s~%"
                         object type-specifier)
                 (break))))))

(defun boot ()
  (let* ((client (make-instance 'client))
         (environment (create-environment client))
         (global-environment (trucler:global-environment client environment))
         (*packages* (make-hash-table :test #'equal))
         (*symbol-package* (make-hash-table :test #'eq)))
    (setf (clostrum:symbol-value client global-environment
                                 'sicl-environment:*environment*)
          global-environment)
    (setf (clostrum:symbol-value client global-environment
                                 'sicl-environment:*client*)
          client)
    (reinitialize-instance client
      :environment global-environment)
    (sicl-new-boot:define-backquote-macros client global-environment)
    (import-from-host client global-environment)
    (setf (clostrum:fdefinition client global-environment 'ensure-method)
          #'ensure-method)
    (setf (clostrum:fdefinition client global-environment 'closer-mop:method-function)
          #'closer-mop:method-function)
    (import-khazern client global-environment)
    (define-environment-functions client global-environment)
    (define-package-functions client global-environment)
    (clostrum:make-variable
     client global-environment '*package* (find-package '#:common-lisp-user))
    (create-common-lisp-package client)
    (loop for name in '("COMMON-LISP-USER" "KEYWORD")
          do (setf (gethash name *packages*) (find-package name)))
    (define-make-instance client global-environment)
    (setf (clostrum:find-class client global-environment 'package)
          (find-class 'parcl-class:package))
    (define-ensure-class client global-environment)
    (sicl-new-boot:ensure-asdf-system
     client environment "clostrophilia-package")
    (sicl-new-boot:ensure-asdf-system
     client environment "sicl-clos-package")
    (sicl-new-boot:ensure-asdf-system
     client environment "clostrophilia-class-hierarchy")
    ;; Now, the class T is defined as a host standard class, but when
    ;; methods specialize to T, we must find the host class named T,
    ;; so we just replace the one we just loaded.
    (setf (clo:find-class client global-environment 't)
          (find-class 't))
    ;; It would be better to load the condition system here.
    (setf (clo:macro-function client global-environment 'define-condition)
          (lambda (form environment)
            (declare (ignore environment))
            (list 'defclass (second form) '() '())))
    (sicl-new-boot:ensure-asdf-system
     client environment "acclimation")
    ;; We need to define HANDLER-BIND becuase it is used by Ecclesia.
    ;; The way we define it is that it just expands to a PROGN of the
    ;; forms in the body, with the bindings having no effect.
    (setf (clostrum:macro-function client global-environment 'handler-bind)
          (lambda (form environment)
            (declare (ignore environment))
            (cons 'progn (rest (rest form)))))
    (clostrum:make-variable
     client global-environment 'lambda-list-keywords lambda-list-keywords)
    (sicl-new-boot:ensure-asdf-system
     client environment "ecclesia")
    (define-typep client global-environment)
    (sicl-new-boot:ensure-asdf-system
     client environment "clostrophilia-generic-function-initialization")
    (sicl-new-boot:ensure-asdf-system
     client environment "clostrophilia-class-initialization")
    (sicl-new-boot:ensure-asdf-system
     client environment "clostrophilia-method-initialization")
    (setf (clostrum:macro-function
           client global-environment 'define-method-combination)
          (lambda (form environment)
            (declare (ignore environment))
            (eval form)
            nil))
    (values global-environment *packages* *symbol-package*)))
