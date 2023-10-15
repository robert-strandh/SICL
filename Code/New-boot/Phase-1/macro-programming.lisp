(cl:in-package #:sicl-new-boot-phase-1)

;;; DEFCLASS programming.

(defmethod cmd:defclass-compile-time-action
  ((client client)
   name
   superclass-names
   metaclass-name
   environment)
  (format *trace-output*
          "compile-time: ~s ~s ~s~%"
          name
          superclass-names
          metaclass-name))

(defun transform-name (name)
  (intern (string-downcase (symbol-name name))
          (find-package "SICL-NEW-BOOT-PHASE-1")))

(defun transform-slot-spec
    (&rest arguments &key readers writers &allow-other-keys)
  (let ((result arguments)
        (new-readers (loop for reader in readers
                           collect  (transform-name reader)))
        (new-writers (loop for writer in writers
                           collect
                           `(setf ,(transform-name (second writer))))))
    (remf result :readers)
    (remf result :writers)
    `(,@result :readers ,new-readers :writers ,new-writers)))

(defmethod cmd:ensure-class
    ((client client)
     name
     superclass-names
     direct-slot-specs
     options
     environment)
  (let ((result
          (closer-mop:ensure-class
           (transform-name name)
           :name name
           :direct-superclasses (mapcar #'transform-name superclass-names)
           :direct-slots (mapcar (lambda (slot-spec)
                                   (apply #'transform-slot-spec slot-spec))
                                 direct-slot-specs)
           :metaclass 'closer-mop:funcallable-standard-class)))
    (setf (clo:find-class client (environment client) name)
          result)))

;;; DEFGENERIC programming.

(defmethod cmd:defgeneric-compile-time-action
    ((client client)
     name
     lambda-list
     argument-precedence-order
     generic-function-class-name
     method-class-name
     method-combination-name
     method-combination-arguments
     documentation-option
     environment)
  nil)

(defmethod cmd:ensure-generic-function
    ((client client)
     name
     lambda-list
     argument-precedence-order
     generic-function-class-name
     method-class-name
     method-combination-name
     method-combination-arguments
     documentation-option
     environment)
  (let ((result 
          (ensure-generic-function
           (transform-name name)
           :lambda-list lambda-list
           :argument-precedence-order argument-precedence-order
           :generic-function-class generic-function-class-name
           :method-class method-class-name)))
    (setf (clo:fdefinition client (environment client) name)
          result)))
