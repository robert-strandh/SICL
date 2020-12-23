(cl:in-package #:sicl-clos)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; ENSURE-CLASS and ENSURE-CLASS-USING-CLASS.

;;; ENSURE-CLASS is called from code resulting from the macroexpansion
;;; of a macro call to DEFCLASS, but it can also be called directly
;;; from user code.  This means that we have to be careful about
;;; default values of initialization arguments.
;;;
;;; When DEFCLASS is used, it is intended to mean that the class be
;;; defined according to the specification in DEFCLASS, with omitted
;;; options given proper defaults.  If the class already exists, and
;;; DEFCLASS is called, the intended meaning is for the class to be
;;; completely redefined according to the new specification, again
;;; with omitted options given proper defaults.  The intended meaning
;;; is NOT to redefine only the parts of the DEFCLASS specification
;;; that were explicitly given.
;;;
;;; But when ENSURE-CLASS is used directly from user code, omitting
;;; some initialization arguments may very well mean that if the class
;;; already exists, we should use the previous values.
;;;
;;; Therefore, clearly, it is safe for DEFCLASS to pass a complete set
;;; of initialization arguments to ENSURE-CLASS, but it is safe for
;;; DEFCLASS to omit some arguments to ENSURE-CLASS only if
;;; ENSURE-CLASS defaults them to the same values that DEFCLASS
;;; requires.

(defun process-direct-superclass (direct-superclass-or-name)
  (cond ((typep direct-superclass-or-name 'class)
         direct-superclass-or-name)
        ((symbolp direct-superclass-or-name)
         (let ((class (find-class direct-superclass-or-name nil)))
           (if (null class)
               (setf (find-class direct-superclass-or-name)
                     (make-instance 'forward-referenced-class
                       :name direct-superclass-or-name))
               class)))
        (t
         (error 'direct-superclass-must-be-a-class-metaobject-or-a-symbol
                :superclass direct-superclass-or-name))))

(defun process-direct-superclasses (direct-superclasses)
  (unless (cleavir-code-utilities:proper-list-p direct-superclasses)
    (error 'direct-superclasses-must-be-proper-list
           :superclasses direct-superclasses))
  (loop for class-or-name in direct-superclasses
        collect (process-direct-superclass class-or-name)))

;;; When the class is created, it is safe to use a default value of
;;; the empty list for the :DIRECT-SUPERCLASSES initialization
;;; argument, because the AMOP says that the default is the same
;;; whether this argument is the empty list, or not given at all.

(defun ensure-class-using-class-null
    (class name
     &rest keys
     &key
       direct-default-initargs
       direct-slots
       direct-superclasses
       (metaclass nil metaclass-p)
     &allow-other-keys)
  (declare (ignore class))
  (unless metaclass-p
    (setf metaclass 'standard-class))
  (setf direct-superclasses
        (process-direct-superclasses direct-superclasses))
  (let ((remaining-keys (copy-list keys)))
    (loop while (remf remaining-keys :metaclass))
    (loop while (remf remaining-keys :direct-superclasses))
    (setf (find-class name)
          (apply #'make-instance metaclass
                 :direct-default-initargs direct-default-initargs
                 :direct-slots direct-slots
                 :direct-superclasses direct-superclasses
                 :name name
                 remaining-keys))))

;;; When the class is reinitialized, it is NOT safe to use a default
;;; value of the empty list for the :DIRECT-SUPERCLASSES
;;; initialization argument.  Instead, we must distinguish the case
;;; where the :DIRECT-SUPERCLASSES initialization argument is not
;;; given, which is interpreted to mean that we keep the old value of
;;; this slot, and the case where the :DIRECT-SUPERCLASSES
;;; initialization argument is given as the empty list, which has
;;; the same meaning as for initialization.

(defun ensure-class-using-class-class
    (class name
     &rest keys
     &key
       (direct-superclasses nil direct-superclasses-p)
       (metaclass nil metaclass-p)
     &allow-other-keys)
  (when metaclass-p
    (cond ((symbolp metaclass)
           (setf metaclass (find-class metaclass)))
          ((typep metaclass 'class)
           nil)
          (t
           (error "metaclass must be a symbol or a class metaobject class")))
    (unless (eq metaclass (class-of class))
      (error "can't change metaclass during reinitialization of class")))
  (when direct-superclasses-p
    (setf direct-superclasses
          (process-direct-superclasses direct-superclasses)))
  (let ((remaining-keys (copy-list keys)))
    (loop while (remf remaining-keys :metaclass))
    (loop while (remf remaining-keys :direct-superclasses))
    (if direct-superclasses-p
        (apply #'reinitialize-instance class
               :name name
               :direct-superclasses direct-superclasses
               remaining-keys)
        (apply #'reinitialize-instance class
               :name name
               remaining-keys)))
  class)

(defun ensure-class-using-class-forward-referenced-class
    (class
     name
     &rest keys
     &key
       direct-default-initargs
       direct-slots
       (direct-superclasses nil direct-superclasses-p)
       (metaclass nil metaclass-p)
       &allow-other-keys)
  (unless metaclass-p
    (setf metaclass 'standard-class))
  (cond ((symbolp metaclass)
         (setf metaclass (find-class metaclass)))
        ((typep metaclass 'class)
         nil)
        (t
         (error "metaclass must be a symbol or a class metaobject class")))
  (change-class class metaclass)
  (when direct-superclasses-p
    (setf direct-superclasses
          (process-direct-superclasses direct-superclasses)))
  (let ((remaining-keys (copy-list keys)))
    (loop while (remf remaining-keys :metaclass))
    (loop while (remf remaining-keys :direct-superclasses))
    (if direct-superclasses-p
        (apply #'reinitialize-instance class
               :direct-default-initargs direct-default-initargs
               :direct-slots direct-slots
               :name name
               :direct-superclasses direct-superclasses
               remaining-keys)
        (apply #'reinitialize-instance class
               :direct-default-initargs direct-default-initargs
               :direct-slots direct-slots
               :name name
               remaining-keys)))
  class)
