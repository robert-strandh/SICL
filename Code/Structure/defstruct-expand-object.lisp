;;;; Expand structure-object-based defstructs.

(cl:in-package #:sicl-structure)

(defun check-included-structure-object (description environment)
  (when (defstruct-included-structure-name description)
    (let* ((parent-name (defstruct-included-structure-name description))
           (included-structure (find-class parent-name environment nil)))
      (unless included-structure
        (if (find-structure-description parent-name nil environment)
            (error 'included-structure-must-not-be-typed :name parent-name)
            (error 'included-structure-does-not-exist :name parent-name)))
      (unless (typep included-structure 'structure-class)
        (error 'included-structure-must-be-structure
               :name parent-name :datum included-structure))
      (closer-mop:ensure-finalized included-structure)
      ;; All included slots must be present in the included structure.
      (dolist (slot (defstruct-included-slots description))
        (let ((existing (find (slot-name slot)
                              (closer-mop:class-slots included-structure)
                              :key #'closer-mop:slot-definition-name
                              :test #'string=)))
          (unless existing
            (error 'included-slot-missing-from-parent
                   :slot-name (slot-name slot)))
          ;; For the sake of sanity, lets require them to be the same symbol too.
          ;; If it is legal for them to be different, then they need to be
          ;; canonicalized to the existing slot so that slot inheritance works
          ;; correctly.
          (unless (eql (slot-name slot) (closer-mop:slot-definition-name existing))
            (error 'included-slot-conflicts-with-parent-slot
                   :slot-name (slot-name slot)
                   :parent-slot-name (closer-mop:slot-definition-name existing)))))
      ;; Direct slots must not be present (string=)
      (dolist (slot (defstruct-direct-slots description))
        (let ((existing (find (slot-name slot)
                              (closer-mop:class-slots included-structure)
                              :key #'closer-mop:slot-definition-name
                              :test #'string=)))
          (when existing
            (error 'direct-slot-conflicts-with-parent-slot
                   :slot-name (slot-name slot)
                   :parent-slot-name (closer-mop:slot-definition-name existing))))))))

(defmethod compute-slot-layout ((description defstruct-object-description) environment)
  (check-included-structure-object description environment)
  ;; All of them, including implicitly included slots.
  (append
   (when (defstruct-included-structure-name description)
     (loop with included-structure = (find-class (defstruct-included-structure-name description)
                                                 environment)
           with included-slots = (defstruct-included-slots description)
           with default-initargs = (closer-mop:class-direct-default-initargs included-structure)
           for slot in (closer-mop:class-slots included-structure)
           for slot-name = (closer-mop:slot-definition-name slot)
           for inclusion = (find slot-name included-slots :key #'slot-name)
           collect (or inclusion
                       ;; For implicitly included slots, reconstruct a slot-description
                       ;; from the effective slot-definition.
                       (let* ((initarg-kw (keywordify slot-name))
                              (default-initarg (find initarg-kw default-initargs :key #'first)))
                         (make-instance 'slot-description
                                        :name slot-name
                                        ;; Generate an accessor with the right name,
                                        ;; don't use the one generated for the parent.
                                        :accessor-name (compute-accessor-name description slot-name)
                                        :initform (second default-initarg)
                                        :initform-p (not (not default-initarg))
                                        :type (closer-mop:slot-definition-type slot)
                                        :read-only (structure-slot-definition-read-only slot))))))
   (defstruct-direct-slots description)))

(defmethod layout-slots ((description defstruct-object-description) layout)
  layout)

(defmethod generate-allocation-form ((description defstruct-object-description) all-slots)
  `(allocate-instance (find-class ',(defstruct-name description))))

(defmethod generate-slot-initialization-form ((description defstruct-object-description) layout object slot value)
  `(setf (slot-value ,object ',(slot-name slot)) ,value))

(defmethod generate-predicate ((description defstruct-object-description) layout predicate-name)
  (declare (ignore layout))
  `(defun ,predicate-name (object)
     (typep object ',(defstruct-name description))))

(defmethod generate-copier ((description defstruct-object-description) layout copier-name)
  `(defun ,copier-name (object)
     (check-type object ,(defstruct-name description))
     (copy-structure object)))

(defun compute-structure-object-defclass-slots (all-slots)
  (loop for slot in all-slots
        collect `(,(slot-name slot)
                  :initarg ,(keywordify (slot-name slot))
                  :type ,(slot-type slot)
                  :read-only ,(slot-read-only slot)
                  ,(if (slot-read-only slot) :reader :accessor)
                  ,(slot-accessor-name slot))))

(defun compute-structure-object-defclass-default-initargs (all-slots)
  (loop for slot in all-slots
        when (slot-initform-p slot)
        collect (keywordify (slot-name slot))
        and collect (slot-initform slot)))

(defmethod generate-defstruct-bits ((description defstruct-object-description) layout)
  `(progn
     (eval-when (:compile-toplevel :load-toplevel :execute)
       (defclass ,(defstruct-name description)
           (,(or (defstruct-included-structure-name description)
                 'structure-object))
         (,@(compute-structure-object-defclass-slots layout))
         (:metaclass structure-class)
         (:default-initargs ,@(compute-structure-object-defclass-default-initargs layout))))
     ,@(when (defstruct-print-object description)
         (list `(defmethod print-object ((object ,(defstruct-name description)) stream)
                  (funcall #',(defstruct-print-object description) object stream))))))
