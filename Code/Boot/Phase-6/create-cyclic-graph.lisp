(cl:in-package #:sicl-boot-phase-6)

(defmacro with-impure-sicl-object (object-var &body body)
  `(when (typep ,object-var 'sicl-boot::header)
     (unless (typep (slot-value ,object-var 'sicl-boot::%class)
                    'sicl-boot::header)
       ,@body)))

(defun create-class-translation-table (e4 e5)
  (let ((result (make-hash-table :test #'eq))
        (visited (make-hash-table :test #'eq)))
    (do-all-symbols (symbol)
      (unless (gethash symbol visited)
        (setf (gethash symbol visited) t)
        (let ((e4-class (env:find-class (env:client e4) e4 symbol)))
          (unless (null e4-class)
            (let ((e5-class (env:find-class (env:client e5) e5 symbol)))
              (assert (not (null e5-class)))
              (setf (gethash e4-class result) e5-class))))))
    result))

(defun find-all-functions (e5)
  (let ((visited (make-hash-table :test #'eq))
        (result '()))
    (do-all-symbols (symbol)
      (unless (gethash symbol visited)
        (setf (gethash symbol visited) t)
        (when (and (env:fboundp (env:client e5) e5 symbol)
                   (not (env:special-operator (env:client e5) e5 symbol))
                   (null (env:macro-function (env:client e5) e5 symbol)))
          (push (env:fdefinition (env:client e5) e5 symbol) result))
        (when (env:fboundp (env:client e5) e5 `(setf ,symbol))
          (push (env:fdefinition (env:client e5) e5 `(setf ,symbol)) result))))
    result))

(defun find-all-sicl-functions (e5)
  (remove-if-not (lambda (x) (typep x 'sicl-boot::header))
                 (find-all-functions e5)))

;;; All SICL objects have a class slot that may contain a non-SICL
;;; class.  TRANSLATION-TABLE maps such non-SICL classes to
;;; corresponding SICL-CLASSES.  CLASS-SLOTS-FUNCTION is a function
;;; that, when applied to a SICL class, returns the list of the
;;; effective slots of that class.  All SICL objects have a list of
;;; effective slots in element 1 of the rack, so we update that list
;;; as well.
(defun update-object (object translation-table class-slots-function)
  (with-impure-sicl-object object
    (let ((old-class (slot-value object 'sicl-boot::%class))
          (rack (slot-value object 'sicl-boot::%rack)))
      (unless (typep old-class 'sicl-boot::header)
        (let ((new-class (gethash old-class translation-table)))
          (setf (slot-value object 'sicl-boot::%class)
                new-class)
          (setf (aref rack 1)
                (funcall class-slots-function new-class)))))))

(defun update-method
    (method translate-table class-slots-function e5)
  (declare (ignore e5))
  (with-impure-sicl-object method
    (update-object method translate-table class-slots-function)))

(defun update-generic-function
    (function translate-table class-slots-function e5)
  (with-impure-sicl-object function
    (update-object function translate-table class-slots-function)
    (let* ((methods-function
             (env:fdefinition
              (env:client e5) e5 'sicl-clos:generic-function-methods))
           (methods (funcall methods-function function)))
      (loop for method in methods
            do (update-method
                method translate-table class-slots-function e5)))
    ;; FIXME: Thre is no slot writer for the slot METHOD-CLASS, so we
    ;; would have to call REINITIALIZE-INSTANCE.  I am not sure at
    ;; this point that it would be possible.
    (let ((rack (slot-value function 'sicl-boot::%rack)))
      (setf (aref rack 11)
            (gethash (aref rack 11) translate-table)))))

(defun update-simple-function
    (function translate-table class-slots-function e5)
  (declare (ignore e5))
  (with-impure-sicl-object function
    (update-object function translate-table class-slots-function)))

(defun update-function
    (function translate-table class-slots-function e5)
  (with-impure-sicl-object function
    (if (eq (gethash (slot-value function 'sicl-boot::%class) translate-table)
            (env:find-class (env:client e5) e5 'standard-generic-function))
        (update-generic-function
         function translate-table class-slots-function e5)
        (update-simple-function
         function translate-table class-slots-function e5))))

(defun update-direct-slot
    (slot translate-table class-slots-function e5)
  (with-impure-sicl-object slot
    (update-object slot translate-table class-slots-function)
    (let* ((initfunction-function
             (env:fdefinition
              (env:client e5) e5 'sicl-clos:slot-definition-initfunction))
           (initfunction (funcall initfunction-function slot)))
      (unless (null initfunction)
        (update-simple-function
         initfunction translate-table class-slots-function e5)))))

(defun update-effective-slot
    (slot translate-table class-slots-function e5)
  (with-impure-sicl-object slot
    (update-object slot translate-table class-slots-function)
    (let* ((initfunction-function
             (env:fdefinition
              (env:client e5) e5 'sicl-clos:slot-definition-initfunction))
           (initfunction (funcall initfunction-function slot)))
      (unless (null initfunction)
        (update-simple-function
         initfunction translate-table class-slots-function e5)))))

(defun update-class
    (class translate-table class-slots-function e5)
  (with-impure-sicl-object class
    (update-object class translate-table class-slots-function)
    (let* ((direct-slots-function
             (env:fdefinition
              (env:client e5) e5 'sicl-clos:class-direct-slots))
           (slots (funcall direct-slots-function class)))
      (loop for slot in slots
            do (update-direct-slot
                slot translate-table class-slots-function e5)))
    (let* ((effective-slots-function
             (env:fdefinition
              (env:client e5) e5 'sicl-clos:class-slots))
           (slots (funcall effective-slots-function class)))
      (loop for slot in slots
            do (update-effective-slot
                slot translate-table class-slots-function e5)))))

(defun find-all-classes (e5)
  (let ((result '())
        (visited (make-hash-table :test #'eq)))
    (do-all-symbols (symbol)
      (unless (gethash symbol visited)
        (setf (gethash symbol visited) t)
        (let ((class (env:find-class (env:client e5) e5 symbol)))
          (unless (null class)
            (push class result)))))
    result))

(defun update-method-combination-template
    (template translate-table class-slots-function e5)
  (with-impure-sicl-object template
    (update-object template translate-table class-slots-function)
    (let* ((variants-function
             (env:fdefinition
              (env:client e5) e5 'sicl-method-combination::variants))
           (variants (funcall variants-function template)))
      (loop for variant in variants
            do (update-object variant translate-table class-slots-function)))))

(defun find-all-method-combination-templates (e5)
  (let ((result '())
        (visited (make-hash-table :test #'eq)))
    (do-all-symbols (symbol)
      (unless (gethash symbol visited)
        (setf (gethash symbol visited) t)
        (let ((template (env:find-method-combination-template symbol e5)))
          (unless (null template)
            (push template result)))))
    result))

(defun update-all-objects (e4 e5)
  (let ((table (create-class-translation-table e4 e5))
        (class-slots-function
          (env:fdefinition (env:client e5) e5 'sicl-clos:class-slots)))
    (loop for function in (find-all-sicl-functions e5)
          do (update-function function table class-slots-function e5))
    (loop for class in (find-all-classes e5)
          do (update-class class table class-slots-function e5))
    (loop for template in (find-all-method-combination-templates e5)
          do (update-method-combination-template
              template table class-slots-function e5))))
