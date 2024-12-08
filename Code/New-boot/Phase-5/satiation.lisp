(cl:in-package #:sicl-new-boot-phase-5)

(eval-when (:compile-toplevel) (sb:enable-parcl-symbols client))

;;; A "metaobject method" is what we call a method that has at least
;;; one parameter specialized to a subclass of METAOBJECT.  A
;;; "metaobject function" is a generic function with at least one
;;; metaobject method.

;;; We use this function to find all the subclasses of the class
;;; METAOBJECT so that we can use MEMBER directly on a specializer to
;;; see whether it is a subclass of METAOBJECT.
(defun find-all-subclasses (class class-direct-subclasses-function)
  (let ((direct-subclasses (funcall class-direct-subclasses-function class)))
    (if (null direct-subclasses)
        (list class)
        (cons class
              (loop for direct-subclass in direct-subclasses
                    append (find-all-subclasses
                            direct-subclass
                            class-direct-subclasses-function))))))

;;; This variable contains a list of all the subclasses of METAOBJECT,
;;; including the class METAOBJECT.
(defvar *metaobject-subclasses*)

;;; This variable contains the function METHOD-SPECIALZERS.
(defvar *method-specializers-function*)

(defun method-is-a-metaobject-method-p (method)
  (let ((specializers (funcall *method-specializers-function* method)))
    (loop for specializer in specializers
            thereis (member specializer *metaobject-subclasses*))))

;;; This variable contains the class STANDARD-GENERIC-FUNCTION.
(defvar *standard-generic-function-class*)

;;; This variable contains the function GENERIC-FUNCTION-METHODS.
(defvar *generic-function-methods-function*)

(defun operator-is-a-metaobject-function-p (operator)
  (and (typep operator 'sb:header)
       (eq (sb:class operator) *standard-generic-function-class*)
       (let ((methods
               (funcall *generic-function-methods-function* operator)))
         (loop for method in methods
                 thereis (method-is-a-metaobject-method-p method)))))

;;; EX is E3 before the graph has been turned into a cycle, and E4
;;; when the graph is cyclic.
(defun satiate-metaobject-functions (client ex e4)
  (let* ((*generic-function-methods-function*
           (clo:fdefinition
            client ex @clostrophilia:generic-function-methods))
         (*standard-generic-function-class*
           (clo:find-class client ex 'standard-generic-function))
         (*method-specializers-function*
           (clo:fdefinition client ex @clostrophilia:method-specializers))
         (metaobject-class
           (clo:find-class client e4 @clostrophilia:metaobject))
         (class-direct-subclasses-function
           (clo:fdefinition client ex @clostrophilia:class-direct-subclasses))
         (*metaobject-subclasses*
           (find-all-subclasses metaobject-class class-direct-subclasses-function))
         (table (clostrum-basic::functions e4))
         (satiate-function
           (clo:fdefinition
            client ex @clostrophilia:satiate-generic-function)))
    (loop for entry being each hash-value of table using (hash-key name)
          for cell = (clostrum-basic::cell entry)
          for operator = (car cell)
          when (operator-is-a-metaobject-function-p operator)
            do ; (format *trace-output* "Satiating ~s~%" name)
               (funcall satiate-function operator))
    ;; As it turns out, METHOD-COMBINATION-TEMPLATE is not a subclass
    ;; of METAOBJECT, so its slot accessors are not satiated by the
    ;; preceding loop.  We satiate them explicitly here.
    (funcall satiate-function
             (clo:fdefinition
              client e4 @clostrophilia:variant-signature-determiner))
    (funcall satiate-function
             (clo:fdefinition client e4 @clostrophilia:variants))
    (funcall satiate-function
             (clo:fdefinition
              client e4 @clostrophilia:effective-method-form-function))))
