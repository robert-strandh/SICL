(cl:in-package #:sicl-new-boot-phase-5)

(eval-when (:compile-toplevel) (sb:enable-parcl-symbols client))

(defun class-is-subclass-of-metaobject-p
    (class metaobject-class class-precedence-list-function)
  (let ((class-precedence-list
          (funcall class-precedence-list-function class)))
    (member metaobject-class class-precedence-list :test #'eq)))

(defun generic-function-is-a-slot-reader-p
    (generic-function
     generic-function-methods-function
     class-of-function
     standard-reader-method-class)
  (let ((methods
          (funcall generic-function-methods-function generic-function)))
    (and (= (length methods) 1)
         (eq (funcall class-of-function (first methods))
             standard-reader-method-class))))

(defun generic-function-is-a-slot-writer-p
    (generic-function
     generic-function-methods-function
     class-of-function
     standard-writer-method-class)
  (let ((methods
          (funcall generic-function-methods-function generic-function)))
    (and (= (length methods) 1)
         (eq (funcall class-of-function (first methods))
             standard-writer-method-class))))

(defun generic-function-is-a-metaobject-slot-reader-p
    (generic-function
     generic-function-methods-function
     class-of-function
     standard-reader-method-class
     method-specializers-function
     metaobject-class
     class-precedence-list-function)
  (and (generic-function-is-a-slot-reader-p
        generic-function
        generic-function-methods-function
        class-of-function
        standard-reader-method-class)
       (let* ((methods
                (funcall generic-function-methods-function generic-function))
              (method (first methods))
              (specializers
                (funcall method-specializers-function method))
              (specializer (first specializers)))
         (class-is-subclass-of-metaobject-p
          specializer metaobject-class class-precedence-list-function)))) 

(defun generic-function-is-a-metaobject-slot-writer-p
    (generic-function
     generic-function-methods-function
     class-of-function
     standard-writer-method-class
     method-specializers-function
     metaobject-class
     class-precedence-list-function)
  (and (generic-function-is-a-slot-writer-p
        generic-function
        generic-function-methods-function
        class-of-function
        standard-writer-method-class)
       (let* ((methods
                (funcall generic-function-methods-function generic-function))
              (method (first methods))
              (specializers
                (funcall method-specializers-function method))
              (specializer (second specializers)))
         (class-is-subclass-of-metaobject-p
          specializer metaobject-class class-precedence-list-function))))

(defun find-all-standard-generic-functions
    (hash-table standard-generic-function-class)
  (loop for entry being each hash-value using (hash-key key) of hash-table
        for cell = (clostrum-basic::cell entry)
        for operator = (car cell)
        when (and (typep operator 'sb:header)
                  (eq (sb:class operator) standard-generic-function-class))
          collect (cons key operator)))

(defun satiate-metaobject-slot-accessors-1 (client e3 e4)
  (let* ((generic-function-methods-function
           (clo:fdefinition
            client e3 @clostrophilia:generic-function-methods))
         (class-of-function
           (clo:fdefinition
            client e3 @clostrophilia:class-of))
         (standard-generic-function-class
           (clo:find-class client e3 'standard-generic-function))
         (standard-reader-method-class
           (clo:find-class client e3 @clostrophilia:standard-reader-method))
         (standard-writer-method-class
           (clo:find-class client e3 @clostrophilia:standard-writer-method))
         (method-specializers-function
           (clo:fdefinition client e3 @clostrophilia:method-specializers))
         (metaobject-class
           (clo:find-class client e4 @clostrophilia:metaobject))
         (class-precedence-list-function
           (clo:fdefinition client e3 @clostrophilia:class-precedence-list))
         (table (clostrum-basic::functions e4))
         (standard-generic-functions
           (find-all-standard-generic-functions
            table standard-generic-function-class))
         (satiate-function
           (clo:fdefinition
            client e3 @clostrophilia:satiate-generic-function)))
    (loop for (name . function) in standard-generic-functions
          when (or (generic-function-is-a-metaobject-slot-reader-p
                    function
                    generic-function-methods-function
                    class-of-function
                    standard-reader-method-class
                    method-specializers-function
                    metaobject-class
                    class-precedence-list-function)
                   (generic-function-is-a-metaobject-slot-writer-p
                    function
                    generic-function-methods-function
                    class-of-function
                    standard-writer-method-class
                    method-specializers-function
                    metaobject-class
                    class-precedence-list-function))
            do (format *trace-output* "Satiating: ~s~%" name)
               (funcall satiate-function function))))

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

(defun function-is-a-metaobject-function-p (function)
  (and (typep function 'sb:header)
       (eq (sb:class function) *standard-generic-function-class*)
       (let ((methods
               (funcall *generic-function-methods-function* function)))
         (loop for method in methods
                 thereis (method-is-a-metaobject-method-p method)))))
