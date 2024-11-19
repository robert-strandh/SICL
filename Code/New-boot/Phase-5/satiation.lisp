(cl:in-package #:sicl-new-boot-phase-5)

(eval-when (:compile-toplevel) (sb:enable-parcl-symbols client))

(defun class-is-subclass-of-metaobject-p
    (class metaobject-class class-precedence-list-function)
  (member metaobject-class (funcall class-precedence-list-function class)
          :test #'eq))

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
              (specializer (first specializers)))
         (class-is-subclass-of-metaobject-p
          specializer metaobject-class class-precedence-list-function))))

(defun find-all-standard-generic-functions
    (hash-table standard-generic-function-class)
  (loop for entry being each hash-value of hash-table
        for cell = (clostrum-basic::cell entry)
        for operator = (car cell)
        when (and (typep operator 'sb:header)
                  (eq (sb:class operator) standard-generic-function-class))
          collect operator))

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
           (clo:find-class client e3 @clostrophilia:metaobject))
         (class-precedence-list-function
           (clo:fdefinition client e3 @clostrophilia:class-precedence-list))
         (table (clostrum-basic::functions e4))
         (standard-generic-functions
           (find-all-standard-generic-functions
            table standard-generic-function-class))
         (satiate-function
           (clo:fdefinition
            client e3 @clostrophilia:satiate-generic-function)))
    (loop for standard-generic-function in standard-generic-functions
          when (or (generic-function-is-a-metaobject-slot-reader-p
                    standard-generic-function
                    generic-function-methods-function
                    class-of-function
                    standard-reader-method-class
                    method-specializers-function
                    metaobject-class
                    class-precedence-list-function)
                   (generic-function-is-a-metaobject-slot-writer-p
                    standard-generic-function
                    generic-function-methods-function
                    class-of-function
                    standard-writer-method-class
                    method-specializers-function
                    metaobject-class
                    class-precedence-list-function))
            do (funcall satiate-function standard-generic-function))))
