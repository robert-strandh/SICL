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
     standard-slot-reader-class)
  (let ((methods
          (funcall generic-function-methods-function generic-function)))
    (and (= (length methods) 1)
         (eq (funcall class-of-function (first methods))
             standard-slot-reader-class))))

(defun generic-function-is-a-slot-writer-p
    (generic-function
     generic-function-methods-function
     class-of-function
     standard-slot-writer-class)
  (let ((methods
          (funcall generic-function-methods-function generic-function)))
    (and (= (length methods) 1)
         (eq (funcall class-of-function (first methods))
             standard-slot-writer-class))))

(defun generic-function-is-a-metaobject-slot-reader-p
    (generic-function
     generic-function-methods-function
     class-of-function
     standard-slot-reader-class
     method-specializers-function
     metaobject-class
     class-precedence-list-function)
  (and (generic-function-is-a-slot-reader-p
        generic-function
        generic-function-methods-function
        class-of-function
        standard-slot-reader-class)
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
     standard-slot-writer-class
     method-specializers-function
     metaobject-class
     class-precedence-list-function)
  (and (generic-function-is-a-slot-writer-p
        generic-function
        generic-function-methods-function
        class-of-function
        standard-slot-writer-class)
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
