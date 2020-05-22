(cl:in-package #:sicl-global-environment)

(defmethod typep-compound
    (object
     (atomic-type-specifier (eql 'or))
     subsidiary-type-information
     environment)
  (loop for type in subsidiary-type-information
        thereis (typep object type environment)))

(defmethod typep-compound
    (object
     (atomic-type-specifier (eql 'and))
     subsidiary-type-information
     environment)
  (loop for type in subsidiary-type-information
       always (typep object type environment)))

(defmethod typep-compound
    (object
     (atomic-type-specifier (eql 'not))
     subsidiary-type-information
     environment)
  (unless (null (rest subsidiary-type-information))
    (error 'malformed-type-specifier
           :type-specifier (cons atomic-type-specifier
                                 subsidiary-type-information)))
  (not (typep object (first subsidiary-type-information) environment)))

(defmethod typep-compound
    (object
     (atomic-type-specifier (eql 'eql))
     subsidiary-type-information
     environment)
  (unless (null (rest subsidiary-type-information))
    (error 'malformed-type-specifier
           :type-specifier (cons atomic-type-specifier
                                 subsidiary-type-information)))
  (eql object (first subsidiary-type-information)))

(defmethod typep-compound
    (object
     (atomic-type-specifier (eql 'member))
     subsidiary-type-information
     environment)
  (member object subsidiary-type-information))

(defmethod typep-compound
    (object
     (atomic-type-specifier (eql 'satisfies))
     subsidiary-type-information
     environment)
  (unless (null (rest subsidiary-type-information))
    (error 'malformed-type-specifier
           :type-specifier (cons atomic-type-specifier
                                 subsidiary-type-information)))
  (funcall (fdefinition (first subsidiary-type-information) environment)
           object))

(defmethod typep-compound
    (object
     (atomic-type-specifier (eql 'values))
     subsidiary-type-information
     environment)
  (error "Illegal type specifier for TYPEP."))

(defmethod typep-compound :around
    (object atomic-type-specifier subsidiary-type-information environment)
  (let ((expander (type-expander atomic-type-specifier environment)))
    (if (null expander)
        (and (typep object atomic-type-specifier environment)
             (call-next-method))
        (let* ((original-type-specifier
                 (cons atomic-type-specifier subsidiary-type-information))
               (expanded-type (funcall expander
                                       original-type-specifier
                                       environment)))
          (typep object expanded-type environment)))))

(defmethod typep (object (type-specifier (eql 'values)) environment)
  (error "Illegal type specifier for TYPEP."))

(defmethod typep (object (type-specifier symbol) environment)
  (let ((expander (type-expander type-specifier environment)))
    (if (null expander)
        (let* ((type-class (find-class type-specifier environment))
               (object-class
                 (funcall
                  (fdefinition 'sicl-clos:class-of environment)
                  object)))
          (if (null type-class)
              (error "There is no type named ~s" type-specifier)
              (member type-class
                      (funcall
                       (fdefinition 'sicl-clos:class-precedence-list environment)
                       object-class))))
        (let ((expanded-type (funcall expander type-specifier environment)))
          (typep object expanded-type environment)))))

(defmethod typep (object (type-specifier cons) environment)
  (unless (cleavir-code-utilities:proper-list-p type-specifier)
    (error "Type specifier must be a proper list: ~s" type-specifier))
  (typep-compound object
                  (first type-specifier)
                  (rest type-specifier)
                  environment))
