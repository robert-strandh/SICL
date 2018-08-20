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
    ;; FIXME: do this with a specific error type.
    (error "Malformed typespecifier"))
  (not (typep object (first subsidiary-type-information) environment)))

(defmethod typep-compound
    (object
     (atomic-type-specifier (eql 'eql))
     subsidiary-type-information
     environment)
  (unless (null (rest subsidiary-type-information))
    ;; FIXME: do this with a specific error type.
    (error "Malformed typespecifier"))
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
    ;; FIXME: do this with a specific error type.
    (error "Malformed typespecifier"))
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
  (and (typep object atomic-type-specifier environment)
       (call-next-method)))

(defmethod typep (object (type-specifier (eql 'values)) environment)
  (error "Illegal type specifier for TYPEP."))
