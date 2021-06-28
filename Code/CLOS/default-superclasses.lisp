(cl:in-package #:sicl-clos)

;;;; The function DEFAULT-SUPERCLASSES is a SICL-SPECIFIC function
;;;; that is called by the class-initialization protocol to determine
;;;; a list of default superclasses when no superclasses are given for
;;;; the creation of a class.  This AMOP section:
;;;; http://metamodular.com/CLOS-MOP/initialization-of-class-metaobjects2.html
;;;; describes that when the class that is being instantiated is an
;;;; instance of STANDARD-CLASS, then the default superclasses is a
;;;; list of a single class, namely the class named STANDARD-OBJECT,
;;;; and that when the class that is being instantiated is an instance
;;;; of FUNCALLABLE-STANDARD-CLASS, then the default superclasses is a
;;;; list of a single class, namely the class named
;;;; FUNCALLABLE-STANDARD-OBJECT.  However, in SICL, we turned that
;;;; rule into a generic function called DEFAULT-SUPERCLASSES that
;;;; have methods specialized to STANDARD-CLASS, and
;;;; FUNCALLABLE-STANDARD-CLASS, but other methods can be added as
;;;; well.

(defgeneric default-superclasses (class))

(defmethod default-superclasses ((class class))
  '())

(defmethod default-superclasses ((class standard-class))
  (list (find-class 'standard-object)))

(defmethod default-superclasses ((class funcallable-standard-class))
  (list (find-class 'funcallable-standard-object)))
