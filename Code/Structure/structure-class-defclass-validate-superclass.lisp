(cl:in-package #:sicl-structure)

(defmethod mop:validate-superclass ((class structure-class) (superclass (eql (find-class 't))))
  ;; T is not a valid direct superclass, all structures inherit from STRUCTURE-OBJECT.
  nil)

(defmethod mop:validate-superclass ((class structure-class) (superclass (eql (find-class 'standard-object))))
  ;; Only STRUCTURE-OBJECT may have STANDARD-OBJECT as a direct superclass, all
  ;; other structure classes must inherit from STRUCTURE-OBJECT.
  (eql (class-name class) 'structure-object))
