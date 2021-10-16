(cl:in-package #:sicl-type)

(defun typep-atomic (object type-specifier)
  (cond ((symbolp type-specifier)
         (case type-specifier
           ((nil)
            nil)
           (atom
            (not (consp object)))
           ((base-char standard-char)
            (characterp object))
           (keyword
            (and (symbolp object)
                 (eq (symbol-package object)
                     (find-package '#:keyword))))
           (simple-array
            (typep object 'array))
           (class
            (let ((object-class (class-of object)))
              ;; RETURN true if and only if the class named CLASS is a
              ;; member of the class precedence list of the class of
              ;; the object.
              (if (member (find-class 'class)
                          (sicl-clos:class-precedence-list object-class))
                  t nil)))
           (otherwise
            (let ((expander (type-expander type-specifier))
                  (type-class (find-class type-specifier nil)))
              (cond ((not (null expander))
                     ;; We found an expander.  Expand TYPE-SPECIFIER and call
                     ;; TYPEP recursively with the expanded type specifier.
                     (typep object (funcall expander (list type-specifier) nil)))
                    ((not (null type-class))
                     ;; TYPE-SPECIFIER is the name of a class.
                     (let ((object-class (class-of object)))
                       ;; RETURN true if and only if TYPE-SPECIFIER is
                       ;; a member of the class precedence list of the
                       ;; class of the object.  This code is
                       ;; duplicated, but the duplication avoids the check that
                       ;; the class of the object is a class, and it simplifies
                       ;; bootstrapping.
                       (if (member type-class
                                   (sicl-clos:class-precedence-list object-class))
                           t nil)))
                    (t
                     ;; TYPE-SPECIFIER has no expander associated with it and it
                     ;; is not also a class.  Furthermore, there was no method
                     ;; on TYPEP-ATOMIC specialized to the name of the type.
                     ;; This can only mean that TYPE-SPECIFIER is not a valid
                     ;; type specifier.
                     (error "unknown type ~s" type-specifier)))))))
        ((typep type-specifier 'class)
         (let ((object-class (class-of object)))
           ;; RETURN true if and only if TYPE-SPECIFIER is a member of the
           ;; class precedence list of the class of the object.
           (if (member type-specifier (sicl-clos:class-precedence-list object-class))
               t nil)))
        (t
         (error "Invalid type specifier ~s" type-specifier))))
