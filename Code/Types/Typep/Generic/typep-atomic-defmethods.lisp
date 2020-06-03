(cl:in-package #:sicl-type)

;;; Most cases are captured by this function, since most atomic type
;;; specifiers have corresponding classes.
(defmethod typep-atomic (object (type-specifier symbol))
  (let* ((global-environment (sicl-genv:global-environment))
	 (expander (sicl-genv:type-expander type-specifier global-environment))
	 (type-class (sicl-genv:find-class type-specifier global-environment)))
    (cond ((not (null expander))
	   ;; We found an expander.  Expand TYPE-SPECIFIER and call
	   ;; TYPEP recursively with the expanded type specifier.
	   (generic-typep object (funcall expander type-specifier)))
	  ((not (null type-class))
	   ;; TYPE-SPECIFIER is the name of a class.
           (typep-atomic object type-class))
	  (t
	   ;; TYPE-SPECIFIER has no expander associated with it and it
	   ;; is not also a class.  Furthermore, there was no method
	   ;; on TYPEP-ATOMIC specialized to the name of the type.
	   ;; This can only mean that TYPE-SPECIFIER is not a valid
	   ;; type specifier.
	   (error "unknown type ~s" type-specifier)))))

(defmethod typep-atomic (object (type-specifier class))
  (let ((object-class (class-of object)))
    ;; RETURN true if and only if TYPE-SPECIFIER is a member of the
    ;; class precedence list of the class of the object.
    (if (member type-specifier (sicl-clos:class-precedence-list object-class))
        t nil)))

;;; ATOM is not a class, so we can either special-case it as we do
;;; here, or we could have a type expander that expands ATOM to (NOT
;;; CONS).
(defmethod typep-atomic (object (type (eql 'atom)))
  t)

(defmethod typep-atomic ((object cons) (type (eql 'atom)))
  nil)

(defmethod typep-atomic (object (type (eql 'base-char)))
  nil)

;;; BASE-CHAR and STANDARD-CHAR are not classes so we handle them
;;; specially here.
(defmethod typep-atomic ((object character) (type (eql 'base-char)))
  t)

(defmethod typep-atomic (object (type (eql 'standard-char)))
  nil)

(defmethod typep-atomic ((object character) (type (eql 'standard-char)))
  t)

(defmethod typep-atomic (object (type (eql 'keyword)))
  nil)

(defmethod typep-atomic ((object symbol) (type (eql 'keyword)))
  (eq (symbol-package object)
      (find-package '#:keyword)))

(defmethod typep-atomic (object (type (eql 'nil)))
  nil)

;; (defmethod typep-atomic (object (type (eql 'simple-array)))
;;   nil)

;; (defmethod typep-atomic ((object array) (type (eql 'simple-array)))
;;   t)
