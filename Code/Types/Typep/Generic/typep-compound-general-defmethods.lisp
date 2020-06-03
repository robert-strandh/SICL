(cl:-in-package #:sicl-type)

(defun proper-list-p (object)
  (integerp (ignore-errors (list-length object))))

(defmethod typep-compound :before (object head rest)
  (assert (proper-list-p rest)))

;;; Given a type specifier that is illegal in the context of TYPEP,
;;; such as VALUES or FUNCTION, signal an error to that effect.
(defmethod typep-compound (object head rest)
  (declare (ignore object))
  (error "Compound type specifier is illegal for typep: ~s."
	 (cons head rest)))
