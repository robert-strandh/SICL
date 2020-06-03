(cl:in-package #:sicl-type)

(defmethod typep-compound (object (head (eql 'complex)) rest)
  nil)

(defmethod typep-compound ((complex object) (head (eql 'complex)) rest)
  (when (null rest)
    ;; the type specifier is (COMPLEX), so since OBJECT is a complex, we
    ;; are done.
    (return-from typep-compound t))
  ;; the type specifier is (COMPLEX ...)
  (if (eq (first rest) '*)
      t
      (let ((type (upgraded-complex-part-type (first rest))))
	;; the type specifier is (COMPLEX <type>).  In order for TYPEP to
	;; return true, the element type of the complex must be the
	;; same as the result of upgrading <type>.
	(and (generic-typep (realpart object) type)
	     (generic-typep (imagpart object) type)))))
