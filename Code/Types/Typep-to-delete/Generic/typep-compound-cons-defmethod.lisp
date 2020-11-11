(cl:in-package #:sicl-type)

(defmethod typep-compound (object (head (eql 'cons)) rest)
  nil)

(defmethod typep-compound ((object cons) (head (eql 'cons)) rest)
  (when (null rest)
    ;; the type specifier is (CONS), so since OBJECT is a CONS, we
    ;; are done.
    (return-from typep-compound t))
  ;; the type specifier is (CONS <type> . ...)
  (unless (generic-typep (car object) (first rest))
    (return-from typep-compound nil))
  ;; The CAR of OBJECT is the right type.  Now check the CDR.
  (when (null (rest rest))
    ;; the type specifier is (CONS <type>), so since OBJECT is a CONS, and
    ;; the CAR is the right type, we are done.
    (return-from typep-compound t))
  ;; the type specifier is (CONS <type> <type>)
  (generic-typep (cdr object) (second rest)))
