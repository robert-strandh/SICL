(cl:in-package #:sicl-type)

(defmethod typep-compound :before (object (head (eql 'array)) rest)
  (assert (or (null rest)
              (let ((element-type (first rest)))
                ;; FIXME: check that the first element is a valid type
                ;; specifier or the symbol *.
                (declare (ignore element-type))
                (or (null (rest rest))
                    (let ((dimension-spec (second rest)))
                      (or (and (typep dimension-spec 'fixnum)
                               (not (minusp dimension-spec)))
                          (eq dimensions-spec '*)
                          (and (proper-list-p dimension-spec)
                               (loop for dimension in dimension-spec
                                     always (or (eq dimension '*)
                                                (and (typep dimension 'fixnum)
                                                     (not (minusp dimension-spec)))))))))))))

;;; Given two type specifiers, both indicating possible upgraded array
;;; element types, return true if and only if the two represent the
;;; same type.
(defun same-array-element-type-p (type-descriptor1 type-descriptor2)
  ;; If we are lucky, the two type descriptors are EQUAL.  In fact, I
  ;; can't imagine how an implementation would NOT make then EQUAL.
  (or (equal type-descriptor1 type-descriptor2)
      ;; If we are not lucky, we must ask SUBTYPEP, and then we insist
      ;; that it return true for the second return value in both
      ;; cases.
      (multiple-value-bind (subtype1-p valid1-p)
	  (subtypep type-descriptor1 type-descriptor2)
	(multiple-value-bind (subtype2-p valid2-p)
	    (subtypep type-descriptor2 type-descriptor1)
	  (assert (and valid1-p valid2-p))
	  (and subtype1-p subtype2-p)))))

(defmethod typep-compound (object (head (eql 'array)) rest)
  nil)

(defmethod typep-compound ((object array) (head (eql 'array)) rest)
  (when (null rest)
    ;; the type specifier is (ARRAY), so since OBJECT is an
    ;; array, we are done.
    (return-from typep-compound t))
  ;; the type specifier is (ARRAY ...)
  (unless (eq (first rest) '*)
    ;; The type specifier is either (ARRAY <type>) or (ARRAY
    ;; <type> ...).  In order for TYPEP to return
    ;; true, the element type of the array must be the
    ;; same as the result of upgrading <type>.  FIXME:
    ;; is EQUAL the right thing to do here?
    (unless (same-array-element-type-p (array-element-type object)
				       (upgraded-array-element-type
					(first rest)))
      ;; No luck, they are not the same.
      (return-from typep-compound nil)))
  ;; The element types are compatible.  Check whether
  ;; we have  (ARRAY <type>) or (ARRAY <type> ...).
  (when (null (rest rest))
    ;; We have (ARRAY <type>) so we are done.
    (return-from typep-compound t))
  ;; We have (ARRAY <type> ...).
  ;; Check whether we have (ARRAY <type> *)
  (when (eq (second rest) '*)
    ;; We are done.
    (return-from typep-compound t))
  ;; Check whether we have (ARRAY <type> rank), and if
  ;; so whether the rank of OBJECT corresponds.
  (when (integerp (second rest))
    (return-from typep-compound
      (= (array-rank object) (second rest))))
  ;; We must have (ARRAY <type> (...)).  Start by
  ;; checking that the rank of OBJECT is the same as
  ;; the length of the list.
  (unless (= (array-rank object) (length (second rest)))
    (return-from typep-compound nil))
  ;; The ranks are the same.  Now check that each
  ;; dimension is valid.
  (loop for d1 in (array-dimensions object)
	for d2 in (second rest)
	unless (or (eq d1 '*) (= d1 d2))
	  do (return-from typep-compound nil))
  ;; Every dimension is valid.
  (return-from typep-compound t))
