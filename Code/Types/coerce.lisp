(cl:in-package #:sicl-type)

;;; COERCE is written as an ordinary function that calls
;;; GENERIC-COERCE.  The reason COERCE itself is not generic is
;;; because COERCE is needed during file loading, so if it is only
;;; partially implemented, the file will fail to load.  By doing it
;;; this way instead, COERCE itself is updated "atomically" so the
;;; issue is no longer present.

(defgeneric generic-coerce (object result-ctype))

(defmethod generic-coerce (object result-ctype)
  ;; This is not quite right.  We should define a subtype of
  ;; TYPE-ERROR for this situaiton.
  (error 'type-error
         :datum object
         :expected-type (ctype:unparse result-ctype)))

;;; This method is invoked if we are trying to coerce something to the
;;; type NULL, but it may be invoked in other situations as well.
(defmethod generic-coerce
    ((object vector) (result-ctype ctype:cmember))
  (let ((members (ctype:cmember-members result-ctype)))
    (if (and (zerop (length object))
             (= (length members) 1)
             (eq (first members) nil))
        '()
        (call-next-method))))

(defmethod generic-coerce
    ((object vector) (result-ctype ctype:ccons))
  (let ((result
          (loop for element across object
                collect element)))
    (if (ctype:ctypep result result-ctype)
        result
        (call-next-method))))

(defmethod generic-coerce
    ((object list) (result-ctype ctype:carray))
  (let* ((dimensions (ctype:carray-dims result-ctype))
         (uaet (ctype::carray-uaet result-ctype))
         (element-type (if (eq uaet '*) 't uaet))
         (length (length object)))
    (cond (;; Check that the array type has a single dimension.
           (not (= (length dimensions) 1))
           (call-next-method))
          (;; Check that the length of type specifier is either * or
           ;; else that the length indicated corresponds to the length
           ;; of the list we were given as an object.
           (and (not (eq (first dimensions) '*))
                (/= length (first dimensions)))
           (call-next-method))
          (;; Check that every element of the list we were given as an
           ;; object is of the type of the element type.  If that is
           ;; not the case, then the elements of the list can not be
           ;; stored in a vector of the type indicated.
           (notevery (lambda (x) (typep x element-type))
                     object)
           (call-next-method))
          (t
           ;; Everything seems to be OK.
           (let ((result (make-array length :element-type element-type)))
             (loop for element in object
                   for index from 0
                   do (setf (aref result index) element))
             result)))))

(defmethod generic-coerce
    ((object string) (result-ctype ctype:charset))
  (if (= (length object) 1)
      (char object 0)
      (call-next-method)))

(defmethod generic-coerce
    ((object real) (result-ctype ctype:ccomplex))
  (let ((result (complex object (coerce 0 (type-of object)))))
    (if (ctype:ctypep result-ctype)
        result
        (call-next-method))))

(defmethod generic-coerce 
    ((object rational) (result-ctype ctype:ccomplex))
  object)

(defmethod generic-coerce
    ((object real) (result-ctype ctype:range))
  (let ((result-type (ctype:unparse result-ctype)))
    (case result-type
      (single-float (float object 1s0))
      (double-float (float object 1d0))
      (otherwise (call-next-method)))))

(defmethod generic-coerce
    ((object cons) (result-ctype ctype:cfunction))
  (if (or (not (eq (first object) 'setf))
          (atom (rest object))
          (not (null (rest (rest object)))))
      (call-next-method)
      (if (fboundp object)
          (fdefinition object)
          ;; We might want to do this a bit better.
          (call-next-method))))

(defmethod generic-coerce
    ((object symbol) (result-ctype ctype:cfunction))
  (if (or (not (fboundp object))
          (not (null (macro-function object)))
          (special-operator-p object))
      (call-next-method)
      (fdefinition object)))

(defun coerce (object result-type)
  (if (typep object result-type)
      object
      (let ((result-ctype (ctype:specifier-ctype result-type)))
        (generic-coerce object result-ctype))))
