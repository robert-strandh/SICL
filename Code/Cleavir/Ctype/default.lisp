(in-package #:cleavir-ctype)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; This is a default implementation of the ctype protocol, for clients that
;;; don't want to bother implementing it themselves.
;;; In this implementation, ctypes are CL type specifiers, though stripped of
;;; environment dependency. CL:SUBTYPEP etc. are used.

;;; Internal: Check whether the given default ctype is a values ctype.
(defun values-ctype-p (ctype)
  (and (consp ctype) (eql (car ctype) 'cl:values)))

;;; Given a possibly non-values ctype, return a values ctype for it.
(defmethod coerce-to-values (ctype sys)
  (declare (ignore sys))
  (if (values-ctype-p ctype)
      ctype
      `(cl:values ,ctype &rest t)))

(defmethod subtypep (ct1 ct2 sys)
  (declare (ignore sys))
  (cl:subtypep ct1 ct2))

(defmethod upgraded-array-element-type (ct sys)
  (declare (ignore sys))
  (cl:upgraded-array-element-type ct))

(defmethod upgraded-complex-part-type (ct sys)
  (declare (ignore sys))
  (cl:upgraded-complex-part-type ct))

(defmethod top (sys) (declare (ignore sys)) 't)
(defmethod bottom (sys) (declare (ignore sys)) 'nil)

(defmethod top-p (ctype sys)
  (declare (ignore sys))
  (or (eql ctype 't)
      ;; Kinda KLUDGEy way to avoid (find-class 't),
      ;; which needs an environment.
      (and (cl:typep ctype 'class)
           (eql (class-name ctype) 't))))

(defmethod bottom-p (ctype sys)
  (declare (ignore sys))
  ;; We assume that NIL is not a class.
  (eql ctype 'nil))

(defmethod conjoin/2 (ct1 ct2 sys)
  ;;; Pick off some very basic cases.
  (cond ((or (bottom-p ct1 sys) (bottom-p ct2 sys)) 'nil)
        ((top-p ct1 sys) ct2)
        ((top-p ct2 sys) ct1)
        (t (let ((ty `(and ,ct1 ,ct2)))
             ;; Checking for bottom-ness is a very basic
             ;; canonicalization we can perform with the
             ;; limited tools CL gives us.
             (if (cl:subtypep ty nil)
                 nil
                 ty)))))

(defmethod disjoin/2 (ct1 ct2 sys)
  (cond ((or (top-p ct1 sys) (top-p ct2 sys)) 't)
        ((bottom-p ct1 sys) ct2)
        ((bottom-p ct2 sys) ct1)
        (t `(or ,ct1 ,ct2))))

(defmethod negate (ct sys)
  (cond ((top-p ct sys) 'nil)
        ((bottom-p ct sys) 't)
        (t `(not ,ct))))

(defmethod subtract (ct1 ct2 sys)
  (cond ((bottom-p ct1 sys) 'nil)
        ((bottom-p ct2 sys) ct1)
        ((top-p ct2 sys) 'nil)
        (t `(and ,ct1 (not ,ct2)))))

(defmethod cons (car cdr sys)
  (declare (ignore sys))
  (cond ((eql car 'nil) 'nil)
        ((eql cdr 'nil) 'nil)
        (t `(cl:cons ,car ,cdr))))

(defmethod array (element dimensions simplicity sys)
  (declare (ignore sys))
  `(,simplicity ,element ,dimensions))

(defmethod complex (part sys)
  (declare (ignore sys))
  `(cl:complex ,part))

(defmethod range (type low high sys)
  (declare (ignore sys))
  `(,type ,low ,high))

(defmethod member (sys &rest elems)
  (declare (ignore sys))
  `(cl:member ,@elems))

(defmethod satisfies (fname sys)
  (declare (ignore sys))
  `(cl:satisfies ,fname))

(defmethod function (req opt rest keyp keys aokp returns sys)
  (declare (ignore sys))
  `(cl:function (,@req &optional ,@opt &rest ,rest
                       ,@(when keyp `(&key ,keys))
                       ,@(when aokp '(&allow-other-keys)))
                ,returns))

(defmethod values (req opt rest sys)
  (declare (ignore sys))
  `(cl:values ,@req &optional ,@opt &rest ,rest))

;;; These readers work on the premise that these type specifiers
;;; are normalized by the above functions, so they always have
;;; certain lambda list keywords.
(defmethod required (ctype system)
  (declare (ignore system))
  (let ((to-search
          (ecase (first ctype)
            ((cl:values) (cl:rest ctype))
            ((cl:function) (second ctype)))))
    (ldiff to-search (member '&optional to-search))))

(defmethod optional (ctype system)
  (declare (ignore system))
  (let ((to-search
          (ecase (first ctype)
            ((cl:values) (cl:rest ctype))
            ((cl:function) (second ctype)))))
    (ldiff (member '&optional to-search)
           (member '&rest to-search))))

(defmethod rest (ctype system)
  (declare (ignore system))
  (let ((to-search
          (ecase (first ctype)
            ((cl:values) (cl:rest ctype))
            ((cl:function) (second ctype)))))
    (second (member '&rest to-search))))

;;; Again, these below are only valid for function ctypes.
(defmethod keysp (ctype system)
  (declare (ignore system))
  (member '&key (second ctype)))

(defmethod keys (ctype system)
  (declare (ignore system))
  (let ((res (member '&key (second ctype))))
    (if res
        (ldiff res (member '&allow-other-keys res))
        nil)))

(defmethod allow-other-keys-p (ctype system)
  (declare (ignore system))
  (member '&allow-other-keys (second ctype)))

(defmethod returns (ctype system)
  (declare (ignore system))
  (third ctype))
