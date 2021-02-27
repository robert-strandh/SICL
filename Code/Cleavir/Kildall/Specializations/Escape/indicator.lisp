(in-package #:cleavir-escape)

(defgeneric object-meet (s o1 o2))
(defgeneric object<= (s o1 o2))
(defgeneric object1 (s variable))

;;;; Escape indicators are bitfields because meet can be OR.
;;;; Bit layout:
;;;; 0 - unknown operation. Cleavir cannot determine DXness.
;;;; 1 - called. DX as long as functions that return themselves
;;;;             are not otherwise allowed. (They're not because
;;;;             WRITE-CELL is an unknown.)
;;;; 2 - returned. Not DX.
;;;; 3 - stored globally. Not DX.
;;;; So a variable can be DXd if it's 0010 or 0000.
;;;; Meet is union of variables and OR of indicators.

(defconstant +none+ #b0000)
(defconstant +unknown+ #b0001)
(defconstant +called+ #b0010)
(defconstant +returned+ #b0100)
(defconstant +stored+ #b1000)

(deftype indicator () '(unsigned-byte 4))

(declaim (inline indicator-union indicator<=
                 dxable-p escapes-p
                 without-unknown))

(defun indicator-union (&rest indicators)
  (apply #'logior indicators))

(defun indicator<= (i1 i2)
  (declare (type indicator i1 i2))
  (zerop (logandc1 i1 i2)))

(defmethod object-meet ((s escape) i1 i2)
  (declare (indicator i1 i2))
  (logior i1 i2))

(defmethod object<= ((s escape) i1 i2)
  (declare (indicator i1 i2))
  (indicator<= i1 i2))

(defmethod object1 ((s escape) (v cleavir-ir:lexical-location))
  +none+)
(defmethod object1 ((s escape) (v cleavir-ir:values-location))
  +none+)

(defun dxable-p (indicator)
  (declare (type indicator indicator))
  ;; only being called is allowed.
  (zerop (logand indicator
                 (indicator-union +unknown+ +returned+ +stored+))))

(defun escapes-p (indicator)
  (declare (type indicator indicator))
  ;; This is used to verify dynamic-extent declarations.
  ;; It's only true if it DEFINITELY escapes, not just maybe escapes.
  (not (zerop (logand indicator
                      (indicator-union +returned+ +stored+)))))

(defun without-unknown (indicator)
  (declare (type indicator indicator))
  ;; This is used for dynamic-extent assertion.
  (logand indicator (lognot +unknown+)))
