(in-package #:cleavir-escape)

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

(defmethod cleavir-kildall:object-meet ((s escape) i1 i2)
  (cond ((arrayp i1) ; have to worry about function indicators now
         (assert (and (arrayp i2) (= (length i1) (length i2))))
         (map '(simple-array indicator (*))
              #'indicator-union i1 i2))
        (t (logior i1 i2))))

(defmethod cleavir-kildall:object<= ((s escape) i1 i2)
  (cond ((arrayp i1)
         (assert (and (arrayp i2) (= (length i1) (length i2))))
         (every #'indicator<= i1 i2))
        (t (indicator<= i1 i2))))

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

;;; Doesn't need a method for function-indicators since those are
;;; never on outputs or inputs. But that's FRAGILE.
(defmethod cleavir-kildall-graphviz:draw-object ((s escape) o)
  (with-output-to-string (s)
    (when (logbitp 3 o) ; store
      (write-char #\S s))
    (when (logbitp 2 o) ; return
      (write-char #\R s))
    (when (logbitp 1 o) ; call
      (write-char #\C s))
    (when (logbitp 0 o) ; unknown
      (write-char #\? s))))
