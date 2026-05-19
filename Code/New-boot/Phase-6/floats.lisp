(cl:in-package #:sicl-new-boot-phase-6)

(eval-when (:compile-toplevel) (sb:enable-parcl-symbols client))

(defvar *single-float-class*)

(defvar *single-float-unique-number*)

(defun make-single-float (pfloat)
  (let* ((pfloat32 (pf:restrict-to-ieee-single pfloat))
         (rack-contents
           (list *single-float-unique-number* nil pfloat32))
         (rack (make-array 3 :initial-contents rack-contents)))
    (make-instance 'sb:header
      :class *single-float-class*
      :rack rack)))

(defmethod sb:primop
    ((operation (eql :convert-fixnum-to-single-float)) &rest arguments)
  (let ((fixnum (car arguments)))
    (make-single-float (pf:pfloat-from-rational fixnum))))

(defvar *double-float-class*)

(defvar *double-float-unique-number*)

(defun make-double-float (rational)
  (let* ((pfloat64 (pf:restrict-to-ieee-double pfloat))
         (rack-contents
           (list *single-float-unique-number* nil pfloat64))
         (rack (make-array 3 :initial-contents rack-contents)))
    (make-instance 'sb:header
      :class *double-float-class*
      :rack rack)))

(defmethod sb:primop
    ((operation (eql :convert-fixnum-to-double-float)) &rest arguments)
  (let ((fixnum (car arguments)))
    (make-double-float (pf:pfloat-from-rational fixnum))))

(defun float-pfloat (float)
  (let* ((rack (sb:rack float)))
    (aref rack 2)))

(defmethod sb:primop
    ((operation (eql :single-float-less)) &rest arguments)
  (let* ((pfloat1 (float-pfloat (first arguments)))
         (pfloat2 (float-pfloat (second arguments))))
    (pf:< pfloat1 pfloat2)))

(defmethod sb:primop
    ((operation (eql :double-float-less)) &rest arguments)
  (let* ((pfloat1 (float-pfloat (first arguments)))
         (pfloat2 (float-pfloat (second arguments))))
    (pf:< pfloat1 pfloat2)))

(defmethod sb:primop
    ((operation (eql :single-float-ln)) &rest arguments)
  (let* ((pfloat (float-pfloat (first arguments)))
         (pfloat-value (sim:pfloat-ln pfloat)))
    (make-single-float pfloat-value)))

(defmethod sb:primop
    ((operation (eql :double-float-ln)) &rest arguments)
  (let* ((pfloat (float-pfloat (first arguments)))
         (pfloat-value (sim:pfloat-ln pfloat)))
    (make-double-float pfloat-value)))

(defmethod sb:primop
    ((operation (eql :single-float-add)) &rest arguments)
  (let ((pfloat1 (float-pfloat (first arguments)))
        (pfloat2 (float-pfloat (second arguments))))
    (make-single-float (pf:+ pfloat1 pfloat2))))

(defmethod sb:primop
    ((operation (eql :double-float-add)) &rest arguments)
  (let ((pfloat1 (float-pfloat (first arguments)))
        (pfloat2 (float-pfloat (second arguments))))
    (make-double-float (pf:+ pfloat1 pfloat2))))

(defmethod sb:primop
    ((operation (eql :single-float-subtract)) &rest arguments)
  (let ((pfloat1 (float-pfloat (first arguments)))
        (pfloat2 (float-pfloat (second arguments))))
    (make-single-float (pf:- pfloat1 pfloat2))))

(defmethod sb:primop
    ((operation (eql :double-float-subtract)) &rest arguments)
  (let ((pfloat1 (float-pfloat (first arguments)))
        (pfloat2 (float-pfloat (second arguments))))
    (make-double-float (pf:- pfloat1 pfloat2))))

(defmethod sb:primop
    ((operation (eql :single-float-multiply)) &rest arguments)
  (let ((pfloat1 (float-pfloat (first arguments)))
        (pfloat2 (float-pfloat (second arguments))))
    (make-single-float (pf:* pfloat1 pfloat2))))

(defmethod sb:primop
    ((operation (eql :double-float-multiply)) &rest arguments)
  (let ((pfloat1 (float-pfloat (first arguments)))
        (pfloat2 (float-pfloat (second arguments))))
    (make-double-float (pf:* pfloat1 pfloat2))))

(defmethod sb:primop
    ((operation (eql :single-float-divide)) &rest arguments)
  (let ((pfloat1 (float-pfloat (first arguments)))
        (pfloat2 (float-pfloat (second arguments))))
    (make-single-float (pf:/ pfloat1 pfloat2))))

(defmethod sb:primop
    ((operation (eql :double-float-divide)) &rest arguments)
  (let ((pfloat1 (float-pfloat (first arguments)))
        (pfloat2 (float-pfloat (second arguments))))
    (make-double-float (pf:/ pfloat1 pfloat2))))

(defvar *complex-class*)

(defvar *complex-unique-number*)

(defun make-complex (realpart imagpart)
  (let* ((rack-contents
           (list *complex-unique-number* nil realpart imagpart))
         (rack (make-array 4 :initial-contents rack-contents)))
    (make-instance 'sb:header
      :class *complex-class*
      :rack rack)))

(defvar *ratio-class*)

(defvar *ratio-unique-number*)

(defun make-ratio (numerator denominator)
  (let* ((rack-contents
           (list *ratio-unique-number* nil numerator denominator))
         (rack (make-array 4 :initial-contents rack-contents)))
    (make-instance 'sb:header
      :class *ratio-class*
      :rack rack)))

(defun find-arithmetic-classes (client e4)
  (setf *single-float-class*
        (clo:find-class client e4 'single-float))
  (setf *double-float-class*
        (clo:find-class client e4 'double-float))
  (setf *complex-class*
        (clo:find-class client e4 'complex))
  (setf *ratio-class*
        (clo:find-class client e4 'ratio))
  (let ((unique-number-function
          (clo:fdefinition client e4 @clostrophilia:unique-number)))
    (setf *single-float-unique-number*
          (funcall unique-number-function *single-float-class*))
    (setf *double-float-unique-number*
          (funcall unique-number-function *double-float-class*))
    (setf *complex-unique-number*
          (funcall unique-number-function *complex-class*))
    (setf *ratio-unique-number*
          (funcall unique-number-function *ratio-class*))))

(defmethod eclector.reader:make-literal
    ((client client) input-stream (kind eclector.reader::complex-kind)
     &key
       real-part
       imaginary-part)
  (declare (ignore input-stream))
  (make-complex real-part imaginary-part))

(defmethod eclector.reader:make-literal
    ((client client) input-stream (kind eclector.reader::ratio-kind)
     &key
       numerator
       denominator)
  (declare (ignore input-stream))
  (make-ratio numerator denominator))

(defun bits-to-single-float (bits)
  (let* ((sign-bit (ldb (byte 1 31) bits))
         (exponent-bits (ldb (byte 8 23) bits))
         (mantissa-bits (ldb (byte 23 0) bits))
         (numerator
           (cond ((zerop exponent-bits)
                  ;; We have a subnormal float.
                  mantissa-bits)
                 ((>= exponent-bits (+ 127 23))
                  (ash (+ mantissa-bits (ash 1 23))
                       (- exponent-bits (+ 127 23))))
                 (t
                  (+ mantissa-bits (ash 1 23)))))
         (denominator
           (if (>= exponent-bits (+ 127 23))
               1
               (ash 1 (- (+ 127 23) exponent-bits)))))
    (make-single-float
     (if (zerop sign-bit) 1 -1)
     (/ numerator denominator))))

(defun bits-to-double-float (bits)
  (let* ((sign-bit (ldb (byte 1 63) bits))
         (exponent-bits (ldb (byte 11 52) bits))
         (mantissa-bits (ldb (byte 52 0) bits))
         (numerator
           (cond ((zerop exponent-bits)
                  ;; We have a subnormal float.
                  mantissa-bits)
                 ((>= exponent-bits (+ 1023 52))
                  (ash (+ mantissa-bits (ash 1 52))
                       (- exponent-bits (+ 1023 52))))
                 (t
                  (+ mantissa-bits (ash 1 52)))))
         (denominator
           (if (>= exponent-bits (+ 1023 52))
               1
               (ash 1 (- (+ 1023 52) exponent-bits)))))
    (make-double-float
     (if (zerop sign-bit) 1 -1)
     (/ numerator denominator))))

(defmethod sicl-new-boot:primop
    ((operation (eql :integer-decode-single-float)) &rest arguments)
  (let ((pfloat1 (float-pfloat (first arguments)))
        (pfloat2 (float-pfloat (second arguments))))
    (values significand exponent sign)))

(defmethod sicl-new-boot:primop
    ((operation (eql :integer-decode-double-float)) &rest arguments)
  (let* ((pfloat1 (float-pfloat (first arguments)))
         (pfloat2 (float-pfloat (second arguments))))
      (values significand exponent sign)))

(defmethod sicl-new-boot:primop
    ((operation (eql :single-float-equal)) &rest arguments)
  (let ((pfloat1 (float-pfloat (first arguments)))
        (pfloat2 (float-pfloat (second arguments))))
    (pf:= pfloat1 pfloat2)))

(defmethod sicl-new-boot:primop
    ((operation (eql :double-float-equal)) &rest arguments)
  (let ((pfloat1 (float-pfloat (first arguments)))
        (pfloat2 (float-pfloat (second arguments))))
    (pf:= pfloat1 pfloat2)))

(defmethod sicl-new-boot:primop
    ((operation (eql :single-float-not-greater))  &rest arguments)
  (let ((pfloat1 (float-pfloat (first arguments)))
        (pfloat2 (float-pfloat (second arguments))))
    (not (pf:< pfloat2 pfloat1))))

(defmethod sicl-new-boot:primop
    ((operation (eql :double-float-not-greater))  &rest arguments)
  (let ((pfloat1 (float-pfloat (first arguments)))
        (pfloat2 (float-pfloat (second arguments))))
    (not (pf:< pfloat2 pfloat1))))

(defun ratio-components (float)
  (let* ((rack (sb:rack float)))
    (values (aref rack 2) (aref rack 3))))

(defmethod sicl-new-boot:primop
    ((operation (eql :convert-ratio-to-single-float)) &rest arguments)
  (multiple-value-bind (numerator denominator)
      (ratio-components (first arguments))
    (make-single-float (pf:pfloat-from-rational (/ numerator denominator)))))

(defmethod sicl-new-boot:primop
    ((operation (eql :convert-ratio-to-double-float)) &rest arguments)
  (multiple-value-bind (numerator denominator)
      (ratio-components (first arguments))
    (make-double-float (pf:pfloat-from-rational (/ numerator denominator)))))
