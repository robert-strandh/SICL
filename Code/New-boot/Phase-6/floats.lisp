(cl:in-package #:sicl-new-boot-phase-6)

(eval-when (:compile-toplevel) (sb:enable-parcl-symbols client))

(defvar *single-float-class*)

(defvar *single-float-unique-number*)

(defun make-single-float (sign rational)
  (let* ((floatr (buoy-simulate:floatr32-from-rational rational))
         (rack-contents
           (list *single-float-unique-number* nil sign floatr))
         (rack (make-array 4 :initial-contents rack-contents)))
    (make-instance 'sb:header
      :class *single-float-class*
      :rack rack)))

(defvar *double-float-class*)

(defvar *double-float-unique-number*)

(defun make-double-float (sign rational)
  (let* ((floatr (buoy-simulate:floatr64-from-rational rational))
         (rack-contents
           (list *double-float-unique-number* nil sign floatr))
         (rack (make-array 4 :initial-contents rack-contents)))
    (make-instance 'sb:header
      :class *double-float-class*
      :rack rack)))

(defun float-components (float)
  (let* ((rack (sb:rack float)))
    (values (aref rack 2) (aref rack 3))))

(defvar *complex-class*)

(defvar *complex-unique-number*)

(defun make-complex (realpart imagpart)
  (let* ((rack-contents
           (list *complex-unique-number* nil realpart imagpart))
         (rack (make-array 4 :initial-contents rack-contents)))
    (make-instance 'sb:header
      :class *complex-class*
      :rack rack)))

(defun find-arithmetic-classes (client e4)
  (setf *single-float-class*
        (clo:find-class client e4 'single-float))
  (setf *double-float-class*
        (clo:find-class client e4 'double-float))
  (setf *complex-class*
        (clo:find-class client e4 'complex))
  (let ((unique-number-function
          (clo:fdefinition client e4 @clostrophilia:unique-number)))
    (setf *single-float-unique-number*
          (funcall unique-number-function *single-float-class*))
    (setf *double-float-unique-number*
          (funcall unique-number-function *double-float-class*))
    (setf *complex-unique-number*
          (funcall unique-number-function *complex-class*))))

(defmethod eclector.reader:make-literal
    ((client client) (kind eclector.reader::complex-kind)
     &key
       real-part
       imaginary-part)
  (make-complex real-part imaginary-part))

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
  (multiple-value-bind (sign floatr)
      (float-components (first arguments))
    (multiple-value-bind (significand exponent)
        (buoy-simulate:integer-decode-floatr floatr 23 8)
      (let ((sign-value (make-single-float sign 1)))
        (values significand exponent sign-value)))))

(defmethod sicl-new-boot:primop
    ((operation (eql :integer-decode-double-float)) &rest arguments)
  (multiple-value-bind (sign floatr)
      (float-components (first arguments))
    (multiple-value-bind (significand exponent)
        (buoy-simulate:integer-decode-floatr floatr 52 11)
      (let ((sign-value (make-double-float sign 1)))
        (values significand exponent sign-value)))))

(defmethod sicl-new-boot:primop
    ((operation (eql :single-float-equal)) &rest arguments)
  (multiple-value-bind (sign1 floatr1)
      (float-components (first arguments))
    (multiple-value-bind (sign2 floatr2)
        (float-components (second arguments))
      (and (= sign1 sign2) (= floatr1 floatr2)))))

(defmethod sicl-new-boot:primop
    ((operation (eql :double-float-equal)) &rest arguments)
  (multiple-value-bind (sign1 floatr1)
      (float-components (first arguments))
    (multiple-value-bind (sign2 floatr2)
        (float-components (second arguments))
      (and (= sign1 sign2) (= floatr1 floatr2)))))
  
