(cl:in-package #:cleavir-processor-x86-64)

(defclass x86-64 ()
  ((%implementation :initarg :implementation :reader implementation)
   ;; We simplify things for now by defining only 16 64-bit registers.
   ;; Later, we need to define floating-point registers, but also gprs
   ;; with 32, 16, and 8 bits.
   (%gprs :initform (make-array 16) :reader gprs)))

(defmethod initialize-instance :after ((object x86-64) &key &allow-other-keys)
  (let ((gprs (gprs object)))
    (macrolet ((reg (i) `(aref gprs ,i)))
      (flet ((make-reg (name)
	       (make-instance 'cleavir-ir:register-location :name name)))
	(setf (reg 0) (make-reg "RAX"))
	(setf (reg 1) (make-reg "RBX"))
	(setf (reg 2) (make-reg "RCX"))
	(setf (reg 3) (make-reg "RDX"))
	(setf (reg 4) (make-reg "RSI"))
	(setf (reg 5) (make-reg "RDI"))
	(setf (reg 6) (make-reg "RBP"))
	(setf (reg 7) (make-reg "RSP"))
	(loop for i from 8 to 15
	      do (setf (reg i) (make-reg (format nil "R~a" i))))))))

