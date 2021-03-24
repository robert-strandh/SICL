(cl:in-package #:sicl-register-allocation)

(defparameter *rax*
  (make-instance 'cleavir-ir:register-location :name "RAX"))

(defparameter *rbx*
  (make-instance 'cleavir-ir:register-location :name "RBX"))

(defparameter *rcx*
  (make-instance 'cleavir-ir:register-location :name "RCX"))

(defparameter *rdx*
  (make-instance 'cleavir-ir:register-location :name "RDX"))

(defparameter *rsp*
  (make-instance 'cleavir-ir:register-location :name "RSP"))

(defparameter *rbp*
  (make-instance 'cleavir-ir:register-location :name "RBP"))

(defparameter *rsi*
  (make-instance 'cleavir-ir:register-location :name "RSI"))

(defparameter *rdi*
  (make-instance 'cleavir-ir:register-location :name "RDI"))

(defparameter *r8*
  (make-instance 'cleavir-ir:register-location :name "R8"))

(defparameter *r9*
  (make-instance 'cleavir-ir:register-location :name "R9"))

(defparameter *r10*
  (make-instance 'cleavir-ir:register-location :name "R10"))

(defparameter *r11*
  (make-instance 'cleavir-ir:register-location :name "R11"))

(defparameter *r12*
  (make-instance 'cleavir-ir:register-location :name "R12"))

(defparameter *r13*
  (make-instance 'cleavir-ir:register-location :name "R13"))

(defparameter *r14*
  (make-instance 'cleavir-ir:register-location :name "R14"))

(defparameter *r15*
  (make-instance 'cleavir-ir:register-location :name "R15"))

(defparameter *registers*
  (vector *rax* *rbx* *rcx* *rdx* *rsp* *rbp* *rsi* *rdi*
          *r8* *r9* *r10* *r11* *r12* *r13* *r14* *r15*))

(defparameter *caller-saves* #*1011001111110000)

(defparameter *callee-saves* #*0100000000001111)

(defun copy-register-map (register-map)
  (let ((result (make-array (length register-map) :element-type 'bit)))
    (replace result register-map)
    result))
