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

(defun register-number-in-map-p (register-number register-map)
  (not (zerop (bit register-map register-number))))

(defun make-empty-register-map ()
  (make-array 16 :element-type 'bit :initial-element 0))

(defun mark-register (register-map register-number)
  (setf (bit register-map register-number) 1))

(defun copy-register-map (register-map)
  (let ((result (make-array (length register-map) :element-type 'bit)))
    (replace result register-map)
    result))

(defun reserve-register (register-map register-number)
  (let ((result (copy-register-map register-map)))
    (assert (zerop (bit result register-number)))
    (setf (bit result register-number) 1)
    result))

(defun free-register (register-map register-number)
  (let ((result (copy-register-map register-map)))
    (assert (= (bit result register-number) 1))
    (setf (bit result register-number) 0)
    result))

(defun register-map-difference (register-map-1 register-map-2)
  (bit-andc2 register-map-1 register-map-2))

(defun find-any-register-in-map (register-map)
  (position 1 register-map))
