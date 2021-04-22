(cl:in-package #:sicl-register-allocation)

(defmacro make-register (variable name)
  `(defparameter ,variable
     (make-instance 'cleavir-ir:register-location :name ,name)))

(make-register *rax "RAX")
(make-register *rax* "RAX")
(make-register *rbx* "RBX")
(make-register *rcx* "RCX")
(make-register *rdx* "RDX")
(make-register *rsp* "RSP")
(make-register *rbp* "RBP")
(make-register *rsi* "RSI")
(make-register *rdi* "RDI")
(make-register *r8* "R8")
(make-register *r9* "R9")
(make-register *r10* "R10")
(make-register *r11* "R11")
(make-register *r12* "R12")
(make-register *r13* "R13")
(make-register *r14* "R14")
(make-register *r15* "R15")
(make-register *xmm0* "XMM0")
(make-register *xmm1* "XMM1")
(make-register *xmm2* "XMM2")
(make-register *xmm3* "XMM3")
(make-register *xmm4* "XMM4")
(make-register *xmm5* "XMM5")
(make-register *xmm6* "XMM6")
(make-register *xmm7* "XMM7")
(make-register *xmm8* "XMM8")
(make-register *xmm9* "XMM9")
(make-register *xmm10* "XMM10")
(make-register *xmm11* "XMM11")
(make-register *xmm12* "XMM12")
(make-register *xmm13* "XMM13")
(make-register *xmm14* "XMM14")
(make-register *xmm15* "XMM15")

(defparameter *registers*
  (vector *rax* *rbx* *rcx* *rdx* *rsp* *rbp* *rsi* *rdi*
          *r8* *r9* *r10* *r11* *r12* *r13* *r14* *r15*
          *xmm0* *xmm1* *xmm2* *xmm3* *xmm4* *xmm5* *xmm6* *xmm7*
          *xmm8* *xmm9* *xmm10* *xmm11* *xmm12* *xmm13* *xmm14* *xmm15*))
(defconstant +register-count+ 32)
(deftype register-map ()
  `(array bit (,+register-count+)))

(defparameter *caller-saves* #*10110011111100000000000000000000)
(defparameter *callee-saves* #*01000000000011110000000000000000)

(defun register-number-is-callee-saves-p (register-number)
  (= (bit *callee-saves* register-number) 1))

(defparameter *xmm* #*00000000000000001111111111111111)
(defparameter *gpr* #*11111111111111110000000000000000)

(defparameter *initial* #*01000000001000000000000000000000)

(defun register-number-in-map-p (register-number register-map)
  (not (zerop (bit register-map register-number))))

(defun make-empty-register-map ()
  (make-array +register-count+
              :element-type 'bit
              :initial-element 0))

(defun mark-register (register-map register-number)
  (setf (bit register-map register-number) 1))

(defun unmark-register (register-map register-number)
  (setf (bit register-map register-number) 0))

(defun copy-register-map (register-map)
  (let ((result (make-array +register-count+ :element-type 'bit)))
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

(defun register-number (register)
  (position register *registers*))

(defun make-register-map (&rest registers)
  (let ((map (make-empty-register-map)))
    (dolist (register registers)
      (mark-register map (register-number register)))
    map))
