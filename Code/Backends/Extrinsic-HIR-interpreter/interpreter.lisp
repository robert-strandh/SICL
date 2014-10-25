(cl:in-package #:sicl-extrinsic-hir-interpreter)

(defclass function () ())

(defclass built-in-function (function)
  ((%host-function :initarg :host-function :reader host-function)))

(defclass interpreted-function (function)
  (;; The initial instruction of the function.  It is always an
   ;; ENTER-INSTRUCTION.
   (%entry-point :initarg :entry-point :reader entry-point)
   ;; The captured static environment.
   (%environment :initarg :environment :reader environment)
   ;; A list of lexical variables that should be used to augment the
   ;; captured environment when the function is called.
   (%entry-lexicals :initarg :entry-lexicals :reader entry-lexicals)))

(defclass stack-frame ()
  (;; The next instruction to be executed.
   (%next-instruction :initarg next-instruction :accessor next-instruction)
   ;; The static environment is a list of hash tables. 
   (%static-env :initarg :static-env :reader static-env)
   ;; The dynamic environment is a list of instances of some subclass
   ;; of the class dynamic-environment-entry.
   (%dynamic-env :initarg :dynamic-env :accessor dynamic-env)
   ;; A list of lexical variables that should be set when this
   ;; frame is returned to from a deeper nested frame.
   (%return-variables :initform '() :accessor return-variables)
   ;; A list of Lisp objects.  The arguments that were passed to the
   ;; function call that resulted in the creation of this frame.
   (%arguments :initarg :arguments :reader arguments)))

(defclass dynamic-environment-entry () ())

(defclass variable-binding (dynamic-environment-entry)
  ((%symbol :initarg :symbol :reader symbol)
   (%value :initarg :value :reader value)))

(defclass unwind-protect (dynamic-environment-entry)
  ((%thunk :initarg :thunk :reader thunk)
   (%frame :initarg :frame :reader frame)))

(defclass catch-tag (dynamic-environment-entry)
  ((%value :initarg :value :reader value)
   (%frame :initarg :frame :reader frame)
   (%next-instruction :initarg :next-instruction :reader next-instruction)))

(defclass process ()
  (;; The global runtime environment. 
   (%global-env :initarg :global-env :reader global-env)
   ;; The stack is a list of stack frames. 
   (%stack :initarg :stack :accessor stack)))
