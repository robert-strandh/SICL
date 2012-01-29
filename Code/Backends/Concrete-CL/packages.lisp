(in-package #:common-lisp-user)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; The UNAMBIGUOUS-COMMON-LISP package.
;;;
;;; This package contains every exported symbol of the
;;; COMMON-LISP package, except those that have names that
;;; clash with symbols in the TARGET package (defined later). 
;;;
;;; In a package that uses this one, one can refer to most
;;; symbols in the COMMON-LISP package without any package 
;;; prefix, but if there is an ambiguity, i.e., if it is
;;; a symbol with a name that also names a symbol in the 
;;; TARGET package, then an explicit package prefix must 
;;; be used, typically host:symbol or target:symbol.
(eval-when (:compile-toplevel :load-toplevel)
  (when (find-package '#:unambiguous-common-lisp)
    (loop for package in (package-used-by-list '#:unambiguous-common-lisp)
	  do (unuse-package '#:unambiguous-common-lisp package))
    (delete-package '#:unambiguous-common-lisp)))

(defpackage #:unambiguous-common-lisp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; The SICL-COMMON-LISP package.  
;;;
;;; This is a package with the the same number of symbols and which
;;; have the same names as those in the COMMON-LISP package, but
;;; but they are distinct symbols.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (when (find-package '#:sicl-common-lisp)
    (loop for package in (package-used-by-list '#:sicl-common-lisp)
	  do (unuse-package '#:sicl-common-lisp package))
    (delete-package '#:sicl-common-lisp)))

(defpackage #:sicl-common-lisp
  (:nicknames #:scl))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (do-external-symbols (symbol '#:common-lisp)
    (export (intern (symbol-name symbol) '#:sicl-common-lisp)
	    '#:sicl-common-lisp)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; The SICL-CL-IMPLEMENTATION package. 

(defpackage #:sicl-cl-implementation
  (:use #:sicl-common-lisp)
  (:export))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Utilities

(defpackage #:sicl-utilities
  (:use #:common-lisp)
  (:export
   #:defunbound #:setunbound
   #:final-address))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; The SICL-EXP-HEAP package.  It contains code for managing the
;;; heap.  Right now it has only memory allocation.  Later, put the
;;; garbage collector here.

(defpackage #:sicl-exp-heap
  (:use #:common-lisp #:sicl-utilities)
  (:export #:+word-size+ #:+word-size-in-bytes+
	   #:word
	   #:word-from-signed-host-number #:signed-host-number-from-word
	   #:word-from-unsigned-host-number #:unsigned-host-number-from-word
	   #:malloc #:malloc-words
	   #:memref #:memset
	   #:initialize-heap
	   #:dump-heap))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; The SICL-SYSTEM package.
;;;
;;; This package contains information about the low-level 
;;; implementation of the system, information that is required
;;; by the compiler or by the runtime system.  

(defpackage #:sicl-system
  (:shadowing-import-from
   #:sicl-common-lisp
   ;; ------------------------------
   ;; Fixnum 
   ;;   System class
   #:fixnum
   ;; ------------------------------
   ;; Cons
   ;;   System class, Function
   #:cons 
   ;;   Predicate
   #:consp
   ;;   Accessors
   #:car #:cdr
   ;; ------------------------------
   ;; Other
   #:nil #:null
   #:eq
   )
  (:use #:common-lisp #:sicl-utilities
	#:sicl-exp-heap)
  (:export
   ;; The value cells of these symbols will contain the corresponding
   ;; class objects.
   #:*class-fixnum*
   #:*class-cons*
   #:*class-character*
   #:*class-vector*
   #:*class-string*
   #:*class-symbol*
   #:*class-package*
   #:*class-code*
   #:*class-function*
   #:*class-dynamic-frame*
   #:*class-unwind-protect*
   ;; The value cells of these symbols will contain vectors containing
   ;; machine instructions for various linkage purposes.
   #:*linkage-error*
   #:*linkage-function*
   #:*linkage-symbol*
   ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; The SICL-LOW package.

(defpackage #:sicl-low
  (:shadowing-import-from
   #:sicl-common-lisp
   ;; ------------------------------
   ;; Fixnum 
   ;;   System class
   #:fixnum
   ;; ------------------------------
   ;; Character 
   ;;   System class
   #:character
   ;;   Predicate
   #:characterp
   ;;   Functions
   #:char-code #:code-char
   ;; ------------------------------
   ;; Cons
   ;;   System class, Function
   #:cons 
   ;;   Predicate
   #:consp
   ;;   Functions
   #:rplaca #:rplacd
   ;;   Accessors
   #:car #:cdr
   ;;
   ;; ------------------------------
   ;; String
   ;;   System class
   #:string
   ;;   Predicate
   #:stringp
   ;;   Accessors
   #:char #:schar
   ;; ------------------------------
   ;; Function
   ;;   System class
   #:function
   ;;   Predicate
   #:functionp
   ;; ------------------------------
   ;; Symbol
   ;;   System class
   #:symbol
   ;;   Predicate
   #:symbolp
   ;;   Functions
   #:symbol-function
   #:symbol-name
   #:symbol-package
   #:symbol-plist
   #:symbol-value
   #:make-symbol
   ;; ------------------------------
   ;; Package
   ;;   System class
   #:package
   ;;   Predicate
   #:packagep
   ;;   Functions
   #:package-name
   #:package-nicknames
   #:package-use-list
   #:package-used-by-list
   #:package-shadowing-symbols
   #:find-symbol #:intern #:export
   #:find-package #:make-package
   ;;   Macros
   #:do-symbols #:do-external-symbols #:do-all-symbols
   ;;   Variables
   #:*package*
   ;; ------------------------------
   ;; Other
   #:nil #:null
   #:eq
   #:class-of
   #:describe
   #:unwind-protect
   )
  (:use #:common-lisp #:sicl-utilities
	#:sicl-exp-heap #:sicl-system)
  (:export
   ;; ------------------------------
   ;; Tags
   #:+tag-fixnum+ #:+tag-cons+ #:+tag-immediate+ #:+tag-other+ #:+tag-mask+
   ;;   Functions for manipulating tags
   #:extract-tag #:add-tag #:remove-tag
   ;; ------------------------------
   ;; Vector
   ;;   Functions
   #:make-word-vector
   #:vector-element
   #:vector-elements
   ;; ------------------------------
   ;; Symbol
   ;;   Accessors
   #:symbol-setf-function
   ;; ------------------------------
   ;; Function
   ;;   Functions
   #:make-function
   ;; ------------------------------
   ;; Code
   ;;   Functions
   #:make-code
   #:code-constants
   #:code-instructions
   ;; ------------------------------
   ;; Dynamic-frame
   ;;   Functions
   #:make-dynamic-frame
   ;; ------------------------------
   ;; Other
   #:initialize-low #:interpret
   #:object-from-host-object #:host-object-from-object
   ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Assembler

(defpackage #:sicl-assembler
  (:nicknames #:asm)
  (:use #:common-lisp)
  (:export #:opcode-to-operation
	   #:define-operation
	   #:define-assembly-macro
	   #:assemble))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;;
;; ;;; The SICL-MACHINE package.

(defpackage #:sicl-machine
  (:use #:common-lisp #:sicl-utilities #:sicl-low #:sicl-exp-heap)
  (:export))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; The SICL-CROSS-COMPILER package.
;;;
;;; This package is defined in the host environment, and uses the
;;; COMMON-LISP package of the host.  However, it shadows a few
;;; symbols to be used for definitions that run in the host (of
;;; course; this is the cross compiler), but that concern definitions
;;; in the target environment.  

(defpackage #:sicl-cross-compiler
  (:use #:common-lisp #:sicl-machine #:sicl-utilities)
  (:shadow #:defmacro
	   #:macro-function
	   #:macroexpand-1
	   #:macroexpand)
  (:export #:cross-compile-file
	   #:cross-compile-form))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Packages host and target

(eval-when (:compile-toplevel :load-toplevel)
  (when (find-package '#:target)
    (loop for package in (package-used-by-list '#:target)
	  do (unuse-package '#:target package))
    (delete-package '#:target)))

(defpackage #:target
  (:import-from #:sicl-low
		#:nil
		#:consp)
  )

(eval-when (:compile-toplevel :load-toplevel)
  (when (find-package '#:host)
    (loop for package in (package-used-by-list '#:host)
	  do (unuse-package '#:host package))
    (delete-package '#:host)))

(defpackage #:host)

(defpackage #:test-package
  (:use #:unambiguous-common-lisp))

(do-external-symbols (symbol '#:common-lisp)
  (import symbol
	  (if (find-symbol (symbol-name symbol) '#:target)
	      '#:host
	      '#:unambiguous-common-lisp)))

(do-symbols (symbol '#:host)
  (export symbol '#:host))

(do-symbols (symbol '#:target)
  (export symbol '#:target))

(do-symbols (symbol '#:unambiguous-common-lisp)
  (export symbol '#:unambiguous-common-lisp))
