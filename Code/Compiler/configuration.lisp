(in-package #:sicl-configuration)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; The size of a machine word.

(defconstant +word-size-in-bytes+ 4)
(defconstant +word-size-in-bits+ (* +word-size-in-bytes+ 8))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Tags
;;;
;;; We define only four main tags.  Two of the tags represent
;;; immediate data (fixnum, immediate) and two represent
;;; heap-allocated data (cons, other).  
;;;
;;; Fixnums are represented with two zero tag bits.  A machine number
;;; is simply shifted two positions to the left to obtain a fixnum.
;;; In other words, a fixnum is just represented as a machine number
;;; with 4 times the magnitude.  Thus, a negative fixnum has the most
;;; significant bit set to `1'.  This representation is well known to
;;; have some good properties, in particular that addition and
;;; subtraction can be performed on fixnums using existing machine
;;; instructions, and multiplcation only reuquires a shift operation
;;; to turn the result into the right representation (unless, of
;;; course, the operations cause overflow and we have to create a
;;; bignum instead.
;;;
;;; Immediate data other than fixnums is typically characters, but
;;; floating-point numbers that require no more than the available
;;; bits can also be represented like this.  We also represent UNBOUND
;;; (i.e. a distinguished object used to fill unbound slots and
;;; variable values) as an immediate quantity.  
;;;
;;; Usually, when we refer to "heap object" or "heap-allocated
;;; object", we exclude CONS cells, even thogh technically, CONS cells
;;; are also heap allocated.  The reson we treat CONS cells
;;; differently from other heap-allocated data is simply to save
;;; space.  Since we require heap-allocated objects with the "other"
;;; tag to have a header word pointing to the class metaobject of the
;;; object, this would add 50% more space to each CONS cell.  Since we
;;; assume that Lisp programs still use a significant number of CONS
;;; cells, this decision seems worth it. 

(defconstant +tag-fixnum+    #b00)
(defconstant +tag-cons+      #b01)
(defconstant +tag-immediate+ #b10)
(defconstant +tag-other+     #b11)

(defconstant +tag-mask+      #b11)

(defconstant +tag-width+ (integer-length +tag-mask+))

(defconstant +immediate-tag-mask+ #b1111)

(defconstant +immediate-tag-width+ (integer-length +immediate-tag-mask+))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Unbound.
;;;
;;; We define the unbound value to be a machine word with immediate
;;; tag, and all the other bits equal to 1. 

(defconstant +unbound+
  (logior (logandc2 (1- (ash 1 +word-size-in-bits+)) +tag-mask+)
	  +tag-immediate+))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Some fixed addresses.
;;;
;;; We use a fixed address in the address space where the entire heap
;;; is mapped.  The first "page" of that space contains items that
;;; need to have a permanent address.  That page is not part of the
;;; heap.  

;;; The place in the address space where the heap is going to be
;;; mapped by mmap.
(defconstant +mmap-base+ (ash 1 30))

;;; This address contains the address of the first free word in the
;;; heap.  The address contained here is a byte address.
(defconstant +free+
  (+ +mmap-base+ (* +word-size-in-bytes+ 0)))

;;; This address contains the global environment as a tagged Lisp
;;; object.
(defconstant +global-environment+
  (+ +mmap-base+ (* +word-size-in-bytes+ 1)))

;;; This address contains the MEMALLOC function as a Lisp object.
(defconstant +function-memalloc+
  (+ +mmap-base+ (* +word-size-in-bytes+ 2)))

;;; This address contains the FIND-PACKAGE function as a Lisp object.
(defconstant +function-find-package+
  (+ +mmap-base+ (* +word-size-in-bytes+ 3)))

;;; This address contains the FIND-SYMBOL function as a Lisp object.
(defconstant +function-find-symbol+
  (+ +mmap-base+ (* +word-size-in-bytes+ 4)))

;;; This address contains the SYMBOL class as a Lisp object.
(defconstant +class-symbol+
  (+ +mmap-base+ (* +word-size-in-bytes+ 5)))

;;; This address contains the PACKAGE class as a Lisp object.
(defconstant +class-package+
  (+ +mmap-base+ (* +word-size-in-bytes+ 6)))

;;; This address contains the SIMPLE-VECTOR class as a Lisp object.
(defconstant +class-simple-vector+
  (+ +mmap-base+ (* +word-size-in-bytes+ 7)))

;;; This address contains the SIMPLE-STRING class as a Lisp object.
(defconstant +class-simple-string+
  (+ +mmap-base+ (* +word-size-in-bytes+ 8)))

;;; This address contains the OCTET-VECTOR class as a Lisp object.
(defconstant +class-octet-vector+
  (+ +mmap-base+ (* +word-size-in-bytes+ 9)))

;;; This address contains the FUNCTION class as a Lisp object.
(defconstant +class-function+
  (+ +mmap-base+ (* +word-size-in-bytes+ 10)))

;;; This address contains the CODE class as a Lisp object.
(defconstant +class-code+
  (+ +mmap-base+ (* +word-size-in-bytes+ 11)))

;;; This address contains the ENVIRONMENT class as a Lisp object.
(defconstant +class-environment+
  (+ +mmap-base+ (* +word-size-in-bytes+ 12)))

;;; This address contains the BUILTIN-CLASS class as a Lisp object.
(defconstant +class-builtin-class+
  (+ +mmap-base+ (* +word-size-in-bytes+ 13)))

;;; This address contains the number of arguments or return values
;;; represented as a raw machine integer.
(defconstant +argument-count+
  (+ +mmap-base+ (* +word-size-in-bytes+ 14)))

;;; This address contains the beginning of 50 consecutive words used
;;; to pass arguments and return values.
(defconstant +arguments+
  (+ +mmap-base+ (* +word-size-in-bytes+ 256)))

;;; This address is the start of the heap
(defconstant +heap-start+
  (+ +mmap-base+ (* +word-size-in-bytes+ 1024)))
    
