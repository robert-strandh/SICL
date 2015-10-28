(cl:in-package #:sicl-low)

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

(defun extract-tag (object)
  (logand object +tag-mask+))

(defun add-tag (untagged-word tag)
  ;; We can't distinguish between an untagged word
  ;; and a fixnum, but this test is better than nothing.
  ;; We certainly don't want to add a tag to something
  ;; that already has one.
  (assert (zerop (extract-tag untagged-word)))
  (logior untagged-word tag))

(defun remove-tag (object)
  (logandc2 object +tag-mask+))

(defun immediatep (object)
  (= (extract-tag object) +tag-immediate+))

(defun otherp (object)
  (= (extract-tag object) +tag-other+))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Get the class of any object.

(defun class-of (object)
  (ecase (extract-tag object)
    (#.+tag-fixnum+      *class-fixnum*)
    (#.+tag-cons+        *class-cons*)
    (#.+tag-immediate+   *class-character*) ; not right
    (#.+tag-other+       (memref (remove-tag object)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; More utilities

(defun final-address (base offset)
  (+ base (* offset +word-size-in-bytes+)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Unbound

(defconstant +unbound+
  (logior (logandc2 (1- (ash 1 +word-size+)) +tag-mask+)
	  +tag-immediate+))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Fixnum.

(defun fixnump (object)
  (declare (type word object))
  (= (logand object +tag-mask+) +tag-fixnum+))

(deftype fixnum ()
  '(satisfies fixnump))

(defun fixnum-from-host-number (host-integer)
  (declare (type integer host-integer))
  ;; Constructing a word will fail if the magnitude is too great.
  (add-tag (word-from-signed-host-number (* host-integer (1+ +tag-mask+)))
	   +tag-fixnum+))

(defun host-number-from-fixnum (lorn)
  (assert (fixnump lorn))
  (/ (signed-host-number-from-word (remove-tag lorn))
     (1+ +tag-mask+)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Characters
;;;
;;; Do this quick and dirty at the moment.

(defun characterp (object)
  (declare (type word object))
  (= (logand object #b111) #b010))

(deftype character ()
  '(satisfies characterp))

(defun char-code (character)
  (declare (type character character))
  (ash (logandc2 character #b111) -1))

(defun code-char (code)
  (declare (type fixnum code))
  (logior (ash code 1) #b010))

;;; We need to test for string equality, so we need a simple 
;;; way of comparing two characters for equality.
(defun simple-char= (char1 char2)
  (declare (type character char1 char2))
  (= (host-number-from-fixnum (char-code char1))
     (host-number-from-fixnum (char-code char2))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; The predicate EQ.
;;; It returns host NIL or T.
(defun eq (object1 object2)
  (declare (type word object1 object2))
  (= object1 object2))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; The target symbol NIL.
;;;
;;; Here, NIL is defined to be a host symbol containing the target
;;; symbol NIL.  We need it early so that we can build lists for
;;; things like symbol plists, and lists of symbols in packages.
;;;
;;; To emphasize that this is not CL:NIL, we use capital letters
;;; whenever this NIL is meant.  Of course, forms such as () or '()
;;; still evaluate to CL:NIL, and 'NIL evaluates to the symbol NIL in
;;; this package.
;;;
;;; It is common to see some special tricky representation of the
;;; symbol NIL, presumably to make it faster to test for it.  The
;;; performance argument used typically involves testing for the end
;;; of a list.  However, more often than not, the test must determine
;;; whether the list is a CONS cell or something else (NIL or some
;;; other atom), so it is faster to test whether it is a CONS cell
;;; first, which is going to be true for in almost every iteration
;;; over some lists; and testing for a CONS cell with our
;;; representation is very fast, since only the tag has to be right.
;;; For that reason, we represent NIL as an ordinary symbol.

(defunbound NIL)

(defun null (object)
  (eq object NIL))

(deftype null ()
  '(satisfies null))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Heap-allocated objects.  We exclue CONS cells when we talk about
;;; heap-allocated objects here, even though technically, CONS cells
;;; are heap allocated.

(defun heap-object-p (object)
  (declare (type word object))
  (= (logand object +tag-mask+) +tag-other+))

(deftype heap-object ()
  '(satisfies heap-object-p))

(defconstant +offset-heap-object-class+                 0)

(defun heap-object-class (object)
  (memref (final-address (remove-tag object) +offset-heap-object-class+)))

(defun (setf heap-object-class) (new-value object)
  (memset (final-address (remove-tag object) +offset-heap-object-class+)
	  new-value))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Cons

(defconstant +offset-cons-car+  0)
(defconstant +offset-cons-cdr+  1)

(defun consp (object)
  (declare (type word object))
  (= (logand object +tag-mask+) +tag-cons+))

(deftype cons ()
  '(satisfies consp))

(defun cons (car cdr)
  (let ((result (the word (malloc (the word #.(* +word-size-in-bytes+ 2))))))
    (memset (wu+
	     result
	     (the word
		  #.(* +word-size-in-bytes+ +offset-cons-car+)))
	    car)
    (memset (wu+
	     result
	     (the word
		  #.(* +word-size-in-bytes+ +offset-cons-cdr+)))
	    cdr)
    (the t (wior
	    result
	    (the word
		 #.+tag-cons+)))))

(defun cons-car (cons)
  (declare (type cons cons))
  (memref (final-address (remove-tag cons) +offset-cons-car+)))

(defun rplaca (cons object)
  (declare (type cons cons))
  (memset (final-address (remove-tag cons) +offset-cons-car+)
	  object)
  cons)

(defun cons-cdr (cons)
  (declare (type cons cons))
  (memref (final-address (remove-tag cons) +offset-cons-cdr+)))

(defun rplacd (cons object)
  (declare (type cons cons))
  (memset (final-address (remove-tag cons) +offset-cons-cdr+)
	  object)
  cons)

(defun car (object)
  (declare (type (or cons null) object))
  (if (null object)
      NIL
      (cons-car object)))

(defun (setf car) (object cons)
  (declare (type cons cons))
  (rplaca cons object)
  object)

(defun cdr (object)
  (declare (type (or cons null) object))
  (if (null object)
      NIL
      (cons-cdr object)))

(defun (setf cdr) (object cons)
  (declare (type cons cons))
  (rplacd cons object)
  object)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Vector.
;;;
;;; A vector is not a standard instance.  It is reprsented like this:
;;;
;;;   * The first two words are as with other heap-allocated
;;;     instances.
;;;
;;;   * The third word contains the number of elements in the
;;;     vector, represented as a fixnum.
;;;
;;;   * The fourth word contains the size of each element in bytes,
;;;     represented as a fixnum.
;;;
;;;   * The firth word contains a true value if the vector is simple.
;;;
;;;   * The sixth word is a pointer to enough consecutive words
;;;     to hold the elements of the vector. 
;;;
;;; A vector is also used to represent a string in which case the
;;; elements are raw characters codes without tags. 
;;;
;;; FIXME: handle distinction between simple and non simple vectors.

(defconstant +offset-vector-class+                 0)
(defconstant +offset-vector-length+                1)
(defconstant +offset-vector-element-type+          2)
(defconstant +offset-vector-simplep+               3)
(defconstant +offset-vector-elements+              4)

(defun allocate-vector (class length)
  (declare (type fixnum length))
  (let* ((length-value (host-number-from-fixnum length))
	 (elements (if (zerop length-value)
		       0
		       (malloc-words length-value)))
	 (vector_ (malloc-words 5)))
    (memset (final-address vector_ +offset-vector-class+)        class)
    (memset (final-address vector_ +offset-vector-length+)       length)
    (memset (final-address vector_ +offset-vector-element-type+) 8)
    (memset (final-address vector_ +offset-vector-simplep+)      0)
    (memset (final-address vector_ +offset-vector-elements+)     elements)
    (add-tag vector_ +tag-other+)))

(defun vector-class (vector)
  (memref (final-address (remove-tag vector) +offset-vector-class+)))

(defun (setf vector-class) (new-value vector)
  (memset (final-address (remove-tag vector) +offset-vector-class+)
	  new-value))

(defun vector-length (vector)
  (memref (final-address (remove-tag vector) +offset-vector-length+)))

(defun (setf vector-length) (new-value vector)
  (memset (final-address (remove-tag vector) +offset-vector-length+)
	  new-value))

(defun vector-element-type (vector)
  (memref (final-address (remove-tag vector) +offset-vector-element-type+)))

(defun (setf vector-element-type) (new-value vector)
  (memset (final-address (remove-tag vector) +offset-vector-element-type+)
	  new-value))

(defun vector-simplep (vector)
  (memref (final-address (remove-tag vector) +offset-vector-simplep+)))

(defun (setf vector-simplep) (new-value vector)
  (memset (final-address (remove-tag vector) +offset-vector-simplep+)
	  new-value))

(defun vector-elements (vector)
  (memref (final-address (remove-tag vector) +offset-vector-elements+)))

(defun (setf vector-elements) (new-value vector)
  (memset (final-address (remove-tag vector) +offset-vector-elements+)
	  new-value))

;;; This function just returns whatever is stored in the vector.  It
;;; does not take into account the fact that some vectors contain
;;; tagged elements and other contain raw machine integers.
(defun vector-element (vector index)
  (let* ((elements (vector-elements vector))
	 (host-index (host-number-from-fixnum index))
	 (element-addr (+ elements (* host-index +word-size-in-bytes+))))
    (memref element-addr)))
  
;;; This function just stores as an element whatever is passed to it
;;; It does not take into account the fact that some vectors contain
;;; tagged elements and other contain raw machine integers.
(defun (setf vector-element) (element vector index)
  (let* ((elements (vector-elements vector))
	 (host-index (host-number-from-fixnum index))
	 (element-addr (+ elements (* host-index +word-size-in-bytes+))))
    (memset element-addr element)))

(defun make-word-vector (length)
  (declare (type fixnum length))
  (allocate-vector *class-vector* length))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; String

(defun stringp (object)
  (declare (type word object))
  (and (heap-object-p object)
       (eql (heap-object-class object) *class-string*)))

(deftype string ()
  '(satisfies stringp))

(defconstant +offset-string-class+           0)
(defconstant +offset-string-length+          1)
(defconstant +offset-string-element-type+    2)
(defconstant +offset-string-simplep+         3)
(defconstant +offset-string-elements+        4)

(defun allocate-string (length)
  (declare (type fixnum length))
  (allocate-vector *class-string* length))

(defun string-class (string)
  (declare (type string string))
  (memref (final-address (remove-tag string) +offset-string-class+)))

(defun (setf string-class) (new-value string)
  (declare (type string string))
  (memset (final-address (remove-tag string) +offset-string-class+)
	  new-value))

(defun string-length (string)
  (declare (type string string))
  (memref (final-address (remove-tag string) +offset-string-length+)))

(defun (setf string-length) (new-value string)
  (declare (type string string))
  (memset (final-address (remove-tag string) +offset-string-length+)
	  new-value))

(defun string-element-type (string)
  (declare (type string string))
  (memref (final-address (remove-tag string) +offset-string-element-type+)))

(defun (setf string-element-type) (new-value string)
  (declare (type string string))
  (memset (final-address (remove-tag string) +offset-string-element-type+)
	  new-value))

(defun string-simplep (string)
  (declare (type string string))
  (memref (final-address (remove-tag string) +offset-string-simplep+)))

(defun (setf string-simplep) (new-value string)
  (declare (type string string))
  (memset (final-address (remove-tag string) +offset-string-simplep+)
	  new-value))

(defun string-elements (string)
  (declare (type string string))
  (memref (final-address (remove-tag string) +offset-string-elements+)))

(defun (setf string-elements) (new-value string)
  (declare (type string string))
  (memset (final-address (remove-tag string) +offset-string-elements+)
	  new-value))

(defun char (string index)
  (declare (type string string)
	   (type fixnum index))
  (let* ((elements (string-elements string))
	 (host-index (host-number-from-fixnum index))
	 (element-addr (+ elements (* host-index +word-size-in-bytes+))))
    (memref element-addr)))
  
(defun (setf char) (character string index)
  (declare (type character character)
	   (type string string)
	   (type fixnum index))
  (let* ((elements (string-elements string))
	 (host-index (host-number-from-fixnum index))
	 (element-addr (+ elements (* host-index +word-size-in-bytes+))))
    (memset element-addr character)))
  
(defun schar (string index)
  (declare (type string string)
	   (type fixnum index))
  (let* ((elements (string-elements string))
	 (host-index (host-number-from-fixnum index))
	 (element-addr (+ elements (* host-index +word-size-in-bytes+))))
    (memref element-addr)))
  
(defun (setf schar) (character string index)
  (declare (type character character)
	   (type string string)
	   (type fixnum index))
  (let* ((elements (string-elements string))
	 (host-index (host-number-from-fixnum index))
	 (element-addr (+ elements (* host-index +word-size-in-bytes+))))
    (memset element-addr character)))
  
;;; We need a way to compare symbol names, so we define a 
;;; simplified version of string= here.  It returns the host
;;; value T or the host value NIL. 
(defun simple-string= (string1 string2)
  (declare (type string string1 string2))
  (let* ((target-length1 (string-length string1))
	 (host-length1 (host-number-from-fixnum target-length1))
	 (target-length2 (string-length string2))
	 (host-length2 (host-number-from-fixnum target-length2)))
    (and (= host-length1 host-length2)
	 (loop for i from 0 below host-length1
	       always
	       (let ((target-index (fixnum-from-host-number i)))
		 (simple-char= (char string1 target-index)
			       (char string2 target-index)))))))

;;; A convenience function
;;; FIXME: this function uses the host's character encoding which
;;; is wrong. 
(defun string-from-host-string (host-string)
  (let* ((host-length (length host-string))
	 (target-length (fixnum-from-host-number host-length))
	 (target-string (allocate-string target-length)))
    (loop for char across host-string
	  for host-index from 0
	  do (let* ((target-index (fixnum-from-host-number host-index))
		    (host-code (cl:char-code char))
		    (target-code (fixnum-from-host-number host-code))
		    (target-char (code-char target-code)))
	       (setf (char target-string target-index) target-char)))
    target-string))

;;; A convenience function
;;; FIXME: this function uses the host's character encoding which
;;; is wrong. 
(defun host-string-from-string (target-string)
  (declare (type string target-string))
  (let* ((target-length (string-length target-string))
	 (host-length (host-number-from-fixnum target-length))
	 (host-string (cl:make-string host-length)))
    (loop for host-index from 0 below host-length
	  do (let* ((target-index (fixnum-from-host-number host-index))
		    (target-char (char target-string target-index))
		    (target-code (char-code target-char))
		    (host-code (host-number-from-fixnum target-code))
		    (host-char (cl:code-char host-code)))
	       (setf (cl:char host-string host-index) host-char)))
    host-string))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Standard instance and standard funcallable instance.
;;;
;;; A standard instance and a standard funcallable instance is
;;; represented by two consecutive words.  
;;;
;;;   * The first word contains the class of the instance.
;;;
;;;   * The second word is a pointer to a slot vector.  The number of
;;;     slots in the slot vector is determined by the class of the
;;;     instance.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Symbol

(defun symbolp (object)
  (declare (type word object))
  (and (heap-object-p object)
       (eql (heap-object-class object) *class-symbol*)))

(deftype symbol ()
  '(satisfies symbolp))

(defconstant +offset-symbol-history+             0)
(defconstant +offset-symbol-name+                1)
(defconstant +offset-symbol-package+             2)
(defconstant +offset-symbol-value+               3)
(defconstant +offset-symbol-function+            4)
(defconstant +offset-symbol-setf-function+       5)
(defconstant +offset-symbol-plist+               6)

(defun make-symbol (name)
  (let ((slots (malloc-words 7)))
    (memset (final-address slots +offset-symbol-history+)        +unbound+)
    (memset (final-address slots +offset-symbol-name+)           name)
    (memset (final-address slots +offset-symbol-package+)        NIL)
    (memset (final-address slots +offset-symbol-value+)          +unbound+)
    (memset (final-address slots +offset-symbol-function+)       +unbound+)
    (memset (final-address slots +offset-symbol-setf-function+)  +unbound+)
    (memset (final-address slots +offset-symbol-plist+)          NIL)
    (let ((symbol  (malloc-words 2)))
      (memset (final-address symbol  0) *class-symbol*)
      (memset (final-address symbol  1) slots)
      (add-tag symbol  +tag-other+))))

(defun symbol-history (symbol)
  (let ((slot-vector (memref (final-address (remove-tag symbol) 1))))
    (memref (+ slot-vector
	       (* +offset-symbol-history+ +word-size-in-bytes+)))))

(defun (setf symbol-history) (new-value symbol)
  (let ((slot-vector (memref (final-address (remove-tag symbol) 1))))
    (memset (+ slot-vector
	       (* +offset-symbol-history+ +word-size-in-bytes+))
	    new-value)))

(defun symbol-name (symbol)
  (let ((slot-vector (memref (final-address (remove-tag symbol) 1))))
    (memref (+ slot-vector
	       (* +offset-symbol-name+ +word-size-in-bytes+)))))
  
(defun (setf symbol-name) (new-value symbol)
  (let ((slot-vector (memref (final-address (remove-tag symbol) 1))))
    (memset (+ slot-vector
	       (* +offset-symbol-name+ +word-size-in-bytes+))
	    new-value)))

(defun symbol-package (symbol)
  (let ((slot-vector (memref (final-address (remove-tag symbol) 1))))
    (memref (+ slot-vector
	       (* +offset-symbol-package+ +word-size-in-bytes+)))))

(defun (setf symbol-package) (new-value symbol)
  (let ((slot-vector (memref (final-address (remove-tag symbol) 1))))
    (memset (+ slot-vector
	       (* +offset-symbol-package+ +word-size-in-bytes+))
	    new-value)))

(defun symbol-value (symbol)
  (let ((slot-vector (memref (final-address (remove-tag symbol) 1))))
    (memref (+ slot-vector
	       (* +offset-symbol-value+ +word-size-in-bytes+)))))

(defun (setf symbol-value) (new-value symbol)
  (let ((slot-vector (memref (final-address (remove-tag symbol) 1))))
    (memset (+ slot-vector
	       (* +offset-symbol-value+ +word-size-in-bytes+))
	    new-value)))

(defun symbol-function (symbol)
  (let ((slot-vector (memref (final-address (remove-tag symbol) 1))))
    (memref (+ slot-vector
	       (* +offset-symbol-function+ +word-size-in-bytes+)))))

(defun (setf symbol-function) (new-value symbol)
  (let ((slot-vector (memref (final-address (remove-tag symbol) 1))))
    (memset (+ slot-vector
	       (* +offset-symbol-function+ +word-size-in-bytes+))
	    new-value)))
	     
(defun symbol-setf-function (symbol)
  (let ((slot-vector (memref (final-address (remove-tag symbol) 1))))
    (memref (+ slot-vector
	       (* +offset-symbol-setf-function+ +word-size-in-bytes+)))))

(defun (setf symbol-setf-function) (new-value symbol)
  (let ((slot-vector (memref (final-address (remove-tag symbol) 1))))
    (memset (+ slot-vector
	       (* +offset-symbol-setf-function+ +word-size-in-bytes+))
	    new-value)))

(defun symbol-plist (symbol)
  (let ((slot-vector (memref (final-address (remove-tag symbol) 1))))
    (memref (+ slot-vector
	       (* +offset-symbol-plist+ +word-size-in-bytes+)))))

(defun (setf symbol-plist) (new-value symbol)
  (let ((slot-vector (memref (final-address (remove-tag symbol) 1))))
    (memset (+ slot-vector
	       (* +offset-symbol-plist+ +word-size-in-bytes+))
	    new-value)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Package
;;;
;;; A package is a standard instance.  As such, it has a header of
;;; three words as described above.  The last such word points to
;;; the slot storage, which is a vector as described below. 

(defun packagep (object)
  (declare (type word object))
  (and (heap-object-p object)
       (eql (heap-object-class object) *class-package*)))

(deftype package ()
  '(satisfies packagep))

(defconstant +offset-package-history+             0)
(defconstant +offset-package-name+                1)
(defconstant +offset-package-nicknames+           2)
(defconstant +offset-package-use-list+            3)
(defconstant +offset-package-used-by-list+        4)
(defconstant +offset-package-external-symbols+    5)
(defconstant +offset-package-internal-symbols+    6)
(defconstant +offset-package-shadowing-symbols+   7)

(defun allocate-package (name nicknames)
  (let ((slots (malloc-words 8)))
    (memset (final-address slots +offset-package-history+)            NIL)
    (memset (final-address slots +offset-package-name+)               name)
    (memset (final-address slots +offset-package-nicknames+)          nicknames)
    (memset (final-address slots +offset-package-use-list+)           NIL)
    (memset (final-address slots +offset-package-used-by-list+)       NIL)
    (memset (final-address slots +offset-package-external-symbols+)   NIL)
    (memset (final-address slots +offset-package-internal-symbols+)   NIL)
    (memset (final-address slots +offset-package-shadowing-symbols+)  NIL)
    (let ((package (malloc-words 2)))
      (memset (final-address package  0) *class-package*)
      (memset (final-address package  1) slots)
      (add-tag package  +tag-other+))))

(defun package-history (package)
  (let ((slot-vector (memref (final-address (remove-tag package) 1))))
    (memref (+ slot-vector
	       (* +offset-package-history+ +word-size-in-bytes+)))))

(defun (setf package-history) (new-value package)
  (let ((slot-vector (memref (final-address (remove-tag package) 1))))
    (memset (+ slot-vector
	       (* +offset-package-history+ +word-size-in-bytes+))
	    new-value)))

(defun package-name (package)
  (let ((slot-vector (memref (final-address (remove-tag package) 1))))
    (memref (+ slot-vector
	       (* +offset-package-name+ +word-size-in-bytes+)))))

(defun (setf package-name) (new-value package)
  (let ((slot-vector (memref (final-address (remove-tag package) 1))))
    (memset (+ slot-vector
	       (* +offset-package-name+ +word-size-in-bytes+))
	    new-value)))

(defun package-nicknames (package)
  (let ((slot-vector (memref (final-address (remove-tag package) 1))))
    (memref (+ slot-vector
	       (* +offset-package-nicknames+ +word-size-in-bytes+)))))

(defun (setf package-nicknames) (new-value package)
  (let ((slot-vector (memref (final-address (remove-tag package) 1))))
    (memset (+ slot-vector
	       (* +offset-package-nicknames+ +word-size-in-bytes+))
	    new-value)))

(defun package-use-list (package)
  (let ((slot-vector (memref (final-address (remove-tag package) 1))))
    (memref (+ slot-vector
	       (* +offset-package-use-list+ +word-size-in-bytes+)))))

(defun (setf package-use-list) (new-value package)
  (let ((slot-vector (memref (final-address (remove-tag package) 1))))
    (memset (+ slot-vector
	       (* +offset-package-use-list+ +word-size-in-bytes+))
	    new-value)))

(defun package-used-by-list (package)
  (let ((slot-vector (memref (final-address (remove-tag package) 1))))
    (memref (+ slot-vector
	       (* +offset-package-used-by-list+ +word-size-in-bytes+)))))

(defun (setf package-used-by-list) (new-value package)
  (let ((slot-vector (memref (final-address (remove-tag package) 1))))
    (memset (+ slot-vector
	       (* +offset-package-used-by-list+ +word-size-in-bytes+))
	    new-value)))

(defun package-external-symbols (package)
  (let ((slot-vector (memref (final-address (remove-tag package) 1))))
    (memref (+ slot-vector
	       (* +offset-package-external-symbols+ +word-size-in-bytes+)))))

(defun (setf package-external-symbols) (new-value package)
  (let ((slot-vector (memref (final-address (remove-tag package) 1))))
    (memset (+ slot-vector
	       (* +offset-package-external-symbols+ +word-size-in-bytes+))
	    new-value)))

(defun package-internal-symbols (package)
  (let ((slot-vector (memref (final-address (remove-tag package) 1))))
    (memref (+ slot-vector
	       (* +offset-package-internal-symbols+ +word-size-in-bytes+)))))

(defun (setf package-internal-symbols) (new-value package)
  (let ((slot-vector (memref (final-address (remove-tag package) 1))))
    (memset (+ slot-vector
	       (* +offset-package-internal-symbols+ +word-size-in-bytes+))
	    new-value)))

(defun package-shadowing-symbols (package)
  (let ((slot-vector (memref (final-address (remove-tag package) 1))))
    (memref (+ slot-vector
	       (* +offset-package-shadowing-symbols+ +word-size-in-bytes+)))))

(defun (setf package-shadowing-symbols) (new-value package)
  (let ((slot-vector (memref (final-address (remove-tag package) 1))))
    (memset (+ slot-vector
	       (* +offset-package-shadowing-symbols+ +word-size-in-bytes+))
	    new-value)))

;;; The value of this variable is used as a default package in many
;;; operations.
(defunbound *package*)

;;; We don't have the mapping functions yet at this point. 
(defun mapc-temp (fun list)
  (loop for rest = list then (cdr rest)
	while (consp rest)
	do (funcall fun (car rest))))

;;; The host NIL is returned if the symbol is not in the list.
(defun symbol-member (symbol list)
  (loop for rest = list then (cdr rest)
	while (consp rest)
	when (eq symbol (car rest))
	  return (car rest)))

;;; FIXME: check syntax better
;;; FIXME: TAGBODY can't handle declarations, so they
;;;        must first be separated out from the body. 
(defmacro do-external-symbols ((symbol-variable
				&optional
				  (package-form '*package*)
				  (result-form 'NIL))
			       &body body)
  (let ((function-name (gensym)))
    `(block nil
       (let ((,function-name (lambda (,symbol-variable)
			       (tagbody
				  ,@body))))
	 (mapc-temp ,function-name
		    (package-external-symbols ,package-form)))
       ,result-form)))

;;; FIXME: check syntax better
;;; FIXME: TAGBODY can't handle declarations, so they
;;;        must first be separated out from the body. 
(defmacro do-symbols ((symbol-variable
		       &optional
			 (package-form '*package*)
			 (result-form 'NIL))
		      &body body)
  (let ((function-name (gensym))
	(package-var (gensym)))
    ;; This can't possibly work. what NIL is this?
    ;; Where is the symbol BLOCK taken from, etc. 
    `(block nil
       (let ((,function-name (lambda (,symbol-variable)
			       (tagbody
				  ,@body)))
	     (,package-var ,package-form))
	 (mapc-temp ,function-name
		    (package-external-symbols ,package-var))
	 (mapc-temp ,function-name
		    (package-internal-symbols ,package-var))
	 (mapc-temp (lambda (used-package)
		      (mapc-temp
		       (lambda (symbol)
			 (unless (symbol-member
				  symbol
				  (package-shadowing-symbols ,package-var))
			   (mapc-temp ,function-name symbol)))
		       (package-external-symbols used-package)))
		    (package-use-list ,package-var)))
       ,result-form)))

;;; FIXME: ugly!!!  We return host symbols in the keyword package
;;;        as the second argument
;;; When no symbol is found, return the target NIL as both values.
(defun find-symbol (name &optional (package *package*))
  (unless (packagep package)
    (setf package (find-package (string package))))
  (loop for symbols = (package-external-symbols package)
	  then (cdr symbols)
	while (consp symbols)
	when (simple-string= (symbol-name (car symbols)) name)
	  do (return-from find-symbol
	       (values (car symbols) :external)))
  (loop for symbols = (package-internal-symbols package)
	  then (cdr symbols)
	while (consp symbols)
	when (simple-string= (symbol-name (car symbols)) name)
	  do (return-from find-symbol
	       (values (car symbols) :internal)))
  (loop for packages = (package-use-list package)
	  then (cdr packages)
	while (consp packages)
	do (loop for symbols = (package-external-symbols (car packages))
		   then (cdr symbols)
		 while (consp symbols)
		 when (simple-string= (symbol-name (car symbols)) name)
		      do (return-from find-symbol
			   (values (car symbols) :inherited))))
  (values NIL NIL))

(defun intern (name &optional (package *package*))
  (multiple-value-bind (symbol-or-nil status)
      (find-symbol name package)
    (if (null symbol-or-nil)
	(let ((new-symbol (make-symbol name)))
	  (unless (packagep package)
	    (setf package (find-package (string package))))
	  (setf (symbol-package new-symbol) package)
	  (setf (package-internal-symbols package)
		(cons new-symbol
		      (package-internal-symbols package)))
	  (values new-symbol NIL))
	(values symbol-or-nil status))))

;;; *Sigh*.  Why do I have to write these functions?
(defun symbol-in-list-p (sym list)
  (if (consp list)
      (if (eq sym (car list))
	  cl:t
	  (symbol-in-list-p sym (cdr list)))
      cl:nil))

(defun remove-symbol-from-list (sym list)
  (if (consp list)
      (if (eq sym (car list))
	  (cdr list)
	  (cons (car list) (remove-symbol-from-list sym (cdr list))))
      list))

;;; FIXME: check for conflicts
;;; FIXME: return T
;;; FIXME: check definition of a designator for a list of symbols
;;;        with respect to NIL.
(defun export (symbols &optional package *package*)
  (unless (packagep package)
    (setf package (find-package (string package))))
  (flet ((make-external (sym)
	   (setf (package-external-symbols package)
		 (cons sym (package-external-symbols package)))))
    (flet ((aux (symbol)
	     (cond ((symbol-in-list-p symbol (package-external-symbols package))
		    ;; do nothing
		    (return-from export cl:t))
		   ((symbol-in-list-p symbol (package-internal-symbols package))
		    ;; change it to be external
		    (setf (package-internal-symbols package)
			  (remove-symbol-from-list
			   symbol (package-internal-symbols package)))
		    (make-external symbol))
		   (t
		    (loop for used = (package-use-list package)
			    then (cdr used)
			  while (consp used)
			  do (loop for syms = (package-use-list (car used))
				     then (cdr syms)
				   do (when (eq (car syms) symbol)
					(make-external symbol)
					(return-from export t))))
		    ;; come here if the symbol is not accessible
		    (error "symbol ~s not accessible in package ~s"
			   (symbol-name symbol)
			   ;; FIXME: This won't work for symbols
			   ;; without a home package.
			   (package-name (symbol-package symbol)))))))
      (cond ((symbolp symbols)
	     (aux symbols))
	    ((or (stringp symbols) (characterp symbols))
	     (aux (find-symbol (string symbols))))
	    ((consp symbols)
	     (loop for rest = symbols
		     then (cdr rest)
		   while (consp rest)
		   do (let ((sym (car rest)))
			(cond ((symbolp sym)
			       (aux sym))
			      ((or (stringp sym) (characterp sym))
			       (aux (find-symbol (string sym))))
			      (t
			       (error "~s is not a symbol designator"
				      (host-object-from-object sym)))))))
	    (t 
	     (error "~s is not a designator for a list of symbols"
		    (host-object-from-object symbols)))))))

;;; The value of this variable is a CONS cell that contains a list 
;;; of all packages in its CAR slot.  This way, we get an indirection
;;; so that we can modify the list of all packages by just keeping a
;;; pointer to the CONS cell.
(defunbound *all-packages*)

(defun string (object)
  (declare (type (or character string symbol) object))
  (cond ((stringp object) object)
	((symbolp object) (symbol-name object))
	(t (let* ((target-zero (fixnum-from-host-number 0))
		  (result (allocate-string target-zero)))
	     (setf (char result target-zero) object)
	     result))))
	 
(defun find-package (name-or-package)
  (declare (type (or character string symbol package) name-or-package))
  (if (packagep name-or-package)
      name-or-package
      (let ((veritable-name (string name-or-package)))
	(loop for packages = (car *all-packages*)
		then (cdr packages)
	      while (consp packages)
	      when (simple-string= (package-name (car packages))
				   veritable-name)
		return (car packages)
	      do (loop for nicknames = (package-nicknames (car packages))
			 then (cdr nicknames)
		       while (consp nicknames)
		       when (simple-string= (car nicknames) veritable-name)
			 do (return-from find-package (car packages)))
	      finally (return NIL)))))

;;; FIXME: do more checks
;;; FIXME: signal a correctable error.
(defun make-package (name &key (nicknames NIL) (use NIL))
  (declare (type (or character string symbol) name))
  (assert (null (find-package name)))
  (loop for rest = nicknames then (cdr rest)
	while (consp rest)
	do (assert (null (find-package (car rest)))))
  (let ((package (allocate-package (string name) nicknames)))
    (setf (package-use-list package) use)
    (setf (car *all-packages*) (cons package (car *all-packages*)))
    package))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Code.
;;;
;;; A code object contains a vector of instructions and a
;;; vector of constants.  Code objects are created by the 
;;; compiler (or rather, by the assembler). 

(defun codep (object)
  (declare (type word object))
  (and (heap-object-p object)
       (eql (heap-object-class object) *class-code*)))

(deftype code ()
  '(satisfies codep))

(defconstant +offset-code-class+          0)
(defconstant +offset-code-instructions+   1)
(defconstant +offset-code-constants+      2)

(defun make-code (instructions constants)
  (let ((code_ (malloc-words 3)))
    (memset (final-address code_ +offset-code-class+)
	    *class-code*)
    (memset (final-address code_ +offset-code-instructions+)
	    instructions)
    (memset (final-address code_ +offset-code-constants+)
	    constants)
    (add-tag code_ +tag-other+)))

(defun code-class (code)
  (declare (type code code))
  (memref (final-address (remove-tag code) +offset-code-class+)))

(defun (setf code-class) (new-value code)
  (declare (type code code))
  (memset (final-address (remove-tag code) +offset-code-class+)
	  new-value))

(defun code-instructions (code)
  (declare (type code code))
  (memref (final-address (remove-tag code) +offset-code-instructions+)))

(defun (setf code-instructions) (new-value code)
  (declare (type code code))
  (memset (final-address (remove-tag code) +offset-code-instructions+)
	  new-value))

(defun code-constants (code)
  (declare (type code code))
  (memref (final-address (remove-tag code) +offset-code-constants+)))

(defun (setf code-constants) (new-value code)
  (declare (type code code))
  (memset (final-address (remove-tag code) +offset-code-constants+)
	  new-value))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function.
;;;
;;; Functions are allocated at runtime by the FUNCTION special
;;; operator. 

(defun functionp (object)
  (declare (type word object))
  (and (heap-object-p object)
       (eql (heap-object-class object) *class-function*)))

(deftype function ()
  '(satisfies functionp))

(defconstant +offset-function-history+             0)
(defconstant +offset-function-code+                1)
(defconstant +offset-function-environment+         2)
(defconstant +offset-function-program-counter+     3)

(defun allocate-function (code environment program-counter)
  (let ((slots (malloc-words 8)))
    (memset (final-address slots +offset-function-history+)
	    NIL)
    (memset (final-address slots +offset-function-code+)
	    code)
    (memset (final-address slots +offset-function-environment+)
	    environment)
    (memset (final-address slots +offset-function-program-counter+)
	    program-counter)
    (let ((function  (malloc-words 2)))
      (memset (final-address function  0) *class-function*)
      (memset (final-address function  1) slots)
      (add-tag function  +tag-other+))))

;;; Provide this entrypoint so that we can stick some function
;;; definitions in there before the machine is up and running.  It
;;; defines a function from a code object by setting the environment
;;; to NIL and the program counter to the first address of the
;;; instructions of the code object. 
(defun make-function (code)
  (let* ((instructions (code-instructions code))
	 (address (vector-elements instructions)))
  (allocate-function code NIL address)))

(defun function-history (function)
  (let ((slot-vector (memref (final-address (remove-tag function) 1))))
    (memref (+ slot-vector
	       (* +offset-function-history+ +word-size-in-bytes+)))))

(defun (setf function-history) (new-value function)
  (let ((slot-vector (memref (final-address (remove-tag function) 1))))
    (memset (+ slot-vector
	       (* +offset-function-history+ +word-size-in-bytes+))
	    new-value)))

(defun function-code (function)
  (let ((slot-vector (memref (final-address (remove-tag function) 1))))
    (memref (+ slot-vector
	       (* +offset-function-code+ +word-size-in-bytes+)))))

(defun (setf function-code) (new-value function)
  (let ((slot-vector (memref (final-address (remove-tag function) 1))))
    (memset (+ slot-vector
	       (* +offset-function-code+ +word-size-in-bytes+))
	    new-value)))

(defun function-environment (function)
  (let ((slot-vector (memref (final-address (remove-tag function) 1))))
    (memref (+ slot-vector
	       (* +offset-function-environment+ +word-size-in-bytes+)))))

(defun (setf function-environment) (new-value function)
  (let ((slot-vector (memref (final-address (remove-tag function) 1))))
    (memset (+ slot-vector
	       (* +offset-function-environment+ +word-size-in-bytes+))
	    new-value)))

(defun function-program-counter (function)
  (let ((slot-vector (memref (final-address (remove-tag function) 1))))
    (memref (+ slot-vector
	       (* +offset-function-program-counter+ +word-size-in-bytes+)))))

(defun (setf function-program-counter) (new-value function)
  (let ((slot-vector (memref (final-address (remove-tag function) 1))))
    (memset (+ slot-vector
	       (* +offset-function-program-counter+ +word-size-in-bytes+))
	    new-value)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Dynamic frame
;;;
;;; Dynamic frames are created at runtime by the linkage code.

(defun dynamic-framep (object)
  (declare (type word object))
  (and (heap-object-p object)
       (eql (heap-object-class object) *class-dynamic-frame*)))

(deftype dynamic-frame ()
  '(satisfies dynamic-framep))

(defconstant +offset-dynamic-frame-class+               0)
(defconstant +offset-dynamic-frame-code+                1)
(defconstant +offset-dynamic-frame-static-environment+  2)
(defconstant +offset-dynamic-frame-program-counter+     3)
(defconstant +offset-dynamic-frame-operand-stack+       4)
(defconstant +offset-dynamic-frame-special-bindings+    5)
(defconstant +offset-dynamic-frame-catch-tags+          6)
(defconstant +offset-dynamic-frame-unwind-protects+     7)

(defun allocate-dynamic-frame (code
			       static-environment
			       program-counter
			       operand-stack
			       special-bindings
			       catch-tags
			       unwind-protects)
  (let ((frame_ (malloc-words 8)))
    (memset (final-address frame_ +offset-dynamic-frame-class+)
	    *class-dynamic-frame*)
    (memset (final-address frame_ +offset-dynamic-frame-code+)
	    code)
    (memset (final-address frame_ +offset-dynamic-frame-static-environment+)
	    static-environment)
    (memset (final-address frame_ +offset-dynamic-frame-program-counter+)
	    program-counter)
    (memset (final-address frame_ +offset-dynamic-frame-operand-stack+)
	    operand-stack)
    (memset (final-address frame_ +offset-dynamic-frame-special-bindings+)
	    special-bindings)
    (memset (final-address frame_ +offset-dynamic-frame-catch-tags+)
	    catch-tags)
    (memset (final-address frame_ +offset-dynamic-frame-unwind-protects+)
	    unwind-protects)
    (add-tag frame_ +tag-other+)))

(defun make-dynamic-frame (code program-counter)
  (allocate-dynamic-frame code
			  NIL
			  program-counter
			  NIL
			  NIL
			  NIL
			  NIL))

(defun dynamic-frame-class (dynamic-frame)
  (declare (type dynamic-frame dynamic-frame))
  (memref (final-address (remove-tag dynamic-frame)
			 +offset-dynamic-frame-class+)))

(defun (setf dynamic-frame-class) (new-value dynamic-frame)
  (declare (type dynamic-frame dynamic-frame))
  (memset (final-address (remove-tag dynamic-frame)
			 +offset-dynamic-frame-class+)
	  new-value))

(defun dynamic-frame-code (dynamic-frame)
  (declare (type dynamic-frame dynamic-frame))
  (memref (final-address (remove-tag dynamic-frame)
			 +offset-dynamic-frame-code+)))

(defun (setf dynamic-frame-code) (new-value dynamic-frame)
  (declare (type dynamic-frame dynamic-frame))
  (memset (final-address (remove-tag dynamic-frame)
			 +offset-dynamic-frame-code+)
	  new-value))

(defun dynamic-frame-static-environment (dynamic-frame)
  (declare (type dynamic-frame dynamic-frame))
  (memref (final-address (remove-tag dynamic-frame)
			 +offset-dynamic-frame-static-environment+)))

(defun (setf dynamic-frame-static-environment) (new-value dynamic-frame)
  (declare (type dynamic-frame dynamic-frame))
  (memset (final-address (remove-tag dynamic-frame)
			 +offset-dynamic-frame-static-environment+)
	  new-value))

(defun dynamic-frame-program-counter (dynamic-frame)
  (declare (type dynamic-frame dynamic-frame))
  (memref (final-address (remove-tag dynamic-frame)
			 +offset-dynamic-frame-program-counter+)))

(defun (setf dynamic-frame-program-counter) (new-value dynamic-frame)
  (declare (type dynamic-frame dynamic-frame))
  (memset (final-address (remove-tag dynamic-frame)
			 +offset-dynamic-frame-program-counter+)
	  new-value))

(defun dynamic-frame-operand-stack (dynamic-frame)
  (declare (type dynamic-frame dynamic-frame))
  (memref (final-address (remove-tag dynamic-frame)
			 +offset-dynamic-frame-operand-stack+)))

(defun (setf dynamic-frame-operand-stack) (new-value dynamic-frame)
  (declare (type dynamic-frame dynamic-frame))
  (memset (final-address (remove-tag dynamic-frame)
			 +offset-dynamic-frame-operand-stack+)
	  new-value))

(defun dynamic-frame-special-bindings (dynamic-frame)
  (declare (type dynamic-frame dynamic-frame))
  (memref (final-address (remove-tag dynamic-frame)
			 +offset-dynamic-frame-special-bindings+)))

(defun (setf dynamic-frame-special-bindings) (new-value dynamic-frame)
  (declare (type dynamic-frame dynamic-frame))
  (memset (final-address (remove-tag dynamic-frame)
			 +offset-dynamic-frame-special-bindings+)
	  new-value))

(defun dynamic-frame-catch-tags (dynamic-frame)
  (declare (type dynamic-frame dynamic-frame))
  (memref (final-address (remove-tag dynamic-frame)
			 +offset-dynamic-frame-catch-tags+)))

(defun (setf dynamic-frame-catch-tags) (new-value dynamic-frame)
  (declare (type dynamic-frame dynamic-frame))
  (memset (final-address (remove-tag dynamic-frame)
			 +offset-dynamic-frame-catch-tags+)
	  new-value))

(defun dynamic-frameunwind-protects (dynamic-frame)
  (declare (type dynamic-frame dynamic-frame))
  (memref (final-address (remove-tag dynamic-frame)
			 +offset-dynamic-frame-unwind-protects+)))

(defun (setf dynamic-frameunwind-protects) (new-value dynamic-frame)
  (declare (type dynamic-frame dynamic-frame))
  (memset (final-address (remove-tag dynamic-frame)
			 +offset-dynamic-frame-unwind-protects+)
	  new-value))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Unwind-protect.
;;;
;;; Unwind-protects are created at runtime by the unwind-protect
;;; special operator.

(defun unwind-protectp (object)
  (declare (type word object))
  (and (heap-object-p object)
       (eql (heap-object-class object) *class-unwind-protect*)))

(deftype unwind-protect ()
  '(satisfies unwind-protectp))

(defconstant +offset-unwind-protect-class+               0)
(defconstant +offset-unwind-protect-thunk+               1)
(defconstant +offset-unwind-protect-special-bindings+    2)
(defconstant +offset-unwind-protect-catch-tags+          3)

(defun make-unwind-protect (thunk special-bindings catch-tags)
  (let ((result_ (malloc-words 4)))
    (memset (final-address result_ +offset-unwind-protect-class+)
	    *class-unwind-protect*)
    (memset (final-address result_ +offset-unwind-protect-thunk+)
	    thunk)
    (memset (final-address result_ +offset-unwind-protect-special-bindings+)
	    special-bindings)
    (memset (final-address result_ +offset-unwind-protect-catch-tags+)
	    catch-tags)
    (add-tag result_ +tag-other+)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Debugging tools.

(defun object-from-host-object (host-object)
  (cond ((cl:consp host-object)
	 (cons (object-from-host-object (cl:car host-object))
	       (object-from-host-object (cl:cdr host-object))))
	((cl:stringp host-object)
	 (string-from-host-string host-object))
	((cl:numberp host-object)
	 (fixnum-from-host-number host-object))
	((cl:characterp host-object)
	 (let* ((host-code (cl:char-code host-object))
		(target-code (object-from-host-object host-code))
		(target-char (code-char target-code)))
	   target-char))
	((cl:symbolp host-object)
	 (multiple-value-bind (symbol status)
	     (find-symbol (object-from-host-object (cl:symbol-name host-object))
			  (object-from-host-object (cl:symbol-package host-object)))
	   (unless (cl:symbolp status)
	     (cl:error "can't find symbol named ~s in package named ~s"
		       (cl:symbol-name host-object)
		       (cl:package-name (cl:symbol-package host-object))))
	   symbol))
	((cl:packagep host-object)
	 (find-package
	  (object-from-host-object (cl:package-name host-object))))
	(t
	 (cl:error "can't translate host object ~s to a target object"
		   host-object))))

(defun host-object-from-object (object)
  (cond ((consp object)
	 (cl:cons (host-object-from-object (car object))
		  (host-object-from-object (cdr object))))
	((stringp object)
	 (host-string-from-string object))
	((fixnump object)
	 (host-number-from-fixnum object))
	((characterp object)
	 (let* ((target-code (char-code object))
		(host-code (host-object-from-object target-code))
		(host-char (cl:code-char host-code)))
	   host-char))
	((symbolp object)
	 (cl:intern (host-object-from-object (symbol-name object))
		    (host-object-from-object (symbol-package object))))
	((packagep object)
	 (cl:find-package
	  (host-object-from-object (package-name object))))
	(t
	 (cl:error "can't translate target object ~s to a host object"
		   object))))
	
(defun interpret (object)
  (cond ((eql object +unbound+)
	 "unbound")
	((eql object *class-fixnum*)
	 (format cl:nil "-> the fixnum class metaobject"))
	((eql object *class-cons*)
	 (format cl:nil "-> the cons class metaobject"))
	((eql object *class-character*)
	 (format cl:nil "-> the character class metaobject"))
	((eql object *class-function*)
	 (format cl:nil "-> the function class metaobject"))
	((eql object *class-vector*)
	 (format cl:nil "-> the vector class metaobject"))
	((eql object *class-string*)
	 (format cl:nil "-> the string class metaobject"))
	((eql object *class-symbol*)
	 (format cl:nil "-> the symbol class metaobject"))
	((eql object *class-package*)
	 (format cl:nil "-> the package class metaobject"))
	((eql object *class-code*)
	 (format cl:nil "-> the code class metaobject"))
	((eql object *class-dynamic-frame*)
	 (format cl:nil "-> the dynamic-frame class metaobject"))
	((eql object *class-unwind-protect*)
	 (format cl:nil "-> the unwind-protect class metaobject"))
	((null object)
	 (format cl:nil "-> the symbol CL:NIL"))
	((fixnump object)
	 (format cl:nil "The fixnum ~d" (host-number-from-fixnum object)))
	((consp object)
	 (format cl:nil "-> a CONS cell in ~8,'0x"
		 (remove-tag object)))
	((characterp object)
	 (format cl:nil "The character ~s"
		 (let* ((target-code (char-code object))
			(host-code (host-number-from-fixnum target-code))
			(host-char (cl:code-char host-code)))
		   host-char)))
	((immediatep object)
	 (format cl:nil "Some immediate object"))
	((not (zerop (mod (remove-tag object)
			  +word-size-in-bytes+)))
	 (format cl:nil "an object with a heap tag, but not a multiple of the word size"))
	((>= (remove-tag object) sicl-exp-heap::*heap-pointer*)
	 (format cl:nil "-> heap object pointing far: ~8,'0x"
		 (remove-tag object)))
	((packagep object)
	 (format cl:nil "-> a package"))
	((symbolp object)
	 (format cl:nil "-> a symbol"))
	((stringp object)
	 (format cl:nil "-> a string"))
	((vectorp object)
	 (format cl:nil "-> a vector"))
	((dynamic-framep object)
	 (format cl:nil "-> a dynamic-frame"))
	((functionp object)
	 (format cl:nil "-> a function"))
	((codep object)
	 (format cl:nil "-> a code"))
	((unwind-protectp object)
	 (format cl:nil "-> a unwind-protect"))
	(t
	 (format cl:nil "I have no idea: ~s" object))))
	 
(defun describe (object &key (stream *trace-output*) (verbose t))
  (cond ((not (integerp object))
	 (format stream "It is a host object, calling host describe:~%")
	 (cl:describe object))
	((eql object +unbound+)
	 (format stream "It is the unbound object"))
	((fixnump object)
	 (format stream
		 "It is the fixnum ~s~%"
		 (host-number-from-fixnum object)))
	((characterp object)
	 (format stream "It is the character ~s"
		 (let* ((target-code (char-code object))
			(host-code (host-number-from-fixnum target-code))
			(host-char (cl:code-char host-code)))
		   host-char)))
	((consp object)
	 (format stream "It is a cons~%"))
	((eql (class-of object) *class-vector*)
	 (format stream "It is a vector~%"))
	((eql (class-of object) *class-string*)
	 (if verbose
	     (format stream "It is the string: \"~a\"~%"
		     (host-string-from-string object))
	     (format stream "It is a string")))
	((eql (class-of object) *class-symbol*)
	 (if verbose
	     (progn
	       (format stream "It is the symbol named: \"~a\"~%"
		       (host-string-from-string
			(symbol-name object)))
	       (format stream "Its package is: \"~a\"~%"
		       (host-string-from-string
			(package-name
			 (symbol-package object)))))
	     (format stream "It is a symbol")))
	((eql (class-of object) *class-package*)
	 (if verbose
	     (format stream "It is the package named: \"~a\"~%"
		     (host-string-from-string
		      (package-name object)))
	     (format stream "It is a package")))
	((eql (class-of object) *class-code*)
	 (format stream "It is a code object"))
	((eql (class-of object) *class-function*)
	 (format stream "It is a function object"))
	((eql (class-of object) *class-dynamic-frame*)
	 (format stream "It is a dynamic-frame"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Initialize low-level data structures needed for the rest of
;;; the system to be booted. 

;;; We want to capture errors as early as possible, so we start by
;;; making everything unbound so that any future access prior to
;;; initialization will signal an error. 
(defun make-everything-unbound ()
  (loop for variable in '(NIL
			  *linkage-function*
			  *linkage-symbol*
			  *linkage-error*
			  *class-fixnum*
			  *class-cons*
			  *class-character*
			  *class-vector*
			  *class-string*
			  *class-symbol*
			  *class-package*
			  *class-code*
			  *class-function*
			  *class-dynamic-frame*
			  *class-unwind-protect*
			  *all-packages*
			  *package*)
	do (makunbound variable)))

;;; Allocate only the 3-word standard instance later to become
;;; the representation of a class.  Right now, all we need is 
;;; a correct heap pointer to the class object; we do not yet 
;;; need to access any structure of the class object.  
(defun make-skeleton-class (linkage)
  (let ((slot-vector (malloc-words 2)))
    (memset (+ slot-vector (* 0 +word-size-in-bytes+)) +unbound+)
    (memset (+ slot-vector (* 1 +word-size-in-bytes+)) linkage)
    (let ((class (malloc-words 2)))
      (memset (+ class (* 0 +word-size-in-bytes+)) +unbound+)
      (memset (+ class (* 1 +word-size-in-bytes+)) slot-vector)
      (add-tag class +tag-other+))))

;;; Make class objects that we will need soon. 
(defun allocate-skeleton-classes ()
  (setunbound *class-fixnum* (make-skeleton-class *linkage-error*))
  (setunbound *class-cons* (make-skeleton-class *linkage-error*))
  (setunbound *class-character* (make-skeleton-class *linkage-error*))
  (setunbound *class-vector* (make-skeleton-class *linkage-error*))
  (setunbound *class-string* (make-skeleton-class *linkage-error*))
  (setunbound *class-symbol* (make-skeleton-class *linkage-symbol*))
  (setunbound *class-package* (make-skeleton-class *linkage-error*))
  (setunbound *class-code* (make-skeleton-class *linkage-error*))
  (setunbound *class-function* (make-skeleton-class *linkage-function*))
  (setunbound *class-dynamic-frame* (make-skeleton-class *linkage-error*))
  (setunbound *class-unwind-protect* (make-skeleton-class *linkage-error*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Linkage routines. 
;;;
;;; As mentioned above, each heap-allocated instance has a field
;;; containing a linkage routine.  The routine is a sequence of
;;; consecutive words containing machine instructions.  The linkage
;;; routine is called (or jumped to in case of tail calls) whenever an
;;; attempt is made to call the object as a function.  For most
;;; objects, the linkage function calls the ERROR function.  For
;;; functions, the linkage does more things like save the program
;;; counter, establish a stack frame, etc., and then jumps to the
;;; instructions of the body of the code of the function.  For symbols,
;;; the linkage routine trampolines to the SYMBOL-FUNCTION unless this
;;; slot is unbound in which case the ERROR function is called.
;;;
;;; When a function is traced, or has breakpoints in it, the linkage
;;; routine for that function is changed so that such information is
;;; checked for before the function is called.
;;;
;;; In this module, we don't know how long the linkage routines are or
;;; what is in them.  But we need to know the address of each one in
;;; order to start allocating heap objects.  To solve this problem, we
;;; allocate a fairly long sequence for each routine.  Later this
;;; sequence will become the elements of a vector with the right
;;; length, and the garbage collector will just recycle the unused
;;; cells at the end.
(defun allocate-space-for-linkage-routines ()
  ;; Allocate more space than needed for each linkage routine.
  (setunbound *linkage-error* (malloc-words 100))
  (setunbound *linkage-function* (malloc-words 100))
  (setunbound *linkage-symbol* (malloc-words 100)))

;;; We will need the NIL symbol early and it is a bit messy to create
;;; it correctly, because the creation of symbols usually needs to
;;; initialize the plist to the empty list, so NIL needs to exist
;;; before NIL can be created.  We solve this problem by giving a
;;; temporary value to the NIL symbol and then fixing our mistake
;;; later.
(defun create-symbol-nil ()
  ;; Temporarily set NIL to something wrong, so that when
  ;; we call MAKE-SYMBOL, this will become the value of
  ;; the SYMBOL-PACKAGE and the SYMBOL-PLIST.
  (setunbound NIL 0)
  (let* ((name (string-from-host-string "NIL")))
    ;; Now set NIL to its right value.
    (setf NIL (make-symbol name))
    ;; Correct the incorrect value of SYMBOL-PLIST of NIL.
    (setf (symbol-plist NIL) NIL)
    ;; Make sure the value of NIL is NIL
    (setf (symbol-value NIL) NIL)
    ;; Wait a while with the package, because we want to
    ;; set it to the COMMON-LISP package, but that one does
    ;; not exist yet.
    ))

;;; Create the COMMON-LISP package.
;;; Just as with the symbol NIL, we have to do a bit of manual
;;; patching here, because we already have the NIL symbol, but it
;;; doesn't have a package yet, and we have to make sure the existing
;;; NIL symbol is indeed an exported symbol of the COMMON-LISP
;;; package.
(defun create-common-lisp-package ()
  (let ((cl (make-package (string-from-host-string "COMMON-LISP")
			  :nicknames
			  (cons (string-from-host-string "CL")
				;; So that we can find this package
				;; when we translate host symbols to
				;; target symbols.
				(cons (string-from-host-string "SICL-COMMON-LISP")
				      NIL)))))
    (setf (symbol-package NIL) cl)
    (setf (package-external-symbols cl)
	  (cons NIL (package-external-symbols cl)))
    ;; Fill the package with symbol that have the same names
    ;; as the external symbols in the host package named 
    ;; SICL-COMMON-LISP and make those symbols external as well.
    (cl:do-external-symbols (host-symbol "SICL-COMMON-LISP")
      (export (intern (string-from-host-string (cl:symbol-name host-symbol))
		      cl)
	      cl))))
	    
(defun create-sicl-cl-implementation-package ()
  (let ((scli (make-package (string-from-host-string "SICL-CL-IMPLEMENTATION"))))
    (setf (package-use-list scli)
	  (cons (find-package (string-from-host-string "CL"))
		NIL))))

(defun create-system-package ()
  (flet ((ofho (o)
	   (object-from-host-object o)))
    (let ((system (make-package (ofho "SICL-SYSTEM")
				:nicknames
				(cons (ofho "SYSTEM") NIL))))
      ;; Make sure the same symbols exist in the target package as in the
      ;; host package.
      (cl:do-external-symbols (sym (cl:find-package "SICL-SYSTEM"))
	(intern (ofho (cl:symbol-name sym)) system))
      ;; Set values of symbols for linkage code
      (setf (symbol-value (ofho 'sicl-system:*linkage-function*))
	    *linkage-function*)
      (setf (symbol-value (ofho 'sicl-system:*linkage-symbol*))
	    *linkage-symbol*)
      (setf (symbol-value (ofho 'sicl-system:*linkage-error*))
	    *linkage-error*)
      ;; Set values of symbols for some classes.
      (setf (symbol-value (ofho 'sicl-system:*class-fixnum*))
	    *class-fixnum*)
      (setf (symbol-value (ofho 'sicl-system:*class-cons*))
	    *class-cons*)
      (setf (symbol-value (ofho 'sicl-system:*class-character*))
	    *class-character*)
      (setf (symbol-value (ofho 'sicl-system:*class-vector*))
	    *class-vector*)
      (setf (symbol-value (ofho 'sicl-system:*class-string*))
	    *class-string*)
      (setf (symbol-value (ofho 'sicl-system:*class-symbol*))
	    *class-symbol*)
      (setf (symbol-value (ofho 'sicl-system:*class-package*))
	    *class-package*)
      (setf (symbol-value (ofho 'sicl-system:*class-code*))
	    *class-code*)
      (setf (symbol-value (ofho 'sicl-system:*class-function*))
	    *class-function*)
      (setf (symbol-value (ofho 'sicl-system:*class-dynamic-frame*))
	    *class-dynamic-frame*)
      (setf (symbol-value (ofho 'sicl-system:*class-unwind-protect*))
	    *class-unwind-protect*))))

(defun initialize-low ()
  (make-everything-unbound)
  (initialize-heap)
  (allocate-space-for-linkage-routines)
  (allocate-skeleton-classes)
  (create-symbol-nil)
  (setunbound *all-packages* (cons NIL NIL))
  (create-common-lisp-package)
  (setunbound *package*
	      (find-package (string-from-host-string "COMMON-LISP")))
  (create-sicl-cl-implementation-package)
  (create-system-package))

  


