(in-package :sicl-cons-high)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Externally visible conditions

;;; This condition is used to mix into other conditions that
;;; will report the construct (function, macro, etc) in which 
;;; the condition was signaled. 
(define-condition name-mixin ()
  ((%name :initarg :name :reader name)))

;;; This condition is used by functions an macros that require 
;;; some argument to be a nonnegative integer. 
(define-condition must-be-nonnegative-integer (type-error name-mixin)
  ()
  (:default-initargs :expected-type '(integer 0)))

;;; This condition is used by functions and macros that require
;;; some argument to be a cons cell.
(define-condition must-be-cons (type-error name-mixin)
  ()
  (:default-initargs :expected-type 'cons))

;;; This condition is used by functions and macros that require
;;; some argument to be a list (a cons or nil).
(define-condition must-be-list (type-error name-mixin)
  ()
  (:default-initargs :expected-type 'list))

;;; This condition is used by functions and macros that require
;;; some list to be a proper list.  
(define-condition must-be-proper-list (type-error name-mixin)
  ()
  (:default-initargs :expected-type 'list))

;;; This condition is used by functions and macros that require
;;; some list to be either a proper list or a circular list.
(define-condition must-be-proper-or-circular-list (type-error name-mixin)
  ()
  (:default-initargs :expected-type 'list))

;;; This condition is used by functions and macros that require some
;;; list to be either a proper list or a dotted list (but not a
;;; circular list). 
(define-condition must-be-proper-or-dotted-list (type-error name-mixin)
  ()
  (:default-initargs :expected-type 'list))

;;; This condition is used by functions and macros that require
;;; some argument to be a propterty list.
(define-condition must-be-property-list (type-error name-mixin)
  ()
  (:default-initargs :expected-type 'list))

;;; This condition is used by functions and macros that require
;;; some argument to be a association list.
(define-condition must-be-association-list (type-error name-mixin)
  ()
  (:default-initargs :expected-type 'list))

;;; This condition is used by functions that take :test and :test-not
;;; keyword arguments, and is signaled when both of those are given.
(define-condition both-test-and-test-not-given (error name-mixin)
  ())

;;; This condition is used by the map* family functions when no lists
;;; were given, since those functions require at least one list
;;; argument.
(define-condition at-least-one-list-required (error name-mixin)
  ())

;;; This condition is used by the list* function when no arguments
;;; were given, since that function requires at least one argument.
(define-condition at-least-one-argument-required (error name-mixin)
  ())

;;; This condition is used by the pairlis function when 
;;; the two lists are not of the same length.
(define-condition lists-must-have-the-same-length (error name-mixin)
  ((%list1 :initarg :list1 :reader list1)
   (%list2 :initarg :list2 :reader list2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Type null

(deftype null () 'cl:null)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function null

;;; We need the null function in macro expanders in this module
;;; so we define it early.  

(eval-when (:compile-toplevel :load-toplevel)
  (defun null (object)
    (eq object '())))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function append

;;; We need the append function in macro expanders in this module
;;; so we define it early.  

;;; It used to be the case that the append function was defined
;;; in terms of (loop ... append ...), but it turns out that
;;; some implementations have buggy versions of (loop ... append ...) 
;;; so it is better to just define it in terms of a lower-level
;;; construct.  Doing it this way allows for implementations with 
;;; a buggy loop to still use this module without first replacing
;;; loop by ours. 

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun append (&rest lists)
    (if (null lists)
	;; The standard doesn't say explicity that calling append with
	;; zero arguments is allowed and returns nil, but one of the
	;; examples on the HyperSpec page for append suggests that. 
	'()
	;; We need to copy every list except the last one.  Also,
	;; every list except the last one must be a proper list.  In
	;; order to avoid making a test in each iteration of the loop,
	;; we use a sentinel, which is a cons cell whose cdr contains
	;; the real result that we return in the end. 
	(let* ((sentinel (list nil))
	       ;; The variable last points to the last cons cell of the
	       ;; resulting list to be accumulated. 
	       (last sentinel))
	  (loop until (null (cdr lists))
		do (loop with list = (car lists)
			 do (cond ((consp list)
				   ;; There are more cells, copy the first one.
				   (setf (cdr last) (list (car list)))
				   (setf last (cdr last))
				   (setf list (cdr list)))
				  ((null list)
				   ;; The list is a proper list and we reached the
				   ;; end of it.
				   (loop-finish))
				  (t
				   ;; The list is a dotted list
				   (error 'must-be-proper-list
					  :datum (car lists)
					  :name 'append))))
		   ;; We are through with the first list in lists
		   (setf lists (cdr lists)))
	  ;; When we get here, there is only one list left in lists.
	  ;; And in fact, it doesn't have to be a list at all. 
	  ;; Attach it to the end of what we have accumulated so far.
	  (setf (cdr last) (car lists))
	  ;; Skip the sentinel and return the rest. 
	  (cdr sentinel)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function list

;;; We need the list function in the implementation of other functions
;;; and or macros in this module, so we define it early.

;;; this implementation assumes that there is no 
;;; structure sharing between the &rest argument
;;; and the last argument to apply

(eval-when (:compile-toplevel :load-toplevel)
  (defun list (&rest elements)
    elements))

(define-compiler-macro list (&rest args)
  (if (null args)
      'nil
      `(cons ,(car args) (list ,@(cdr args)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function mapcar
;;;
;;; The compiler macro is defined later because it uses the append
;;; function that may not be defined at this point.

(eval-when (:compile-toplevel :load-toplevel)
  (defun mapcar (function &rest lists)
    (when (null lists)
      (error 'at-least-one-list-required :name 'mapcar))
    (loop for remaining = lists
	    then (loop for list in remaining collect (cdr list))
	  until (loop for list in remaining thereis (atom list))
	  collect (apply function
			 (loop for list in remaining collect (car list)))
	  finally (loop for rem in remaining
			for list in lists
			do (when (not (listp rem))
			     (error 'must-be-proper-list
				    :datum list
				    :name 'mapcar))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Macro push

;;; We need the push macro in the implementation of other functions
;;; and or macros in this module, so we define it early.

(defmacro push (item place &environment env)
  (multiple-value-bind (vars vals store-vars writer-form reader-form)
      (get-setf-expansion place env)
    (let ((item-var (gensym)))
      `(let* ((,item-var ,item)
	      ,@(mapcar #'list vars vals)
	      (,(car store-vars) (cons ,item-var ,reader-form)))
	 ,writer-form))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Macro pop

;;; We need the pop macro in the implementation of other functions
;;; and or macros in this module, so we define it early.

(defmacro pop (place &environment env)
  (multiple-value-bind (vars vals store-vars writer-form reader-form)
      (get-setf-expansion place env)
    `(let* (,@(mapcar #'list vars vals)
	    (,(car store-vars) ,reader-form))
       (if (listp ,(car store-vars))
	   (prog1 (car ,(car store-vars))
	     (setq ,(car store-vars) (cdr ,(car store-vars)))
	     ,writer-form)
	   (error 'must-be-list
		  :datum ,(car store-vars)
		  :name 'pop)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function nconc

;;; It used to be the case that the append function was defined
;;; in terms of (loop ... nconc ...), but it turns out that
;;; some implementations have buggy versions of (loop ... nconc ...) 
;;; so it is better to just define it in terms of a lower-level
;;; construct.  Doing it this way allows for implementations with 
;;; a buggy loop to still use this module without first replacing
;;; loop by ours. 

(defun nconc (&rest lists)
  (if (null lists)
      nil
      ;; As with append, we use a sentinel to avoid a test in a loop. 
      (let* ((sentinel (list nil))
	     ;; The variable last is a pointer to the last cell of the
	     ;; accumulated list. 
	     (last sentinel))
	(loop for remaining = lists then (cdr remaining)
	      ;; Stop when we have processed every list except the last
	      ;; (which may be any object). 
	      until (null (cdr remaining))
	      do (let ((list (car remaining)))
		   (cond ((consp list)
			  ;; This is the normal case, the list has at
			  ;; least one cons cell, but may be proper or
			  ;; dotted. 
			  ;; 
			  ;; Hook it up to the end of the accumulated
			  ;; list.
			  (setf (cdr last) list)
			  ;; And move to the next item, making sure
			  ;; that last is right behind list. 
			  (setf last list)
			  (setf list (cdr list))
			  ;; Continue this process until list reaches
			  ;; an atom.  What that happens, last points
			  ;; to the last cons cell of the list, which
			  ;; is exactly what we need for next
			  ;; iteration.
			  (loop while (consp list)
				do (setf last list)
				   (setf list (cdr list))))
			 ((null list)
			  ;; If the list is nil, we just skip it. 
			  nil)
			 (t
			  ;; It is neither a cons nor nil, so we
			  ;; signal an error. 
			  (error 'must-be-list
				 :datum (car remaining)
				 :name 'nconc))))
	      finally (progn
			;; Hook up the last list to the end
			;; of what we have accumulated
			(setf (cdr last) (car remaining))))
	;; Skip the sentinel and return the rest. 
	(cdr sentinel))))

(defmacro define-c*r-function (function-name letters)
  (flet ((primitive (letter)
           (if (eql letter #\A) 'car 'cdr)))
    (flet ((one-iteration (letter)
             `(progn (cond ((consp remaining)
			    (setf remaining
				  (,(primitive letter) remaining)))
			   ((null remaining)
			    (return-from ,function-name nil))
			   (t
			    (error 'must-be-list
				   :datum remaining
				   :name ',function-name))))))
      `(defun ,function-name (list)
         (let ((remaining list))
           ,@(loop for letter across (reverse letters)
                   collect (one-iteration letter))
           remaining)))))

(define-c*r-function caar "AA")
(define-c*r-function cadr "AD")
(define-c*r-function cdar "DA")
(define-c*r-function cddr "DD")
(define-c*r-function caaar "AAA")
(define-c*r-function caadr "AAD")
(define-c*r-function cadar "ADA")
(define-c*r-function caddr "ADD")
(define-c*r-function cdaar "DAA")
(define-c*r-function cdadr "DAD")
(define-c*r-function cddar "DDA")
(define-c*r-function cdddr "DDD")
(define-c*r-function caaaar "AAAA")
(define-c*r-function caaadr "AAAD")
(define-c*r-function caadar "AADA")
(define-c*r-function caaddr "AADD")
(define-c*r-function cadaar "ADAA")
(define-c*r-function cadadr "ADAD")
(define-c*r-function caddar "ADDA")
(define-c*r-function cadddr "ADDD")
(define-c*r-function cdaaar "DAAA")
(define-c*r-function cdaadr "DAAD")
(define-c*r-function cdadar "DADA")
(define-c*r-function cdaddr "DADD")
(define-c*r-function cddaar "DDAA")
(define-c*r-function cddadr "DDAD")
(define-c*r-function cdddar "DDDA")
(define-c*r-function cddddr "DDDD")

(define-c*r-function first   "A")
(define-c*r-function second  "AD")
(define-c*r-function third   "ADD")
(define-c*r-function fourth  "ADDD")
(define-c*r-function fifth   "ADDDD")
(define-c*r-function sixth   "ADDDDD")
(define-c*r-function seventh "ADDDDDD")
(define-c*r-function eighth  "ADDDDDDD")
(define-c*r-function ninth  "ADDDDDDDD")
(define-c*r-function tenth   "ADDDDDDDDD")

(eval-when (:compile-toplevel :load-toplevel)
  (defmacro define-setf-c*r-function (function-name letters)
    (flet ((primitive (letter)
             (if (eql letter #\A) 'car 'cdr)))
      (flet ((one-iteration (letter)
               `(progn (if (consp remaining)
			   (setf remaining
				 (,(primitive letter) remaining))
			   (error 'must-be-cons
				  :datum remaining
				  :name '(setf ,function-name))))))
        `(defun (setf ,function-name) (new-object list)
           (let ((remaining list))
             ,@(append (loop for letter across (reverse (subseq letters 1))
			     collect (one-iteration letter))
		       `((if (consp remaining)
			     (setf (,(primitive (aref letters 0)) remaining)
				   new-object)
			     (error 'must-be-cons
				    :datum remaining
				    :name '(setf ,function-name)))))))))))

;;; These are commented out for now because they
;;; trip the package lock of the CL package.
;; (defun (setf car) (object cons)
;;   (rplaca cons object)
;;   object)

;; (defun (setf cdr) (object cons)
;;   (rplacd cons object)
;;   object)

(define-setf-c*r-function caar "AA")
(define-setf-c*r-function cadr "AD")
(define-setf-c*r-function cdar "DA")
(define-setf-c*r-function cddr "DD")
(define-setf-c*r-function caaar "AAA")
(define-setf-c*r-function caadr "AAD")
(define-setf-c*r-function cadar "ADA")
(define-setf-c*r-function caddr "ADD")
(define-setf-c*r-function cdaar "DAA")
(define-setf-c*r-function cdadr "DAD")
(define-setf-c*r-function cddar "DDA")
(define-setf-c*r-function cdddr "DDD")
(define-setf-c*r-function caaaar "AAAA")
(define-setf-c*r-function caaadr "AAAD")
(define-setf-c*r-function caadar "AADA")
(define-setf-c*r-function caaddr "AADD")
(define-setf-c*r-function cadaar "ADAA")
(define-setf-c*r-function cadadr "ADAD")
(define-setf-c*r-function caddar "ADDA")
(define-setf-c*r-function cadddr "ADDD")
(define-setf-c*r-function cdaaar "DAAA")
(define-setf-c*r-function cdaadr "DAAD")
(define-setf-c*r-function cdadar "DADA")
(define-setf-c*r-function cdaddr "DADD")
(define-setf-c*r-function cddaar "DDAA")
(define-setf-c*r-function cddadr "DDAD")
(define-setf-c*r-function cdddar "DDDA")
(define-setf-c*r-function cddddr "DDDD")

(define-setf-c*r-function first   "A")
(define-setf-c*r-function second  "AD")
(define-setf-c*r-function third   "ADD")
(define-setf-c*r-function fourth  "ADDD")
(define-setf-c*r-function fifth   "ADDDD")
(define-setf-c*r-function sixth   "ADDDDD")
(define-setf-c*r-function seventh "ADDDDDD")
(define-setf-c*r-function eighth  "ADDDDDDD")
(define-setf-c*r-function ninth  "ADDDDDDDD")
(define-setf-c*r-function tenth   "ADDDDDDDDD")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function rest

(defun rest (list)
  (cond ((consp list)
	 (cdr list))
	((null list)
	 nil)
	(t
	 (error 'must-be-list
		:datum list
		:name 'rest))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Setf expander for rest

(define-setf-expander rest (x)
  (let ((subform-temp (gensym))
	(store-temp (gensym)))
    (values (list subform-temp)
	    (list x)
	    (list store-temp)
	    `(if (consp ,subform-temp)
		 (progn 
		   (rplacd ,subform-temp ,store-temp)
		   ,store-temp)
		 (error 'must-be-cons
			:datum ,subform-temp
			:name '(setf rest)))
	    `(cdr ,subform-temp))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; type list

(deftype list () '(or cons null))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function list*

;;; this implementation assumes that there is no 
;;; structure sharing between the &rest argument
;;; and the last argument to apply
(defun list* (&rest elements)
  (when (null elements)
    (error 'at-least-one-argument-required :name 'list*))
  (if (null (cdr elements))
      (car elements)
      (loop for remaining on elements
	    until (null (cddr remaining))
	    finally (setf (cdr remaining)
			  (cadr remaining))
                    (return elements))))

(define-compiler-macro list* (&whole form &rest args)
  (cond  ((null args) form)
	 ((null (cdr args)) (car args))
	 (t `(cons ,(car args) (list* ,@(cdr args))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function last

(defun last (list &optional (n 1))
  (unless (typep list 'list)
    (error 'must-be-list
	   :datum list
	   :name 'last))
  (unless (typep n '(integer 0))
    (error 'must-be-nonnegative-integer
	   :datum n
	   :name 'last))
  (let ((remaining list))
    (loop repeat n
	  until (atom remaining)
	  do (setf remaining (cdr remaining)))
    (loop until (atom remaining)
	  do (setf list (cdr list))
	  do (setf remaining (cdr remaining)))
    list))

;;; special version of last used when the second argument to
;;; last is 1. 
(defun last-1 (list)
  (unless (typep list 'list)
    (error 'must-be-list
	   :datum list
	   :name 'last))
  ;; We can use for ... on, because it uses atom to test for
  ;; the end of the list. 
  (loop for rest on list
	do (setf list rest))
  list)

(define-compiler-macro last (&whole form list &optional (n 1))
  (if (eql n 1)
      `(last-1 ,list)
      form))

(defun copy-list (list)
  (unless (typep list 'list)
    (error 'must-be-list
	   :datum list
	   :name 'copy-list))
  (if (null list)
      nil
      (let* ((result (list (car list)))
	     (trailer result))
	(loop do (setf list (cdr list))
	      until (atom list)
	      ;; list is not an atom, allocate a new cell
	      ;; at the end of the result list
	      do (setf (cdr trailer) (list (car list)))
	      do (setf trailer (cdr trailer)))
	;; when we come here, list is an atom,
	;; either NIL because the list was a proper list
	;; or some other atom because it was not a proper list
	(setf (cdr trailer) list)
	result)))

;;; The standard requires the argument to be either a circular list
;;; or a proper list.  In case of a circular list, NIL should be 
;;; returned.  
(defun list-length (list)
  (cond ((consp list)
	 ;; The list starts with a cons.  We use the method by which
	 ;; one pointer (fast) advences by two steps in each
	 ;; iteration, and another pointer (slow) advances by one
	 ;; step.  If the two meet we have a circular list.  The only
	 ;; purpose of slow is to detect circularity.  The length of
	 ;; the list is determined by the number of cons cells seen by
	 ;; fast.
	 (let ((slow list)
	       (fast (cdr list))
	       ;; This variable counts the number of cons cells as
	       ;; seen by fast.
	       (length 1))
	   (loop do (when (eq slow fast)
		      ;; We have a circular list
		      (return-from list-length nil))
		    ;; Start by checking whether fast is a cons,
		    ;; because this is the most likely case, so will
		    ;; likely succeed.
		    (cond ((consp fast)
			   ;; We are not at the end of the list
			   ;; try to advance by cddr, but do it in
			   ;; two steps so that we can check that
			   ;; we are allowed to do that.
			   (setf fast (cdr fast))
			   ;; We now need to remember that the value of
			   ;; length is one less that the number of cons
			   ;; cells seen by fast. 
			   (cond ((consp fast)
				  ;; We indeed had two conses, so we can
				  ;; make fast advance by one more cons.
				  ;; This is the normal iteration case.
				  (setf fast (cdr fast))
				  ;; We advanced fast by cddr, so now we need to
				  ;; advance slow by cdr. 
				  (setf slow (cdr slow))
				  ;; The length is counted by the
				  ;; position of fast, so since we
				  ;; advanced by cdr, the length
				  ;; increases by 2. 
				  (incf length 2))
				 ((null fast)
				  ;; We reached the end of a proper
				  ;; list.  But we already advanced
				  ;; fast by cdr without incrementing
				  ;; length, so we need to compensate
				  ;; for that.
				  (return-from list-length (1+ length)))
				 (t
				  ;; The variable fast contains
				  ;; neither a a cons cell nor nil.
				  ;; This is a violation of the
				  ;; requirement that the list given
				  ;; should be either proper or
				  ;; circular.
				  (error 'must-be-proper-or-circular-list
					 :datum list
					 :name 'list-length))))
			  ((null fast)
			   ;; We reached the end of the list.
			   (return-from list-length length))
			  (t
			   ;; The variable fast contains
			   ;; neither a a cons cell nor nil.
			   ;; This is a violation of the
			   ;; requirement that the list given
			   ;; should be either proper or
			   ;; circular.
			   (error 'must-be-proper-or-circular-list
				  :datum list
				  :name 'list-length))))))
	((null list)
	 ;; The initial list was empty, and such a list has zero length
	 0)
	(t
	 ;; The argument given is neither a cons cell nor nil.  This
	 ;; is a violation of the requirement that the list given
	 ;; should be either proper or circular.
	 (error 'must-be-proper-or-circular-list
		:datum list
		:name 'list-length))))

(defun make-list (length &key (initial-element nil))
  (unless (typep length '(integer 0))
    (error 'must-be-nonnegative-integer
	   :datum length
	   :name 'make-list))
  (loop repeat length
	collect initial-element))

(defun nthcdr (n list)
  (unless (typep n '(integer 0))
    (error 'must-be-nonnegative-integer
	   :datum n
	   :name 'nthcdr))
  (loop until (zerop n)
	until (atom list)
	do (decf n)
	do (setf list (cdr list)))
  (when (and (plusp n) (not (null list)))
    (error 'must-be-cons
	   :datum list
	   :name 'nthcdr))
  list)

(defun nth (n list)
  (unless (typep n '(integer 0))
    (error 'must-be-nonnegative-integer
	   :datum n
	   :name 'nthcdr))
  (loop until (zerop n)
	until (atom list)
	do (decf n)
	do (setf list (cdr list)))
  (when (not (listp list))
    (error 'must-be-list
	   :datum list
	   :name 'nth))
  (car list))

(defun (setf nth) (object n list)
  (unless (typep n '(integer 0))
    (error 'must-be-nonnegative-integer
	   :datum n
	   :name 'nthcdr))
  (loop until (zerop n)
	until (atom list)
	do (decf n)
	do (setf list (cdr list)))
  (when (not (consp list))
    (error 'must-be-cons
	   :datum list
	   :name '(setf nth)))
  (setf (car list) object))

(defun copy-tree (tree)
  (if (atom tree)
      tree
      (cons (copy-tree (car tree)) (copy-tree (cdr tree)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function tree-equal 

(defun tree-equal-eq (tree1 tree2)
  (or (and (atom tree1)
           (atom tree2)
           (eq tree1 tree2))
      (and (consp tree1)
           (consp tree2)
           (tree-equal-eq (car tree1) (car tree2))
           (tree-equal-eq (cdr tree1) (cdr tree2)))))

(defun tree-equal-eql (tree1 tree2)
  (or (and (atom tree1)
           (atom tree2)
           (eql tree1 tree2))
      (and (consp tree1)
           (consp tree2)
           (tree-equal-eql (car tree1) (car tree2))
           (tree-equal-eql (cdr tree1) (cdr tree2)))))

(defun tree-equal-not-eq (tree1 tree2)
  (or (and (atom tree1)
           (atom tree2)
           (not (eq tree1 tree2)))
      (and (consp tree1)
           (consp tree2)
           (tree-equal-not-eq (car tree1) (car tree2))
           (tree-equal-not-eq (cdr tree1) (cdr tree2)))))

(defun tree-equal-not-eql (tree1 tree2)
  (or (and (atom tree1)
           (atom tree2)
           (not (eql tree1 tree2)))
      (and (consp tree1)
           (consp tree2)
           (tree-equal-not-eql (car tree1) (car tree2))
           (tree-equal-not-eql (cdr tree1) (cdr tree2)))))

(defun tree-equal-test (tree1 tree2 test)
  (or (and (atom tree1)
           (atom tree2)
           (funcall test tree1 tree2))
      (and (consp tree1)
           (consp tree2)
           (tree-equal-test (car tree1) (car tree2) test)
           (tree-equal-test (cdr tree1) (cdr tree2) test))))

(defun tree-equal-test-not (tree1 tree2 test)
  (or (and (atom tree1)
           (atom tree2)
           (not (funcall test tree1 tree2)))
      (and (consp tree1)
           (consp tree2)
           (tree-equal-test-not (car tree1) (car tree2) test)
           (tree-equal-test-not (cdr tree1) (cdr tree2) test))))

(defun tree-equal (tree1 tree2
                   &key
                   (test nil testp)
                   (test-not nil test-not-p))
  (when (and testp test-not-p)
    (error 'both-test-and-test-not-given
	   :name 'tree-equal))
  (if testp
      (if (eq test #'eq)
          (tree-equal-eq tree1 tree2)
          (if (eq test #'eql)
              (tree-equal-eql tree1 tree2)
              (tree-equal-test tree1 tree2 test)))
      (if test-not-p 
          (if (eq test-not #'eq)
              (tree-equal-not-eq tree1 tree2)
              (if (eq test-not #'eql)
                  (tree-equal-not-eql tree1 tree2)
                  (tree-equal-test-not tree1 tree2 test-not)))
          (tree-equal-eql tree1 tree2))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function endp

(defun endp (list)
  (unless (typep list 'list)
    (error 'must-be-list
	   :datum list
	   :name 'endp))
  (null list))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compiler macro for mapcar.  
;;;
;;; The function is defined earlier because it is needed in
;;; macroexpanders for push and pop.

;;; The compiler macro for mapcar generates code to loop
;;; individually over each list given, and thus avoids having
;;; a list of lists (which implies consing).  Since the number
;;; of lists given is known, we can use funcall instead of 
;;; apply when we call the function. 
(define-compiler-macro mapcar (&whole form function &rest lists)
  (if (null lists)
      form
      (let ((funvar (gensym))
	    (listvars (loop for var in lists collect (gensym)))
	    (vars (loop for var in lists collect (gensym))))
	`(loop with ,funvar = ,function
	       ,@(apply #'append (loop for listvar in listvars
				       for list in lists
				       collect `(with ,listvar = ,list)))
	       ,@(apply #'append (loop for var in vars
				       for listvar in listvars
				       collect `(for ,var = ,listvar then (cdr ,var))))
	       ,@(apply #'append (loop for var in vars
				       collect `(until (atom ,var))))
	       collect (funcall ,funvar ,@(loop for var in vars
						collect `(car ,var)))
	       finally (progn ,@(loop for var in vars
				      for listvar in listvars
				      collect `(unless (listp ,var)
						 (error 'must-be-proper-list
							:datum ,listvar
							:name 'mapcar))))))))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function mapc

(defun mapc (function &rest lists)
  (when (null lists)
    (error 'at-least-one-list-required :name 'mapc))
  (loop for remaining = lists
	  then (loop for list in remaining collect (cdr list))
	until (loop for list in remaining thereis (atom list))
	do (apply function
		  (loop for list in remaining collect (car list)))
	finally (loop for rem in remaining
		      for list in lists
		      do (when (not (listp rem))
			   (error 'must-be-proper-list
				  :datum list
				  :name 'mapc))))
  ;; The mapc function returns the first list
  (car lists))

;;; The compiler macro for mapc generates code to loop
;;; individually over each list given, and thus avoids having
;;; a list of lists (which implies consing).  Since the number
;;; of lists given is known, we can use funcall instead of 
;;; apply when we call the function. 
(define-compiler-macro mapc (&whole form function &rest lists)
  (if (null lists)
      form
      (let ((funvar (gensym))
	    (listvars (loop for var in lists collect (gensym)))
	    (vars (loop for var in lists collect (gensym))))
	`(loop with ,funvar = ,function
	       ,@(apply #'append (loop for listvar in listvars
				       for list in lists
				       collect `(with ,listvar = ,list)))
	       ,@(apply #'append (loop for var in vars
				       for listvar in listvars
				       collect `(for ,var = ,listvar then (cdr ,var))))
	       ,@(apply #'append (loop for var in vars
				       collect `(until (atom ,var))))
	       do (funcall ,funvar ,@(loop for var in vars
					   collect `(car ,var)))
	       finally (progn ,@(loop for var in vars
				      for listvar in listvars
				      collect `(unless (listp ,var)
						 (error 'must-be-proper-list
							:datum ,listvar
							:name 'mapc)))
			      (return ,(car listvars)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function maplist

(defun maplist (function &rest lists)
  (when (null lists)
    (error 'at-least-one-list-required :name 'maplist))
  (loop for remaining = lists
	  then (loop for list in remaining collect (cdr list))
	until (loop for list in remaining thereis (atom list))
	collect (apply function remaining)
	finally (loop for rem in remaining
		      for list in lists
		      do (when (not (listp rem))
			   (error 'must-be-proper-list
				  :datum list
				  :name 'maplist)))))

;;; The compiler macro for maplist generates code to loop
;;; individually over each list given, and thus avoids having
;;; a list of lists (which implies consing).  Since the number
;;; of lists given is known, we can use funcall instead of 
;;; apply when we call the function. 
(define-compiler-macro maplist (&whole form function &rest lists)
  (if (null lists)
      form
      (let ((funvar (gensym))
	    (listvars (loop for var in lists collect (gensym)))
	    (vars (loop for var in lists collect (gensym))))
	`(loop with ,funvar = ,function
	       ,@(apply #'append (loop for listvar in listvars
				       for list in lists
				       collect `(with ,listvar = ,list)))
	       ,@(apply #'append (loop for var in vars
				       for listvar in listvars
				       collect `(for ,var = ,listvar then (cdr ,var))))
	       ,@(apply #'append (loop for var in vars
				       collect `(until (atom ,var))))
	       collect (funcall ,funvar ,@vars)
	       finally (progn ,@(loop for var in vars
				      for listvar in listvars
				      collect `(unless (listp ,var)
						 (error 'must-be-proper-list
							:datum ,listvar
							:name 'maplist))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function mapl

(defun mapl (function &rest lists)
  (when (null lists)
    (error 'at-least-one-list-required :name 'mapl))
  (loop for remaining = lists
	  then (loop for list in remaining collect (cdr list))
	until (loop for list in remaining thereis (atom list))
	do (apply function remaining)
	finally (loop for rem in remaining
		      for list in lists
		      do (when (not (listp rem))
			   (error 'must-be-proper-list
				  :datum list
				  :name 'mapl))))
  ;; The mapl function returns the first list
  (car lists))

;;; The compiler macro for mapl generates code to loop
;;; individually over each list given, and thus avoids having
;;; a list of lists (which implies consing).  Since the number
;;; of lists given is known, we can use funcall instead of 
;;; apply when we call the function. 
(define-compiler-macro mapl (&whole form function &rest lists)
  (if (null lists)
      form
      (let ((funvar (gensym))
	    (listvars (loop for var in lists collect (gensym)))
	    (vars (loop for var in lists collect (gensym))))
	`(loop with ,funvar = ,function
	       ,@(apply #'append (loop for listvar in listvars
				       for list in lists
				       collect `(with ,listvar = ,list)))
	       ,@(apply #'append (loop for var in vars
				       for listvar in listvars
				       collect `(for ,var = ,listvar then (cdr ,var))))
	       ,@(apply #'append (loop for var in vars
				       collect `(until (atom ,var))))
	       do (funcall ,funvar ,@vars)
	       finally (progn ,@(loop for var in vars
				      for listvar in listvars
				      collect `(unless (listp ,var)
						 (error 'must-be-proper-list
							:datum ,listvar
							:name 'mapl)))
			      (return ,(car listvars)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function mapcan

(defun mapcan (function &rest lists)
  (when (null lists)
    (error 'at-least-one-list-required :name 'mapcan))
  (loop for remaining = lists
	  then (loop for list in remaining collect (cdr list))
	until (loop for list in remaining thereis (atom list))
	nconc (apply function
		     (loop for list in remaining collect (car list)))
	finally (loop for rem in remaining
		      for list in lists
		      do (when (not (listp rem))
			   (error 'must-be-proper-list
				  :datum list
				  :name 'mapcan)))))

;;; The compiler macro for mapcan generates code to loop
;;; individually over each list given, and thus avoids having
;;; a list of lists (which implies consing).  Since the number
;;; of lists given is known, we can use funcall instead of 
;;; apply when we call the function. 
(define-compiler-macro mapcan (&whole form function &rest lists)
  (if (null lists)
      form
      (let ((funvar (gensym))
	    (listvars (loop for var in lists collect (gensym)))
	    (vars (loop for var in lists collect (gensym))))
	`(loop with ,funvar = ,function
	       ,@(apply #'append (loop for listvar in listvars
				       for list in lists
				       collect `(with ,listvar = ,list)))
	       ,@(apply #'append (loop for var in vars
				       for listvar in listvars
				       collect `(for ,var = ,listvar then (cdr ,var))))
	       ,@(apply #'append (loop for var in vars
				       collect `(until (atom ,var))))
	       nconc (funcall ,funvar ,@(loop for var in vars
					      collect `(car ,var)))
	       finally (progn ,@(loop for var in vars
				      for listvar in listvars
				      collect `(unless (listp ,var)
						 (error 'must-be-proper-list
							:datum ,listvar
							:name 'mapcan))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function mapcon

(defun mapcon (function &rest lists)
  (when (null lists)
    (error 'at-least-one-list-required :name 'mapcon))
  (loop for remaining = lists
	  then (loop for list in remaining collect (cdr list))
	until (loop for list in remaining thereis (atom list))
	nconc (apply function remaining)
	finally (loop for rem in remaining
		      for list in lists
		      do (when (not (listp rem))
			   (error 'must-be-proper-list
				  :datum list
				  :name 'mapcon)))))

;;; The compiler macro for mapcon generates code to loop
;;; individually over each list given, and thus avoids having
;;; a list of lists (which implies consing).  Since the number
;;; of lists given is known, we can use funcall instead of 
;;; apply when we call the function. 
(define-compiler-macro mapcon (&whole form function &rest lists)
  (if (null lists)
      form
      (let ((funvar (gensym))
	    (listvars (loop for var in lists collect (gensym)))
	    (vars (loop for var in lists collect (gensym))))
	`(loop with ,funvar = ,function
	       ,@(apply #'append (loop for listvar in listvars
				       for list in lists
				       collect `(with ,listvar = ,list)))
	       ,@(apply #'append (loop for var in vars
				       for listvar in listvars
				       collect `(for ,var = ,listvar then (cdr ,var))))
	       ,@(apply #'append (loop for var in vars
				       collect `(until (atom ,var))))
	       nconc (funcall ,funvar ,@vars)
	       finally (progn ,@(loop for var in vars
				      for listvar in listvars
				      collect `(unless (listp ,var)
						 (error 'must-be-proper-list
							:datum ,listvar
							:name 'mapcon))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function revappend

(defun revappend (list tail)
  (loop with result = tail
	for remaining = list then (cdr remaining)
	until (atom remaining)
        do (push (car remaining) result)
        finally (unless (null remaining)
		  (error 'must-be-proper-list
			 :datum list
			 :name 'revappend))
		(return result)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function nreconc

(defun nreconc (list tail)
  (loop with remaining = list
        with result = tail
        until (atom remaining)
        do (let ((temp (cdr remaining)))
             (setf (cdr remaining) result
                   result remaining
                   remaining temp))
        finally (unless (null remaining)
		  (error 'must-be-proper-list
			 :datum list
			 :name 'nreconc))
		(return result)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function butlast

;;; The HyperSpec doesn't specify what happens if n is 0.
;;; There is a note that says:
;;;
;;;     (butlast list n) ==  (ldiff list (last list n))
;;;
;;; but notes are not normative.  If you do believe this note,
;;; then (butlast '(1 2 3 . 4) 0) should return (1 2 3), i.e., 
;;; it doesn't return the unmodified list.  
;;;
;;; This implementation works according to the HyperSpec note
;;; when n is 0.

(defun butlast (list &optional (n 1))
  (unless (typep list 'list)
    (error 'must-be-list
	   :datum list
	   :name 'butlast))
  (unless (typep n '(integer 0))
    (error 'must-be-nonnegative-integer
	   :datum n
	   :name 'butlast))
  (let ((remaining list))
    (loop repeat n
	  until (atom remaining)
	  do (setf remaining (cdr remaining)))
    (loop for slow on remaining
	  until (atom remaining)
          do (setf remaining (cdr remaining))
          collect (prog1 (car list) (setf list (cdr list)))
	  until (atom remaining)
          do (setf remaining (cdr remaining))
          collect (prog1 (car list) (setf list (cdr list)))
	  do (when (eq slow remaining)
	       ;; we have a circular list
	       (error 'must-be-proper-or-dotted-list
		      :datum list
		      :name 'butlast)))))

;;; There is probably no point in making a special version of 
;;; butlast for n = 1, because the time is going to be dominated
;;; by consing up the new list anyawy. 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function nbutlast

;;; Special version for n = 1.  This version avoids that 
;;; the list be traversed several times, and since list traversal
;;; dominates the time here, this is a win.

;;; FIXME: The HyperSpec says that we must signal a type-error
;;; for circular lists, but we currently don't. 

(defun nbutlast-1 (list)
  (unless (typep list 'list)
    (error 'must-be-list
	   :datum list
	   :name 'nbutlast))
  (if (atom (cdr list))
      nil
      ;; The compiler probably isn't smart enough to eliminate
      ;; common subexpressions such as (cdr x), so we do it
      ;; manually by only doing a single cdr in each iteration,
      ;; and by storing pointers to the result in local variables.
      (let ((a list)
            (b (cdr list))
            (c (cddr list)))
        (loop for slow on list
	      until (atom c)
              do (setf a b
                       b c
                       c (cdr c))
	      until (atom c)
              do (setf a b
                       b c
                       c (cdr c))
	      do (when (eq slow c)
		   ;; we have a circular list
		   (error 'must-be-proper-or-dotted-list
			  :datum list
			  :name 'nbutlast)))
	(setf (cdr a) nil)
        list)))

;;; FIXME: The HyperSpec says that we must signal a type-error
;;; for circular lists, but we currently don't. 

(defun nbutlast (list &optional (n 1))
  (unless (typep list 'list)
    (error 'must-be-list
	   :datum list
	   :name 'nbutlast))
  (unless (typep n '(integer 0))
    (error 'must-be-nonnegative-integer
	   :datum n
	   :name 'nbutlast))
  (if (= n 1)
      (nbutlast-1 list)
      (let ((length (loop for slow on list
			  with conses = list
                          until (atom conses)
                          count t
			  do (setf conses (cdr conses))
                          until (atom conses)
                          count t
			  do (setf conses (cdr conses))
			  do (when (eq slow conses)
			       ;; we have a circular list
			       (error 'must-be-proper-or-dotted-list
				      :datum list
				      :name 'nbutlast)))))
        (if (<= length n)
            nil
            (progn (setf (cdr (nthcdr (- length (1+ n)) list)) nil)
                   list)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function subst

;;; Special version if test is eq and key is identity.

(defun subst-eq-identity (new old tree)
  (labels ((traverse (tree)
             (cond ((eq tree old) new)
                   ((atom tree) tree)
                   (t (cons (traverse (car tree)) (traverse (cdr tree)))))))
    (traverse tree)))

;;; Special version if test is eql and key is identity.

(defun subst-eql-identity (new old tree)
  (labels ((traverse (tree)
             (cond ((eql tree old) new)
                   ((atom tree) tree)
                   (t (cons (traverse (car tree)) (traverse (cdr tree)))))))
    (traverse tree)))

;;; Special version if test is eq and key is given.

(defun subst-eq-key (new old tree key)
  (labels ((traverse (tree)
             (cond ((eq (funcall key tree) old) new)
                   ((atom tree) tree)
                   (t (cons (traverse (car tree)) (traverse (cdr tree)))))))
    (traverse tree)))

;;; Special version if test is eql and key is given.

(defun subst-eql-key (new old tree key)
  (labels ((traverse (tree)
             (cond ((eql (funcall key tree) old) new)
                   ((atom tree) tree)
                   (t (cons (traverse (car tree)) (traverse (cdr tree)))))))
    (traverse tree)))

;;; Special version if test is given and key is identity.

(defun subst-test-identity (new old tree test)
  (labels ((traverse (tree)
             (cond ((funcall test old tree) new)
                   ((atom tree) tree)
                   (t (cons (traverse (car tree)) (traverse (cdr tree)))))))
    (traverse tree)))

;;; Special version if test and key both are given.

(defun subst-test-key (new old tree test key)
  (labels ((traverse (tree)
             (cond ((funcall test old (funcall key tree)) new)
                   ((atom tree) tree)
                   (t (cons (traverse (car tree)) (traverse (cdr tree)))))))
    (traverse tree)))

;;; We do not provide special versions for when the key is identity and
;;; the test-not is either eq or eql.  Here is why:
;;;
;;; Let's start with eq, and let's suppose the old item is an atom.
;;; Then either the old item is eq to the tree or not eq to the tree.
;;; If the old item is eq to the tree, then the tree too is an atom,
;;; and it is returned, because no substitutions are made, and we
;;; reached a leaf of the tree.  If the old item is not eq to the
;;; tree, then we make a substitution and return the new item.  In
;;; other words, this operation is the same as:
;;; (if (eq old-item tree) old-item new-item)
;;;
;;; Now suppose the old item is not an atom, and thus a cons.  Then
;;; the old item is either eq to the tree or not.  If the old item is
;;; not eq to the tree, then we return the new item and stop.  If the
;;; old item is eq to the tree, consider the two immediate subtrees
;;; (car and cdr) of the tree.  If none of them is eq to the the old
;;; item, then we return a cons where the car and cdr both contain the
;;; new item.  Now let's say that the car of the tree is eq to the old
;;; item.  Then it is also eq to the tree itself (beacuase we are
;;; handling the case where the old item and the tree are eq).  Then
;;; we have a circular structure, and it is undefined (the Hyperspec
;;; actually doesn't say that, but the Notes on the relevant page
;;; suggest it) what happens.  It is therefore very unlikely that
;;; someone would use this special case, so we have decided not to
;;; include it.
;;;
;;; A similar argument can be made for eql. 

;;; While it is possible that a test-not of eq or eql could be useful
;;; in combination with a key argument that is not identity, we do not
;;; think that case is very common either, so we omit it too. 

;;; Special version if test-not is given and key is identity.

(defun subst-test-not-identity (new old tree test)
  (labels ((traverse (tree)
             (cond ((not (funcall test old tree)) new)
                   ((atom tree) tree)
                   (t (cons (traverse (car tree)) (traverse (cdr tree)))))))
    (traverse tree)))

;;; Special version if test-not and key both are given

(defun subst-test-not-key (new old tree test key)
  (labels ((traverse (tree)
             (cond ((not (funcall test old (funcall key tree))) new)
                   ((atom tree) tree)
                   (t (cons (traverse (car tree)) (traverse (cdr tree)))))))
    (traverse tree)))

(defun subst (new old tree
	      &key key (test nil test-given) (test-not nil test-not-given))
  (when (and test-given test-not-given)
    (error 'both-test-and-test-not-given :name 'subst))
  (if key
      (if test-given
          (if (or (eq test #'eq) (eq test 'eq))
              (subst-eq-key new old tree key)
              (if (or (eq test #'eql) (eq test 'eql))
                  (subst-eql-key new old tree key)
                  (subst-test-key new old tree test key)))
          (if test-not-given
	      (subst-test-not-key new old tree test-not key)
              (subst-eql-key new old tree key)))
      (if test-given
          (if (or (eq test #'eq) (eq test 'eq))
              (subst-eq-identity new old tree)
              (if (or (eq test #'eql) (eq test 'eql))
                  (subst-eql-identity new old tree)
                  (subst-test-identity new old tree test)))
          (if test-not-given
	      (subst-test-not-identity new old tree test-not)
              (subst-eql-identity new old tree)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function subst-if

;;; Special version where the key function is identity.

(defun subst-if-identity (new predicate tree)
  ;; Define a local function so as to avoid passing the new and
  ;; predicate arguments to each recursive call.
  (labels ((traverse (tree)
             (cond ((funcall predicate tree) new)
                   ((atom tree) tree)
                   (t (cons (traverse (car tree)) (traverse (cdr tree)))))))
    (traverse tree)))

;;; Special version where the key function is something other than
;;; identity.

(defun subst-if-key (new predicate tree key)
  ;; Define a local function so as to avoid passing the new and
  ;; predicate arguments to each recursive call.
  (labels ((traverse (tree)
             (cond ((funcall predicate (funcall key tree)) new)
                   ((atom tree) tree)
                   (t (cons (traverse (car tree)) (traverse (cdr tree)))))))
    (traverse tree)))

(defun subst-if (new predicate tree &key key)
  (if (null key)
      (subst-if-identity new predicate tree)
      (subst-if-key new predicate tree key)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function subst-if-not

;;; Special version where the key function is identity.

(defun subst-if-not-identity (new predicate tree)
  ;; Define a local function so as to avoid passing the new and
  ;; predicate arguments to each recursive call.
  (labels ((traverse (tree)
             (cond ((not (funcall predicate tree)) new)
                   ((atom tree) tree)
                   (t (cons (traverse (car tree)) (traverse (cdr tree)))))))
    (traverse tree)))

;;; Special version where the key function is something other than
;;; identity.

(defun subst-if-not-key (new predicate tree key)
  ;; Define a local function so as to avoid passing the new and
  ;; predicate arguments to each recursive call.
  (labels ((traverse (tree)
             (cond ((not (funcall predicate (funcall key tree))) new)
                   ((atom tree) tree)
                   (t (cons (traverse (car tree)) (traverse (cdr tree)))))))
    (traverse tree)))

(defun subst-if-not (new predicate tree &key key)
  (if (null key)
      (subst-if-not-identity new predicate tree)
      (subst-if-not-key new predicate tree key)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function nsubst

;;; Special version if test is eq and key is identity.

(defun nsubst-eq-identity (new old tree)
  (labels ((traverse (tree)
             (cond ((eq (car tree) old)
                    (setf (car tree) new))
                   ((atom (car tree))
                    nil)
                   (t
                    (traverse (car tree))))
             (cond ((eq (cdr tree) old)
                    (setf (cdr tree) new))
                   ((atom (cdr tree))
                    nil)
                   (t
                    (traverse (cdr tree))))))
    (cond ((eq tree old) new)
          ((atom tree) tree)
          (t (traverse tree) tree))))

;;; Special version if test is eql and key is identity.

(defun nsubst-eql-identity (new old tree)
  (labels ((traverse (tree)
             (cond ((eql (car tree) old)
                    (setf (car tree) new))
                   ((atom (car tree))
                    nil)
                   (t
                    (traverse (car tree))))
             (cond ((eql (cdr tree) old)
                    (setf (cdr tree) new))
                   ((atom (cdr tree))
                    nil)
                   (t
                    (traverse (cdr tree))))))
    (cond ((eql tree old) new)
          ((atom tree) tree)
          (t (traverse tree) tree))))

;;; Special version if test is eq and key is given.

(defun nsubst-eq-key (new old tree key)
  (labels ((traverse (tree)
             (cond ((eq (funcall key (car tree)) old)
                    (setf (car tree) new))
                   ((atom (car tree))
                    nil)
                   (t
                    (traverse (car tree))))
             (cond ((eq (funcall key (cdr tree)) old)
                    (setf (cdr tree) new))
                   ((atom (cdr tree))
                    nil)
                   (t
                    (traverse (cdr tree))))))
    (cond ((eq (funcall key tree) old) new)
          ((atom tree) tree)
          (t (traverse tree) tree))))

;;; Special version if test is eql and key is given.

(defun nsubst-eql-key (new old tree key)
  (labels ((traverse (tree)
             (cond ((eql (funcall key (car tree)) old)
                    (setf (car tree) new))
                   ((atom (car tree))
                    nil)
                   (t
                    (traverse (car tree))))
             (cond ((eql (funcall key (cdr tree)) old)
                    (setf (cdr tree) new))
                   ((atom (cdr tree))
                    nil)
                   (t
                    (traverse (cdr tree))))))
    (cond ((eql (funcall key tree) old) new)
          ((atom tree) tree)
          (t (traverse tree) tree))))

;;; Special version if test is given and key is identity.

(defun nsubst-test-identity (new old tree test)
  (labels ((traverse (tree)
             (cond ((funcall test old (car tree))
                    (setf (car tree) new))
                   ((atom (car tree))
                    nil)
                   (t
                    (traverse (car tree))))
             (cond ((funcall test old (cdr tree))
                    (setf (cdr tree) new))
                   ((atom (cdr tree))
                    nil)
                   (t
                    (traverse (cdr tree))))))
    (cond ((funcall test old tree) new)
          ((atom tree) tree)
          (t (traverse tree) tree))))

;;; Special version if test and key both are given.

(defun nsubst-test-key (new old tree test key)
  (labels ((traverse (tree)
             (cond ((funcall test old (funcall key (car tree)))
                    (setf (car tree) new))
                   ((atom (car tree))
                    nil)
                   (t
                    (traverse (car tree))))
             (cond ((funcall test old (funcall key (cdr tree)))
                    (setf (cdr tree) new))
                   ((atom (cdr tree))
                    nil)
                   (t
                    (traverse (cdr tree))))))
    (cond ((funcall test old (funcall key tree)) new)
          ((atom tree) tree)
          (t (traverse tree) tree))))

;;; As with subst, we do not provide special versions for a :test-not
;;; of eq or eql.  See comment above for an explanation.

;;; Special version if test-not is given and key is identity.

(defun nsubst-test-not-identity (new old tree test)
  (labels ((traverse (tree)
             (cond ((not (funcall test old (car tree)))
                    (setf (car tree) new))
                   ((atom (car tree))
                    nil)
                   (t
                    (traverse (car tree))))
             (cond ((not (funcall test old (cdr tree)))
                    (setf (cdr tree) new))
                   ((atom (cdr tree))
                    nil)
                   (t
                    (traverse (cdr tree))))))
    (cond ((not (funcall test old tree)) new)
          ((atom tree) tree)
          (t (traverse tree) tree))))

;;; Special version if test-not and key both are given

(defun nsubst-test-not-key (new old tree test key)
  (labels ((traverse (tree)
             (cond ((not (funcall test old (funcall key (car tree))))
                    (setf (car tree) new))
                   ((atom (car tree))
                    nil)
                   (t
                    (traverse (car tree))))
             (cond ((not (funcall test old (funcall key (cdr tree))))
                    (setf (cdr tree) new))
                   ((atom (cdr tree))
                    nil)
                   (t
                    (traverse (cdr tree))))))
    (cond ((not (funcall test old (funcall key tree))) new)
          ((atom tree) tree)
          (t (traverse tree) tree))))

(defun nsubst (new old tree
	       &key key (test nil test-given) (test-not nil test-not-given))
  (when (and test-given test-not-given)
    (error 'both-test-and-test-not-given :name 'nsubst))
  (if key
      (if test-given
          (if (or (eq test #'eq) (eq test 'eq))
              (nsubst-eq-key new old tree key)
              (if (or (eq test #'eql) (eq test 'eql))
                  (nsubst-eql-key new old tree key)
                  (nsubst-test-key  new old tree test key)))
          (if test-not-given
	      (nsubst-test-not-key  new old tree test-not key)
              (nsubst-eql-key new old tree key)))
      (if test-given
          (if (or (eq test #'eq) (eq test 'eq))
              (nsubst-eq-identity new old tree)
              (if (or (eq test #'eql) (eq test 'eql))
                  (nsubst-eql-identity new old tree)
                  (nsubst-test-identity  new old tree test)))
          (if test-not-given
	      (nsubst-test-not-identity  new old tree test-not)
              (nsubst-eql-identity new old tree)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function nsubst-if

;;; Special version where the key function is identity.

(defun nsubst-if-identity (new predicate tree)
  ;; Define a local function so as to avoid passing the new and
  ;; predicate arguments to each recursive call.
  (labels ((traverse (tree)
             (cond ((funcall predicate (car tree))
                    (setf (car tree) new))
                   ((atom (car tree))
                    nil)
                   (t
                    (traverse (car tree))))
             (cond ((funcall predicate (cdr tree))
                    (setf (cdr tree) new))
                   ((atom (cdr tree))
                    nil)
                   (t
                    (traverse (cdr tree))))))
    (cond ((funcall predicate tree) new)
          ((atom tree) tree)
          (t (traverse tree) tree))))

;;; Special version where the key function is something other than
;;; identity.

(defun nsubst-if-key (new predicate tree key)
  ;; Define a local function so as to avoid passing the new and
  ;; predicate arguments to each recursive call.
  (labels ((traverse (tree)
             (cond ((funcall predicate (funcall key (car tree)))
                    (setf (car tree) new))
                   ((atom (car tree))
                    nil)
                   (t
                    (traverse (car tree))))
             (cond ((funcall predicate (funcall key (cdr tree)))
                    (setf (cdr tree) new))
                   ((atom (cdr tree))
                    nil)
                   (t
                    (traverse (cdr tree))))))
    (cond ((funcall predicate (funcall key tree)) new)
          ((atom tree) tree)
          (t (traverse tree) tree))))

(defun nsubst-if (new predicate tree &key key)
  (if (null key)
      (nsubst-if-identity new predicate tree)
      (nsubst-if-key new predicate tree key)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function nsubst-if-not

;;; Special version where the key function is identity.

(defun nsubst-if-not-identity (new predicate tree)
  ;; Define a local function so as to avoid passing the new and
  ;; predicate arguments to each recursive call.
  (labels ((traverse (tree)
             (cond ((not (funcall predicate (car tree)))
                    (setf (car tree) new))
                   ((atom (car tree))
                    nil)
                   (t
                    (traverse (car tree))))
             (cond ((not (funcall predicate (cdr tree)))
                    (setf (cdr tree) new))
                   ((atom (cdr tree))
                    nil)
                   (t
                    (traverse (cdr tree))))))
    (cond ((not (funcall predicate tree)) new)
          ((atom tree) tree)
          (t (traverse tree) tree))))

;;; Special version where the key function is something other than
;;; identity.

(defun nsubst-if-not-key (new predicate tree key)
  ;; Define a local function so as to avoid passing the new and
  ;; predicate arguments to each recursive call.
  (labels ((traverse (tree)
             (cond ((not (funcall predicate (funcall key (car tree))))
                    (setf (car tree) new))
                   ((atom (car tree))
                    nil)
                   (t
                    (traverse (car tree))))
             (cond ((not (funcall predicate (funcall key (cdr tree))))
                    (setf (cdr tree) new))
                   ((atom (cdr tree))
                    nil)
                   (t
                    (traverse (cdr tree))))))
    (cond ((not (funcall predicate (funcall key tree))) new)
          ((atom tree) tree)
          (t (traverse tree) tree))))

(defun nsubst-if-not (new predicate tree &key key)
  (if (null key)
      (nsubst-if-not-identity new predicate tree)
      (nsubst-if-not-key new predicate tree key)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function assoc

(defmacro with-alist-elements ((element-var alist function-name) &body body)
  ;; We can use for ... on, because it uses atom to test the end
  ;; of the list
  (let ((remaining (gensym))
	(alist-var (gensym)))
    `(loop with ,alist-var = ,alist
	   for ,remaining on ,alist-var
	   do (let ((,element-var (car ,remaining)))
		(cond ((consp ,element-var)
		       ,@body)
		      ((null ,element-var)
		       nil)
		      (t
		       (error 'must-be-list
			      :datum ,element-var
			      :name ',function-name))))
	   finally (unless (null ,remaining)
		     (error 'must-be-association-list
			    :datum ,alist-var
			    :name ',function-name))
		   (return nil))))

;;; Special version when test is eq and key is identity.

(defun assoc-eq-identity (item alist)
  (with-alist-elements (element alist assoc)
    (when (eq item (car element))
      (return element))))

;;; Special version when test is eql and key is identity.

(defun assoc-eql-identity (item alist)
  (with-alist-elements (element alist assoc)
    (when (eql item (car element))
      (return element))))
        
;;; Special version when test is eq and key is given.

(defun assoc-eq-key (item alist key)
  (with-alist-elements (element alist assoc)
    (when (eq item (funcall key (car element)))
      (return element))))
  
;;; Special version when test is eql and key is given.

(defun assoc-eql-key (item alist key)
  (with-alist-elements (element alist assoc)
    (when (eql item (funcall key (car element)))
      (return element))))

;;; Special version when test is given and key is identity.

(defun assoc-test-identity (item alist test)
  (with-alist-elements (element alist assoc)
    (when (funcall test item (car element))
      (return element))))

;;; Special version when test and key are both given.

(defun assoc-test-key (item alist test key)
  (with-alist-elements (element alist assoc)
    (when (funcall test item (funcall key (car element)))
      (return element))))
  
;;; Special version when test-not is eq and key is identity.

(defun assoc-not-eq-identity (item alist)
  (with-alist-elements (element alist assoc)
    (when (not (eq item (car element)))
      (return element))))

;;; Special version when test-not is eql and key is identity.

(defun assoc-not-eql-identity (item alist)
  (with-alist-elements (element alist assoc)
    (when (not (eql item (car element)))
      (return element))))
        
;;; Special version when test-not is eq and key is given.

(defun assoc-not-eq-key (item alist key)
  (with-alist-elements (element alist assoc)
    (when (not (eq item (funcall key (car element))))
      (return element))))
  
;;; Special version when test-not is eql and key is given.

(defun assoc-not-eql-key (item alist key)
  (with-alist-elements (element alist assoc)
    (when (not (eql item (funcall key (car element))))
      (return element))))

;;; Special version when test-not is given and key is identity.

(defun assoc-test-not-identity (item alist test)
  (with-alist-elements (element alist assoc)
    (when (not (funcall test item (car element)))
      (return element))))

;;; Special version when test-not and key are both given.

(defun assoc-test-not-key (item alist test key)
  (with-alist-elements (element alist assoc)
    (when (not (funcall test item (funcall key (car element))))
      (return element))))
  
(defun assoc (item alist
	      &key key (test nil test-given) (test-not nil test-not-given))
  (when (and test-given test-not-given)
    (error 'both-test-and-test-not-given :name 'assoc))
  (if key
      (if test-given
          (if (or (eq test #'eq) (eq test 'eq))
              (assoc-eq-key item alist key)
              (if (or (eq test #'eql) (eq test 'eql))
                  (assoc-eql-key item alist key)
                  (assoc-test-key item alist test key)))
          (if test-not-given
              (if (or (eq test-not #'eq) (eq test-not 'eq))
                  (assoc-not-eq-key item alist key)
                  (if (or (eq test-not #'eql) (eq test-not 'eql))
                      (assoc-not-eql-key item alist key)
                      (assoc-test-not-key item alist test-not key)))
              (assoc-eql-key item alist key)))
      (if test-given
          (if (or (eq test #'eq) (eq test 'eq))
              (assoc-eq-identity item alist)
              (if (or (eq test #'eql) (eq test 'eql))
                  (assoc-eql-identity item alist)
                  (assoc-test-identity item alist test)))
          (if test-not-given
              (if (or (eq test-not #'eq) (eq test-not 'eq))
                  (assoc-not-eq-identity item alist)
                  (if (or (eq test-not #'eql) (eq test-not 'eql))
                      (assoc-not-eql-identity item alist)
                      (assoc-test-not-identity item alist test-not)))
              (assoc-eql-identity item alist)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function assoc-if

;;; Special version when key is identity

(defun assoc-if-identity (predicate alist)
  (with-alist-elements (element alist assoc-if)
    (when (funcall predicate (car element))
      (return element))))

;;; Special version when key is given

(defun assoc-if-key (predicate alist key)
  (with-alist-elements (element alist assoc-if)
    (when (funcall predicate (funcall key (car element)))
      (return element))))

(defun assoc-if (predicate alist &key key)
  (if key
      (assoc-if-key predicate alist key)
      (assoc-if-identity predicate alist)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function assoc-if-not

;;; Special version when key is identity

(defun assoc-if-not-identity (predicate alist)
  (with-alist-elements (element alist assoc-if-not)
    (when (not (funcall predicate (car element)))
      (return element))))

;;; Special version when key is given

(defun assoc-if-not-key (predicate alist key)
  (with-alist-elements (element alist assoc-if-not)
    (when (not (funcall predicate (funcall key (car element))))
      (return element))))

(defun assoc-if-not (predicate alist &key key)
  (if key
      (assoc-if-not-key predicate alist key)
      (assoc-if-not-identity predicate alist)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function rassoc

;;; Special version when test is eq and key is identity.

(defun rassoc-eq-identity (item alist)
  (with-alist-elements (element alist rassoc)
    (when (eq item (cdr element))
      (return element))))

;;; Special version when test is eql and key is identity.

(defun rassoc-eql-identity (item alist)
  (with-alist-elements (element alist rassoc)
    (when (eql item (cdr element))
      (return element))))
        
;;; Special version when test is eq and key is given.

(defun rassoc-eq-key (item alist key)
  (with-alist-elements (element alist rassoc)
    (when (eq item (funcall key (cdr element)))
      (return element))))
  
;;; Special version when test is eql and key is given.

(defun rassoc-eql-key (item alist key)
  (with-alist-elements (element alist rassoc)
    (when (eql item (funcall key (cdr element)))
      (return element))))

;;; Special version when test is given and key is identity.

(defun rassoc-test-identity (item alist test)
  (with-alist-elements (element alist rassoc)
    (when (funcall test item (cdr element))
      (return element))))

;;; Special version when test and key are both given.

(defun rassoc-test-key (item alist test key)
  (with-alist-elements (element alist rassoc)
    (when (funcall test item (funcall key (cdr element)))
      (return element))))
  
;;; Special version when test-not is eq and key is identity.

(defun rassoc-not-eq-identity (item alist)
  (with-alist-elements (element alist rassoc)
    (when (not (eq item (cdr element)))
      (return element))))

;;; Special version when test-not is eql and key is identity.

(defun rassoc-not-eql-identity (item alist)
  (with-alist-elements (element alist rassoc)
    (when (not (eql item (cdr element)))
      (return element))))
        
;;; Special version when test-not is eq and key is given.

(defun rassoc-not-eq-key (item alist key)
  (with-alist-elements (element alist rassoc)
    (when (not (eq item (funcall key (cdr element))))
      (return element))))
  
;;; Special version when test-not is eql and key is given.

(defun rassoc-not-eql-key (item alist key)
  (with-alist-elements (element alist rassoc)
    (when (not (eql item (funcall key (cdr element))))
      (return element))))

;;; Special version when test-not is given and key is identity.

(defun rassoc-test-not-identity (item alist test)
  (with-alist-elements (element alist rassoc)
    (when (not (funcall test item (cdr element)))
      (return element))))

;;; Special version when test-not and key are both given.

(defun rassoc-test-not-key (item alist test key)
  (with-alist-elements (element alist rassoc)
    (when (not (funcall test item (funcall key (cdr element))))
      (return element))))
  
(defun rassoc (item alist
	       &key key (test nil test-given) (test-not nil test-not-given))
  (when (and test-given test-not-given)
    (error 'both-test-and-test-not-given :name 'rassoc))
  (if key
      (if test-given
          (if (or (eq test #'eq) (eq test 'eq))
              (rassoc-eq-key item alist key)
              (if (or (eq test #'eql) (eq test 'eql))
                  (rassoc-eql-key item alist key)
                  (rassoc-test-key item alist test key)))
          (if test-not-given
              (if (or (eq test-not #'eq) (eq test-not 'eq))
                  (rassoc-not-eq-key item alist key)
                  (if (or (eq test-not #'eql) (eq test-not 'eql))
                      (rassoc-not-eql-key item alist key)
                      (rassoc-test-not-key item alist test-not key)))
              (rassoc-eql-key item alist key)))
      (if test-given
          (if (or (eq test #'eq) (eq test 'eq))
              (rassoc-eq-identity item alist)
              (if (or (eq test #'eql) (eq test 'eql))
                  (rassoc-eql-identity item alist)
                  (rassoc-test-identity item alist test)))
          (if test-not-given
              (if (or (eq test-not #'eq) (eq test-not 'eq))
                  (rassoc-not-eq-identity item alist)
                  (if (or (eq test-not #'eql) (eq test-not 'eql))
                      (rassoc-not-eql-identity item alist)
                      (rassoc-test-not-identity item alist test-not)))
              (rassoc-eql-identity item alist)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function rassoc-if

;;; Special version when key is identity

(defun rassoc-if-identity (predicate alist)
  (with-alist-elements (element alist rassoc-if)
    (when (funcall predicate (cdr element))
      (return element))))

;;; Special version when key is given

(defun rassoc-if-key (predicate alist key)
  (with-alist-elements (element alist rassoc-if)
    (when (funcall predicate (funcall key (cdr element)))
      (return element))))

(defun rassoc-if (predicate alist &key key)
  (if key
      (rassoc-if-key predicate alist key)
      (rassoc-if-identity predicate alist)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function rassoc-if-not

;;; Special version when key is identity

(defun rassoc-if-not-identity (predicate alist)
  (with-alist-elements (element alist rassoc-if-not)
    (when (not (funcall predicate (cdr element)))
      (return element))))

;;; Special version when key is given

(defun rassoc-if-not-key (predicate alist key)
  (with-alist-elements (element alist rassoc-if-not)
    (when (not (funcall predicate (funcall key (cdr element))))
      (return element))))

(defun rassoc-if-not (predicate alist &key key)
  (if key
      (rassoc-if-not-key predicate alist key)
      (rassoc-if-not-identity predicate alist)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function sublis

;;; Special case when test is eq and key is identity

(defun sublis-eq-identity (alist tree)
  (let ((substitution-p nil))
    (labels ((traverse (tree)
	       (let ((entry (with-alist-elements (element alist sublis)
			      (when (eq tree (car element))
				(return element)))))
		 (cond ((not (null entry))
			(setf substitution-p t)
			(cdr entry))
		       ((atom tree) tree)
		       (t (cons (traverse (car tree))
				(traverse (cdr tree))))))))
      (let ((new-tree (traverse tree)))
	(if substitution-p new-tree tree)))))
  
;;; Special case when test is eql and key is identity

(defun sublis-eql-identity (alist tree)
  (let ((substitution-p nil))
    (labels ((traverse (tree)
	       (let ((entry (with-alist-elements (element alist sublis)
			      (when (eql tree (car element))
				(return element)))))
		 (cond ((not (null entry))
			(setf substitution-p t)
			(cdr entry))
		       ((atom tree) tree)
		       (t (cons (traverse (car tree))
				(traverse (cdr tree))))))))
      (let ((new-tree (traverse tree)))
	(if substitution-p new-tree tree)))))
  
;;; Special case when test is eq and key is given.

(defun sublis-eq-key (alist tree key)
  (let ((substitution-p nil))
    (labels ((traverse (tree)
	       (let ((entry (with-alist-elements (element alist sublis)
			      (when (eq (funcall key tree) (car element))
				(return element)))))
		 (cond ((not (null entry))
			(setf substitution-p t)
			(cdr entry))
		       ((atom tree) tree)
		       (t (cons (traverse (car tree))
				(traverse (cdr tree))))))))
      (let ((new-tree (traverse tree)))
	(if substitution-p new-tree tree)))))
  
;;; Special case when test is eql and key is given.

(defun sublis-eql-key (alist tree key)
  (let ((substitution-p nil))
    (labels ((traverse (tree)
	       (let ((entry (with-alist-elements (element alist sublis)
			      (when (eql (funcall key tree) (car element))
				(return element)))))
		 (cond ((not (null entry))
			(setf substitution-p t)
			(cdr entry))
		       ((atom tree) tree)
		       (t (cons (traverse (car tree))
				(traverse (cdr tree))))))))
      (let ((new-tree (traverse tree)))
	(if substitution-p new-tree tree)))))
  
;;; Special case when test is given and key is identity.

(defun sublis-test-identity (alist tree test)
  (let ((substitution-p nil))
    (labels ((traverse (tree)
	       (let ((entry (with-alist-elements (element alist sublis)
			      (when (funcall test (car element) tree)
				(return element)))))
		 (cond ((not (null entry))
			(setf substitution-p t)
			(cdr entry))
		       ((atom tree) tree)
		       (t (cons (traverse (car tree))
				(traverse (cdr tree))))))))
      (let ((new-tree (traverse tree)))
	(if substitution-p new-tree tree)))))
  
;;; Special case when test and key are both given.

(defun sublis-test-key (alist tree test key)
  (let ((substitution-p nil))
    (labels ((traverse (tree)
	       (let ((entry (with-alist-elements (element alist sublis)
			      (when (funcall test (car element) (funcall key tree))
				(return element)))))
		 (cond ((not (null entry))
			(setf substitution-p t)
			(cdr entry))
		       ((atom tree) tree)
		       (t (cons (traverse (car tree))
				(traverse (cdr tree))))))))
      (let ((new-tree (traverse tree)))
	(if substitution-p new-tree tree)))))

;;; Special case when test-not is given and key is identity.

(defun sublis-test-not-identity (alist tree test)
  (let ((substitution-p nil))
    (labels ((traverse (tree)
	       (let ((entry (with-alist-elements (element alist sublis)
			      (when (not (funcall test (car element) tree))
				(return element)))))
		 (cond ((not (null entry))
			(setf substitution-p t)
			(cdr entry))
		       ((atom tree) tree)
		       (t (cons (traverse (car tree))
				(traverse (cdr tree))))))))
      (let ((new-tree (traverse tree)))
	(if substitution-p new-tree tree)))))
  
;;; Special case when test-not and key are both given.

(defun sublis-test-not-key (alist tree test key)
  (let ((substitution-p nil))
    (labels ((traverse (tree)
	       (let ((entry (with-alist-elements (element alist sublis)
			      (when (not (funcall test (car element) (funcall key tree)))
				(return element)))))
		 (cond ((not (null entry))
			(setf substitution-p t)
			(cdr entry))
		       ((atom tree) tree)
		       (t (cons (traverse (car tree))
				(traverse (cdr tree))))))))
      (let ((new-tree (traverse tree)))
	(if substitution-p new-tree tree)))))

(defun sublis (alist tree
	       &key key (test nil test-given) (test-not nil test-not-given))
  (when (and test-given test-not-given)
    (error 'both-test-and-test-not-given :name 'sublis))
  (if key
      (if test-given
          (if (or (eq test #'eq) (eq test 'eq))
              (sublis-eq-key alist tree key)
              (if (or (eq test #'eql) (eq test 'eql))
                  (sublis-eql-key alist tree key)
                  (sublis-test-key alist tree test key)))
          (if test-not-given
	      (sublis-test-not-key alist tree test-not key)
              (sublis-eql-key alist tree key)))
      (if test-given
          (if (or (eq test #'eq) (eq test 'eq))
              (sublis-eq-identity alist tree)
              (if (or (eq test #'eql) (eq test 'eql))
                  (sublis-eql-identity alist tree)
                  (sublis-test-identity alist tree test)))
          (if test-not-given
	      (sublis-test-not-identity alist tree test-not)
              (sublis-eql-identity alist tree)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function nsublis

;;; Special case when test is eq and key is identity

(defun nsublis-eq-identity (alist tree)
  (labels ((traverse (tree)
             (let ((entry (with-alist-elements (element alist nsublis)
			    (when (eq (car tree) (car element))
			      (return element)))))
               (cond ((not (null entry)) (setf (car tree) (cdr entry)))
                     ((atom (car tree)) nil)
                     (t (traverse (car tree)))))
	     (let ((entry (with-alist-elements (element alist nsublis)
			    (when (eq (cdr tree) (car element))
			      (return element)))))
               (cond ((not (null entry)) (setf (cdr tree) (cdr entry)))
                     ((atom (cdr tree)) nil)
                     (t (traverse (cdr tree)))))))
    (let ((entry (with-alist-elements (element alist nsublis)
			    (when (eq tree (car element))
			      (return element)))))
      (cond ((not (null entry)) (cdr entry))
	    ((atom tree) tree)
	    (t (traverse tree) tree)))))

;;; Special case when test is eql and key is identity

(defun nsublis-eql-identity (alist tree)
  (labels ((traverse (tree)
             (let ((entry (with-alist-elements (element alist nsublis)
			    (when (eql (car tree) (car element))
			      (return element)))))
               (cond ((not (null entry)) (setf (car tree) (cdr entry)))
                     ((atom (car tree)) nil)
                     (t (traverse (car tree)))))
	     (let ((entry (with-alist-elements (element alist nsublis)
			    (when (eql (cdr tree) (car element))
			      (return element)))))
               (cond ((not (null entry)) (setf (cdr tree) (cdr entry)))
                     ((atom (cdr tree)) nil)
                     (t (traverse (cdr tree)))))))
    (let ((entry (with-alist-elements (element alist nsublis)
			    (when (eql tree (car element))
			      (return element)))))
      (cond ((not (null entry)) (cdr entry))
	    ((atom tree) tree)
	    (t (traverse tree) tree)))))

;;; Special case when test is eq and key is given.

(defun nsublis-eq-key (alist tree key)
  (labels ((traverse (tree)
             (let ((entry (with-alist-elements (element alist nsublis)
			    (when (eq (funcall key (car tree)) (car element))
			      (return element)))))
               (cond ((not (null entry)) (setf (car tree) (cdr entry)))
                     ((atom (car tree)) nil)
                     (t (traverse (car tree)))))
	     (let ((entry (with-alist-elements (element alist nsublis)
			    (when (eq (funcall key (cdr tree)) (car element))
			      (return element)))))
               (cond ((not (null entry)) (setf (cdr tree) (cdr entry)))
                     ((atom (cdr tree)) nil)
                     (t (traverse (cdr tree)))))))
    (let ((entry (with-alist-elements (element alist nsublis)
		   (when (eq (funcall key tree) (car element))
		     (return element)))))
      (cond ((not (null entry)) (cdr entry))
	    ((atom tree) tree)
	    (t (traverse tree) tree)))))

;;; Special case when test is eql and key is given.

(defun nsublis-eql-key (alist tree key)
  (labels ((traverse (tree)
             (let ((entry (with-alist-elements (element alist nsublis)
			    (when (eql (funcall key (car tree)) (car element))
			      (return element)))))
               (cond ((not (null entry)) (setf (car tree) (cdr entry)))
                     ((atom (car tree)) nil)
                     (t (traverse (car tree)))))
	     (let ((entry (with-alist-elements (element alist nsublis)
			    (when (eql (funcall key (cdr tree)) (car element))
			      (return element)))))
               (cond ((not (null entry)) (setf (cdr tree) (cdr entry)))
                     ((atom (cdr tree)) nil)
                     (t (traverse (cdr tree)))))))
    (let ((entry (with-alist-elements (element alist nsublis)
			    (when (eql (funcall key tree) (car element))
			      (return element)))))
      (cond ((not (null entry)) (cdr entry))
	    ((atom tree) tree)
	    (t (traverse tree) tree)))))

;;; Special case when test is given and key is identity.

(defun nsublis-test-identity (alist tree test)
  (labels ((traverse (tree)
             (let ((entry (with-alist-elements (element alist nsublis)
			    (when (funcall test (car element) (car tree))
			      (return element)))))
               (cond ((not (null entry)) (setf (car tree) (cdr entry)))
                     ((atom (car tree)) nil)
                     (t (traverse (car tree)))))
	     (let ((entry (with-alist-elements (element alist nsublis)
			    (when (funcall test (car element) (cdr tree))
			      (return element)))))
               (cond ((not (null entry)) (setf (cdr tree) (cdr entry)))
                     ((atom (cdr tree)) nil)
                     (t (traverse (cdr tree)))))))
    (let ((entry (with-alist-elements (element alist nsublis)
			    (when (funcall test (car element) tree)
			      (return element)))))
      (cond ((not (null entry)) (cdr entry))
	    ((atom tree) tree)
	    (t (traverse tree) tree)))))

;;; Special case when test and key are both given.

(defun nsublis-test-key (alist tree test key)
  (labels ((traverse (tree)
             (let ((entry (with-alist-elements (element alist nsublis)
			    (when (funcall test (car element) (funcall key (car tree)))
			      (return element)))))
               (cond ((not (null entry)) (setf (car tree) (cdr entry)))
                     ((atom (car tree)) nil)
                     (t (traverse (car tree)))))
	     (let ((entry (with-alist-elements (element alist nsublis)
			    (when (funcall test (car element) (funcall key (cdr tree)))
			      (return element)))))
               (cond ((not (null entry)) (setf (cdr tree) (cdr entry)))
                     ((atom (cdr tree)) nil)
                     (t (traverse (cdr tree)))))))
    (let ((entry (with-alist-elements (element alist nsublis)
			    (when (funcall test (car element) (funcall key tree))
			      (return element)))))
      (cond ((not (null entry)) (cdr entry))
	    ((atom tree) tree)
	    (t (traverse tree) tree)))))

(defun nsublis-test-not-identity (alist tree test)
  (labels ((traverse (tree)
             (let ((entry (with-alist-elements (element alist nsublis)
			    (when (not (funcall test (car element) (car tree)))
			      (return element)))))
               (cond ((not (null entry)) (setf (car tree) (cdr entry)))
                     ((atom (car tree)) nil)
                     (t (traverse (car tree)))))
	     (let ((entry (with-alist-elements (element alist nsublis)
			    (when (not (funcall test (car element) (cdr tree)))
			      (return element)))))
               (cond ((not (null entry)) (setf (cdr tree) (cdr entry)))
                     ((atom (cdr tree)) nil)
                     (t (traverse (cdr tree)))))))
    (let ((entry (with-alist-elements (element alist nsublis)
			    (when (not (funcall test (car element) tree))
			      (return element)))))
      (cond ((not (null entry)) (cdr entry))
	    ((atom tree) tree)
	    (t (traverse tree) tree)))))

;;; Special case when test-not and key are both given.

(defun nsublis-test-not-key (alist tree test key)
  (labels ((traverse (tree)
             (let ((entry (with-alist-elements (element alist nsublis)
			    (when (not (funcall test (car element) (funcall key (car tree))))
			      (return element)))))
               (cond ((not (null entry)) (setf (car tree) (cdr entry)))
                     ((atom (car tree)) nil)
                     (t (traverse (car tree)))))
	     (let ((entry (with-alist-elements (element alist nsublis)
			    (when (not (funcall test (car element) (funcall key (cdr tree))))
			      (return element)))))
               (cond ((not (null entry)) (setf (cdr tree) (cdr entry)))
                     ((atom (cdr tree)) nil)
                     (t (traverse (cdr tree)))))))
    (let ((entry (with-alist-elements (element alist nsublis)
			    (when (not (funcall test (car element) (funcall key tree)))
			      (return element)))))
      (cond ((not (null entry)) (cdr entry))
	    ((atom tree) tree)
	    (t (traverse tree) tree)))))

(defun nsublis (alist tree
		&key key (test nil test-given) (test-not nil test-not-given))
  (when (and test-given test-not-given)
    (error 'both-test-and-test-not-given :name 'nsublis))
  (if key
      (if test-given
          (if (or (eq test #'eq) (eq test 'eq))
              (nsublis-eq-key alist tree key)
              (if (or (eq test #'eql) (eq test 'eql))
                  (nsublis-eql-key alist tree key)
                  (nsublis-test-key alist tree test key)))
          (if test-not-given
	      (nsublis-test-not-key alist tree test-not key)
              (nsublis-eql-key alist tree key)))
      (if test-given
          (if (or (eq test #'eq) (eq test 'eq))
              (nsublis-eq-identity alist tree)
              (if (or (eq test #'eql) (eq test 'eql))
                  (nsublis-eql-identity alist tree)
                  (nsublis-test-identity alist tree test)))
          (if test-not-given
	      (nsublis-test-not-identity alist tree test-not)
              (nsublis-eql-identity alist tree)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Some helper macros

(defmacro with-proper-list-rests ((rest-var list function-name) &body body)
  (let ((list-var (gensym)))
    `(loop with ,list-var = ,list
	   for ,rest-var on ,list-var
	   do ,@body
	   finally (unless (null ,rest-var)
		     (error 'must-be-proper-list
			    :datum ,list-var
			    :name ,function-name)))))

(defmacro with-proper-list-elements ((element-var list function-name) &body body)
  (let ((list-var (gensym))
	(rest-var (gensym)))
    `(loop with ,list-var = ,list
	   for ,rest-var on ,list-var
	   for ,element-var = (car ,rest-var)
	   do ,@body
	   finally (unless (null ,rest-var)
		     (error 'must-be-proper-list
			    :datum ,list-var
			    :name ,function-name)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function member

(defun |member test=eq key=identity| (name item list)
  (with-proper-list-rests (rest list name)
    (when (eq item (car rest))
      (return rest))))

(defun |member test=eq key=other| (name item list key)
  (with-proper-list-rests (rest list name)
    (when (eq item (funcall key (car rest)))
      (return rest))))

(defun |member-test-not=eq key=identity| (name item list)
  (with-proper-list-rests (rest list name)
    (when (not (eq item (car rest)))
      (return rest))))

(defun |member-test-not=eq key=other| (name item list key)
  (with-proper-list-rests (rest list name)
    (when (not (eq item (funcall key (car rest))))
      (return rest))))

(defun |member test=eql key=identity| (name item list)
  (with-proper-list-rests (rest list name)
    (when (eql item (car rest))
      (return rest))))

(defun |member test=eql key=other| (name item list key)
  (with-proper-list-rests (rest list name)
    (when (eql item (funcall key (car rest)))
      (return rest))))

(defun |member-test-not=eql key=identity| (name item list)
  (with-proper-list-rests (rest list name)
    (when (not (eql item (car rest)))
      (return rest))))

(defun |member-test-not=eql key=other| (name item list key)
  (with-proper-list-rests (rest list name)
    (when (not (eql item (funcall key (car rest))))
      (return rest))))

(defun |member test=other key=identity| (name item list test)
  (with-proper-list-rests (rest list name)
    (when (funcall test item (car rest))
      (return rest))))

(defun |member test=other key=other| (name item list test key)
  (with-proper-list-rests (rest list name)
    (when (funcall test item (funcall key (car rest)))
      (return rest))))

(defun |member test-not=other key=identity| (name item list test)
  (with-proper-list-rests (rest list name)
    (when (not (funcall test item (car rest)))
      (return rest))))

(defun |member test-not=other key=other| (name item list test key)
  (with-proper-list-rests (rest list name)
    (when (not (funcall test item (funcall key (car rest))))
      (return rest))))

(defun member (item list
	       &key key (test nil test-given) (test-not nil test-not-given))
  (when (and test-given test-not-given)
    (error 'both-test-and-test-not-given :name 'member))
  (if key
      (if test-given
          (if (or (eq test #'eq) (eq test 'eq))
              (|member test=eq key=other| 'member item list key)
              (if (or (eq test #'eql) (eq test 'eql))
                  (|member test=eql key=other| 'member item list key)
                  (|member test=other key=other| 'member item list test key)))
          (if test-not-given
              (if (or (eq test-not #'eq) (eq test-not 'eq))
                  (|member-test-not=eq key=other| 'member item list key)
                  (if (or (eq test-not #'eql) (eq test-not 'eql))
                      (|member-test-not=eql key=other| 'member item list key)
                      (|member test-not=other key=other| 'member item list test-not key)))
              (|member test=eql key=other| 'member item list key)))
      (if test-given
          (if (or (eq test #'eq) (eq test 'eq))
              (|member test=eq key=identity| 'member item list)
              (if (or (eq test #'eql) (eq test 'eql))
                  (|member test=eql key=identity| 'member item list)
                  (|member test=other key=identity| 'member item list test)))
          (if test-not-given
              (if (or (eq test-not #'eq) (eq test-not 'eq))
                  (|member-test-not=eq key=identity| 'member item list)
                  (if (or (eq test-not #'eql) (eq test-not 'eql))
                      (|member-test-not=eql key=identity| 'member item list)
                      (|member test-not=other key=identity| 'member item list test-not)))
              (|member test=eql key=identity| 'member item list)))))

;;; Special versions of the member function with the arguments
;;; of the test reversed.

(defun |member reversed-test=other key=identity| (name item list test)
  (with-proper-list-rests (rest list name)
    (when (funcall test (car rest) item)
      (return rest))))

(defun |member reversed-test=other key=other| (name item list test key)
  (with-proper-list-rests (rest list name)
    (when (funcall test (funcall key (car rest)) item)
      (return rest))))

(defun |member reversed-test-not=other key=identity| (name item list test)
  (with-proper-list-rests (rest list name)
    (when (not (funcall test (car rest) item))
      (return rest))))

(defun |member reversed-test-not=other key=other| (name item list test key)
  (with-proper-list-rests (rest list name)
    (when (not (funcall test (funcall key (car rest)) item))
      (return rest))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function member-if

(defun |member-if key=identity| (predicate list)
  (with-proper-list-rests (rest list 'member-if)
    (when (funcall predicate (car rest))
      (return rest))))

(defun |member-if key=other| (predicate list key)
  (with-proper-list-rests (rest list 'member-if)
    (when (funcall predicate (funcall key (car rest)))
      (return rest))))

(defun |member-if| (predicate list &key key)
  (if key
      (|member-if key=other| predicate list key)
      (|member-if key=identity| predicate list)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function member-if-not

(defun |member-if-not key=identity| (predicate list)
  (with-proper-list-rests (rest list 'member-if-not)
    (unless (funcall predicate (car rest))
      (return rest))))

(defun |member-if-not key=other| (predicate list key)
  (with-proper-list-rests (rest list 'member-if-not)
    (unless (funcall predicate (funcall key (car rest)))
      (return rest))))

(defun |member-if-not| (predicate list &key key)
  (if key
      (|member-if-not key=other| predicate list key)
      (|member-if-not key=identity| predicate list)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function acons

(defun acons (key datum alist)
  (cons (cons key datum) alist))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function pairlis

;;; The hyperspec says the consequences are undefined if the two
;;; lists do not have the same length.  We check for this situation
;;; and signal an error in that case. 

(defun pairlis (keys data &optional alist)
  (loop with result = alist
	with remaining-keys = keys
	with remaining-data = data
	until (or (atom remaining-keys) (atom remaining-data))
	do (push (cons (pop remaining-keys) (pop remaining-data)) result)
	finally (unless (and (null remaining-keys) (null remaining-data))
		  (cond ((and (atom remaining-keys) (not (null remaining-keys)))
			 (error 'must-be-proper-list
				:datum keys
				:name 'pairlis))
			((and (atom remaining-data) (not (null remaining-data)))
			 (error 'must-be-proper-list
				:datum data
				:name 'pairlis))
			(t
			 (error 'lists-must-have-the-same-length
				:list1 keys
				:list2 data
				:name 'pairlis))))
		(return result)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function copy-alist

;;; The HyperSpec says that the argument is an alist, and 
;;; in those cases, whenever the type doesn't correspond, it
;;; is legitimate to signal an error.  However, for copy-alist,
;;; the HyperSpec also says that any object that is referred to
;;; directly or indirectly is still shared betwee the argument
;;; and the resulting list, which suggests that any element that
;;; is not a cons should just be included in the resulting
;;; list as well.  And that is what we are doing. 

;;; We use (loop for ... on ...) because it uses ATOM to test for
;;; the end of the list.  Then we check that the atom is really 
;;; null, and if not, signal a type error.

(defun copy-alist (alist)
  (loop for remaining on alist
	collect (if (consp (car remaining))
		    (cons (caar remaining) (cdar remaining))
		    (car remaining))
	finally (unless (null remaining)
		  (error 'must-be-proper-list
			 :datum alist
			 :name 'copy-alist))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function tailp

;;; We could use (loop for ... on ...) here for consistency. 

(defun tailp (object list)
  (loop for rest = list then (cdr rest)
	until (atom rest)
	when (eql object rest)
	  return t
	finally (return (eql object rest))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function ldiff

(defun ldiff (list object)
  (if (or (eql list object) (atom list))
      nil
      (let* ((result (list (car list)))
             (current result)
             (remaining (cdr list)))
        (loop until (or (eql remaining object) (atom remaining))
              do (setf (cdr current) (list (car remaining)))
                 (setf current (cdr current))
                 (setf remaining (cdr remaining)))
        (if (eql remaining object)
            result
            (progn (setf (cdr current) remaining)
                   result)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function union

(defun union-identity-eql (name list1 list2)
  (let ((result list2))
    (with-proper-list-elements (element1 list1 name)
      (unless (|member test=eql key=identity| name element1 list2)
	(push element1 result)))
    result))

(defun union-identity-eq (name list1 list2)
  (let ((result list2))
    (with-proper-list-elements (element1 list1 name)
      (unless (|member test=eq key=identity| name element1 list2)
	(push element1 result)))
    result))

(defun union-key-eql (name list1 list2 key)
  (let ((result list2))
    (with-proper-list-elements (element1 list1 name)
      (unless (|member test=eql key=other| name (funcall key element1) list2 key)
	(push element1 result)))
    result))

(defun union-key-eq (name list1 list2 key)
  (let ((result list2))
    (with-proper-list-elements (element1 list1 name)
      (unless (|member test=eq key=other| name (funcall key element1) list2 key)
	(push element1 result)))
    result))

(defun union-identity-test (name list1 list2 test)
  (let ((result list2))
    (with-proper-list-elements (element1 list1 name)
      (unless (|member test=other key=identity| name element1 list2 test)
	(push element1 result)))
    result))

(defun union-key-test (name list1 list2 key test)
  (let ((result list2))
    (with-proper-list-elements (element1 list1 name)
      (unless (|member test=other key=other| name (funcall key element1 ) list2 test key)
	(push element1 result)))
    result))

(defun union-identity-test-not (name list1 list2 test-not)
  (let ((result list2))
    (with-proper-list-elements (element1 list1 name)
      (unless (|member test-not=other key=identity| name element1 list2 test-not)
	(push element1 result)))
    result))

(defun union-key-test-not (name list1 list2 key test-not)
  (let ((result list2))
    (with-proper-list-elements (element1 list1 name)
      (unless (|member test-not=other key=other| name element1 list2 test-not key)
	(push element1 result)))
    result))

(defun union-identity-eq-hash (name list1 list2)
  (let ((table (make-hash-table :test #'eq)))
    (with-proper-list-elements (element list1 name)
      (setf (gethash element table) element))
    (with-proper-list-elements (element list2 name)
      (setf (gethash element table) element))
    (loop for element being the hash-values of table
	  collect element)))

(defun union-identity-eql-hash (name list1 list2)
  (let ((table (make-hash-table :test #'eql)))
    (with-proper-list-elements (element list1 name)
      (setf (gethash element table) element))
    (with-proper-list-elements (element list2 name)
      (setf (gethash element table) element))
    (loop for element being the hash-values of table
	  collect element)))

(defun union-identity-equal-hash (name list1 list2)
  (let ((table (make-hash-table :test #'equal)))
    (with-proper-list-elements (element list1 name)
      (setf (gethash element table) element))
    (with-proper-list-elements (element list2 name)
      (setf (gethash element table) element))
    (loop for element being the hash-values of table
	  collect element)))

(defun union-identity-equalp-hash (name list1 list2)
  (let ((table (make-hash-table :test #'equalp)))
    (with-proper-list-elements (element list1 name)
      (setf (gethash element table) element))
    (with-proper-list-elements (element list2 name)
      (setf (gethash element table) element))
    (loop for element being the hash-values of table
	  collect element)))

(defun union-key-eq-hash (name list1 list2 key)
  (let ((table (make-hash-table :test #'eq)))
    (with-proper-list-elements (element list1 name)
      (setf (gethash (funcall key element) table) element))
    (with-proper-list-elements (element list2 name)
      (setf (gethash (funcall key element) table) element))
    (loop for element being the hash-values of table
	  collect element)))

(defun union-key-eql-hash (name list1 list2 key)
  (let ((table (make-hash-table :test #'eql)))
    (with-proper-list-elements (element list1 name)
      (setf (gethash (funcall key element) table) element))
    (with-proper-list-elements (element list2 name)
      (setf (gethash (funcall key element) table) element))
    (loop for element being the hash-values of table
	  collect element)))

(defun union-key-equal-hash (name list1 list2 key)
  (let ((table (make-hash-table :test #'equal)))
    (with-proper-list-elements (element list1 name)
      (setf (gethash (funcall key element) table) element))
    (with-proper-list-elements (element list2 name)
      (setf (gethash (funcall key element) table) element))
    (loop for element being the hash-values of table
	  collect element)))

(defun union-key-equalp-hash (name list1 list2 key)
  (let ((table (make-hash-table :test #'equalp)))
    (with-proper-list-elements (element list1 name)
      (setf (gethash (funcall key element) table) element))
    (with-proper-list-elements (element list2 name)
      (setf (gethash (funcall key element) table) element))
    (loop for element being the hash-values of table
	  collect element)))

(defun union (list1 list2 &key key test test-not)
  (when (and (not (null test)) (not (null test-not)))
    (error 'both-test-and-test-not-given :name 'union))
  (let ((use-hash (> (* (length list1) (length list2)) 1000)))
    (if key
	(if test
	    (cond ((or (eq test #'eq) (eq test 'eq))
		   (if use-hash
		       (union-key-eq-hash 'union list1 list2 key)
		       (union-key-eq 'union list1 list2 key)))
		  ((or (eq test #'eql) (eq test 'eql))
		   (if use-hash
		       (union-key-eql-hash 'union list1 list2 key)
		       (union-key-eql 'union list1 list2 key)))
		  ((or (eq test #'equal) (eq test 'equal))
		   (if use-hash
		       (union-key-equal-hash 'union list1 list2 key)
		       (union-key-test 'union list1 list2 key #'equal)))
		  ((or (eq test #'equalp) (eq test 'equalp))
		   (if use-hash
		       (union-key-equalp-hash 'union list1 list2 key)
		       (union-key-test 'union list1 list2 key #'equalp)))
		  (t
		   (union-key-test 'union list1 list2 key test)))
	    (if test-not
		(union-key-test-not 'union list1 list2 key test-not)
		(if use-hash
		    (union-key-eql-hash 'union list1 list2 key)
		    (union-key-eql 'union list1 list2 key))))
	(if test
	    (cond ((or (eq test #'eq) (eq test 'eq))
		   (if use-hash
		       (union-identity-eq-hash 'union list1 list2)
		       (union-identity-eq 'union list1 list2)))
		  ((or (eq test #'eql) (eq test 'eql))
		   (if use-hash
		       (union-identity-eql-hash 'union list1 list2)
		       (union-identity-eql 'union list1 list2)))
		  ((or (eq test #'equal) (eq test 'equal))
		   (if use-hash
		       (union-identity-equal-hash 'union list1 list2)
		       (union-identity-test 'union list1 list2 #'equal)))
		  ((or (eq test #'equalp) (eq test 'equalp))
		   (if use-hash
		       (union-identity-equalp-hash 'union list1 list2)
		       (union-identity-test 'union list1 list2 #'equalp)))
		  (t
		   (union-identity-test 'union list1 list2 test)))
	    (if test-not
		(union-identity-test-not 'union list1 list2 test-not)
		(if use-hash
		    (union-identity-eql-hash 'union list1 list2)
		    (union-identity-eql 'union list1 list2)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function nunion

;;; We take advantage of the fact that the standard doesn't 
;;; require this function to have any side effects. 

(defun nunion (list1 list2 &key key test test-not)
  (when (and (not (null test)) (not (null test-not)))
    (error 'both-test-and-test-not-given :name 'nunion))
  (let ((use-hash (> (* (length list1) (length list2)) 1000)))
    (if key
	(if test
	    (cond ((or (eq test #'eq) (eq test 'eq))
		   (if use-hash
		       (union-key-eq-hash 'nunion list1 list2 key)
		       (union-key-eq 'nunion list1 list2 key)))
		  ((or (eq test #'eql) (eq test 'eql))
		   (if use-hash
		       (union-key-eql-hash 'nunion list1 list2 key)
		       (union-key-eql 'nunion list1 list2 key)))
		  ((or (eq test #'equal) (eq test 'equal))
		   (if use-hash
		       (union-key-equal-hash 'nunion list1 list2 key)
		       (union-key-test 'nunion list1 list2 key #'equal)))
		  ((or (eq test #'equalp) (eq test 'equalp))
		   (if use-hash
		       (union-key-equalp-hash 'nunion list1 list2 key)
		       (union-key-test 'nunion list1 list2 key #'equalp)))
		  (t
		   (union-key-test 'nunion list1 list2 key test)))
	    (if test-not
		(union-key-test-not 'nunion list1 list2 key test-not)
		(if use-hash
		    (union-key-eql-hash 'nunion list1 list2 key)
		    (union-key-eql 'nunion list1 list2 key))))
	(if test
	    (cond ((or (eq test #'eq) (eq test 'eq))
		   (if use-hash
		       (union-identity-eq-hash 'nunion list1 list2)
		       (union-identity-eq 'nunion list1 list2)))
		  ((or (eq test #'eql) (eq test 'eql))
		   (if use-hash
		       (union-identity-eql-hash 'nunion list1 list2)
		       (union-identity-eql 'nunion list1 list2)))
		  ((or (eq test #'equal) (eq test 'equal))
		   (if use-hash
		       (union-identity-equal-hash 'nunion list1 list2)
		       (union-identity-test 'nunion list1 list2 #'equal)))
		  ((or (eq test #'equalp) (eq test 'equalp))
		   (if use-hash
		       (union-identity-equalp-hash 'nunion list1 list2)
		       (union-identity-test 'nunion list1 list2 #'equalp)))
		  (t
		   (union-identity-test 'nunion list1 list2 test)))
	    (if test-not
		(union-identity-test-not 'nunion list1 list2 test-not)
		(if use-hash
		    (union-identity-eql-hash 'nunion list1 list2)
		    (union-identity-eql 'nunion list1 list2)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function intersection

(defun intersection-identity-eql (name list1 list2)
  (let ((result '()))
    (with-proper-list-elements (element1 list1 name)
      (when (|member test=eql key=identity| name element1 list2)
	(push element1 result)))
    result))

(defun intersection-identity-eq (name list1 list2)
  (let ((result '()))
    (with-proper-list-elements (element1 list1 name)
      (when (|member test=eq key=identity| name element1 list2)
	(push element1 result)))
    result))

(defun intersection-key-eql (name list1 list2 key)
  (let ((result '()))
    (with-proper-list-elements (element1 list1 name)
      (when (|member test=eql key=other| name (funcall key element1) list2 key)
	(push element1 result)))
    result))

(defun intersection-key-eq (name list1 list2 key)
  (let ((result '()))
    (with-proper-list-elements (element1 list1 name)
      (when (|member test=eq key=other| name (funcall key element1) list2 key)
	(push element1 result)))
    result))

(defun intersection-identity-test (name list1 list2 test)
  (let ((result '()))
    (with-proper-list-elements (element1 list1 name)
      (when (|member test=other key=identity| name element1 list2 test)
	(push element1 result)))
    result))

(defun intersection-key-test (name list1 list2 key test)
  (let ((result '()))
    (with-proper-list-elements (element1 list1 name)
      (when (|member test=other key=other| name (funcall key element1) list2 test key)
	(push element1 result)))
    result))

(defun intersection-identity-test-not (name list1 list2 test-not)
  (let ((result '()))
    (with-proper-list-elements (element1 list1 name)
      (when (|member test-not=other key=identity| name element1 list2 test-not)
	(push element1 result)))
    result))

(defun intersection-key-test-not (name list1 list2 key test-not)
  (let ((result '()))
    (with-proper-list-elements (element1 list1 name)
      (when (|member test-not=other key=other| name (funcall key element1) list2 test-not key)
	(push element1 result)))
    result))

(defun intersection-identity-eq-hash (name list1 list2)
  (let ((table (make-hash-table :test #'eq))
	(result '()))
    (with-proper-list-elements (element list2 name)
      (setf (gethash element table) t))
    (with-proper-list-elements (element list1 name)
      (when (gethash element table)
	(push element result)))
    result))

(defun intersection-identity-eql-hash (name list1 list2)
  (let ((table (make-hash-table :test #'eql))
	(result '()))
    (with-proper-list-elements (element list2 name)
      (setf (gethash element table) t))
    (with-proper-list-elements (element list1 name)
      (when (gethash element table)
	(push element result)))
    result))

(defun intersection-identity-equal-hash (name list1 list2)
  (let ((table (make-hash-table :test #'equal))
	(result '()))
    (with-proper-list-elements (element list2 name)
      (setf (gethash element table) t))
    (with-proper-list-elements (element list1 name)
      (when (gethash element table)
	(push element result)))
    result))

(defun intersection-identity-equalp-hash (name list1 list2)
  (let ((table (make-hash-table :test #'equalp))
	(result '()))
    (with-proper-list-elements (element list2 name)
      (setf (gethash element table) t))
    (with-proper-list-elements (element list1 name)
      (when (gethash element table)
	(push element result)))
    result))

(defun intersection-key-eq-hash (name list1 list2 key)
  (let ((table (make-hash-table :test #'eq))
	(result '()))
    (with-proper-list-elements (element list2 name)
      (setf (gethash (funcall key element) table) t))
    (with-proper-list-elements (element list1 name)
      (when (gethash (funcall key element) table)
	(push element result)))
    result))

(defun intersection-key-eql-hash (name list1 list2 key)
  (let ((table (make-hash-table :test #'eql))
	(result '()))
    (with-proper-list-elements (element list2 name)
      (setf (gethash (funcall key element) table) t))
    (with-proper-list-elements (element list1 name)
      (when (gethash (funcall key element) table)
	(push element result)))
    result))

(defun intersection-key-equal-hash (name list1 list2 key)
  (let ((table (make-hash-table :test #'equal))
	(result '()))
    (with-proper-list-elements (element list2 name)
      (setf (gethash (funcall key element) table) t))
    (with-proper-list-elements (element list1 name)
      (when (gethash (funcall key element) table)
	(push element result)))
    result))
	 
(defun intersection-key-equalp-hash (name list1 list2 key)
  (let ((table (make-hash-table :test #'equalp))
	(result '()))
    (with-proper-list-elements (element list2 name)
      (setf (gethash (funcall key element) table) t))
    (with-proper-list-elements (element list1 name)
      (when (gethash (funcall key element) table)
	(push element result)))
    result))

(defun intersection (list1 list2
		     &key key (test nil test-given) (test-not nil test-not-given))
  (when (and test-given test-not-given)
    (error 'both-test-and-test-not-given :name 'intersection))
  (let ((use-hash (> (* (length list1) (length list2)) 1000)))
    (if key
	(if test-given
	    (cond ((or (eq test #'eq) (eq test 'eq))
		   (if use-hash
		       (intersection-key-eq-hash 'intersection list1 list2 key)
		       (intersection-key-eq 'intersection list1 list2 key)))
		  ((or (eq test #'eql) (eq test 'eql))
		   (if use-hash
		       (intersection-key-eql-hash 'intersection list1 list2 key)
		       (intersection-key-eql 'intersection list1 list2 key)))
		  ((or (eq test #'equal) (eq test 'equal))
		   (if use-hash
		       (intersection-key-equal-hash 'intersection list1 list2 key)
		       (intersection-key-test 'intersection list1 list2 key #'equal)))
		  ((or (eq test #'equalp) (eq test 'equalp))
		   (if use-hash
		       (intersection-key-equalp-hash 'intersection list1 list2 key)
		       (intersection-key-test 'intersection list1 list2 key #'equalp)))
		  (t
		   (intersection-key-test 'intersection list1 list2 key test)))
	    (if test-not-given
		(intersection-key-test-not 'intersection list1 list2 key test-not)
		(if use-hash
		    (intersection-key-eql-hash 'intersection list1 list2 key)
		    (intersection-key-eql 'intersection list1 list2 key))))
	(if test-given
	    (cond ((or (eq test #'eq) (eq test 'eq))
		   (if use-hash
		       (intersection-identity-eq-hash 'intersection list1 list2)
		       (intersection-identity-eq 'intersection list1 list2)))
		  ((or (eq test #'eql) (eq test 'eql))
		   (if use-hash
		       (intersection-identity-eql-hash 'intersection list1 list2)
		       (intersection-identity-eql 'intersection list1 list2)))
		  ((or (eq test #'equal) (eq test 'equal))
		   (if use-hash
		       (intersection-identity-equal-hash 'intersection list1 list2)
		       (intersection-identity-test 'intersection list1 list2 #'equal)))
		  ((or (eq test #'equalp) (eq test 'equalp))
		   (if use-hash
		       (intersection-identity-equalp-hash 'intersection list1 list2)
		       (intersection-identity-test 'intersection list1 list2 #'equalp)))
		  (t
		   (intersection-identity-test 'intersection list1 list2 test)))
	    (if test-not-given
		(intersection-identity-test-not 'intersection list1 list2 test-not)
		(if use-hash
		    (intersection-identity-eql-hash 'intersection list1 list2)
		    (intersection-identity-eql 'intersection list1 list2)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function nintersection

;;; We take advantage of the fact that the standard doesn't 
;;; require this function to have any side effects. 

(defun nintersection (list1 list2
		      &key key (test nil test-given) (test-not nil test-not-given))
  (when (and test-given test-not-given)
    (error 'both-test-and-test-not-given :name 'nintersection))
  (let ((use-hash (> (* (length list1) (length list2)) 1000)))
    (if key
	(if test-given
	    (cond ((or (eq test #'eq) (eq test 'eq))
		   (if use-hash
		       (intersection-key-eq-hash 'nintersection list1 list2 key)
		       (intersection-key-eq 'nintersection list1 list2 key)))
		  ((or (eq test #'eql) (eq test 'eql))
		   (if use-hash
		       (intersection-key-eql-hash 'nintersection list1 list2 key)
		       (intersection-key-eql 'nintersection list1 list2 key)))
		  ((or (eq test #'equal) (eq test 'equal))
		   (if use-hash
		       (intersection-key-equal-hash 'nintersection list1 list2 key)
		       (intersection-key-test 'nintersection list1 list2 key #'equal)))
		  ((or (eq test #'equalp) (eq test 'equalp))
		   (if use-hash
		       (intersection-key-equalp-hash 'nintersection list1 list2 key)
		       (intersection-key-test 'nintersection list1 list2 key #'equalp)))
		  (t
		   (intersection-key-test 'nintersection list1 list2 key test)))
	    (if test-not-given
		(intersection-key-test-not 'nintersection list1 list2 key test-not)
		(if use-hash
		    (intersection-key-eql-hash 'nintersection list1 list2 key)
		    (intersection-key-eql 'nintersection list1 list2 key))))
	(if test-given
	    (cond ((or (eq test #'eq) (eq test 'eq))
		   (if use-hash
		       (intersection-identity-eq-hash 'nintersection list1 list2)
		       (intersection-identity-eq 'nintersection list1 list2)))
		  ((or (eq test #'eql) (eq test 'eql))
		   (if use-hash
		       (intersection-identity-eql-hash 'nintersection list1 list2)
		       (intersection-identity-eql 'nintersection list1 list2)))
		  ((or (eq test #'equal) (eq test 'equal))
		   (if use-hash
		       (intersection-identity-equal-hash 'nintersection list1 list2)
		       (intersection-identity-test 'nintersection list1 list2 #'equal)))
		  ((or (eq test #'equalp) (eq test 'equalp))
		   (if use-hash
		       (intersection-identity-equalp-hash 'nintersection list1 list2)
		       (intersection-identity-test 'nintersection list1 list2 #'equalp)))
		  (t
		   (intersection-identity-test 'nintersection list1 list2 test)))
	    (if test-not-given
		(intersection-identity-test-not 'nintersection list1 list2 test-not)
		(if use-hash
		    (intersection-identity-eql-hash 'nintersection list1 list2)
		    (intersection-identity-eql 'nintersection list1 list2)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function set-difference

(defun set-difference-identity-eql (name list1 list2)
  (let ((result '()))
    (with-proper-list-elements (element list1 name)
      (unless (|member test=eql key=identity| name element list2)
	(push element result)))
    result))

(defun set-difference-identity-eq (name list1 list2)
  (let ((result '()))
    (with-proper-list-elements (element list1 name)
      (unless (|member test=eq key=identity| name element list2)
	(push element result)))
    result))

(defun set-difference-key-eql (name list1 list2 key)
  (let ((result '()))
    (with-proper-list-elements (element list1 name)
      (unless (|member test=eql key=other| name (funcall key element) list2 key)
	(push element result)))
    result))

(defun set-difference-key-eq (name list1 list2 key)
  (let ((result '()))
    (with-proper-list-elements (element list1 name)
      (unless (|member test=eq key=other| name (funcall key element) list2 key)
	(push element result)))
    result))

(defun set-difference-identity-test (name list1 list2 test)
  (let ((result '()))
    (with-proper-list-elements (element list1 name)
      (unless (|member test=other key=identity| name element list2  test)
	(push element result)))
    result))

(defun set-difference-key-test (name list1 list2 key test)
  (let ((result '()))
    (with-proper-list-elements (element list1 name)
      (unless (|member test=other key=other| name (funcall key element) list2 test key)
	(push element result)))
    result))

(defun set-difference-identity-test-not (name list1 list2 test-not)
  (let ((result '()))
    (with-proper-list-elements (element list1 name)
      (unless (|member test-not=other key=identity| name element list2 test-not)
	(push element result)))
    result))

(defun set-difference-key-test-not (name list1 list2 key test-not)
  (let ((result '()))
    (with-proper-list-elements (element list1 name)
      (unless (|member test-not=other key=other| name (funcall key element) list2 test-not key)
	(push element result)))
    result))

(defun set-difference-identity-eq-hash (name list1 list2)
  (let ((table (make-hash-table :test #'eq))
	(result '()))
    (with-proper-list-elements (element list2 name)
      (setf (gethash element table) t))
    (with-proper-list-elements (element list1 name)
      (unless (gethash element table)
	(push element result)))
    result))

(defun set-difference-identity-eql-hash (name list1 list2)
  (let ((table (make-hash-table :test #'eql))
	(result '()))
    (with-proper-list-elements (element list2 name)
      (setf (gethash element table) t))
    (with-proper-list-elements (element list1 name)
      (unless (gethash element table)
	(push element result)))
    result))

(defun set-difference-identity-equal-hash (name list1 list2)
  (let ((table (make-hash-table :test #'equal))
	(result '()))
    (with-proper-list-elements (element list2 name)
      (setf (gethash element table) t))
    (with-proper-list-elements (element list1 name)
      (unless (gethash element table)
	(push element result)))
    result))

(defun set-difference-identity-equalp-hash (name list1 list2)
  (let ((table (make-hash-table :test #'equalp))
	(result '()))
    (with-proper-list-elements (element list2 name)
      (setf (gethash element table) t))
    (with-proper-list-elements (element list1 name)
      (unless (gethash element table)
	(push element result)))
    result))

(defun set-difference-key-eq-hash (name list1 list2 key)
  (let ((table (make-hash-table :test #'eq))
	(result '()))
    (with-proper-list-elements (element list2 name)
      (setf (gethash (funcall key element) table) t))
    (with-proper-list-elements (element list1 name)
      (unless (gethash (funcall key element) table)
	(push element result)))
    result))

(defun set-difference-key-eql-hash (name list1 list2 key)
  (let ((table (make-hash-table :test #'eql))
	(result '()))
    (with-proper-list-elements (element list2 name)
      (setf (gethash (funcall key element) table) t))
    (with-proper-list-elements (element list1 name)
      (unless (gethash (funcall key element) table)
	(push element result)))
    result))

(defun set-difference-key-equal-hash (name list1 list2 key)
  (let ((table (make-hash-table :test #'equal))
	(result '()))
    (with-proper-list-elements (element list2 name)
      (setf (gethash (funcall key element) table) t))
    (with-proper-list-elements (element list1 name)
      (unless (gethash (funcall key element) table)
	(push element result)))
    result))

(defun set-difference-key-equalp-hash (name list1 list2 key)
  (let ((table (make-hash-table :test #'equalp))
	(result '()))
    (with-proper-list-elements (element list2 name)
      (setf (gethash (funcall key element) table) t))
    (with-proper-list-elements (element list1 name)
      (unless (gethash (funcall key element) table)
	(push element result)))
    result))

(defun set-difference (list1 list2 &key key test test-not)
  (when (and (not (null test)) (not (null test-not)))
    (error 'both-test-and-test-not-given :name 'set-difference))
  (let ((use-hash (> (* (length list1) (length list2)) 1000)))
    (if key
	(if test
	    (cond ((or (eq test #'eq) (eq test 'eq))
		   (if use-hash
		       (set-difference-key-eq-hash 'set-difference list1 list2 key)
		       (set-difference-key-eq 'set-difference list1 list2 key)))
		  ((or (eq test #'eql) (eq test 'eql))
		   (if use-hash
		       (set-difference-key-eql-hash 'set-difference list1 list2 key)
		       (set-difference-key-eql 'set-difference list1 list2 key)))
		  ((or (eq test #'equal) (eq test 'equal))
		   (if use-hash
		       (set-difference-key-equal-hash 'set-difference list1 list2 key)
		       (set-difference-key-test 'set-difference list1 list2 key #'equal)))
		  ((or (eq test #'equalp) (eq test 'equalp))
		   (if use-hash
		       (set-difference-key-equalp-hash 'set-difference list1 list2 key)
		       (set-difference-key-test 'set-difference list1 list2 key #'equalp)))
		  (t
		   (set-difference-key-test 'set-difference list1 list2 key test)))
	    (if test-not
		(set-difference-key-test-not 'set-difference list1 list2 key test-not)
		(if use-hash
		    (set-difference-key-eql-hash 'set-difference list1 list2 key)
		    (set-difference-key-eql 'set-difference list1 list2 key))))
	(if test
	    (cond ((or (eq test #'eq) (eq test 'eq))
		   (if use-hash
		       (set-difference-identity-eq-hash 'set-difference list1 list2)
		       (set-difference-identity-eq 'set-difference list1 list2)))
		  ((or (eq test #'eql) (eq test 'eql))
		   (if use-hash
		       (set-difference-identity-eql-hash 'set-difference list1 list2)
		       (set-difference-identity-eql 'set-difference list1 list2)))
		  ((or (eq test #'equal) (eq test 'equal))
		   (if use-hash
		       (set-difference-identity-equal-hash 'set-difference list1 list2)
		       (set-difference-identity-test 'set-difference list1 list2 #'equal)))
		  ((or (eq test #'equalp) (eq test 'equalp))
		   (if use-hash
		       (set-difference-identity-equalp-hash 'set-difference list1 list2)
		       (set-difference-identity-test 'set-difference list1 list2 #'equalp)))
		  (t
		   (set-difference-identity-test 'set-difference list1 list2 test)))
	    (if test-not
		(set-difference-identity-test-not 'set-difference list1 list2 test-not)
		(if use-hash
		    (set-difference-identity-eql-hash 'set-difference list1 list2)
		    (set-difference-identity-eql 'set-difference list1 list2)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function nset-difference

;;; We take advantage of the fact that the standard doesn't 
;;; require this function to have any side effects. 

(defun nset-difference (list1 list2 &key key test test-not)
  (when (and (not (null test)) (not (null test-not)))
    (error 'both-test-and-test-not-given :name 'nset-difference))
  (let ((use-hash (> (* (length list1) (length list2)) 1000)))
    (if key
	(if test
	    (cond ((or (eq test #'eq) (eq test 'eq))
		   (if use-hash
		       (set-difference-key-eq-hash 'nset-difference list1 list2 key)
		       (set-difference-key-eq 'nset-difference list1 list2 key)))
		  ((or (eq test #'eql) (eq test 'eql))
		   (if use-hash
		       (set-difference-key-eql-hash 'nset-difference list1 list2 key)
		       (set-difference-key-eql 'nset-difference list1 list2 key)))
		  ((or (eq test #'equal) (eq test 'equal))
		   (if use-hash
		       (set-difference-key-equal-hash 'nset-difference list1 list2 key)
		       (set-difference-key-test 'nset-difference list1 list2 key #'equal)))
		  ((or (eq test #'equalp) (eq test 'equalp))
		   (if use-hash
		       (set-difference-key-equalp-hash 'nset-difference list1 list2 key)
		       (set-difference-key-test 'nset-difference list1 list2 key #'equalp)))
		  (t
		   (set-difference-key-test 'nset-difference list1 list2 key test)))
	    (if test-not
		(set-difference-key-test-not 'nset-difference list1 list2 key test-not)
		(if use-hash
		    (set-difference-key-eql-hash 'nset-difference list1 list2 key)
		    (set-difference-key-eql 'nset-difference list1 list2 key))))
	(if test
	    (cond ((or (eq test #'eq) (eq test 'eq))
		   (if use-hash
		       (set-difference-identity-eq-hash 'nset-difference list1 list2)
		       (set-difference-identity-eq 'nset-difference list1 list2)))
		  ((or (eq test #'eql) (eq test 'eql))
		   (if use-hash
		       (set-difference-identity-eql-hash 'nset-difference list1 list2)
		       (set-difference-identity-eql 'nset-difference list1 list2)))
		  ((or (eq test #'equal) (eq test 'equal))
		   (if use-hash
		       (set-difference-identity-equal-hash 'nset-difference list1 list2)
		       (set-difference-identity-test 'nset-difference list1 list2 #'equal)))
		  ((or (eq test #'equalp) (eq test 'equalp))
		   (if use-hash
		       (set-difference-identity-equalp-hash 'nset-difference list1 list2)
		       (set-difference-identity-test 'nset-difference list1 list2 #'equalp)))
		  (t
		   (set-difference-identity-test 'nset-difference list1 list2 test)))
	    (if test-not
		(set-difference-identity-test-not 'nset-difference list1 list2 test-not)
		(if use-hash
		    (set-difference-identity-eql-hash 'nset-difference list1 list2)
		    (set-difference-identity-eql 'nset-difference list1 list2)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function adjoin

(defun adjoin-identity-eq (name item list)
  (if (|member test=eq key=identity| name item list)
      list
      (cons item list)))

(defun adjoin-identity-eql (name item list)
  (if (|member test=eql key=identity| name item list)
      list
      (cons item list)))

(defun adjoin-identity-test (name item list test)
  (if (|member test=other key=identity| name item list test)
      list
      (cons item list)))

(defun adjoin-identity-test-not (name item list test-not)
  (if (|member test-not=other key=identity| name item list test-not)
      list
      (cons item list)))

(defun adjoin-key-eq (name item list key)
  (if (|member test=eq key=other| name (funcall key item) list key)
      list
      (cons item list)))

(defun adjoin-key-eql (name item list key)
  (if (|member test=eql key=other| name (funcall key item) list key)
      list
      (cons item list)))

(defun adjoin-key-test (name item list key test)
  (if (|member test=other key=other| name (funcall key item) list test key)
      list
      (cons item list)))

(defun adjoin-key-test-not (name item list key test-not)
  (if (|member test-not=other key=other| name (funcall key item) list test-not key)
      list
      (cons item list)))

(defun adjoin (item list 
	       &key key (test nil test-given) (test-not nil test-not-given))
  (when (and test-given test-not-given)
    (error 'both-test-and-test-not-given :name 'adjoin))
  (if key
      (if test-given
	  (if (or (eq test #'eq) (eq test 'eq))
	      (adjoin-key-eq 'adjoin item list key)
	      (if (or (eq test #'eql) (eq test 'eql))
		  (adjoin-key-eql 'adjoin item list key)
		  (adjoin-key-test 'adjoin item list key test)))
	  (if test-not-given
	      (adjoin-key-test-not 'adjoin item list key test-not)
	      (adjoin-key-eql 'adjoin item list key)))
      (if test-given
	  (if (or (eq test #'eq) (eq test 'eq))
	      (adjoin-identity-eq 'adjoin item list)
	      (if (or (eq test #'eql) (eq test 'eql))
		  (adjoin-identity-eql 'adjoin item list)
		  (adjoin-identity-test 'adjoin item list test)))
	  (if test-not-given
	      (adjoin-identity-test-not 'adjoin item list test-not)
	      (adjoin-identity-eql 'adjoin item list)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function set-exclusive-or

(defun set-exclusive-or-identity-eql (name list1 list2)
  (let ((result '()))
    (with-proper-list-elements (element list1 name)
      (unless (|member test=eql key=identity| name element list2)
	(push element result)))
    (with-proper-list-elements (element list2 name)
      (unless (|member test=eql key=identity| name element list1)
	(push element result)))
    result))

(defun set-exclusive-or-identity-eq (name list1 list2)
  (let ((result '()))
    (with-proper-list-elements (element list1 name)
      (unless (|member test=eq key=identity| name element list2)
	(push element result)))
    (with-proper-list-elements (element list2 name)
      (unless (|member test=eq key=identity| name  element list1)
	(push element result)))
    result))

(defun set-exclusive-or-key-eql (name list1 list2 key)
  (let ((result '()))
    (with-proper-list-elements (element list1 name)
      (unless (|member test=eql key=other| name (funcall key element) list2 key)
	(push element result)))
    (with-proper-list-elements (element list2 name)
      (unless (|member test=eql key=other| name (funcall key element) list1 key)
	(push element result)))
    result))

(defun set-exclusive-or-key-eq (name list1 list2 key)
  (let ((result '()))
    (with-proper-list-elements (element list1 name)
      (unless (|member test=eq key=other| name (funcall key element) list2 key)
	(push element result)))
    (with-proper-list-elements (element list2 name)
      (unless (|member test=eq key=other| name (funcall key element) list1 key)
	(push element result)))
    result))

(defun set-exclusive-or-identity-test (name list1 list2 test)
  (let ((result '()))
    (with-proper-list-elements (element1 list1 name)
      (unless (|member test=other key=identity| name element1 list2 test)
	(push element1 result)))
    ;; we need to use a member with a test with reversed order or arguments
    (with-proper-list-elements (element2 list2 name)
      (unless (|member reversed-test=other key=identity| name element2 list1 test)
	(push element2 result)))
    result))

(defun set-exclusive-or-key-test (name list1 list2 key test)
  (let ((result '()))
    (with-proper-list-elements (element1 list1 name)
      (unless (|member test=other key=other| name (funcall key element1) list2 test key)
	(push element1 result)))
    ;; we need to use a member with a test with reversed order or arguments
    (with-proper-list-elements (element2 list2 name)
      (unless (|member reversed-test=other key=other| name (funcall key element2) list1 test key)
	(push element2 result)))
    result))

(defun set-exclusive-or-identity-test-not (name list1 list2 test-not)
  (let ((result '()))
    (with-proper-list-elements (element1 list1 name)
      (unless (|member test-not=other key=identity| name element1 list2 test-not)
	(push element1 result)))
    ;; we need to use a member with a test with reversed order or arguments
    (with-proper-list-elements (element2 list2 name)
      (unless (|member reversed-test-not=other key=identity| name element2 list1 test-not)
	(push element2 result)))
    result))

(defun set-exclusive-or-key-test-not (name list1 list2 key test-not)
  (let ((result '()))
    (with-proper-list-elements (element1 list1 name)
      (unless (|member test-not=other key=other| name (funcall key element1) list2 test-not key)
	(push element1 result)))
    ;; we need to use a member with a test with reversed order or arguments
    (with-proper-list-elements (element2 list2 name)
      (unless (|member reversed-test-not=other key=other| name (funcall key element2) list1 test-not key)
	(push element2 result)))
    result))

(defun set-exclusive-or-identity-eq-hash (name list1 list2)
  (let ((table1 (make-hash-table :test #'eq))
	(table2 (make-hash-table :test #'eq))
	(result '()))
    (with-proper-list-elements (element list1 name)
      (setf (gethash element table1) t))
    (with-proper-list-elements (element list2 name)
      (setf (gethash element table2) t))
    (with-proper-list-elements (element list1 name)
      (unless (gethash element table2)
	(push element result)))
    (with-proper-list-elements (element list2 name)
      (unless (gethash element table1)
	(push element result)))
    result))

(defun set-exclusive-or-identity-eql-hash (name list1 list2)
  (let ((table1 (make-hash-table :test #'eql))
	(table2 (make-hash-table :test #'eql))
	(result '()))
    (with-proper-list-elements (element list1 name)
      (setf (gethash element table1) t))
    (with-proper-list-elements (element list2 name)
      (setf (gethash element table2) t))
    (with-proper-list-elements (element list1 name)
      (unless (gethash element table2)
	(push element result)))
    (with-proper-list-elements (element list2 name)
      (unless (gethash element table1)
	(push element result)))
    result))

(defun set-exclusive-or-identity-equal-hash (name list1 list2)
  (let ((table1 (make-hash-table :test #'equal))
	(table2 (make-hash-table :test #'equal))
	(result '()))
    (with-proper-list-elements (element list1 name)
      (setf (gethash element table1) t))
    (with-proper-list-elements (element list2 name)
      (setf (gethash element table2) t))
    (with-proper-list-elements (element list1 name)
      (unless (gethash element table2)
	(push element result)))
    (with-proper-list-elements (element list2 name)
      (unless (gethash element table1)
	(push element result)))
    result))

(defun set-exclusive-or-identity-equalp-hash (name list1 list2)
  (let ((table1 (make-hash-table :test #'equalp))
	(table2 (make-hash-table :test #'equalp))
	(result '()))
    (with-proper-list-elements (element list1 name)
      (setf (gethash element table1) t))
    (with-proper-list-elements (element list2 name)
      (setf (gethash element table2) t))
    (with-proper-list-elements (element list1 name)
      (unless (gethash element table2)
	(push element result)))
    (with-proper-list-elements (element list2 name)
      (unless (gethash element table1)
	(push element result)))
    result))

(defun set-exclusive-or-key-eq-hash (name list1 list2 key)
  (let ((table1 (make-hash-table :test #'eq))
	(table2 (make-hash-table :test #'eq))
	(result '()))
    (with-proper-list-elements (element list1 name)
      (setf (gethash (funcall key element) table1) t))
    (with-proper-list-elements (element list2 name)
      (setf (gethash (funcall key element) table2) t))
    (with-proper-list-elements (element list1 name)
      (unless (gethash (funcall key element) table2)
	(push element result)))
    (with-proper-list-elements (element list2 name)
      (unless (gethash (funcall key element) table1)
	(push element result)))
    result))

(defun set-exclusive-or-key-eql-hash (name list1 list2 key)
  (let ((table1 (make-hash-table :test #'eql))
	(table2 (make-hash-table :test #'eql))
	(result '()))
    (with-proper-list-elements (element list1 name)
      (setf (gethash (funcall key element) table1) t))
    (with-proper-list-elements (element list2 name)
      (setf (gethash (funcall key element) table2) t))
    (with-proper-list-elements (element list1 name)
      (unless (gethash (funcall key element) table2)
	(push element result)))
    (with-proper-list-elements (element list2 name)
      (unless (gethash (funcall key element) table1)
	(push element result)))
    result))

(defun set-exclusive-or-key-equal-hash (name list1 list2 key)
  (let ((table1 (make-hash-table :test #'equal))
	(table2 (make-hash-table :test #'equal))
	(result '()))
    (with-proper-list-elements (element list1 name)
      (setf (gethash (funcall key element) table1) t))
    (with-proper-list-elements (element list2 name)
      (setf (gethash (funcall key element) table2) t))
    (with-proper-list-elements (element list1 name)
      (unless (gethash (funcall key element) table2)
	(push element result)))
    (with-proper-list-elements (element list2 name)
      (unless (gethash (funcall key element) table1)
	(push element result)))
    result))

(defun set-exclusive-or-key-equalp-hash (name list1 list2 key)
  (let ((table1 (make-hash-table :test #'equalp))
	(table2 (make-hash-table :test #'equalp))
	(result '()))
    (with-proper-list-elements (element list1 name)
      (setf (gethash (funcall key element) table1) t))
    (with-proper-list-elements (element list2 name)
      (setf (gethash (funcall key element) table2) t))
    (with-proper-list-elements (element list1 name)
      (unless (gethash (funcall key element) table2)
	(push element result)))
    (with-proper-list-elements (element list2 name)
      (unless (gethash (funcall key element) table1)
	(push element result)))
    result))

(defun set-exclusive-or (list1 list2 &key key test test-not)
  (when (and (not (null test)) (not (null test-not)))
    (error 'both-test-and-test-not-given :name 'set-exclusive-or))
  (let ((use-hash (> (* (length list1) (length list2)) 1000)))
    (if key
	(if test
	    (cond ((or (eq test #'eq) (eq test 'eq))
		   (if use-hash
		       (set-exclusive-or-key-eq-hash 'set-exclusive-or list1 list2 key)
		       (set-exclusive-or-key-eq 'set-exclusive-or list1 list2 key)))
		  ((or (eq test #'eql) (eq test 'eql))
		   (if use-hash
		       (set-exclusive-or-key-eql-hash 'set-exclusive-or list1 list2 key)
		       (set-exclusive-or-key-eql 'set-exclusive-or list1 list2 key)))
		  ((or (eq test #'equal) (eq test 'equal))
		   (if use-hash
		       (set-exclusive-or-key-equal-hash 'set-exclusive-or list1 list2 key)
		       (set-exclusive-or-key-test 'set-exclusive-or list1 list2 key #'equal)))
		  ((or (eq test #'equalp) (eq test 'equalp))
		   (if use-hash
		       (set-exclusive-or-key-equalp-hash 'set-exclusive-or list1 list2 key)
		       (set-exclusive-or-key-test 'set-exclusive-or list1 list2 key #'equalp)))
		  (t
		   (set-exclusive-or-key-test 'set-exclusive-or list1 list2 key test)))
	    (if test-not
		(set-exclusive-or-key-test-not 'set-exclusive-or list1 list2 key test-not)
		(if use-hash
		    (set-exclusive-or-key-eql-hash 'set-exclusive-or list1 list2 key)
		    (set-exclusive-or-key-eql 'set-exclusive-or list1 list2 key))))
	(if test
	    (cond ((or (eq test #'eq) (eq test 'eq))
		   (if use-hash
		       (set-exclusive-or-identity-eq-hash 'set-exclusive-or list1 list2)
		       (set-exclusive-or-identity-eq 'set-exclusive-or list1 list2)))
		  ((or (eq test #'eql) (eq test 'eql))
		   (if use-hash
		       (set-exclusive-or-identity-eql-hash 'set-exclusive-or list1 list2)
		       (set-exclusive-or-identity-eql 'set-exclusive-or list1 list2)))
		  ((or (eq test #'equal) (eq test 'equal))
		   (if use-hash
		       (set-exclusive-or-identity-equal-hash 'set-exclusive-or list1 list2)
		       (set-exclusive-or-identity-test 'set-exclusive-or list1 list2 #'equal)))
		  ((or (eq test #'equalp) (eq test 'equalp))
		   (if use-hash
		       (set-exclusive-or-identity-equalp-hash 'set-exclusive-or list1 list2)
		       (set-exclusive-or-identity-test 'set-exclusive-or list1 list2 #'equalp)))
		  (t
		   (set-exclusive-or-identity-test 'set-exclusive-or list1 list2 test)))
	    (if test-not
		(set-exclusive-or-identity-test-not 'set-exclusive-or list1 list2 test-not)
		(if use-hash
		    (set-exclusive-or-identity-eql-hash 'set-exclusive-or list1 list2)
		    (set-exclusive-or-identity-eql 'set-exclusive-or list1 list2)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function nset-exclusive-or

;;; We take advantage of the fact that the standard doesn't 
;;; require this function to have any side effects. 

(defun nset-exclusive-or (list1 list2 &key key test test-not)
  (when (and (not (null test)) (not (null test-not)))
    (error 'both-test-and-test-not-given :name 'nset-exclusive-or))
  (let ((use-hash (> (* (length list1) (length list2)) 1000)))
    (if key
	(if test
	    (cond ((or (eq test #'eq) (eq test 'eq))
		   (if use-hash
		       (set-exclusive-or-key-eq-hash 'nset-exclusive-or list1 list2 key)
		       (set-exclusive-or-key-eq 'nset-exclusive-or list1 list2 key)))
		  ((or (eq test #'eql) (eq test 'eql))
		   (if use-hash
		       (set-exclusive-or-key-eql-hash 'nset-exclusive-or list1 list2 key)
		       (set-exclusive-or-key-eql 'nset-exclusive-or list1 list2 key)))
		  ((or (eq test #'equal) (eq test 'equal))
		   (if use-hash
		       (set-exclusive-or-key-equal-hash 'nset-exclusive-or list1 list2 key)
		       (set-exclusive-or-key-test 'nset-exclusive-or list1 list2 key #'equal)))
		  ((or (eq test #'equalp) (eq test 'equalp))
		   (if use-hash
		       (set-exclusive-or-key-equalp-hash 'nset-exclusive-or list1 list2 key)
		       (set-exclusive-or-key-test 'nset-exclusive-or list1 list2 key #'equalp)))
		  (t
		   (set-exclusive-or-key-test 'nset-exclusive-or list1 list2 key test)))
	    (if test-not
		(set-exclusive-or-key-test-not 'nset-exclusive-or list1 list2 key test-not)
		(if use-hash
		    (set-exclusive-or-key-eql-hash 'nset-exclusive-or list1 list2 key)
		    (set-exclusive-or-key-eql 'nset-exclusive-or list1 list2 key))))
	(if test
	    (cond ((or (eq test #'eq) (eq test 'eq))
		   (if use-hash
		       (set-exclusive-or-identity-eq-hash 'nset-exclusive-or list1 list2)
		       (set-exclusive-or-identity-eq 'nset-exclusive-or list1 list2)))
		  ((or (eq test #'eql) (eq test 'eql))
		   (if use-hash
		       (set-exclusive-or-identity-eql-hash 'nset-exclusive-or list1 list2)
		       (set-exclusive-or-identity-eql 'nset-exclusive-or list1 list2)))
		  ((or (eq test #'equal) (eq test 'equal))
		   (if use-hash
		       (set-exclusive-or-identity-equal-hash 'nset-exclusive-or list1 list2)
		       (set-exclusive-or-identity-test 'nset-exclusive-or list1 list2 #'equal)))
		  ((or (eq test #'equalp) (eq test 'equalp))
		   (if use-hash
		       (set-exclusive-or-identity-equalp-hash 'nset-exclusive-or list1 list2)
		       (set-exclusive-or-identity-test 'nset-exclusive-or list1 list2 #'equalp)))
		  (t
		   (set-exclusive-or-identity-test 'nset-exclusive-or list1 list2 test)))
	    (if test-not
		(set-exclusive-or-identity-test-not 'nset-exclusive-or list1 list2 test-not)
		(if use-hash
		    (set-exclusive-or-identity-eql-hash 'nset-exclusive-or list1 list2)
		    (set-exclusive-or-identity-eql 'nset-exclusive-or list1 list2)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function subsetp

(defun subsetp-identity-eql (name list1 list2)
  (with-proper-list-elements (element list1 name)
    (unless (|member test=eql key=identity| name element list2)
      (return-from subsetp-identity-eql nil)))
  t)

(defun subsetp-identity-eq (name list1 list2)
  (with-proper-list-elements (element list1 name)
    (unless (|member test=eq key=identity| name element list2)
      (return-from subsetp-identity-eq nil)))
  t)

(defun subsetp-key-eql (name list1 list2 key)
  (with-proper-list-elements (element list1 name)
    (unless (|member test=eql key=other| name (funcall key element) list2 key)
      (return-from subsetp-key-eql nil)))
  t)

(defun subsetp-key-eq (name list1 list2 key)
  (with-proper-list-elements (element list1 name)
    (unless (|member test=eq key=other| name (funcall key element) list2 key)
      (return-from subsetp-key-eq nil)))
  t)

(defun subsetp-identity-test (name list1 list2 test)
  (with-proper-list-elements (element list1 name)
    (unless (|member test=other key=identity| name element list2 test)
      (return-from subsetp-identity-test nil)))
  t)

(defun subsetp-key-test (name list1 list2 key test)
  (with-proper-list-elements (element list1 name)
    (unless (|member test=other key=other| name (funcall key element) list2 test key)
      (return-from subsetp-key-test nil)))
  t)

(defun subsetp-identity-test-not (name list1 list2 test-not)
  (with-proper-list-elements (element list1 name)
    (unless (|member test-not=other key=identity| name  element list2 test-not)
      (return-from subsetp-identity-test-not nil)))
  t)

(defun subsetp-key-test-not (name list1 list2 key test-not)
  (with-proper-list-elements (element list1 name)
    (unless (|member test-not=other key=other| name (funcall key element) list2 test-not key)
      (return-from subsetp-key-test-not nil)))
  t)

(defun subsetp-identity-eq-hash (name list1 list2)
  (let ((table (make-hash-table :test #'eq)))
    (with-proper-list-elements (element list2 name)
      (setf (gethash element table) t))
    (with-proper-list-elements (element list1 name)
      (unless (gethash element table)
	(return-from subsetp-identity-eq-hash nil))))
  t)

(defun subsetp-identity-eql-hash (name list1 list2)
  (let ((table (make-hash-table :test #'eql)))
    (with-proper-list-elements (element list2 name)
      (setf (gethash element table) t))
    (with-proper-list-elements (element list1 name)
      (unless (gethash element table)
	(return-from subsetp-identity-eql-hash nil))))
  t)

(defun subsetp-identity-equal-hash (name list1 list2)
  (let ((table (make-hash-table :test #'equal)))
    (with-proper-list-elements (element list2 name)
      (setf (gethash element table) t))
    (with-proper-list-elements (element list1 name)
      (unless (gethash element table)
	(return-from subsetp-identity-equal-hash nil))))
  t)

(defun subsetp-identity-equalp-hash (name list1 list2)
  (let ((table (make-hash-table :test #'equalp)))
    (with-proper-list-elements (element list2 name)
      (setf (gethash element table) t))
    (with-proper-list-elements (element list1 name)
      (unless (gethash element table)
	(return-from subsetp-identity-equalp-hash nil))))
  t)

(defun subsetp-key-eq-hash (name list1 list2 key)
  (let ((table (make-hash-table :test #'eq)))
    (with-proper-list-elements (element list2 name)
      (setf (gethash (funcall key element) table) t))
    (with-proper-list-elements (element list1 name)
      (unless (gethash (funcall key element) table)
	(return-from subsetp-key-eq-hash nil))))
  t)

(defun subsetp-key-eql-hash (name list1 list2 key)
  (let ((table (make-hash-table :test #'eql)))
    (with-proper-list-elements (element list2 name)
      (setf (gethash (funcall key element) table) t))
    (with-proper-list-elements (element list1 name)
      (unless (gethash (funcall key element) table)
	(return-from subsetp-key-eql-hash nil))))
  t)

(defun subsetp-key-equal-hash (name list1 list2 key)
  (let ((table (make-hash-table :test #'equal)))
    (with-proper-list-elements (element list2 name)
      (setf (gethash (funcall key element) table) t))
    (with-proper-list-elements (element list1 name)
      (unless (gethash (funcall key element) table)
	(return-from subsetp-key-equal-hash nil))))
  t)

(defun subsetp-key-equalp-hash (name list1 list2 key)
  (let ((table (make-hash-table :test #'equalp)))
    (with-proper-list-elements (element list2 name)
      (setf (gethash (funcall key element) table) t))
    (with-proper-list-elements (element list1 name)
      (unless (gethash (funcall key element) table)
	(return-from subsetp-key-equalp-hash nil))))
  t)

(defun subsetp (list1 list2 &key key test test-not)
  (when (and (not (null test)) (not (null test-not)))
    (error 'both-test-and-test-not-given :name 'subsetp))
  (let ((use-hash (> (* (length list1) (length list2)) 1000)))
    (if key
	(if test
	    (cond ((or (eq test #'eq) (eq test 'eq))
		   (if use-hash
		       (subsetp-key-eq-hash 'subsetp list1 list2 key)
		       (subsetp-key-eq 'subsetp list1 list2 key)))
		  ((or (eq test #'eql) (eq test 'eql))
		   (if use-hash
		       (subsetp-key-eql-hash 'subsetp list1 list2 key)
		       (subsetp-key-eql 'subsetp list1 list2 key)))
		  ((or (eq test #'equal) (eq test 'equal))
		   (if use-hash
		       (subsetp-key-equal-hash 'subsetp list1 list2 key)
		       (subsetp-key-test 'subsetp list1 list2 key #'equal)))
		  ((or (eq test #'equalp) (eq test 'equalp))
		   (if use-hash
		       (subsetp-key-equalp-hash 'subsetp list1 list2 key)
		       (subsetp-key-test 'subsetp list1 list2 key #'equalp)))
		  (t
		   (subsetp-key-test 'subsetp list1 list2 key test)))
	    (if test-not
		(subsetp-key-test-not 'subsetp list1 list2 key test-not)
		(if use-hash
		    (subsetp-key-eql-hash 'subsetp list1 list2 key)
		    (subsetp-key-eql 'subsetp list1 list2 key))))
	(if test
	    (cond ((or (eq test #'eq) (eq test 'eq))
		   (if use-hash
		       (subsetp-identity-eq-hash 'subsetp list1 list2)
		       (subsetp-identity-eq 'subsetp list1 list2)))
		  ((or (eq test #'eql) (eq test 'eql))
		   (if use-hash
		       (subsetp-identity-eql-hash 'subsetp list1 list2)
		       (subsetp-identity-eql 'subsetp list1 list2)))
		  ((or (eq test #'equal) (eq test 'equal))
		   (if use-hash
		       (subsetp-identity-equal-hash 'subsetp list1 list2)
		       (subsetp-identity-test 'subsetp list1 list2 #'equal)))
		  ((or (eq test #'equalp) (eq test 'equalp))
		   (if use-hash
		       (subsetp-identity-equalp-hash 'subsetp list1 list2)
		       (subsetp-identity-test 'subsetp list1 list2 #'equalp)))
		  (t
		   (subsetp-identity-test 'subsetp list1 list2 test)))
	    (if test-not
		(subsetp-identity-test-not 'subsetp list1 list2 test-not)
		(if use-hash
		    (subsetp-identity-eql-hash 'subsetp list1 list2)
		    (subsetp-identity-eql 'subsetp list1 list2)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function getf

(defun getf (plist indicator &optional default)
  (unless (typep plist 'list)
    (error 'must-be-property-list
	   :datum plist
	   :name 'getf))
  (loop for rest on plist by #'cddr
	do (unless (consp (cdr rest))
	     (error 'must-be-property-list
		    :datum plist
		    'getf))
	when (eq (car rest) indicator)
	  return (cadr rest)
	finally (unless (null rest)
		  (error 'must-be-property-list
		    :datum plist
		    'getf))
		(return default)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Setf expander for getf

(define-setf-expander getf (place indicator &optional default &environment env)
  (let ((indicator-var (gensym))
	(default-var (gensym))
	(value-var (gensym)))
    (multiple-value-bind (vars vals store-vars writer-form reader-form)
	(get-setf-expansion place env)
      (values (append vars (list indicator-var default-var))
	      (append vals (list indicator default))
	      (list value-var)
	      `(progn (loop for rest on ,reader-form by #'cddr
			    when (eq (car rest) ,indicator-var)
			      do (setf (cadr rest) ,value-var)
				 (return nil)
			    finally (let ((,(car store-vars)
					   (list* ,indicator-var ,value-var ,reader-form)))
				      ,writer-form))
		      ,value-var)
	      `(getf ,reader-form ,indicator-var ,default)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function get-properties

(defun get-properties (plist indicator-list)
  (unless (typep plist 'list)
    (error 'must-be-property-list
	   :datum plist
	   :name 'getf))
  (loop for rest on plist by #'cddr
	do (unless (consp (cdr rest))
	     (error 'must-be-property-list
		    :datum plist
		    :name 'getf))
	   (let ((temp (member (car rest) indicator-list :test #'eq)))
	     (unless (null temp)
	       (return-from get-properties (values (car temp) (cadr rest) rest))))
	finally (unless (null rest)
		  (error 'must-be-property-list
			 :datum plist
			 :name 'getf))
		(return (values nil nil nil))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Macro remf

;;; FIXME, I guess macros such as remf should check that 
;;; store-vars has a length of 1.

(defmacro remf (place indicator &environment env)
  (let ((indicator-var (gensym)))
    (multiple-value-bind (vars vals store-vars writer-form reader-form)
	(get-setf-expansion place env)
      `(let (,@(mapcar #'list vars vals)
	     (,indicator-var ,indicator))
	 (let ((,(car store-vars) ,reader-form))
	   (if (null ,(car store-vars))
	       nil
	       (if (eq (car ,(car store-vars)) ,indicator-var)
		   (progn (setf ,(car store-vars) (cddr ,(car store-vars)))
			  ,writer-form
			  t)
		   (loop for rest on (cdr ,(car store-vars)) by #'cddr
			 when (null (cdr rest))
			   return nil
			 do (unless (consp (cddr rest))
			      (error 'must-be-property-list
				     :datum plist
				     :name 'getf))
			 when (eq (cadr rest) ,indicator-var)
			   do (setf (cdr rest) (cdddr rest))
			      (return t)
			 finally (unless (null rest)
				   (error 'must-be-property-list
					  :datum plist
					  :name 'getf))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Macro pushnew

;;; The function make-bindings is duplicated from the sequences module. 
;;; It would probably be better to avoid such duplication and to factor 
;;; it out, but then I would introduce some dependencies, so keep it like
;;; this for now.  Also see the sequences module for an explanation on
;;; how it works. 

;;; Translate from a keyword to a variable name
(defparameter *vars* '((:start . start)
                       (:end . end)
                       (:from-end . from-end)
                       (:key . key)
                       (:test . test)
                       (:test-not . test-not)
                       (:count . count)))

;;; For a list with alternating keywords, and expressions, 
;;; generate a list of binding for let.  For instance,
;;; if we have (:key <x> :end <y> :start <z>), we generate
;;; ((key <x>) (end <y>) (start <z>)).  If a keyword occurs 
;;; more than once in the list, generate a binding with a 
;;; generated symbol instead. 
(defun make-bindings (plist)
  (loop with keywords = '()
        for (key value) on plist by #'cddr
        collect (list (if (member key keywords)
                          (gensym)
                          (or (cdr (assoc key *vars*)) (gensym)))
                      value)
        do (push key keywords)))

(defmacro pushnew (item place
		   &environment env
		   &rest args
		   &key
		   (key nil key-p)
		   (test nil test-p)
		   (test-not nil test-not-p))
  (when (and test-p test-not-p)
    (error 'both-test-and-test-not-given :name 'pushnew))
  (let ((item-var (gensym)))
    (multiple-value-bind (vars vals store-vars writer-form reader-form)
	(get-setf-expansion place env)
      `(let ((,item-var ,item)
	     ,@(mapcar #'list vars vals)
	     ,@(make-bindings args))
	 ,@(if key-p `((declare (ignorable key))) `())
	 (let ((,(car store-vars) ,reader-form))
	   ,(if key
		(if test-p
		    `(unless (|member test=other key=other|
			      'pushnew
			      (funcall key ,item-var)
			      ,(car store-vars)
			      test
			      key)
		       (push ,item-var ,(car store-vars)))
		    (if test-not-p
			`(unless (|member test-not=other key=other|
				  'pushnew
				  (funcall key ,item-var)
				  ,(car store-vars)
				  test-not
				  key)
			   (push ,item-var ,(car store-vars)))
			`(unless (|member test=eql key=other|
				  'pushnew
				  (funcall key ,item-var)
				  ,(car store-vars)
				  key)
			   (push ,item-var ,(car store-vars)))))
		(if test-p
		    `(unless (|member test=other key=identity|
			      'pushnew
			      ,item-var
			      ,(car store-vars)
			      test)
		       (push ,item-var ,(car store-vars)))
		    (if test-not-p
			`(unless (|member test-not=other key=identity|
				  'pushnew
				  ,item-var
				  ,(car store-vars)
				  test-not)
			   (push ,item-var ,(car store-vars)))
			`(unless (|member test=eql key=identity|
				  'pushnew
				  ,item-var
				  ,(car store-vars))
			   (push ,item-var ,(car store-vars))))))
	   ,writer-form)))))

