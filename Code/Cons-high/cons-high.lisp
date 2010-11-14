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

;;; This condition is used by functions and macros that require
;;; some argument to be a propterty list.
(define-condition must-be-property-list (type-error name-mixin)
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
							:name 'map)))
			      (return ,(car listvars)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function maplist

(defun maplist (function &rest lists)
  (when (null lists)
    (error 'at-least-one-list-required :name 'maplist))
  (loop for remaining = lists
	  then (loop for list in remaining collect (cdr list))
	until (loop for list in remaining thereis (null list))
	collect (apply function remaining)))

;;; The compiler macro for maplist generates code to loop
;;; individually over each list given, and thus avoids having
;;; a list of lists (which implies consing).  Since the number
;;; of lists given is known, we can use funcall instead of 
;;; apply when we call the function. 
(define-compiler-macro maplist (&whole form function &rest lists)
  (if (null lists)
      form
      (let ((funvar (gensym))
	    (vars (loop for var in lists collect (gensym))))
        `(loop with ,funvar = ,function
              ,@(loop for var in vars
                   for list in lists
                   append `(for ,var = ,list then (cdr ,var)))
              ,@(loop for var in vars
                   for list in lists
                   append `(until (endp ,var)))
            collect (funcall ,funvar ,@vars)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function mapl

(defun mapl (function &rest lists)
  (when (null lists)
    (error 'at-least-one-list-required :name 'mapl))
  (loop for remaining = lists
	  then (loop for list in remaining collect (cdr list))
	until (loop for list in remaining thereis (null list))
	do (apply function remaining))
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
	    (firstlist (gensym))
	    (vars (loop for var in lists collect (gensym))))
	`(loop with ,funvar = ,function
            with ,firstlist = ,(car lists)
            ,@ (loop for var in vars
                  for list in (cons firstlist (cdr lists))
                  append `(for ,var = ,list
                               then (cdr ,var)))
            ,@ (loop for var in vars
                  for list in (cons firstlist (cdr lists))
                  append `(until (endp ,var)))
            do (funcall ,funvar ,@vars)
            finally (return ,firstlist)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function mapcan

(defun mapcan (function &rest lists)
  (when (null lists)
    (error 'at-least-one-list-required :name 'mapcan))
  (loop for remaining = lists
	  then (loop for list in remaining collect (cdr list))
	until (loop for list in remaining thereis (null list))
	nconc (apply function
		     (loop for list in remaining collect (car list)))))

;;; The compiler macro for mapcan generates code to loop
;;; individually over each list given, and thus avoids having
;;; a list of lists (which implies consing).  Since the number
;;; of lists given is known, we can use funcall instead of 
;;; apply when we call the function. 
(define-compiler-macro mapcan (&whole form function &rest lists)
  (if (null lists)
      form
      (let ((funvar (gensym))
	    (vars (loop for var in lists collect (gensym))))
	`(loop with ,funvar = ,function
	       ,@(apply #'append (loop for var in vars
				       for list in lists
				       collect `(for ,var in ,list)))
	       nconc (funcall ,funvar ,@vars)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function mapcon

(defun mapcon (function &rest lists)
  (when (null lists)
    (error 'at-least-one-list-required :name 'mapcon))
  (loop for remaining = lists
	  then (loop for list in remaining collect (cdr list))
	until (loop for list in remaining thereis (null list))
	nconc (apply function remaining)))

;;; The compiler macro for mapcon generates code to loop
;;; individually over each list given, and thus avoids having
;;; a list of lists (which implies consing).  Since the number
;;; of lists given is known, we can use funcall instead of 
;;; apply when we call the function. 
(define-compiler-macro mapcon (&whole form function &rest lists)
  (if (null lists)
      form
      (let ((funvar (gensym))
	    (vars (loop for var in lists collect (gensym))))
	`(loop with ,funvar = ,function
	       ,@(loop for var in vars
		       for list in lists
		       append `(for ,var = ,list then (cdr ,var)))
	       ,@(loop for var in vars
		       for list in lists
		       append `(until (endp ,var)))
	       nconc (funcall ,funvar ,@vars)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function revappend

(defun revappend (list tail)
  (loop with result = tail
        for element in list
        do (push element result)
        finally (return result)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function nreconc

(defun nreconc (list tail)
  (loop with remaining = list
        with result = tail
        until (null remaining)
        do (let ((temp (cdr remaining)))
             (setf (cdr remaining) result
                   result remaining
                   remaining temp))
        finally (return result)))

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

;;; FIXME: The HyperSpec says that we must signal a type-error
;;; for circular lists, but we currently don't. 

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
    (loop until (atom remaining)
          do (setf remaining (cdr remaining))
          collect (prog1 (car list) (setf list (cdr list))))))

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
        (loop until (atom c)
              do (setf a b
                       b c
                       c (cdr c)))
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
      (let ((length (loop for conses = list then (cdr conses)
                          until (atom conses)
                          count t)))
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

(defun subst (new old tree &key key test test-not)
  (when (and (not (null test)) (not (null test-not)))
    (error 'both-test-and-test-not-given :name 'subst))
  (if key
      (if test
          (if (or (eq test #'eq) (eq test 'eq))
              (subst-eq-key new old tree key)
              (if (or (eq test #'eql) (eq test 'eql))
                  (subst-eql-key new old tree key)
                  (subst-test-key new old tree test key)))
          (if test-not
	      (subst-test-not-key new old tree test-not key)
              (subst-eql-key new old tree key)))
      (if test
          (if (or (eq test #'eq) (eq test 'eq))
              (subst-eq-identity new old tree)
              (if (or (eq test #'eql) (eq test 'eql))
                  (subst-eql-identity new old tree)
                  (subst-test-identity new old tree test)))
          (if test-not
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

(defun nsubst (new old tree &key key test test-not)
  (when (and (not (null test)) (not (null test-not)))
    (error 'both-test-and-test-not-given :name 'nsubst))
  (if key
      (if test
          (if (or (eq test #'eq) (eq test 'eq))
              (nsubst-eq-key new old tree key)
              (if (or (eq test #'eql) (eq test 'eql))
                  (nsubst-eql-key new old tree key)
                  (nsubst-test-key  new old tree test key)))
          (if test-not
	      (nsubst-test-not-key  new old tree test-not key)
              (nsubst-eql-key new old tree key)))
      (if test
          (if (or (eq test #'eq) (eq test 'eq))
              (nsubst-eq-identity new old tree)
              (if (or (eq test #'eql) (eq test 'eql))
                  (nsubst-eql-identity new old tree)
                  (nsubst-test-identity  new old tree test)))
          (if test-not
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

;;; Special version when test is eq and key is identity.

(defun assoc-eq-identity (item alist)
  (loop for element in alist
        when (and (not (null element))
		  (eq item (car element)))
          return element))

;;; Special version when test is eql and key is identity.

(defun assoc-eql-identity (item alist)
  (loop for element in alist
        when (and (not (null element))
		  (eql item (car element)))
          return element))
        
;;; Special version when test is eq and key is given.

(defun assoc-eq-key (item alist key)
  (loop for element in alist
        when (and (not (null element))
		  (eq item (funcall key (car element))))
          return element))
  
;;; Special version when test is eql and key is given.

(defun assoc-eql-key (item alist key)
  (loop for element in alist
        when (and (not (null element))
		  (eql item (funcall key (car element))))
          return element))

;;; Special version when test is given and key is identity.

(defun assoc-test-identity (item alist test)
  (loop for element in alist
        when (and (not (null element))
		  (funcall test item (car element)))
          return element))

;;; Special version when test and key are both given.

(defun assoc-test-key (item alist test key)
  (loop for element in alist
        when (and (not (null element))
		  (funcall test item (funcall key (car element))))
          return element))
  
;;; Special version when test-not is eq and key is identity.

(defun assoc-not-eq-identity (item alist)
  (loop for element in alist
        when (and (not (null element))
		  (not (eq item (car element))))
          return element))

;;; Special version when test-not is eql and key is identity.

(defun assoc-not-eql-identity (item alist)
  (loop for element in alist
        when (and (not (null element))
		  (not (eql item (car element))))
          return element))
        
;;; Special version when test-not is eq and key is given.

(defun assoc-not-eq-key (item alist key)
  (loop for element in alist
        when (and (not (null element))
		  (not (eq item (funcall key (car element)))))
          return element))
  
;;; Special version when test-not is eql and key is given.

(defun assoc-not-eql-key (item alist key)
  (loop for element in alist
        when (and (not (null element))
		  (not (eql item (funcall key (car element)))))
          return element))

;;; Special version when test-not is given and key is identity.

(defun assoc-test-not-identity (item alist test)
  (loop for element in alist
        when (and (not (null element))
		  (not (funcall test item (car element))))
          return element))

;;; Special version when test-not and key are both given.

(defun assoc-test-not-key (item alist test key)
  (loop for element in alist
        when (and (not (null element))
		  (not (funcall test item (funcall key (car element)))))
          return element))
  
(defun assoc (item alist &key key test test-not)
  (when (and (not (null test)) (not (null test-not)))
    (error 'both-test-and-test-not-given :name 'assoc))
  (if key
      (if test
          (if (or (eq test #'eq) (eq test 'eq))
              (assoc-eq-key item alist key)
              (if (or (eq test #'eql) (eq test 'eql))
                  (assoc-eql-key item alist key)
                  (assoc-test-key item alist test key)))
          (if test-not
              (if (or (eq test-not #'eq) (eq test-not 'eq))
                  (assoc-not-eq-key item alist key)
                  (if (or (eq test-not #'eql) (eq test-not 'eql))
                      (assoc-not-eql-key item alist key)
                      (assoc-test-not-key item alist test-not key)))
              (assoc-eql-key item alist key)))
      (if test
          (if (or (eq test #'eq) (eq test 'eq))
              (assoc-eq-identity item alist)
              (if (or (eq test #'eql) (eq test 'eql))
                  (assoc-eql-identity item alist)
                  (assoc-test-identity item alist test)))
          (if test-not
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
  (loop for element in alist
        when (and (not (null element))
		  (funcall predicate (car element)))
          return element))

;;; Special version when key is given

(defun assoc-if-key (predicate alist key)
  (loop for element in alist
        when (and (not (null element))
		  (funcall predicate (funcall key (car element))))
          return element))

(defun assoc-if (predicate alist &key key)
  (if key
      (assoc-if-key predicate alist key)
      (assoc-if-identity predicate alist)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function assoc-if-not

;;; Special version when key is identity

(defun assoc-if-not-identity (predicate alist)
  (loop for element in alist
        when (and (not (null element))
		  (not (funcall predicate (car element))))
          return element))

;;; Special version when key is given

(defun assoc-if-not-key (predicate alist key)
  (loop for element in alist
        when (and (not (null element))
		  (not (funcall predicate (funcall key (car element)))))
          return element))

(defun assoc-if-not (predicate alist &key key)
  (if key
      (assoc-if-not-key predicate alist key)
      (assoc-if-not-identity predicate alist)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function rassoc

;;; Special version when test is eq and key is identity.

(defun rassoc-eq-identity (item alist)
  (loop for element in alist
        when (and (not (null element))
		  (eq item (cdr element)))
          return element))

;;; Special version when test is eql and key is identity.

(defun rassoc-eql-identity (item alist)
  (loop for element in alist
        when (and (not (null element))
		  (eql item (cdr element)))
          return element))
        
;;; Special version when test is eq and key is given.

(defun rassoc-eq-key (item alist key)
  (loop for element in alist
        when (and (not (null element))
		  (eq item (funcall key (cdr element))))
          return element))
  
;;; Special version when test is eql and key is given.

(defun rassoc-eql-key (item alist key)
  (loop for element in alist
        when (and (not (null element))
		  (eql item (funcall key (cdr element))))
          return element))

;;; Special version when test is given and key is identity.

(defun rassoc-test-identity (item alist test)
  (loop for element in alist
        when (and (not (null element))
		  (funcall test item (cdr element)))
          return element))

;;; Special version when test and key are both given.

(defun rassoc-test-key (item alist test key)
  (loop for element in alist
        when (and (not (null element))
		  (funcall test item (funcall key (cdr element))))
          return element))
  
;;; Special version when test-not is eq and key is identity.

(defun rassoc-not-eq-identity (item alist)
  (loop for element in alist
        when (and (not (null element))
		  (not (eq item (cdr element))))
          return element))

;;; Special version when test-not is eql and key is identity.

(defun rassoc-not-eql-identity (item alist)
  (loop for element in alist
        when (and (not (null element))
		  (not (eql item (cdr element))))
          return element))
        
;;; Special version when test-not is eq and key is given.

(defun rassoc-not-eq-key (item alist key)
  (loop for element in alist
        when (and (not (null element))
		  (not (eq item (funcall key (cdr element)))))
          return element))
  
;;; Special version when test-not is eql and key is given.

(defun rassoc-not-eql-key (item alist key)
  (loop for element in alist
        when (and (not (null element))
		  (not (eql item (funcall key (cdr element)))))
          return element))

;;; Special version when test-not is given and key is identity.

(defun rassoc-test-not-identity (item alist test)
  (loop for element in alist
        when (and (not (null element))
		  (not (funcall test item (cdr element))))
          return element))

;;; Special version when test-not and key are both given.

(defun rassoc-test-not-key (item alist test key)
  (loop for element in alist
        when (and (not (null element))
		  (not (funcall test item (funcall key (cdr element)))))
          return element))
  
(defun rassoc (item alist &key key test test-not)
  (when (and (not (null test)) (not (null test-not)))
    (error 'both-test-and-test-not-given :name 'rassoc))
  (if key
      (if test
          (if (or (eq test #'eq) (eq test 'eq))
              (rassoc-eq-key item alist key)
              (if (or (eq test #'eql) (eq test 'eql))
                  (rassoc-eql-key item alist key)
                  (rassoc-test-key item alist test key)))
          (if test-not
              (if (or (eq test-not #'eq) (eq test-not 'eq))
                  (rassoc-not-eq-key item alist key)
                  (if (or (eq test-not #'eql) (eq test-not 'eql))
                      (rassoc-not-eql-key item alist key)
                      (rassoc-test-not-key item alist test-not key)))
              (rassoc-eql-key item alist key)))
      (if test
          (if (or (eq test #'eq) (eq test 'eq))
              (rassoc-eq-identity item alist)
              (if (or (eq test #'eql) (eq test 'eql))
                  (rassoc-eql-identity item alist)
                  (rassoc-test-identity item alist test)))
          (if test-not
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
  (loop for element in alist
        when (and (not (null element))
		  (funcall predicate (cdr element)))
          return element))

;;; Special version when key is given

(defun rassoc-if-key (predicate alist key)
  (loop for element in alist
        when (and (not (null element))
		  (funcall predicate (funcall key (cdr element))))
          return element))

(defun rassoc-if (predicate alist &key key)
  (if key
      (rassoc-if-key predicate alist key)
      (rassoc-if-identity predicate alist)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function rassoc-if-not

;;; Special version when key is identity

(defun rassoc-if-not-identity (predicate alist)
  (loop for element in alist
        when (and (not (null element))
		  (not (funcall predicate (cdr element))))
          return element))

;;; Special version when key is given

(defun rassoc-if-not-key (predicate alist key)
  (loop for element in alist
        when (and (not (null element))
		  (not (funcall predicate (funcall key (cdr element)))))
          return element))

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
	       (let ((entry (assoc-eq-identity tree alist)))
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
	       (let ((entry (assoc-eql-identity tree alist)))
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
	       (let ((entry (assoc-eq-identity (funcall key tree) alist)))
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
	       (let ((entry (assoc-eql-identity (funcall key tree) alist)))
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
	       (let ((entry (assoc-test-identity tree alist (lambda (x y) (funcall test y x)))))
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
	       (let ((entry (assoc-test-identity (funcall key tree)
						 alist
						 (lambda (x y) (funcall test y x)))))
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
	       (let ((entry (assoc-test-not-identity tree alist (lambda (x y) (funcall test y x)))))
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
	       (let ((entry (assoc-test-not-identity (funcall key tree)
						     alist
						     (lambda (x y) (funcall test y x)))))
		 (cond ((not (null entry))
			(setf substitution-p t)
			(cdr entry))
		       ((atom tree) tree)
		       (t (cons (traverse (car tree))
				(traverse (cdr tree))))))))
      (let ((new-tree (traverse tree)))
	(if substitution-p new-tree tree)))))

(defun sublis (alist tree &key key test test-not)
  (when (and (not (null test)) (not (null test-not)))
    (error 'both-test-and-test-not-given :name 'sublis))
  (if key
      (if test
          (if (or (eq test #'eq) (eq test 'eq))
              (sublis-eq-key alist tree key)
              (if (or (eq test #'eql) (eq test 'eql))
                  (sublis-eql-key alist tree key)
                  (sublis-test-key alist tree test key)))
          (if test-not
	      (sublis-test-not-key alist tree test-not key)
              (sublis-eql-key alist tree key)))
      (if test
          (if (or (eq test #'eq) (eq test 'eq))
              (sublis-eq-identity alist tree)
              (if (or (eq test #'eql) (eq test 'eql))
                  (sublis-eql-identity alist tree)
                  (sublis-test-identity alist tree test)))
          (if test-not
	      (sublis-test-not-identity alist tree test-not)
              (sublis-eql-identity alist tree)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function nsublis

;;; Special case when test is eq and key is identity

(defun nsublis-eq-identity (alist tree)
  (labels ((traverse (tree)
             (let ((entry (assoc-eq-identity (car tree) alist)))
               (cond ((not (null entry)) (setf (car tree) (cdr entry)))
                     ((atom (car tree)) nil)
                     (t (traverse (car tree)))))
	     (let ((entry (assoc-eq-identity (cdr tree) alist)))
               (cond ((not (null entry)) (setf (cdr tree) (cdr entry)))
                     ((atom (cdr tree)) nil)
                     (t (traverse (cdr tree)))))))
    (let ((entry (assoc-eq-identity tree alist)))
      (cond ((not (null entry)) (cdr entry))
	    ((atom tree) tree)
	    (t (traverse tree) tree)))))

;;; Special case when test is eql and key is identity

(defun nsublis-eql-identity (alist tree)
  (labels ((traverse (tree)
             (let ((entry (assoc-eql-identity (car tree) alist)))
               (cond ((not (null entry)) (setf (car tree) (cdr entry)))
                     ((atom (car tree)) nil)
                     (t (traverse (car tree)))))
	     (let ((entry (assoc-eql-identity (cdr tree) alist)))
               (cond ((not (null entry)) (setf (cdr tree) (cdr entry)))
                     ((atom (cdr tree)) nil)
                     (t (traverse (cdr tree)))))))
    (let ((entry (assoc-eql-identity tree alist)))
      (cond ((not (null entry)) (cdr entry))
	    ((atom tree) tree)
	    (t (traverse tree) tree)))))

;;; Special case when test is eq and key is given.

(defun nsublis-eq-key (alist tree key)
  (labels ((traverse (tree)
             (let ((entry (assoc-eq-identity (funcall key (car tree))
					     alist)))
               (cond ((not (null entry)) (setf (car tree) (cdr entry)))
                     ((atom (car tree)) nil)
                     (t (traverse (car tree)))))
	     (let ((entry (assoc-eq-identity (funcall key (cdr tree))
					     alist)))
               (cond ((not (null entry)) (setf (cdr tree) (cdr entry)))
                     ((atom (cdr tree)) nil)
                     (t (traverse (cdr tree)))))))
    (let ((entry (assoc-eq-identity (funcall key tree) alist)))
      (cond ((not (null entry)) (cdr entry))
	    ((atom tree) tree)
	    (t (traverse tree) tree)))))

;;; Special case when test is eql and key is given.

(defun nsublis-eql-key (alist tree key)
  (labels ((traverse (tree)
             (let ((entry (assoc-eql-identity (funcall key (car tree))
					      alist)))
               (cond ((not (null entry)) (setf (car tree) (cdr entry)))
                     ((atom (car tree)) nil)
                     (t (traverse (car tree)))))
	     (let ((entry (assoc-eql-identity (funcall key (cdr tree))
					      alist)))
               (cond ((not (null entry)) (setf (cdr tree) (cdr entry)))
                     ((atom (cdr tree)) nil)
                     (t (traverse (cdr tree)))))))
    (let ((entry (assoc-eql-identity (funcall key tree) alist)))
      (cond ((not (null entry)) (cdr entry))
	    ((atom tree) tree)
	    (t (traverse tree) tree)))))

;;; Special case when test is given and key is identity.

(defun nsublis-test-identity (alist tree test)
  (labels ((traverse (tree)
             (let ((entry (assoc-test-identity (car tree) alist (lambda (x y) (funcall test y x)))))
               (cond ((not (null entry)) (setf (car tree) (cdr entry)))
                     ((atom (car tree)) nil)
                     (t (traverse (car tree)))))
	     (let ((entry (assoc-test-identity (cdr tree) alist (lambda (x y) (funcall test y x)))))
               (cond ((not (null entry)) (setf (cdr tree) (cdr entry)))
                     ((atom (cdr tree)) nil)
                     (t (traverse (cdr tree)))))))
    (let ((entry (assoc-test-identity tree alist (lambda (x y) (funcall test y x)))))
      (cond ((not (null entry)) (cdr entry))
	    ((atom tree) tree)
	    (t (traverse tree) tree)))))

;;; Special case when test and key are both given.

(defun nsublis-test-key (alist tree test key)
  (labels ((traverse (tree)
             (let ((entry (assoc-test-identity (funcall key (car tree))
					       alist
					       (lambda (x y) (funcall test y x)))))
               (cond ((not (null entry)) (setf (car tree) (cdr entry)))
                     ((atom (car tree)) nil)
                     (t (traverse (car tree)))))
	     (let ((entry (assoc-test-identity (funcall key (cdr tree))
					       alist
					       (lambda (x y) (funcall test y x)))))
               (cond ((not (null entry)) (setf (cdr tree) (cdr entry)))
                     ((atom (cdr tree)) nil)
                     (t (traverse (cdr tree)))))))
    (let ((entry (assoc-test-identity (funcall key tree) alist (lambda (x y) (funcall test y x)))))
      (cond ((not (null entry)) (cdr entry))
	    ((atom tree) tree)
	    (t (traverse tree) tree)))))

(defun nsublis-test-not-identity (alist tree test)
  (labels ((traverse (tree)
             (let ((entry (assoc-test-not-identity (car tree) alist (lambda (x y) (funcall test y x)))))
               (cond ((not (null entry)) (setf (car tree) (cdr entry)))
                     ((atom (car tree)) nil)
                     (t (traverse (car tree)))))
	     (let ((entry (assoc-test-not-identity (cdr tree) alist (lambda (x y) (funcall test y x)))))
               (cond ((not (null entry)) (setf (cdr tree) (cdr entry)))
                     ((atom (cdr tree)) nil)
                     (t (traverse (cdr tree)))))))
    (let ((entry (assoc-test-not-identity tree alist (lambda (x y) (funcall test y x)))))
      (cond ((not (null entry)) (cdr entry))
	    ((atom tree) tree)
	    (t (traverse tree) tree)))))

;;; Special case when test-not and key are both given.

(defun nsublis-test-not-key (alist tree test key)
  (labels ((traverse (tree)
             (let ((entry (assoc-test-not-identity (funcall key (car tree))
						   alist
						   (lambda (x y) (funcall test y x)))))
               (cond ((not (null entry)) (setf (car tree) (cdr entry)))
                     ((atom (car tree)) nil)
                     (t (traverse (car tree)))))
	     (let ((entry (assoc-test-not-identity (funcall key (cdr tree))
						   alist
						   (lambda (x y) (funcall test y x)))))
               (cond ((not (null entry)) (setf (cdr tree) (cdr entry)))
                     ((atom (cdr tree)) nil)
                     (t (traverse (cdr tree)))))))
    (let ((entry (assoc-test-not-identity (funcall key tree) alist (lambda (x y) (funcall test y x)))))
      (cond ((not (null entry)) (cdr entry))
	    ((atom tree) tree)
	    (t (traverse tree) tree)))))

(defun nsublis (alist tree &key key test test-not)
  (when (and (not (null test)) (not (null test-not)))
    (error 'both-test-and-test-not-given :name 'nsublis))
  (if key
      (if test
          (if (or (eq test #'eq) (eq test 'eq))
              (nsublis-eq-key alist tree key)
              (if (or (eq test #'eql) (eq test 'eql))
                  (nsublis-eql-key alist tree key)
                  (nsublis-test-key alist tree test key)))
          (if test-not
	      (nsublis-test-not-key alist tree test-not key)
              (nsublis-eql-key alist tree key)))
      (if test
          (if (or (eq test #'eq) (eq test 'eq))
              (nsublis-eq-identity alist tree)
              (if (or (eq test #'eql) (eq test 'eql))
                  (nsublis-eql-identity alist tree)
                  (nsublis-test-identity alist tree test)))
          (if test-not
	      (nsublis-test-not-identity alist tree test-not)
              (nsublis-eql-identity alist tree)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function member

(defun member-eq-identity (item list)
  (loop for rest = list then (cdr rest)
	until (null rest)
        when (eq item (car rest))
          return rest))  

(defun member-eq-key (item list key)
  (loop for rest = list then (cdr rest)
	until (null rest)
        when (eq item (funcall key (car rest)))
          return rest))  

(defun member-not-eq-identity (item list)
  (loop for rest = list then (cdr rest)
	until (null rest)
        when (not (eq item (car rest)))
          return rest))  

(defun member-not-eq-key (item list key)
  (loop for rest = list then (cdr rest)
	until (null rest)
        when (not (eq item (funcall key (car rest))))
          return rest))  

(defun member-eql-identity (item list)
  (loop for rest = list then (cdr rest)
	until (null rest)
        when (eql item (car rest))
          return rest))  

(defun member-eql-key (item list key)
  (loop for rest = list then (cdr rest)
	until (null rest)
        when (eql item (funcall key (car rest)))
          return rest))  

(defun member-not-eql-identity (item list)
  (loop for rest = list then (cdr rest)
	until (null rest)
        when (not (eql item (car rest)))
          return rest))  

(defun member-not-eql-key (item list key)
  (loop for rest = list then (cdr rest)
	until (null rest)
        when (not (eql item (funcall key (car rest))))
          return rest))  

(defun member-test-identity (item list test)
  (loop for rest = list then (cdr rest)
	until (null rest)
        when (funcall test item (car rest))
          return rest))  

(defun member-test-key (item list test key)
  (loop for rest = list then (cdr rest)
	until (null rest)
        when (funcall test item (funcall key (car rest)))
          return rest))

(defun member-test-not-identity (item list test)
  (loop for rest = list then (cdr rest)
	until (null rest)
        when (not (funcall test item (car rest)))
          return rest))

(defun member-test-not-key (item list test key)
  (loop for rest = list then (cdr rest)
	until (null rest)
        when (not (funcall test item (funcall key (car rest))))
          return rest))

(defun member (item list &key key test test-not)
  (when (and (not (null test)) (not (null test-not)))
    (error 'both-test-and-test-not-given :name 'member))
  (if key
      (if test
          (if (or (eq test #'eq) (eq test 'eq))
              (member-eq-key item list key)
              (if (or (eq test #'eql) (eq test 'eql))
                  (member-eql-key item list key)
                  (member-test-key item list test key)))
          (if test-not
              (if (or (eq test-not #'eq) (eq test-not 'eq))
                  (member-not-eq-key item list key)
                  (if (or (eq test-not #'eql) (eq test-not 'eql))
                      (member-not-eql-key item list key)
                      (member-test-not-key item list test-not key)))
              (member-eql-key item list key)))
      (if test
          (if (or (eq test #'eq) (eq test 'eq))
              (member-eq-identity item list)
              (if (or (eq test #'eql) (eq test 'eql))
                  (member-eql-identity item list)
                  (member-test-identity item list test)))
          (if test-not
              (if (or (eq test-not #'eq) (eq test-not 'eq))
                  (member-not-eq-identity item list)
                  (if (or (eq test-not #'eql) (eq test-not 'eql))
                      (member-not-eql-identity item list)
                      (member-test-not-identity item list test-not)))
              (member-eql-identity item list)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function member-if

(defun member-if-identity (predicate list)
  (loop for rest = list then (cdr rest)
	until (null rest)
	when (funcall predicate (car rest))
	  return rest))

(defun member-if-key (predicate list key)
  (loop for rest = list then (cdr rest)
	until (null rest)
	when (funcall predicate (funcall key (car rest)))
	  return rest))

(defun member-if (predicate list &key key)
  (if key
      (member-if-key predicate list key)
      (member-if-identity predicate list)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function member-if-not

(defun member-if-not-identity (predicate list)
  (loop for rest = list then (cdr rest)
	until (null rest)
	unless (funcall predicate (car rest))
	  return rest))

(defun member-if-not-key (predicate list key)
  (loop for rest = list then (cdr rest)
	until (null rest)
	unless (funcall predicate (funcall key (car rest)))
	  return rest))

(defun member-if-not (predicate list &key key)
  (if key
      (member-if-not-key predicate list key)
      (member-if-not-identity predicate list)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function acons

(defun acons (key datum alist)
  (cons (cons key datum) alist))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function pairlis

(defun pairlis (keys data &optional alist)
  (loop for result = alist then (acons key datum result)
	for key in keys
	for datum in data
	finally (return result)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function copy-alist

(defun copy-alist (alist)
  (loop for element in alist
	collect (cons (car element) (cdr element))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function tailp

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

(defun union-identity-eql (list1 list2)
  (loop with result = list2
	for element in list1
	unless (member element list2)
	  do (push element result)
	finally (return result)))

(defun union-identity-eq (list1 list2)
  (loop with result = list2
	for element in list1
	unless (member element list2 :test #'eq)
	  do (push element result)
	finally (return result)))

(defun union-key-eql (list1 list2 key)
  (loop with result = list2
	for element in list1
	unless (member (funcall key element) list2 :key key)
	  do (push element result)
	finally (return result)))

(defun union-key-eq (list1 list2 key)
  (loop with result = list2
	for element in list1
	unless (member (funcall key element) list2 :test #'eq :key key)
	  do (push element result)
	finally (return result)))

(defun union-identity-test (list1 list2 test)
  (loop with result = list2
	for element in list1
	unless (member element list2 :test test)
	  do (push element result)
	finally (return result)))

(defun union-key-test (list1 list2 key test)
  (loop with result = list2
	for element in list1
	unless (member (funcall key element) list2 :test test :key key)
	  do (push element result)
	finally (return result)))

(defun union-identity-test-not (list1 list2 test-not)
  (loop with result = list2
	for element in list1
	unless (member element list2 :test-not test-not)
	  do (push element result)
	finally (return result)))

(defun union-key-test-not (list1 list2 key test-not)
  (loop with result = list2
	for element in list1
	unless (member (funcall key element) list2 :test-not test-not :key key)
	  do (push element result)
	finally (return result)))

(defun union-identity-eq-hash (list1 list2)
  (let ((table (make-hash-table :test #'eq)))
    (loop for element in list1
	  do (setf (gethash element table) element))
    (loop for element in list2
	  do (setf (gethash element table) element))
    (loop for element being the hash-values of table
	  collect element)))

(defun union-identity-eql-hash (list1 list2)
  (let ((table (make-hash-table :test #'eql)))
    (loop for element in list1
	  do (setf (gethash element table) element))
    (loop for element in list2
	  do (setf (gethash element table) element))
    (loop for element being the hash-values of table
	  collect element)))

(defun union-identity-equal-hash (list1 list2)
  (let ((table (make-hash-table :test #'equal)))
    (loop for element in list1
	  do (setf (gethash element table) element))
    (loop for element in list2
	  do (setf (gethash element table) element))
    (loop for element being the hash-values of table
	  collect element)))

(defun union-identity-equalp-hash (list1 list2)
  (let ((table (make-hash-table :test #'equalp)))
    (loop for element in list1
	  do (setf (gethash element table) element))
    (loop for element in list2
	  do (setf (gethash element table) element))
    (loop for element being the hash-values of table
	  collect element)))

(defun union-key-eq-hash (list1 list2 key)
  (let ((table (make-hash-table :test #'eq)))
    (loop for element in list1
	  do (setf (gethash (funcall key element) table) element))
    (loop for element in list2
	  do (setf (gethash (funcall key element) table) element))
    (loop for element being the hash-values of table
	  collect element)))

(defun union-key-eql-hash (list1 list2 key)
  (let ((table (make-hash-table :test #'eql)))
    (loop for element in list1
	  do (setf (gethash (funcall key element) table) element))
    (loop for element in list2
	  do (setf (gethash (funcall key element) table) element))
    (loop for element being the hash-values of table
	  collect element)))

(defun union-key-equal-hash (list1 list2 key)
  (let ((table (make-hash-table :test #'equal)))
    (loop for element in list1
	  do (setf (gethash (funcall key element) table) element))
    (loop for element in list2
	  do (setf (gethash (funcall key element) table) element))
    (loop for element being the hash-values of table
	  collect element)))

(defun union-key-equalp-hash (list1 list2 key)
  (let ((table (make-hash-table :test #'equalp)))
    (loop for element in list1
	  do (setf (gethash (funcall key element) table) element))
    (loop for element in list2
	  do (setf (gethash (funcall key element) table) element))
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
		       (union-key-eq-hash list1 list2 key)
		       (union-key-eq list1 list2 key)))
		  ((or (eq test #'eql) (eq test 'eql))
		   (if use-hash
		       (union-key-eql-hash list1 list2 key)
		       (union-key-eql list1 list2 key)))
		  ((or (eq test #'equal) (eq test 'equal))
		   (if use-hash
		       (union-key-equal-hash list1 list2 key)
		       (union-key-test list1 list2 key #'equal)))
		  ((or (eq test #'equalp) (eq test 'equalp))
		   (if use-hash
		       (union-key-equalp-hash list1 list2 key)
		       (union-key-test list1 list2 key #'equalp)))
		  (t
		   (union-key-test list1 list2 key test)))
	    (if test-not
		(union-key-test-not list1 list2 key test-not)
		(if use-hash
		    (union-key-eql-hash list1 list2 key)
		    (union-key-eql list1 list2 key))))
	(if test
	    (cond ((or (eq test #'eq) (eq test 'eq))
		   (if use-hash
		       (union-identity-eq-hash list1 list2)
		       (union-identity-eq list1 list2)))
		  ((or (eq test #'eql) (eq test 'eql))
		   (if use-hash
		       (union-identity-eql-hash list1 list2)
		       (union-identity-eql list1 list2)))
		  ((or (eq test #'equal) (eq test 'equal))
		   (if use-hash
		       (union-identity-equal-hash list1 list2)
		       (union-identity-test list1 list2 #'equal)))
		  ((or (eq test #'equalp) (eq test 'equalp))
		   (if use-hash
		       (union-identity-equalp-hash list1 list2)
		       (union-identity-test list1 list2 #'equalp)))
		  (t
		   (union-identity-test list1 list2 test)))
	    (if test-not
		(union-identity-test-not list1 list2 test-not)
		(if use-hash
		    (union-identity-eql-hash list1 list2)
		    (union-identity-eql list1 list2)))))))

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
		       (union-key-eq-hash list1 list2 key)
		       (union-key-eq list1 list2 key)))
		  ((or (eq test #'eql) (eq test 'eql))
		   (if use-hash
		       (union-key-eql-hash list1 list2 key)
		       (union-key-eql list1 list2 key)))
		  ((or (eq test #'equal) (eq test 'equal))
		   (if use-hash
		       (union-key-equal-hash list1 list2 key)
		       (union-key-test list1 list2 key #'equal)))
		  ((or (eq test #'equalp) (eq test 'equalp))
		   (if use-hash
		       (union-key-equalp-hash list1 list2 key)
		       (union-key-test list1 list2 key #'equalp)))
		  (t
		   (union-key-test list1 list2 key test)))
	    (if test-not
		(union-key-test-not list1 list2 key test-not)
		(if use-hash
		    (union-key-eql-hash list1 list2 key)
		    (union-key-eql list1 list2 key))))
	(if test
	    (cond ((or (eq test #'eq) (eq test 'eq))
		   (if use-hash
		       (union-identity-eq-hash list1 list2)
		       (union-identity-eq list1 list2)))
		  ((or (eq test #'eql) (eq test 'eql))
		   (if use-hash
		       (union-identity-eql-hash list1 list2)
		       (union-identity-eql list1 list2)))
		  ((or (eq test #'equal) (eq test 'equal))
		   (if use-hash
		       (union-identity-equal-hash list1 list2)
		       (union-identity-test list1 list2 #'equal)))
		  ((or (eq test #'equalp) (eq test 'equalp))
		   (if use-hash
		       (union-identity-equalp-hash list1 list2)
		       (union-identity-test list1 list2 #'equalp)))
		  (t
		   (union-identity-test list1 list2 test)))
	    (if test-not
		(union-identity-test-not list1 list2 test-not)
		(if use-hash
		    (union-identity-eql-hash list1 list2)
		    (union-identity-eql list1 list2)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function intersection

(defun intersection-identity-eql (list1 list2)
  (loop for element in list1
	when (member element list2)
	  collect element))

(defun intersection-identity-eq (list1 list2)
  (loop for element in list1
	when (member element list2 :test #'eq)
	  collect element))

(defun intersection-key-eql (list1 list2 key)
  (loop for element in list1
	when (member (funcall key element) list2 :key key)
	  collect element))

(defun intersection-key-eq (list1 list2 key)
  (loop for element in list1
	when (member (funcall key element) list2 :test #'eq :key key)
	  collect element))

(defun intersection-identity-test (list1 list2 test)
  (loop for element in list1
	when (member element list2 :test test)
	  collect element))

(defun intersection-key-test (list1 list2 key test)
  (loop for element in list1
	when (member (funcall key element) list2 :test test :key key)
	  collect element))

(defun intersection-identity-test-not (list1 list2 test-not)
  (loop for element in list1
	when (member element list2 :test-not test-not)
	  collect element))

(defun intersection-key-test-not (list1 list2 key test-not)
  (loop for element in list1
	when (member (funcall key element) list2 :test-not test-not :key key)
	  collect element))

(defun intersection-identity-eq-hash (list1 list2)
  (let ((table (make-hash-table :test #'eq)))
    (loop for element in list2
	  do (setf (gethash element table) t))
    (loop for element in list1
	  when (gethash element table)
	    collect element)))

(defun intersection-identity-eql-hash (list1 list2)
  (let ((table (make-hash-table :test #'eql)))
    (loop for element in list2
	  do (setf (gethash element table) t))
    (loop for element in list1
	  when (gethash element table)
	    collect element)))

(defun intersection-identity-equal-hash (list1 list2)
  (let ((table (make-hash-table :test #'equal)))
    (loop for element in list2
	  do (setf (gethash element table) t))
    (loop for element in list1
	  when (gethash element table)
	    collect element)))

(defun intersection-identity-equalp-hash (list1 list2)
  (let ((table (make-hash-table :test #'equalp)))
    (loop for element in list2
	  do (setf (gethash element table) t))
    (loop for element in list1
	  when (gethash element table)
	    collect element)))

(defun intersection-key-eq-hash (list1 list2 key)
  (let ((table (make-hash-table :test #'eq)))
    (loop for element in list2
	  do (setf (gethash (funcall key element) table) t))
    (loop for element in list1
	  when (gethash (funcall key element) table)
	    collect element)))

(defun intersection-key-eql-hash (list1 list2 key)
  (let ((table (make-hash-table :test #'eql)))
    (loop for element in list2
	  do (setf (gethash (funcall key element) table) t))
    (loop for element in list1
	  when (gethash (funcall key element) table)
	    collect element)))

(defun intersection-key-equal-hash (list1 list2 key)
  (let ((table (make-hash-table :test #'equal)))
    (loop for element in list2
	  do (setf (gethash (funcall key element) table) t))
    (loop for element in list1
	  when (gethash (funcall key element) table)
	    collect element)))

(defun intersection-key-equalp-hash (list1 list2 key)
  (let ((table (make-hash-table :test #'equalp)))
    (loop for element in list2
	  do (setf (gethash (funcall key element) table) t))
    (loop for element in list1
	  when (gethash (funcall key element) table)
	    collect element)))

(defun intersection (list1 list2 &key key test test-not)
  (when (and (not (null test)) (not (null test-not)))
    (error 'both-test-and-test-not-given :name 'intersection))
  (let ((use-hash (> (* (length list1) (length list2)) 1000)))
    (if key
	(if test
	    (cond ((or (eq test #'eq) (eq test 'eq))
		   (if use-hash
		       (intersection-key-eq-hash list1 list2 key)
		       (intersection-key-eq list1 list2 key)))
		  ((or (eq test #'eql) (eq test 'eql))
		   (if use-hash
		       (intersection-key-eql-hash list1 list2 key)
		       (intersection-key-eql list1 list2 key)))
		  ((or (eq test #'equal) (eq test 'equal))
		   (if use-hash
		       (intersection-key-equal-hash list1 list2 key)
		       (intersection-key-test list1 list2 key #'equal)))
		  ((or (eq test #'equalp) (eq test 'equalp))
		   (if use-hash
		       (intersection-key-equalp-hash list1 list2 key)
		       (intersection-key-test list1 list2 key #'equalp)))
		  (t
		   (intersection-key-test list1 list2 key test)))
	    (if test-not
		(intersection-key-test-not list1 list2 key test-not)
		(if use-hash
		    (intersection-key-eql-hash list1 list2 key)
		    (intersection-key-eql list1 list2 key))))
	(if test
	    (cond ((or (eq test #'eq) (eq test 'eq))
		   (if use-hash
		       (intersection-identity-eq-hash list1 list2)
		       (intersection-identity-eq list1 list2)))
		  ((or (eq test #'eql) (eq test 'eql))
		   (if use-hash
		       (intersection-identity-eql-hash list1 list2)
		       (intersection-identity-eql list1 list2)))
		  ((or (eq test #'equal) (eq test 'equal))
		   (if use-hash
		       (intersection-identity-equal-hash list1 list2)
		       (intersection-identity-test list1 list2 #'equal)))
		  ((or (eq test #'equalp) (eq test 'equalp))
		   (if use-hash
		       (intersection-identity-equalp-hash list1 list2)
		       (intersection-identity-test list1 list2 #'equalp)))
		  (t
		   (intersection-identity-test list1 list2 test)))
	    (if test-not
		(intersection-identity-test-not list1 list2 test-not)
		(if use-hash
		    (intersection-identity-eql-hash list1 list2)
		    (intersection-identity-eql list1 list2)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function nintersection

;;; We take advantage of the fact that the standard doesn't 
;;; require this function to have any side effects. 

(defun nintersection (list1 list2 &key key test test-not)
  (when (and (not (null test)) (not (null test-not)))
    (error 'both-test-and-test-not-given :name 'nintersection))
  (let ((use-hash (> (* (length list1) (length list2)) 1000)))
    (if key
	(if test
	    (cond ((or (eq test #'eq) (eq test 'eq))
		   (if use-hash
		       (intersection-key-eq-hash list1 list2 key)
		       (intersection-key-eq list1 list2 key)))
		  ((or (eq test #'eql) (eq test 'eql))
		   (if use-hash
		       (intersection-key-eql-hash list1 list2 key)
		       (intersection-key-eql list1 list2 key)))
		  ((or (eq test #'equal) (eq test 'equal))
		   (if use-hash
		       (intersection-key-equal-hash list1 list2 key)
		       (intersection-key-test list1 list2 key #'equal)))
		  ((or (eq test #'equalp) (eq test 'equalp))
		   (if use-hash
		       (intersection-key-equalp-hash list1 list2 key)
		       (intersection-key-test list1 list2 key #'equalp)))
		  (t
		   (intersection-key-test list1 list2 key test)))
	    (if test-not
		(intersection-key-test-not list1 list2 key test-not)
		(if use-hash
		    (intersection-key-eql-hash list1 list2 key)
		    (intersection-key-eql list1 list2 key))))
	(if test
	    (cond ((or (eq test #'eq) (eq test 'eq))
		   (if use-hash
		       (intersection-identity-eq-hash list1 list2)
		       (intersection-identity-eq list1 list2)))
		  ((or (eq test #'eql) (eq test 'eql))
		   (if use-hash
		       (intersection-identity-eql-hash list1 list2)
		       (intersection-identity-eql list1 list2)))
		  ((or (eq test #'equal) (eq test 'equal))
		   (if use-hash
		       (intersection-identity-equal-hash list1 list2)
		       (intersection-identity-test list1 list2 #'equal)))
		  ((or (eq test #'equalp) (eq test 'equalp))
		   (if use-hash
		       (intersection-identity-equalp-hash list1 list2)
		       (intersection-identity-test list1 list2 #'equalp)))
		  (t
		   (intersection-identity-test list1 list2 test)))
	    (if test-not
		(intersection-identity-test-not list1 list2 test-not)
		(if use-hash
		    (intersection-identity-eql-hash list1 list2)
		    (intersection-identity-eql list1 list2)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function set-difference

(defun set-difference-identity-eql (list1 list2)
  (loop for element in list1
	unless (member element list2)
	  collect element))

(defun set-difference-identity-eq (list1 list2)
  (loop for element in list1
	unless (member element list2 :test #'eq)
	  collect element))

(defun set-difference-key-eql (list1 list2 key)
  (loop for element in list1
	unless (member (funcall key element) list2 :key key)
	  collect element))

(defun set-difference-key-eq (list1 list2 key)
  (loop for element in list1
	unless (member (funcall key element) list2 :test #'eq :key key)
	  collect element))

(defun set-difference-identity-test (list1 list2 test)
  (loop for element in list1
	unless (member element list2 :test test)
	  collect element))

(defun set-difference-key-test (list1 list2 key test)
  (loop for element in list1
	unless (member (funcall key element) list2 :test test :key key)
	  collect element))

(defun set-difference-identity-test-not (list1 list2 test-not)
  (loop for element in list1
	unless (member element list2 :test-not test-not)
	  collect element))

(defun set-difference-key-test-not (list1 list2 key test-not)
  (loop for element in list1
	unless (member (funcall key element) list2 :test-not test-not :key key)
	  collect element))

(defun set-difference-identity-eq-hash (list1 list2)
  (let ((table (make-hash-table :test #'eq)))
    (loop for element in list2
	  do (setf (gethash element table) t))
    (loop for element in list1
	  unless (gethash element table)
	    collect element)))

(defun set-difference-identity-eql-hash (list1 list2)
  (let ((table (make-hash-table :test #'eql)))
    (loop for element in list2
	  do (setf (gethash element table) t))
    (loop for element in list1
	  unless (gethash element table)
	    collect element)))

(defun set-difference-identity-equal-hash (list1 list2)
  (let ((table (make-hash-table :test #'equal)))
    (loop for element in list2
	  do (setf (gethash element table) t))
    (loop for element in list1
	  unless (gethash element table)
	    collect element)))

(defun set-difference-identity-equalp-hash (list1 list2)
  (let ((table (make-hash-table :test #'equalp)))
    (loop for element in list2
	  do (setf (gethash element table) t))
    (loop for element in list1
	  unless (gethash element table)
	    collect element)))

(defun set-difference-key-eq-hash (list1 list2 key)
  (let ((table (make-hash-table :test #'eq)))
    (loop for element in list2
	  do (setf (gethash (funcall key element) table) t))
    (loop for element in list1
	  unless (gethash (funcall key element) table)
	    collect element)))

(defun set-difference-key-eql-hash (list1 list2 key)
  (let ((table (make-hash-table :test #'eql)))
    (loop for element in list2
	  do (setf (gethash (funcall key element) table) t))
    (loop for element in list1
	  unless (gethash (funcall key element) table)
	    collect element)))

(defun set-difference-key-equal-hash (list1 list2 key)
  (let ((table (make-hash-table :test #'equal)))
    (loop for element in list2
	  do (setf (gethash (funcall key element) table) t))
    (loop for element in list1
	  unless (gethash (funcall key element) table)
	    collect element)))

(defun set-difference-key-equalp-hash (list1 list2 key)
  (let ((table (make-hash-table :test #'equalp)))
    (loop for element in list2
	  do (setf (gethash (funcall key element) table) t))
    (loop for element in list1
	  unless (gethash (funcall key element) table)
	    collect element)))

(defun set-difference (list1 list2 &key key test test-not)
  (when (and (not (null test)) (not (null test-not)))
    (error 'both-test-and-test-not-given :name 'set-difference))
  (let ((use-hash (> (* (length list1) (length list2)) 1000)))
    (if key
	(if test
	    (cond ((or (eq test #'eq) (eq test 'eq))
		   (if use-hash
		       (set-difference-key-eq-hash list1 list2 key)
		       (set-difference-key-eq list1 list2 key)))
		  ((or (eq test #'eql) (eq test 'eql))
		   (if use-hash
		       (set-difference-key-eql-hash list1 list2 key)
		       (set-difference-key-eql list1 list2 key)))
		  ((or (eq test #'equal) (eq test 'equal))
		   (if use-hash
		       (set-difference-key-equal-hash list1 list2 key)
		       (set-difference-key-test list1 list2 key #'equal)))
		  ((or (eq test #'equalp) (eq test 'equalp))
		   (if use-hash
		       (set-difference-key-equalp-hash list1 list2 key)
		       (set-difference-key-test list1 list2 key #'equalp)))
		  (t
		   (set-difference-key-test list1 list2 key test)))
	    (if test-not
		(set-difference-key-test-not list1 list2 key test-not)
		(if use-hash
		    (set-difference-key-eql-hash list1 list2 key)
		    (set-difference-key-eql list1 list2 key))))
	(if test
	    (cond ((or (eq test #'eq) (eq test 'eq))
		   (if use-hash
		       (set-difference-identity-eq-hash list1 list2)
		       (set-difference-identity-eq list1 list2)))
		  ((or (eq test #'eql) (eq test 'eql))
		   (if use-hash
		       (set-difference-identity-eql-hash list1 list2)
		       (set-difference-identity-eql list1 list2)))
		  ((or (eq test #'equal) (eq test 'equal))
		   (if use-hash
		       (set-difference-identity-equal-hash list1 list2)
		       (set-difference-identity-test list1 list2 #'equal)))
		  ((or (eq test #'equalp) (eq test 'equalp))
		   (if use-hash
		       (set-difference-identity-equalp-hash list1 list2)
		       (set-difference-identity-test list1 list2 #'equalp)))
		  (t
		   (set-difference-identity-test list1 list2 test)))
	    (if test-not
		(set-difference-identity-test-not list1 list2 test-not)
		(if use-hash
		    (set-difference-identity-eql-hash list1 list2)
		    (set-difference-identity-eql list1 list2)))))))

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
		       (set-difference-key-eq-hash list1 list2 key)
		       (set-difference-key-eq list1 list2 key)))
		  ((or (eq test #'eql) (eq test 'eql))
		   (if use-hash
		       (set-difference-key-eql-hash list1 list2 key)
		       (set-difference-key-eql list1 list2 key)))
		  ((or (eq test #'equal) (eq test 'equal))
		   (if use-hash
		       (set-difference-key-equal-hash list1 list2 key)
		       (set-difference-key-test list1 list2 key #'equal)))
		  ((or (eq test #'equalp) (eq test 'equalp))
		   (if use-hash
		       (set-difference-key-equalp-hash list1 list2 key)
		       (set-difference-key-test list1 list2 key #'equalp)))
		  (t
		   (set-difference-key-test list1 list2 key test)))
	    (if test-not
		(set-difference-key-test-not list1 list2 key test-not)
		(if use-hash
		    (set-difference-key-eql-hash list1 list2 key)
		    (set-difference-key-eql list1 list2 key))))
	(if test
	    (cond ((or (eq test #'eq) (eq test 'eq))
		   (if use-hash
		       (set-difference-identity-eq-hash list1 list2)
		       (set-difference-identity-eq list1 list2)))
		  ((or (eq test #'eql) (eq test 'eql))
		   (if use-hash
		       (set-difference-identity-eql-hash list1 list2)
		       (set-difference-identity-eql list1 list2)))
		  ((or (eq test #'equal) (eq test 'equal))
		   (if use-hash
		       (set-difference-identity-equal-hash list1 list2)
		       (set-difference-identity-test list1 list2 #'equal)))
		  ((or (eq test #'equalp) (eq test 'equalp))
		   (if use-hash
		       (set-difference-identity-equalp-hash list1 list2)
		       (set-difference-identity-test list1 list2 #'equalp)))
		  (t
		   (set-difference-identity-test list1 list2 test)))
	    (if test-not
		(set-difference-identity-test-not list1 list2 test-not)
		(if use-hash
		    (set-difference-identity-eql-hash list1 list2)
		    (set-difference-identity-eql list1 list2)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function adjoin

(defun adjoin-identity-eq (item list)
  (if (member item list :test #'eq)
      list
      (cons item list)))

(defun adjoin-identity-eql (item list)
  (if (member item list)
      list
      (cons item list)))

(defun adjoin-identity-test (item list test)
  (if (member item list :test test)
      list
      (cons item list)))

(defun adjoin-identity-test-not (item list test-not)
  (if (member item list :test-not test-not)
      list
      (cons item list)))

(defun adjoin-key-eq (item list key)
  (if (member (funcall key item) list :key key :test #'eq)
      list
      (cons item list)))

(defun adjoin-key-eql (item list key)
  (if (member (funcall key item) list :key key)
      list
      (cons item list)))

(defun adjoin-key-test (item list key test)
  (if (member (funcall key item) list :key key :test test)
      list
      (cons item list)))

(defun adjoin-key-test-not (item list key test-not)
  (if (member (funcall key item) list :key key :test-not test-not)
      list
      (cons item list)))

(defun adjoin (item list &key key test test-not)
  (when (and (not (null test)) (not (null test-not)))
    (error 'both-test-and-test-not-given :name 'adjoin))
  (if key
      (if test
	  (if (or (eq test #'eq) (eq test 'eq))
	      (adjoin-key-eq item list key)
	      (if (or (eq test #'eql) (eq test 'eql))
		  (adjoin-key-eql item list key)
		  (adjoin-key-test item list key test)))
	  (if test-not
	      (adjoin-key-test-not item list key test-not)
	      (adjoin-key-eql item list key)))
      (if test
	  (if (or (eq test #'eq) (eq test 'eq))
	      (adjoin-identity-eq item list)
	      (if (or (eq test #'eql) (eq test 'eql))
		  (adjoin-identity-eql item list)
		  (adjoin-identity-test item list test)))
	  (if test-not
	      (adjoin-identity-test-not item list test-not)
	      (adjoin-identity-eql item list)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function set-exclusive-or

(defun set-exclusive-or-identity-eql (list1 list2)
  (let ((result '()))
    (loop for element in list1
	  unless (member element list2)
	    do (push element result))
    (loop for element in list2
	  unless (member element list1)
	    do (push element result))
    result))

(defun set-exclusive-or-identity-eq (list1 list2)
  (let ((result '()))
    (loop for element in list1
	  unless (member element list2 :test #'eq)
	    do (push element result))
    (loop for element in list2
	  unless (member element list1 :test #'eq)
	    do (push element result))
    result))

(defun set-exclusive-or-key-eql (list1 list2 key)
  (let ((result '()))
    (loop for element in list1
	  unless (member (funcall key element) list2 :key key)
	    do (push element result))
    (loop for element in list2
	  unless (member (funcall key element) list1 :key key)
	    do (push element result))
    result))

(defun set-exclusive-or-key-eq (list1 list2 key)
  (let ((result '()))
    (loop for element in list1
	  unless (member (funcall key element) list2 :test #'eq :key key)
	    do (push element result))
    (loop for element in list2
	  unless (member (funcall key element) list1 :test #'eq :key key)
	    do (push element result))
    result))

;;; This is tricky because we can't use member to test whether 
;;; an element of list2 is a member of list1, simply because 
;;; that would reverse the arguments to the test, and the 
;;; test is not necessarily a commutative function. 
(defun set-exclusive-or-identity-test (list1 list2 test)
  (let ((result '()))
    (loop for element1 in list1
	  unless (loop for element2 in list2
		       thereis (funcall test element1 element2))
	    do (push element1 result))
    (loop for element2 in list2
	  unless (loop for element1 in list1
		       thereis (funcall test element1 element2))
	    do (push element2 result))
    result))

;;; This is tricky because we can't use member to test whether 
;;; an element of list2 is a member of list1, simply because 
;;; that would reverse the arguments to the test, and the 
;;; test is not necessarily a commutative function. 
(defun set-exclusive-or-key-test (list1 list2 key test)
  (let ((result '()))
    (loop for element1 in list1
	  unless (loop for element2 in list2
		       thereis (funcall test
					(funcall key element1)
					(funcall key element2)))
	    do (push element1 result))
    (loop for element2 in list2
	  unless (loop for element1 in list1
		       thereis (funcall test
					(funcall key element1)
					(funcall key element2)))
	    do (push element2 result))
    result))

;;; This is tricky because we can't use member to test whether 
;;; an element of list2 is a member of list1, simply because 
;;; that would reverse the arguments to the test, and the 
;;; test is not necessarily a commutative function. 
(defun set-exclusive-or-identity-test-not (list1 list2 test-not)
  (let ((result '()))
    (loop for element1 in list1
	  when (loop for element2 in list2
		     always (funcall test-not element1 element2))
	    do (push element1 result))
    (loop for element2 in list2
	  when (loop for element1 in list1
		     always (funcall test-not element1 element2))
	    do (push element2 result))
    result))

;;; This is tricky because we can't use member to test whether 
;;; an element of list2 is a member of list1, simply because 
;;; that would reverse the arguments to the test, and the 
;;; test is not necessarily a commutative function. 
(defun set-exclusive-or-key-test-not (list1 list2 key test-not)
  (let ((result '()))
    (loop for element1 in list1
	  when (loop for element2 in list2
		     always (funcall test-not
				     (funcall key element1)
				     (funcall key element2)))
	    do (push element1 result))
    (loop for element2 in list2
	  when (loop for element1 in list1
		     always (funcall test-not
				     (funcall key element1)
				     (funcall key element2)))
	    do (push element2 result))
    result))

(defun set-exclusive-or-identity-eq-hash (list1 list2)
  (let ((table1 (make-hash-table :test #'eq))
	(table2 (make-hash-table :test #'eq))
	(result '()))
    (loop for element in list1
	  do (setf (gethash element table1) t))
    (loop for element in list2
	  do (setf (gethash element table2) t))
    (loop for element in list1
	  unless (gethash element table2)
	    do (push element result))
    (loop for element in list2
	  unless (gethash element table1)
	    do (push element result))
    result))

(defun set-exclusive-or-identity-eql-hash (list1 list2)
  (let ((table1 (make-hash-table :test #'eql))
	(table2 (make-hash-table :test #'eql))
	(result '()))
    (loop for element in list1
	  do (setf (gethash element table1) t))
    (loop for element in list2
	  do (setf (gethash element table2) t))
    (loop for element in list1
	  unless (gethash element table2)
	    do (push element result))
    (loop for element in list2
	  unless (gethash element table1)
	    do (push element result))
    result))

(defun set-exclusive-or-identity-equal-hash (list1 list2)
  (let ((table1 (make-hash-table :test #'equal))
	(table2 (make-hash-table :test #'equal))
	(result '()))
    (loop for element in list1
	  do (setf (gethash element table1) t))
    (loop for element in list2
	  do (setf (gethash element table2) t))
    (loop for element in list1
	  unless (gethash element table2)
	    do (push element result))
    (loop for element in list2
	  unless (gethash element table1)
	    do (push element result))
    result))

(defun set-exclusive-or-identity-equalp-hash (list1 list2)
  (let ((table1 (make-hash-table :test #'equalp))
	(table2 (make-hash-table :test #'equalp))
	(result '()))
    (loop for element in list1
	  do (setf (gethash element table1) t))
    (loop for element in list2
	  do (setf (gethash element table2) t))
    (loop for element in list1
	  unless (gethash element table2)
	    do (push element result))
    (loop for element in list2
	  unless (gethash element table1)
	    do (push element result))
    result))

(defun set-exclusive-or-key-eq-hash (list1 list2 key)
  (let ((table1 (make-hash-table :test #'eq))
	(table2 (make-hash-table :test #'eq))
	(result '()))
    (loop for element in list1
	  do (setf (gethash (funcall key element) table1) t))
    (loop for element in list2
	  do (setf (gethash (funcall key element) table2) t))
    (loop for element in list1
	  unless (gethash (funcall key element) table2)
	    do (push element result))
    (loop for element in list2
	  unless (gethash (funcall key element) table1)
	    do (push element result))
    result))

(defun set-exclusive-or-key-eql-hash (list1 list2 key)
  (let ((table1 (make-hash-table :test #'eql))
	(table2 (make-hash-table :test #'eql))
	(result '()))
    (loop for element in list1
	  do (setf (gethash (funcall key element) table1) t))
    (loop for element in list2
	  do (setf (gethash (funcall key element) table2) t))
    (loop for element in list1
	  unless (gethash (funcall key element) table2)
	    do (push element result))
    (loop for element in list2
	  unless (gethash (funcall key element) table1)
	    do (push element result))
    result))

(defun set-exclusive-or-key-equal-hash (list1 list2 key)
  (let ((table1 (make-hash-table :test #'equal))
	(table2 (make-hash-table :test #'equal))
	(result '()))
    (loop for element in list1
	  do (setf (gethash (funcall key element) table1) t))
    (loop for element in list2
	  do (setf (gethash (funcall key element) table2) t))
    (loop for element in list1
	  unless (gethash (funcall key element) table2)
	    do (push element result))
    (loop for element in list2
	  unless (gethash (funcall key element) table1)
	    do (push element result))
    result))

(defun set-exclusive-or-key-equalp-hash (list1 list2 key)
  (let ((table1 (make-hash-table :test #'equalp))
	(table2 (make-hash-table :test #'equalp))
	(result '()))
    (loop for element in list1
	  do (setf (gethash (funcall key element) table1) t))
    (loop for element in list2
	  do (setf (gethash (funcall key element) table2) t))
    (loop for element in list1
	  unless (gethash (funcall key element) table2)
	    do (push element result))
    (loop for element in list2
	  unless (gethash (funcall key element) table1)
	    do (push element result))
    result))

(defun set-exclusive-or (list1 list2 &key key test test-not)
  (when (and (not (null test)) (not (null test-not)))
    (error 'both-test-and-test-not-given :name 'set-exclusive-or))
  (let ((use-hash (> (* (length list1) (length list2)) 1000)))
    (if key
	(if test
	    (cond ((or (eq test #'eq) (eq test 'eq))
		   (if use-hash
		       (set-exclusive-or-key-eq-hash list1 list2 key)
		       (set-exclusive-or-key-eq list1 list2 key)))
		  ((or (eq test #'eql) (eq test 'eql))
		   (if use-hash
		       (set-exclusive-or-key-eql-hash list1 list2 key)
		       (set-exclusive-or-key-eql list1 list2 key)))
		  ((or (eq test #'equal) (eq test 'equal))
		   (if use-hash
		       (set-exclusive-or-key-equal-hash list1 list2 key)
		       (set-exclusive-or-key-test list1 list2 key #'equal)))
		  ((or (eq test #'equalp) (eq test 'equalp))
		   (if use-hash
		       (set-exclusive-or-key-equalp-hash list1 list2 key)
		       (set-exclusive-or-key-test list1 list2 key #'equalp)))
		  (t
		   (set-exclusive-or-key-test list1 list2 key test)))
	    (if test-not
		(set-exclusive-or-key-test-not list1 list2 key test-not)
		(if use-hash
		    (set-exclusive-or-key-eql-hash list1 list2 key)
		    (set-exclusive-or-key-eql list1 list2 key))))
	(if test
	    (cond ((or (eq test #'eq) (eq test 'eq))
		   (if use-hash
		       (set-exclusive-or-identity-eq-hash list1 list2)
		       (set-exclusive-or-identity-eq list1 list2)))
		  ((or (eq test #'eql) (eq test 'eql))
		   (if use-hash
		       (set-exclusive-or-identity-eql-hash list1 list2)
		       (set-exclusive-or-identity-eql list1 list2)))
		  ((or (eq test #'equal) (eq test 'equal))
		   (if use-hash
		       (set-exclusive-or-identity-equal-hash list1 list2)
		       (set-exclusive-or-identity-test list1 list2 #'equal)))
		  ((or (eq test #'equalp) (eq test 'equalp))
		   (if use-hash
		       (set-exclusive-or-identity-equalp-hash list1 list2)
		       (set-exclusive-or-identity-test list1 list2 #'equalp)))
		  (t
		   (set-exclusive-or-identity-test list1 list2 test)))
	    (if test-not
		(set-exclusive-or-identity-test-not list1 list2 test-not)
		(if use-hash
		    (set-exclusive-or-identity-eql-hash list1 list2)
		    (set-exclusive-or-identity-eql list1 list2)))))))

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
		       (set-exclusive-or-key-eq-hash list1 list2 key)
		       (set-exclusive-or-key-eq list1 list2 key)))
		  ((or (eq test #'eql) (eq test 'eql))
		   (if use-hash
		       (set-exclusive-or-key-eql-hash list1 list2 key)
		       (set-exclusive-or-key-eql list1 list2 key)))
		  ((or (eq test #'equal) (eq test 'equal))
		   (if use-hash
		       (set-exclusive-or-key-equal-hash list1 list2 key)
		       (set-exclusive-or-key-test list1 list2 key #'equal)))
		  ((or (eq test #'equalp) (eq test 'equalp))
		   (if use-hash
		       (set-exclusive-or-key-equalp-hash list1 list2 key)
		       (set-exclusive-or-key-test list1 list2 key #'equalp)))
		  (t
		   (set-exclusive-or-key-test list1 list2 key test)))
	    (if test-not
		(set-exclusive-or-key-test-not list1 list2 key test-not)
		(if use-hash
		    (set-exclusive-or-key-eql-hash list1 list2 key)
		    (set-exclusive-or-key-eql list1 list2 key))))
	(if test
	    (cond ((or (eq test #'eq) (eq test 'eq))
		   (if use-hash
		       (set-exclusive-or-identity-eq-hash list1 list2)
		       (set-exclusive-or-identity-eq list1 list2)))
		  ((or (eq test #'eql) (eq test 'eql))
		   (if use-hash
		       (set-exclusive-or-identity-eql-hash list1 list2)
		       (set-exclusive-or-identity-eql list1 list2)))
		  ((or (eq test #'equal) (eq test 'equal))
		   (if use-hash
		       (set-exclusive-or-identity-equal-hash list1 list2)
		       (set-exclusive-or-identity-test list1 list2 #'equal)))
		  ((or (eq test #'equalp) (eq test 'equalp))
		   (if use-hash
		       (set-exclusive-or-identity-equalp-hash list1 list2)
		       (set-exclusive-or-identity-test list1 list2 #'equalp)))
		  (t
		   (set-exclusive-or-identity-test list1 list2 test)))
	    (if test-not
		(set-exclusive-or-identity-test-not list1 list2 test-not)
		(if use-hash
		    (set-exclusive-or-identity-eql-hash list1 list2)
		    (set-exclusive-or-identity-eql list1 list2)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function subsetp

(defun subsetp-identity-eql (list1 list2)
  (loop for element in list1
	always (member element list2)))

(defun subsetp-identity-eq (list1 list2)
  (loop for element in list1
	always (member element list2 :test #'eq)))

(defun subsetp-key-eql (list1 list2 key)
  (loop for element in list1
	always (member (funcall key element) list2 :key key)))

(defun subsetp-key-eq (list1 list2 key)
  (loop for element in list1
	always (member (funcall key element) list2 :test #'eq :key key)))

(defun subsetp-identity-test (list1 list2 test)
  (loop for element in list1
	always (member element list2 :test test)))

(defun subsetp-key-test (list1 list2 key test)
  (loop for element in list1
	always (member (funcall key element) list2 :test test :key key)))

(defun subsetp-identity-test-not (list1 list2 test-not)
  (loop for element in list1
	always (member element list2 :test-not test-not)))

(defun subsetp-key-test-not (list1 list2 key test-not)
  (loop for element in list1
	always (member (funcall key element) list2 :test-not test-not :key key)))

(defun subsetp-identity-eq-hash (list1 list2)
  (let ((table (make-hash-table :test #'eq)))
    (loop for element in list2
	  do (setf (gethash element table) t))
    (loop for element in list1
	  always (gethash element table))))

(defun subsetp-identity-eql-hash (list1 list2)
  (let ((table (make-hash-table :test #'eql)))
    (loop for element in list2
	  do (setf (gethash element table) t))
    (loop for element in list1
	  always (gethash element table))))

(defun subsetp-identity-equal-hash (list1 list2)
  (let ((table (make-hash-table :test #'equal)))
    (loop for element in list2
	  do (setf (gethash element table) t))
    (loop for element in list1
	  always (gethash element table))))

(defun subsetp-identity-equalp-hash (list1 list2)
  (let ((table (make-hash-table :test #'equalp)))
    (loop for element in list2
	  do (setf (gethash element table) t))
    (loop for element in list1
	  always (gethash element table))))

(defun subsetp-key-eq-hash (list1 list2 key)
  (let ((table (make-hash-table :test #'eq)))
    (loop for element in list2
	  do (setf (gethash (funcall key element) table) t))
    (loop for element in list1
	  always (gethash (funcall key element) table))))

(defun subsetp-key-eql-hash (list1 list2 key)
  (let ((table (make-hash-table :test #'eql)))
    (loop for element in list2
	  do (setf (gethash (funcall key element) table) t))
    (loop for element in list1
	  always (gethash (funcall key element) table))))

(defun subsetp-key-equal-hash (list1 list2 key)
  (let ((table (make-hash-table :test #'equal)))
    (loop for element in list2
	  do (setf (gethash (funcall key element) table) t))
    (loop for element in list1
	  always (gethash (funcall key element) table))))

(defun subsetp-key-equalp-hash (list1 list2 key)
  (let ((table (make-hash-table :test #'equalp)))
    (loop for element in list2
	  do (setf (gethash (funcall key element) table) t))
    (loop for element in list1
	  always (gethash (funcall key element) table))))

(defun subsetp (list1 list2 &key key test test-not)
  (when (and (not (null test)) (not (null test-not)))
    (error 'both-test-and-test-not-given :name 'subsetp))
  (let ((use-hash (> (* (length list1) (length list2)) 1000)))
    (if key
	(if test
	    (cond ((or (eq test #'eq) (eq test 'eq))
		   (if use-hash
		       (subsetp-key-eq-hash list1 list2 key)
		       (subsetp-key-eq list1 list2 key)))
		  ((or (eq test #'eql) (eq test 'eql))
		   (if use-hash
		       (subsetp-key-eql-hash list1 list2 key)
		       (subsetp-key-eql list1 list2 key)))
		  ((or (eq test #'equal) (eq test 'equal))
		   (if use-hash
		       (subsetp-key-equal-hash list1 list2 key)
		       (subsetp-key-test list1 list2 key #'equal)))
		  ((or (eq test #'equalp) (eq test 'equalp))
		   (if use-hash
		       (subsetp-key-equalp-hash list1 list2 key)
		       (subsetp-key-test list1 list2 key #'equalp)))
		  (t
		   (subsetp-key-test list1 list2 key test)))
	    (if test-not
		(subsetp-key-test-not list1 list2 key test-not)
		(if use-hash
		    (subsetp-key-eql-hash list1 list2 key)
		    (subsetp-key-eql list1 list2 key))))
	(if test
	    (cond ((or (eq test #'eq) (eq test 'eq))
		   (if use-hash
		       (subsetp-identity-eq-hash list1 list2)
		       (subsetp-identity-eq list1 list2)))
		  ((or (eq test #'eql) (eq test 'eql))
		   (if use-hash
		       (subsetp-identity-eql-hash list1 list2)
		       (subsetp-identity-eql list1 list2)))
		  ((or (eq test #'equal) (eq test 'equal))
		   (if use-hash
		       (subsetp-identity-equal-hash list1 list2)
		       (subsetp-identity-test list1 list2 #'equal)))
		  ((or (eq test #'equalp) (eq test 'equalp))
		   (if use-hash
		       (subsetp-identity-equalp-hash list1 list2)
		       (subsetp-identity-test list1 list2 #'equalp)))
		  (t
		   (subsetp-identity-test list1 list2 test)))
	    (if test-not
		(subsetp-identity-test-not list1 list2 test-not)
		(if use-hash
		    (subsetp-identity-eql-hash list1 list2)
		    (subsetp-identity-eql list1 list2)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function getf

(defun getf (plist indicator &optional default)
  (unless (typep plist 'list)
    (error 'must-be-property-list
	   :datum plist
	   :name 'getf))
  (loop for rest on plist by #'cddr
	;; FIXME do this better
	do (assert (consp (cdr rest)))
	when (eq (car rest) indicator)
	  return (cadr rest)
	finally (return default)))

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
	;; FIXME do this better
	do (assert (consp (cdr rest)))
	   (let ((temp (member (car rest) indicator-list :test #'eq)))
	     (unless (null temp)
	       (return-from get-properties (values (car temp) (cadr rest) rest))))
	finally (return (values nil nil nil))))

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
			 do (assert (consp (cddr rest)))
			 when (eq (cadr rest) ,indicator-var)
			   do (setf (cdr rest) (cdddr rest))
			      (return t)))))))))

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
  (when (and (not (null test)) (not (null test-not)))
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
		    `(unless (member (funcall key ,item-var) ,(car store-vars)
				     :test test :key key)
		       (push ,item-var ,(car store-vars)))
		    (if test-not-p
			`(unless (member (funcall key ,item-var) ,(car store-vars)
					 :test-not test-not :key key)
			   (push ,item-var ,(car store-vars)))
			`(unless (member (funcall key ,item-var) ,(car store-vars)
					 :key key)
			   (push ,item-var ,(car store-vars)))))
		(if test-p
		    `(unless (member ,item-var ,(car store-vars)
				     :test test)
		       (push ,item-var ,(car store-vars)))
		    (if test-not-p
			`(unless (member ,item-var ,(car store-vars)
					 :test-not test-not)
			   (push ,item-var ,(car store-vars)))
			`(unless (member ,item-var ,(car store-vars))
			   (push ,item-var ,(car store-vars))))))
	   ,writer-form)))))

