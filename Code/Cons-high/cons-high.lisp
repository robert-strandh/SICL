(in-package :sicl-cons-high)

;;; We need a version of append for many of the expansions.  But since
;;; we don't have one yet, we define our own version of it, and use
;;; that version instead.

(eval-when (:compile-toplevel :load-toplevel)
  (defun xappend (&rest lists)
    (loop for list in lists
	  append list)))

;;; We want for error messages to be phrased in terms of a construct
;;; that was directly used by the user's code.  So for instance, if
;;; the user code had a call to CADDR, giving it a list where the CDDR
;;; is not a list, we would like the error message to mention that,
;;; and not for instance that CAR was given a non-list. 

(eval-when (:compile-toplevel :load-toplevel)
  (defun generate-c*r-message (function-name prefix)
    (if (null prefix)
        (format nil
                "The function ~a was given an argument~@
               that is neither NIL nor a CONS cell"
                function-name)
        (format nil
                "The function ~a was given a list whose ~@
               C~{~a~}R is neither NIL nor a CONS cell"
                function-name
                (reverse prefix)))))

;;; FIXME fix the error messsage to signal a particular condition
(defmacro define-c*r-function (function-name letters)
  (flet ((primitive (letter)
           (if (eql letter #\A) 'car 'cdr)))
    (flet ((one-iteration (letter prefix)
             `(if (null remaining)
                  (return-from ,function-name nil)
                  (if (consp remaining)
                      (setf remaining
                            (,(primitive letter) remaining))
                      (error ,(generate-c*r-message function-name
                                                    prefix))))))
      `(defun ,function-name (list)
         (let ((remaining list))
           ,@(loop for letter across (reverse letters)
                   collect (one-iteration letter prefix)
                   collect letter into prefix)
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
  (defun generate-setf-c*r-message (function-name prefix)
    (if (null prefix)
        (format nil
                "The function (SETF ~a) was given an argument~@
                that is not a CONS cell"
                function-name)
        (format nil
                "The function (SETF ~a) was given a list whose ~@
                C~{~a~}R is not a CONS cell"
                function-name
                (reverse (coerce prefix 'cons))))))

;;; FIXME fix the error messsage to signal a particular condition
(eval-when (:compile-toplevel :load-toplevel)
  (defmacro define-setf-c*r-function (function-name letters)
    (flet ((primitive (letter)
             (if (eql letter #\A) 'car 'cdr)))
      (flet ((one-iteration (letter prefix)
               `(if (consp remaining)
                    (setf remaining
                          (,(primitive letter) remaining))
                    (error ,(generate-setf-c*r-message function-name
                                                                   prefix)))))
        `(defun (setf ,function-name) (new-object list)
           (let ((remaining list))
             ,@(xappend (loop for letter across (reverse (subseq letters 1))
			      collect (one-iteration letter prefix)
			      collect letter into prefix)
			`(if (consp remaining)
			     (setf (,(primitive (aref letters 0)) remaining)
				   new-object)
			     (error ,(generate-setf-c*r-message
				      function-name
				      (subseq letters 1)))))))))))

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

;; (define-setf-c*r-function first   "A")
(define-setf-c*r-function second  "AD")
(define-setf-c*r-function third   "ADD")
(define-setf-c*r-function fourth  "ADDD")
(define-setf-c*r-function fifth   "ADDDD")
(define-setf-c*r-function sixth   "ADDDDD")
(define-setf-c*r-function seventh "ADDDDDD")
(define-setf-c*r-function eighth  "ADDDDDDD")
(define-setf-c*r-function ninth  "ADDDDDDDD")
(define-setf-c*r-function tenth   "ADDDDDDDDD")

;;; this implementation assumes that there is no 
;;; structure sharing between the &rest argument
;;; and the last argument to apply
(defun list (&rest elements)
  elements)

(define-compiler-macro list (&rest args)
  (if (null args)
      'nil
      `(cons ,(car args) (list ,@(cdr args)))))

;;; this implementation assumes that there is no 
;;; structure sharing between the &rest argument
;;; and the last argument to apply
;;; FIXME: fix the assert to signal a particular condition
(defun list* (&rest elements)
  (assert (not (null elements)))
  (if (null (cdr elements))
      (car elements)
      (loop for remaining on elements
	    until (null (cddr remaining))
	    finally (setf (cdr remaining)
			  (cadr remaining)))))

(define-compiler-macro list* (&whole form &rest args)
  (cond  ((null args) form)
	 ((null (cdr args)) (car args))
	 (t `(cons ,(car args) (list* ,@(cdr args))))))

(defun last (list &optional (n 1))
  (check-type list list "a list")
  (check-type n (and integer (not (satisfies minusp))) "a nonnegative integer")
  (let ((remaining list))
    (loop repeat n
	  until (atom remaining)
	  do (pop remaining))
    (loop until (atom remaining)
	  do (pop list)
	  do (pop remaining))
    list))

;;; special version of last used when the second argument to
;;; last is 1. 
(defun last-1 (list)
  (assert (listp list)
	  (list)
	  must-be-list :datum list)
  (loop for rest on list
	do (setf list rest))
  list)

(define-compiler-macro last (&whole form list &optional (n 1))
  (if (eql n 1)
      `(last-1 ,list)
      form))

(defun copy-list (list)
  (assert (listp list))
  (if (null list)
      nil
      (let* ((result (list (car list)))
	     (trailer result))
	(loop do (pop list)
	      until (atom list)
	      ;; list is not an atom, allocate a new cell
	      ;; at the end of the result list
	      do (setf (cdr trailer) (list (car list)))
	      do (pop trailer))
	;; when we come here, list is an atom,
	;; either NIL because the list was a proper list
	;; or some other atom because it was not a proper list
	(setf (cdr trailer) list)
	result)))

;;; The standard requires the argument to be either a circular list
;;; or a proper list.  In case of a circular list, NIL should be 
;;; returned.  
;;; FIXME make sure a type error is returned if the list is a dotted list. 
(defun list-length (list)
  (loop for length from 0 by 2
	for slow on list
	for fast on list by #'cddr
	do (cond ((endp (cdr fast)) (return (1+ length)))
		 ((and (eq fast slow) (plusp length)) (return nil)))
	finally (assert (listp fast)) (return length)))

(defun make-list (length &key (initial-element nil))
  (assert (and (integerp length) (>= length 0)))
  (loop repeat length
	collect initial-element))

;;; FIXME: Check at each iteration that we have a list. 
(defun nthcdr (n list)
  (assert (and (integerp n) (>= n 0)))
  (loop repeat n
	until (null list)
	do (pop list))
  list)

;;; FIXME: Iterate and check at each iteration that we have a list. 
(defun nth (n list)
  (car (nthcdr n list)))

;;; FIXME: Iterate and check at each iteration that we have a list. 
(defun (setf nth) (object n list)
  (setf (car (nthcdr n list)) object))

(defun copy-tree (tree)
  (if (atom tree)
      tree
      (cons (copy-tree (car tree)) (copy-tree (cdr tree)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function tree-equal 

(defun tree-equal-eq (tree1 tree2)
  (or (eq tree1 tree2)
      (and (consp tree1)
           (consp tree2)
           (tree-equal-eq (car tree1) (car tree2))
           (tree-equal-eq (cdr tree1) (cdr tree2)))))

(defun tree-equal-eql (tree1 tree2)
  (or (eql tree1 tree2)
      (and (consp tree1)
           (consp tree2)
           (tree-equal-eql (car tree1) (car tree2))
           (tree-equal-eql (cdr tree1) (cdr tree2)))))

(defun tree-equal-not-eq (tree1 tree2)
  (or (not (eq tree1 tree2))
      (and (consp tree1)
           (consp tree2)
           (tree-equal-not-eq (car tree1) (car tree2))
           (tree-equal-not-eq (cdr tree1) (cdr tree2)))))

(defun tree-equal-not-eql (tree1 tree2)
  (or (not (eql tree1 tree2))
      (and (consp tree1)
           (consp tree2)
           (tree-equal-not-eql (car tree1) (car tree2))
           (tree-equal-not-eql (cdr tree1) (cdr tree2)))))

(defun tree-equal-test (tree1 tree2 test)
  (or (funcall test tree1 tree2)
      (and (consp tree1)
           (consp tree2)
           (tree-equal-test (car tree1) (car tree2) test)
           (tree-equal-test (cdr tree1) (cdr tree2) test))))

(defun tree-equal-test-not (tree1 tree2 test)
  (or (not (funcall test tree1 tree2))
      (and (consp tree1)
           (consp tree2)
           (tree-equal-test-not (car tree1) (car tree2) test)
           (tree-equal-test-not (cdr tree1) (cdr tree2) test))))

(defun tree-equal (tree1 tree2
                   &key
                   (test nil testp)
                   (test-not nil test-not-p))
  ;; FIXME Do this better
  (assert (or (null testp) (null test-not-p)))
  (if testp
      (if (eq test #'eq)
          (tree-equal-eq tree1 tree2)
          (if (eq test #'eql)
              (tree-equal-eql tree1 tree2)
              (tree-equal-test tree1 tree2 test)))
      (if (eq test-not #'eq)
          (tree-equal-not-eq tree1 tree2)
          (if (eq test #'eql)
              (tree-equal-not-eql tree1 tree2)
              (tree-equal-test-not tree1 tree2 test-not)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function endp

(defun endp (list)
  (cond ((null list) t)
        ((consp list) nil)
        (t (error (make-instance 'type-error
                                 :datum list
                                 :expected-type 'cl:list)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function mapcar

(defun mapcar (function &rest lists)
  ;; FIXME: do this better
  (assert (not (null lists)))
  (loop for remaining = lists
	  then (loop for list in remaining collect (cdr list))
	until (loop for list in remaining thereis (null list))
	collect (apply function
		       (loop for list in remaining collect (car list)))))

;;; The compiler macro for mapcar generates code to loop
;;; individually over each list given, and thus avoids having
;;; a list of lists (which implies consing).  Since the number
;;; of lists given is known, we can use funcall instead of 
;;; apply when we call the function. 
(define-compiler-macro mapcar (&whole form function &rest lists)
  (if (null lists)
      form
      (let ((funvar (gensym))
	    (vars (loop for var in lists collect (gensym))))
	`(loop with ,funvar = ,function
	       ,@(apply #'xappend (loop for var in vars
					for list in lists
					collect `(for ,var in ,list)))
	       collect (funcall ,funvar ,@vars)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function mapc

(defun mapc (function &rest lists)
  ;; FIXME: do this better
  (assert (not (null lists)))
  (loop for remaining = lists
	  then (loop for list in remaining collect (cdr list))
	until (loop for list in remaining thereis (null list))
	do (apply function
		  (loop for list in remaining collect (car list))))
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
	    (firstlist (gensym))
	    (vars (loop for var in lists collect (gensym))))
	`(loop with ,funvar = ,function
	       with ,firstlist = ,(car lists)
	       ,@(apply #'xappend (loop for var in vars
					for list in (cons firstlist (cdr lists))
					collect `(for ,var in ,list)))
	       do (funcall ,funvar ,@vars)
	       finally (return ,firstlist)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function maplist

(defun maplist (function &rest lists)
  ;; FIXME: do this better
  (assert (not (null lists)))
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
	       ,@(apply #'xappend (loop for var in vars
					for list in lists
					collect `(for ,var on ,list)))
	       collect (funcall ,funvar ,@vars)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function mapl

(defun mapl (function &rest lists)
  ;; FIXME: do this better
  (assert (not (null lists)))
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
	       ,@(apply #'xappend (loop for var in vars
					for list in (cons firstlist (cdr lists))
					collect `(for ,var on ,list)))
	       do (funcall ,funvar ,@vars)
	       finally (return ,firstlist)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function mapcan

(defun mapcan (function &rest lists)
  ;; FIXME: do this better
  (assert (not (null lists)))
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
	       ,@(apply #'xappend (loop for var in vars
					for list in lists
					collect `(for ,var in ,list)))
	       nconc (funcall ,funvar ,@vars)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function mapcon

(defun mapcon (function &rest lists)
  ;; FIXME: do this better
  (assert (not (null lists)))
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
	       ,@(apply #'xappend (loop for var in vars
					for list in lists
					collect `(for ,var on ,list)))
	       nconc (funcall ,funvar ,@vars)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function append

(defun append (&rest lists)
  (let ((remaining lists))
    ;; Remove a prefix of lists containing only empty lists.
    (loop until (or (null remaining) (not (null (car remaining))))
	  do (pop remaining))
    (cond ((null remaining)
	   ;; All the lists given were empty.
	   '())
	  ((null (cdr remaining))
	   ;; Only the last list given was nonempty.
	   ;; Don't copy it in this case. 
	   (car remaining))
	  (t
	   ;; The first list of remaining is non empty,
	   ;; and there are more lists after it.
	   ;; Start duplication by copying the first
	   ;; cons cell of the list that is the first
	   ;; element of remaining.  Keep two pointers
	   ;; to that cell; one that remains intact, to
	   ;; be returned in the end, and another that
	   ;; points to the last cell having been copied. 
	   ;; current-list contains the remaining cells
	   ;; to be copied of the current list. 
	   (let* ((current-list (cdr (car remaining)))
		  (last-cell (list (car (car remaining))))
		  (retval last-cell))
	     (pop remaining)
	     (loop until (null remaining)
		   ;; copy the cells of current list
		   do (loop until (null current-list)
			    do (setf (cdr last-cell)
				     (list (pop current-list)))
			       ;; make last-cell point to the
			       ;; last cell copied. 
			       (pop last-cell))
		      ;; we have copied all the cells of
		      ;; current-list. 
		      (setf current-list (pop remaining))
		   finally (setf (cdr last-cell) current-list)
			   (return retval)))))))
