(in-package :sicl-cons-high)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function append

;;; We need the append function in macro expanders in this module
;;; so we define it first.  

;;; It used to be the case that the append function was defined
;;; in terms of (loop ... append ...), but it turns out that
;;; some implementations have buggy versions of (loop ... append ...) 
;;; so it is better to just define it in terms of a lower-level
;;; construct.  Doing it this way allows for implementations with 
;;; a buggy loop to still use this module without first replacing
;;; loop by ours. 

(eval-when (:compile-toplevel :load-toplevel)
  (defun append (&rest lists)
    (let ((result nil)
          (frontier nil))
      (loop for list in lists
            do (if (null result)
                   (setf result list)
                   (progn (when (null frontier)
                            (setf result (cons (car result) (cdr result))
                                  frontier result))
                          (let ((next (cdr frontier)))
                            (loop until (atom next)
                                  do (setf next (cons (car next) (cdr next)))
                                     (setf (cdr frontier) next)
                                     (setf frontier next)
                                     (setf next (cdr next)))
                            (check-type next null "nil"))
                          (setf (cdr frontier) list))))
      result)))

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
  (let ((result nil)
        (frontier nil))
    (loop for list in lists
          do (if (null result)
                 (setf result list)
                 (progn (when (null frontier)
                          (setf frontier result))
                        (let ((next (cdr frontier)))
                          (loop until (atom next)
                                do (setf frontier next)
                                   (setf next (cdr next)))
                          (check-type next null "nil"))
                        (setf (cdr frontier) list))))
    result))

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

(defmacro define-c*r-function (function-name letters)
  (flet ((primitive (letter)
           (if (eql letter #\A) 'car 'cdr)))
    (flet ((one-iteration (letter prefix)
             `(progn (check-type remaining list "a list")
                     (if (null remaining)
                         (return-from ,function-name nil)
                         (if (consp remaining)
                             (setf remaining
                                   (,(primitive letter) remaining)))))))
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

(eval-when (:compile-toplevel :load-toplevel)
  (defmacro define-setf-c*r-function (function-name letters)
    (flet ((primitive (letter)
             (if (eql letter #\A) 'car 'cdr)))
      (flet ((one-iteration (letter prefix)
               `(progn (check-type remaining cons "a cons cell")
                       (setf remaining
                             (,(primitive letter) remaining)))))
        `(defun (setf ,function-name) (new-object list)
           (let ((remaining list))
             ,@(append (loop for letter across (reverse (subseq letters 1))
			     collect (one-iteration letter prefix)
			     collect letter into prefix)
		       `((check-type remaining cons "a cons cell")
                         (setf (,(primitive (aref letters 0)) remaining)
                               new-object)))))))))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function rest

(defun rest (list)
  (cdr list))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; type list

(deftype list () '(or cons null))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function list

;;; this implementation assumes that there is no 
;;; structure sharing between the &rest argument
;;; and the last argument to apply
(defun list (&rest elements)
  elements)

(define-compiler-macro list (&rest args)
  (if (null args)
      'nil
      `(cons ,(car args) (list ,@(cdr args)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function list*

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function last

(defun last (list &optional (n 1))
  (check-type list list "a list")
  (check-type n (integer 0) "a nonnegative integer")
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
  (check-type list list "a list")
  (loop for rest on list
	do (setf list rest))
  list)

(define-compiler-macro last (&whole form list &optional (n 1))
  (if (eql n 1)
      `(last-1 ,list)
      form))

(defun copy-list (list)
  (check-type list list "a list")
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
  (check-type length (integer 0) "a nonnegative integer")
  (loop repeat length
	collect initial-element))

;;; FIXME: Check at each iteration that we have a list. 
(defun nthcdr (n list)
  (check-type n (integer 0) "a nonnegative integer")
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
  (check-type list list "a list")
  (null list))

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
	       ,@(apply #'append (loop for var in vars
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
	       ,@(apply #'append (loop for var in vars
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
	       ,@(apply #'append (loop for var in vars
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
	       ,@(apply #'append (loop for var in vars
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
	       ,@(apply #'append (loop for var in vars
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
	       ,@(apply #'append (loop for var in vars
				       for list in lists
				       collect `(for ,var on ,list)))
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

(defun butlast (list &optional (n 1))
  (check-type list list "a list")
  (check-type n (integer 0) "a nonnegative integer")
  (let ((remaining list))
    (loop repeat n
	  until (atom remaining)
	  do (pop remaining))
    (loop until (atom remaining)
          do (pop remaining)
          collect (pop list))))

;;; There is probably no point in making a special version of 
;;; butlast for n = 1, because the time is going to be dominated
;;; by consing up the new list anyawy. 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function nbutlast

;;; Special version for n = 1.  This version avoids that 
;;; the list be traversed several times, and since list traversal
;;; dominates the time here, this is a win.

(defun nbutlast-1 (list)
  (check-type list list "a list")
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

(defun nbutlast (list &optional (n 1))
  (check-type list list "a list")
  (check-type n (integer 0) "a nonnegative integer")
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

;;; Special version if test-not is eq and key is identity.

(defun subst-not-eq-identity (new old tree)
  (labels ((traverse (tree)
             (cond ((not (eq tree old)) new)
                   ((atom tree) tree)
                   (t (cons (traverse (car tree)) (traverse (cdr tree)))))))
    (traverse tree)))

;;; Special version if test-not is eql and key is identity.

(defun subst-not-eql-identity (new old tree)
  (labels ((traverse (tree)
             (cond ((not (eql tree old)) new)
                   ((atom tree) tree)
                   (t (cons (traverse (car tree)) (traverse (cdr tree)))))))
    (traverse tree)))

;;; Special version if test-not is eq and key is given.

(defun subst-not-eq-key (new old tree key)
  (labels ((traverse (tree)
             (cond ((not (eq (funcall key tree) old)) new)
                   ((atom tree) tree)
                   (t (cons (traverse (car tree)) (traverse (cdr tree)))))))
    (traverse tree)))

;;; Special version if test-not is eql and key is given.

(defun subst-not-eql-key (new old tree key)
  (labels ((traverse (tree)
             (cond ((not (eql (funcall key tree) old)) new)
                   ((atom tree) tree)
                   (t (cons (traverse (car tree)) (traverse (cdr tree)))))))
    (traverse tree)))

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
  ;; FIXME do this better
  (assert (or (null test) (null test-not)))
  (if key
      (if test
          (if (or (eq test #'eq) (eq test 'eq))
              (subst-eq-key new old tree key)
              (if (or (eq test #'eql) (eq test 'eql))
                  (subst-eql-key new old tree key)
                  (subst-test-key  new old tree test key)))
          (if test-not
              (if (or (eq test-not #'eq) (eq test-not 'eq))
                  (subst-not-eq-key new old tree key)
                  (if (or (eq test-not #'eql) (eq test-not 'eql))
                      (subst-not-eql-key new old tree key)
                      (subst-test-not-key  new old tree test key)))
              (subst-eql-key new old tree key)))
      (if test
          (if (or (eq test #'eq) (eq test 'eq))
              (subst-eq-identity new old tree)
              (if (or (eq test #'eql) (eq test 'eql))
                  (subst-eql-identity new old tree)
                  (subst-test-identity  new old tree test)))
          (if test-not
              (if (or (eq test-not #'eq) (eq test-not 'eq))
                  (subst-not-eq-identity new old tree)
                  (if (or (eq test-not #'eql) (eq test-not 'eql))
                      (subst-not-eql-identity new old tree)
                      (subst-test-not-identity  new old tree test)))
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

;;; Special version if test-not is eq and key is identity.

(defun nsubst-not-eq-identity (new old tree)
  (labels ((traverse (tree)
             (cond ((not (eq (car tree) old))
                    (setf (car tree) new))
                   ((atom (car tree))
                    nil)
                   (t
                    (traverse (car tree))))
             (cond ((not (eq (cdr tree) old))
                    (setf (cdr tree) new))
                   ((atom (cdr tree))
                    nil)
                   (t
                    (traverse (cdr tree))))))
    (cond ((not (eq tree old)) new)
          ((atom tree) tree)
          (t (traverse tree) tree))))

;;; Special version if test-not is eql and key is identity.

(defun nsubst-not-eql-identity (new old tree)
  (labels ((traverse (tree)
             (cond ((not (eql (car tree) old))
                    (setf (car tree) new))
                   ((atom (car tree))
                    nil)
                   (t
                    (traverse (car tree))))
             (cond ((not (eql (cdr tree) old))
                    (setf (cdr tree) new))
                   ((atom (cdr tree))
                    nil)
                   (t
                    (traverse (cdr tree))))))
    (cond ((not (eql tree old)) new)
          ((atom tree) tree)
          (t (traverse tree) tree))))

;;; Special version if test-not is eq and key is given.

(defun nsubst-not-eq-key (new old tree key)
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

;;; Special version if test-not is eql and key is given.

(defun nsubst-not-eql-key (new old tree key)
  (labels ((traverse (tree)
             (cond ((not (eql (funcall key (car tree)) old))
                    (setf (car tree) new))
                   ((atom (car tree))
                    nil)
                   (t
                    (traverse (car tree))))
             (cond ((not (eql (funcall key (cdr tree)) old))
                    (setf (cdr tree) new))
                   ((atom (cdr tree))
                    nil)
                   (t
                    (traverse (cdr tree))))))
    (cond ((not (eql (funcall key tree) old)) new)
          ((atom tree) tree)
          (t (traverse tree) tree))))

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
    (cond ((not (funcall test tree old)) new)
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
  ;; FIXME do this better
  (assert (or (null test) (null test-not)))
  (if key
      (if test
          (if (or (eq test #'eq) (eq test 'eq))
              (nsubst-eq-key new old tree key)
              (if (or (eq test #'eql) (eq test 'eql))
                  (nsubst-eql-key new old tree key)
                  (nsubst-test-key  new old tree test key)))
          (if test-not
              (if (or (eq test-not #'eq) (eq test-not 'eq))
                  (nsubst-not-eq-key new old tree key)
                  (if (or (eq test-not #'eql) (eq test-not 'eql))
                      (nsubst-not-eql-key new old tree key)
                      (nsubst-test-not-key  new old tree test key)))
              (nsubst-eql-key new old tree key)))
      (if test
          (if (or (eq test #'eq) (eq test 'eq))
              (nsubst-eq-identity new old tree)
              (if (or (eq test #'eql) (eq test 'eql))
                  (nsubst-eql-identity new old tree)
                  (nsubst-test-identity  new old tree test)))
          (if test-not
              (if (or (eq test-not #'eq) (eq test-not 'eq))
                  (nsubst-not-eq-identity new old tree)
                  (if (or (eq test-not #'eql) (eq test-not 'eql))
                      (nsubst-not-eql-identity new old tree)
                      (nsubst-test-not-identity  new old tree test)))
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
    (cond ((funcall predicate tree) new)
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
    (cond ((not (funcall predicate tree)) new)
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
        when (eq (car element) item)
          return element))

;;; Special version when test is eql and key is identity.

(defun assoc-eql-identity (item alist)
  (loop for element in alist
        when (eql (car element) item)
          return element))
        
;;; Special version when test is eq and key is given.

(defun assoc-eq-key (item alist key)
  (loop for element in alist
        when (eq (funcall key (car element)) item)
          return element))
  
;;; Special version when test is eql and key is given.

(defun assoc-eql-key (item alist key)
  (loop for element in alist
        when (eql (funcall key (car element)) item)
          return element))

;;; Special version when test is given and key is identity.

(defun assoc-test-identity (item alist test)
  (loop for element in alist
        when (funcall test (car element) item)
          return element))

;;; Special version when test and key are both given.

(defun assoc-test-key (item alist test key)
  (loop for element in alist
        when (funcall test (funcall key (car element)) item)
          return element))
  
;;; Special version when test-not is eq and key is identity.

(defun assoc-not-eq-identity (item alist)
  (loop for element in alist
        when (not (eq (car element) item))
          return element))

;;; Special version when test-not is eql and key is identity.

(defun assoc-not-eql-identity (item alist)
  (loop for element in alist
        when (not (eql (car element) item))
          return element))
        
;;; Special version when test-not is eq and key is given.

(defun assoc-not-eq-key (item alist key)
  (loop for element in alist
        when (not (eq (funcall key (car element)) item))
          return element))
  
;;; Special version when test-not is eql and key is given.

(defun assoc-not-eql-key (item alist key)
  (loop for element in alist
        when (not (eql (funcall key (car element)) item))
          return element))

;;; Special version when test-not is given and key is identity.

(defun assoc-test-not-identity (item alist test)
  (loop for element in alist
        when (not (funcall test (car element) item))
          return element))

;;; Special version when test-not and key are both given.

(defun assoc-test-not-key (item alist test key)
  (loop for element in alist
        when (not (funcall test (funcall key (car element)) item))
          return element))
  
(defun assoc (item alist &key key test test-not)
  ;; FIXME: do this better
  (assert (or (null test) (null test-not)))
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
        when (funcall predicate (car element))
          return element))

;;; Special version when key is given

(defun assoc-if-key (predicate alist key)
  (loop for element in alist
        when (funcall predicate (funcall key (car element)))
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
        when (not (funcall predicate (car element)))
          return element))

;;; Special version when key is given

(defun assoc-if-not-key (predicate alist key)
  (loop for element in alist
        when (not (funcall predicate (funcall key (car element))))
          return element))

(defun assoc-if-not (predicate alist &key key)
  (if key
      (assoc-if-not-key predicate alist key)
      (assoc-if-not-identity predicate alist)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function sublis

;;; Special case when test is eq and key is identity

(defun sublis-eq-identity (alist tree)
  (labels ((traverse (tree)
             (let ((entry (assoc-eq-identity tree alist)))
               (cond ((not (null entry)) (cdr entry))
                     ((atom tree) tree)
                     (t (cons (traverse (car tree))
                              (traverse (cdr tree))))))))
    (traverse tree)))

;;; Special case when test is eql and key is identity

(defun sublis-eql-identity (alist tree)
  (labels ((traverse (tree)
             (let ((entry (assoc-eql-identity tree alist)))
               (cond ((not (null entry)) (cdr entry))
                     ((atom tree) tree)
                     (t (cons (traverse (car tree))
                              (traverse (cdr tree))))))))
    (traverse tree)))

;;; Special case when test is eq and key is given.

(defun sublis-eq-key (alist tree key)
  (labels ((traverse (tree)
             (let ((entry (assoc-eq-key tree alist key)))
               (cond ((not (null entry)) (cdr entry))
                     ((atom tree) tree)
                     (t (cons (traverse (car tree))
                              (traverse (cdr tree))))))))
    (traverse tree)))

;;; Special case when test is eql and key is given.

(defun sublis-eql-key (alist tree key)
  (labels ((traverse (tree)
             (let ((entry (assoc-eql-key tree alist key)))
               (cond ((not (null entry)) (cdr entry))
                     ((atom tree) tree)
                     (t (cons (traverse (car tree))
                              (traverse (cdr tree))))))))
    (traverse tree)))

;;; Special case when test is given and key is identity.

(defun sublis-test-identity (alist tree test)
  (labels ((traverse (tree)
             (let ((entry (assoc-test-identity tree alist test)))
               (cond ((not (null entry)) (cdr entry))
                     ((atom tree) tree)
                     (t (cons (traverse (car tree))
                              (traverse (cdr tree))))))))
    (traverse tree)))

;;; Special case when test and key are both given.

(defun sublis-test-key (alist tree test key)
  (labels ((traverse (tree)
             (let ((entry (assoc-test-key tree alist test key)))
               (cond ((not (null entry)) (cdr entry))
                     ((atom tree) tree)
                     (t (cons (traverse (car tree))
                              (traverse (cdr tree))))))))
    (traverse tree)))

;;; Special case when test-not is eq and key is identity

(defun sublis-not-eq-identity (alist tree)
  (labels ((traverse (tree)
             (let ((entry (assoc-not-eq-identity tree alist)))
               (cond ((not (null entry)) (cdr entry))
                     ((atom tree) tree)
                     (t (cons (traverse (car tree))
                              (traverse (cdr tree))))))))
    (traverse tree)))

;;; Special case when test-not is eql and key is identity

(defun sublis-not-eql-identity (alist tree)
  (labels ((traverse (tree)
             (let ((entry (assoc-not-eql-identity tree alist)))
               (cond ((not (null entry)) (cdr entry))
                     ((atom tree) tree)
                     (t (cons (traverse (car tree))
                              (traverse (cdr tree))))))))
    (traverse tree)))

;;; Special case when test-not is eq and key is given.

(defun sublis-not-eq-key (alist tree key)
  (labels ((traverse (tree)
             (let ((entry (assoc-not-eq-key tree alist key)))
               (cond ((not (null entry)) (cdr entry))
                     ((atom tree) tree)
                     (t (cons (traverse (car tree))
                              (traverse (cdr tree))))))))
    (traverse tree)))

;;; Special case when test-not is eql and key is given.

(defun sublis-not-eql-key (alist tree key)
  (labels ((traverse (tree)
             (let ((entry (assoc-not-eql-key tree alist key)))
               (cond ((not (null entry)) (cdr entry))
                     ((atom tree) tree)
                     (t (cons (traverse (car tree))
                              (traverse (cdr tree))))))))
    (traverse tree)))

;;; Special case when test-not is given and key is identity.

(defun sublis-test-not-identity (alist tree test)
  (labels ((traverse (tree)
             (let ((entry (assoc-test-not-identity tree alist test)))
               (cond ((not (null entry)) (cdr entry))
                     ((atom tree) tree)
                     (t (cons (traverse (car tree))
                              (traverse (cdr tree))))))))
    (traverse tree)))

;;; Special case when test-not and key are both given.

(defun sublis-test-not-key (alist tree test key)
  (labels ((traverse (tree)
             (let ((entry (assoc-test-not-key tree alist test key)))
               (cond ((not (null entry)) (cdr entry))
                     ((atom tree) tree)
                     (t (cons (traverse (car tree))
                              (traverse (cdr tree))))))))
    (traverse tree)))

(defun sublis (alist tree &key key test test-not)
  ;; FIXME: do this better
  (assert (or (null test) (null test-not)))
  (if key
      (if test
          (if (or (eq test #'eq) (eq test 'eq))
              (sublis-eq-key alist tree key)
              (if (or (eq test #'eql) (eq test 'eql))
                  (sublis-eql-key alist tree key)
                  (sublis-test-key alist tree test key)))
          (if test-not
              (if (or (eq test-not #'eq) (eq test-not 'eq))
                  (sublis-not-eq-key alist tree key)
                  (if (or (eq test-not #'eql) (eq test-not 'eql))
                      (sublis-not-eql-key alist tree key)
                      (sublis-test-not-key alist tree test-not key)))
              (sublis-eql-key alist tree key)))
      (if test
          (if (or (eq test #'eq) (eq test 'eq))
              (sublis-eq-identity alist tree)
              (if (or (eq test #'eql) (eq test 'eql))
                  (sublis-eql-identity alist tree)
                  (sublis-test-identity alist tree test)))
          (if test-not
              (if (or (eq test-not #'eq) (eq test-not 'eq))
                  (sublis-not-eq-identity alist tree)
                  (if (or (eq test-not #'eql) (eq test-not 'eql))
                      (sublis-not-eql-identity alist tree)
                      (sublis-test-not-identity alist tree test-not)))
              (sublis-eql-identity alist tree)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function member

(defun member-eq-identity (item list)
  (loop for rest on list
        when (eq item (car rest))
          return rest))  

(defun member-eq-key (item list key)
  (loop for rest on list
        when (eq item (funcall key (car rest)))
          return rest))  

(defun member-not-eq-identity (item list)
  (loop for rest on list
        when (not (eq item (car rest)))
          return rest))  

(defun member-not-eq-key (item list key)
  (loop for rest on list
        when (not (eq item (funcall key (car rest))))
          return rest))  

(defun member-eql-identity (item list)
  (loop for rest on list
        when (eql item (car rest))
          return rest))  

(defun member-eql-key (item list key)
  (loop for rest on list
        when (eql item (funcall key (car rest)))
          return rest))  

(defun member-not-eql-identity (item list)
  (loop for rest on list
        when (not (eql item (car rest)))
          return rest))  

(defun member-not-eql-key (item list key)
  (loop for rest on list
        when (not (eql item (funcall key (car rest))))
          return rest))  

(defun member-test-identity (item list test)
  (loop for rest on list
        when (funcall test item (car rest))
          return rest))  

(defun member-test-key (item list test key)
  (loop for rest on list
        when (funcall test item (funcall key (car rest)))
          return rest))

(defun member-test-not-identity (item list test)
  (loop for rest on list
        when (not (funcall test item (car rest)))
          return rest))

(defun member-test-not-key (item list test key)
  (loop for rest on list
        when (not (funcall test item (funcall key (car rest))))
          return rest))

(defun member (item list &key key test test-not)
  ;; FIXME: do this better
  (assert (or (null test) (null test-not)))
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

