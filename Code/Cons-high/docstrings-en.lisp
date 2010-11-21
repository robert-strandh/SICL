(in-package #:sicl-cons-high)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun fmt (&rest args)
    (apply #'format nil args)))

;;; Create documentation for a function.
(defun fundoc (name string)
  (setf (documentation name 'function) string)
  (setf (documentation (fdefinition name) 'function)
        (documentation name 'function)))

(fundoc 'car
        (fmt "Lambda list: (OBJECT)~@
              When OBJECT is a CONS cell, return the CAR of that cell.~@
              When OBJECT is NIL, return NIL."))

(fundoc 'cdr
        (fmt "Lambda list: (OBJECT)~@
              When OBJECT is a CONS cell, return the CDR of that cell.~@
              When OBJECT is NIL, return NIL."))

(defmacro make-c*r-documentation (function-name)
  (let* ((name (symbol-name function-name))
         (letters (reverse (cdr (butlast (coerce name 'list))))))
    (flet ((primitive (letter)
             (if (eql letter #\A) 'car 'cdr)))
      `(fundoc ',function-name
                ,(fmt "Lambda list: (OBJECT)~@
                       Equivalent to ~a"
                      (loop with form = 'object
                            for letter in letters
                            do (setf form
                                     (list (primitive letter) form))
                            finally (return form)))))))
             
(make-c*r-documentation caar)
(make-c*r-documentation cadr)
(make-c*r-documentation cdar)
(make-c*r-documentation cddr)
(make-c*r-documentation caaar)
(make-c*r-documentation caadr)
(make-c*r-documentation cadar)
(make-c*r-documentation caddr)
(make-c*r-documentation cdaar)
(make-c*r-documentation cdadr)
(make-c*r-documentation cddar)
(make-c*r-documentation cdddr)
(make-c*r-documentation caaaar)
(make-c*r-documentation caaadr)
(make-c*r-documentation caadar)
(make-c*r-documentation caaddr)
(make-c*r-documentation cadaar)
(make-c*r-documentation cadadr)
(make-c*r-documentation caddar)
(make-c*r-documentation cadddr)
(make-c*r-documentation cdaaar)
(make-c*r-documentation cdaadr)
(make-c*r-documentation cdadar)
(make-c*r-documentation cdaddr)
(make-c*r-documentation cddaar)
(make-c*r-documentation cddadr)
(make-c*r-documentation cdddar)
(make-c*r-documentation cddddr)

(fundoc 'list
        (fmt "Lambda list: (&rest OBJECTS)~@
              Return a list containing the objects in OBJECTS."))

(fundoc 'list*
        (fmt "Lambda list: (&rest OBJECTS)~@
              At least one argument must be given.~@
              Return a list containing the objects in OBJECTS,~@
              except that the last object in OBJECTS becomes the~@
              CDR of the last CONS cell created.~@
              When given a single argument, return that argument."))

(fundoc 'first
        (fmt "Lambda list: (LIST)~@
              Return the first element of the list LIST.~@
              When LIST is neither a list nor NIL,~@
              an error is signaled."))

(defmacro make-nth-documentation (function-name number)
  `(fundoc ',function-name
            ,(fmt "Lambda list: (LIST)~@
                   Return the ~a element of the list LIST.~@
                   When LIST is a proper list with fewer than ~a element,~@
                   NIL is returned.~@
                   When LIST is not a proper list, and it has fewer than~@
                   ~a elements, an error is signaled.~@
                   In particular, when LIST is neither a list nor NIL,~@
                   an error is signaled."
                  (string-downcase (symbol-name function-name))
                  number
                  number)))

(make-nth-documentation second  "two")
(make-nth-documentation third   "three")
(make-nth-documentation fourth  "four")
(make-nth-documentation fifth   "five")
(make-nth-documentation sixth   "six")
(make-nth-documentation seventh "seven")
(make-nth-documentation eighth  "eight")
(make-nth-documentation ninth   "nine")
(make-nth-documentation tenth   "ten")

(fundoc 'cons
        (fmt "Lambda list: (OBJECT-1 OBJECT-2)~@
              Return a new CONS cell with OBJECT-1 in the~@
              CAR field and OBJECT-2 in the CDR field."))

(fundoc 'nth
        (fmt "Lambda list: (N LIST)~@
              where N is a non-negative integer~@
              and LIST is a (not necessarily proper) list.~@
              Return the Nth element of the list LIST~@
              where the first element is the zeroth.~@
              When LIST is not a proper list, and it has fewer than~@
              N+1 elements, an error is signaled.~@
              In particular, when LIST is neither a list nor NIL,~@
              an error is signaled.~@
              When N is not a non-negative integer, an error~@
              of type TYPE-ERROR is signaled."))

(fundoc 'nthcdr
        (fmt "Lambda list: (N LIST)~@
              where N is a non-negative integer~@
              and LIST is a (not necessarily proper) list.~@
              Return the result of calling CDR N times on LIST.~@
              When LIST is not a proper list, and it has fewer than~@
              N elements, an error is signaled.~@
              In particular, when LIST is neither a list nor NIL,~@
              an error is signaled.~@
              When N is not a non-negative integer, an error~@
              of type TYPE-ERROR is signaled."))
              
(fundoc 'consp
	(fmt "Lambda list: (OBJECT)~@
              where OBJECT is any object.~@
              Return true if OBJECT is a cons cell.~@
              Return false otherwise."))

(fundoc 'atom
	(fmt "Lambda list: (OBJECT)~@
              where OBJECT is any object.~@
              Return true if OBJECT is an atom~@
              (i.e., not a cons cell).~@
              Return false otherwise."))

(fundoc 'rplaca
	(fmt "Lambda list: (CONS OBJECT)~@
              where CONS is a cons cell and OBJECT is any object.~@
              Replace the the CAR of the CONS cell by OBJECT.~@
              Return the modified cons cell.~@
              The difference between RPLACA and (SETF CAR) is that~@
              (SETF CAR) returns OBJECT instead of the CONS."))

(fundoc 'rplacd
	(fmt "Lambda list: (CONS OBJECT)~@
              where CONS is a cons cell and OBJECT is any object.~@
              Replace the the CDR of the CONS cell by OBJECT.~@
              Return the modified cons cell.~@
              The difference between RPLACD and (SETF CDR) is that~@
              (SETF CDR) returns OBJECT instead of the CONS."))

(fundoc 'copy-tree
	(fmt "Lambda list: (TREE)~@
              where TREE is any tree consisting of internal nodes~@
              in the form of CONS cells, and leaves in the form of atoms.~@
              Return a copy of the tree, i.e. a tree which is like TREE,~@
              except that every CONS cell has been copied."))

(fundoc 'sublis
	(fmt "Lambda list: (ALIST TREE &key KEY TEST TEST-NOT)~@
              where ALIST is an association list, TREE is a tree~@
              consisting of internal nodes in the form of CONS cells,~@
              and leaves in the form of atoms.  KEY is a designator for~@
              a function of one argument which is applied the nodes of~@
              the tree before the test is applied, or KEY could be NIL~@
              which means IDENTITY.  TEST and TEST-NOT are designators~@
              for functions of two arguments that return a generalized~@
              boolean indicating whether the test passed.  The default~@
              if neither TEST nor TEST-NOT is given is a TEST of EQL.~@
              The tree is traversed top-down, and for each node~@
              (including internal nodes) the KEY function is applied,~@
              and then the resulting value is used as a key too look up~@
              an entry in ALIST as if with ASSOC.  If an entry is found,~@
              the value of the entry is substituted for the node in the tree.~@
              If not, and the node is a CONS cell, then its CAR and CDR are~@
              traversed recursively.  If it is an atom, the atom is returned.~@
              A new tree is returned, i.e., there are no modifications to~@
              the original TREE.~@
              If ALIST is not an association list, an error of type TYPE-ERROR~@
              might be signales.~@
              An error is signaled if both TEST and TEST-NOT are given."))

(fundoc 'nsublis
	(fmt "Lambda list: (ALIST TREE &key KEY TEST TEST-NOT)~@
              where ALIST is an association list, TREE is a tree~@
              consisting of internal nodes in the form of CONS cells,~@
              and leaves in the form of atoms.  KEY is a designator for~@
              a function of one argument which is applied the nodes of~@
              the tree before the test is applied, or KEY could be NIL~@
              which means IDENTITY.  TEST and TEST-NOT are designators~@
              for functions of two arguments that return a generalized~@
              boolean indicating whether the test passed.  The default~@
              if neither TEST nor TEST-NOT is given is a TEST of EQL.~@
              The tree is traversed top-down, and for each node~@
              (including internal nodes) the KEY function is applied,~@
              and then the resulting value is used as a key too look up~@
              an entry in ALIST as if with ASSOC.  If an entry is found,~@
              the value of the entry is substituted for the node in the tree.~@
              If not, and the node is a CONS cell, then its CAR and CDR are~@
              traversed recursively.  If it is an atom, the atom is returned.~@
              The original TREE might be modified, i.e., the cons cells of~@
              TREE might be altered to reflect the substitutions.~@
              If ALIST is not an association list, an error of type TYPE-ERROR~@
              might be signales.~@
              An error is signaled if both TEST and TEST-NOT are given."))

