(cl:in-package #:sicl-cons-high)

;;;; Copyright (c) 2008 - 2013
;;;;
;;;;     Robert Strandh (robert.strandh@gmail.com)
;;;;
;;;; all rights reserved. 
;;;;
;;;; Permission is hereby granted to use this software for any 
;;;; purpose, including using, modifying, and redistributing it.
;;;;
;;;; The software is provided "as-is" with no warranty.  The user of
;;;; this software assumes any responsibility of the consequences. 

;;;; This file is part of the cons-high module of the SICL project.
;;;; See the file SICL.text for a description of the project. 
;;;; See the file cons-high.text for a description of the module.

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
              When OBJECT is NIL, return NIL.~@
              Otherwise signal an error of type TYPE-ERROR."))

(fundoc '(setf car)
        (fmt "Lambda list: (NEW-VALUE OBJECT)~@
              When OBJECT is a CONS cell, replace the CAR of that CONS cell~@
              by NEW-VALUE, and return NEW-VALUE.~@
              Otherwise signal an error of type TYPE-ERROR."))

(setf (documentation 'car 'setf)
      (fmt "Syntax: (SETF (CAR OBJECT) NEW-VALUE)~@
            When OBJECT is a CONS cell, replace the CAR of that CONS cell~@
            by NEW-VALUE, and return NEW-VALUE.~@
            Otherwise signal an error of type TYPE-ERROR."))

(fundoc 'cdr
        (fmt "Lambda list: (OBJECT)~@
              When OBJECT is a CONS cell, return the CDR of that cell.~@
              When OBJECT is NIL, return NIL.~@
              Otherwise signal an error of type TYPE-ERROR."))

(fundoc '(setf cdr)
        (fmt "Lambda list: (NEW-VALUE OBJECT)~@
              When OBJECT is a CONS cell, replace the CDR of that CONS cell~@
              by NEW-VALUE, and return NEW-VALUE.~@
              Otherwise signal an error of type TYPE-ERROR."))

(setf (documentation 'cdr 'setf)
      (fmt "Syntax: (SETF (CDR OBJECT) NEW-VALUE)~@
            When OBJECT is a CONS cell, replace the CDR of that CONS cell~@
            by NEW-VALUE, and return NEW-VALUE.~@
            Otherwise signal an error of type TYPE-ERROR."))

(defmacro make-c*r-documentation (function-name)
  (let* ((name (symbol-name function-name))
         (letters (reverse (cdr (butlast (coerce name 'list))))))
    (flet ((primitive (letter)
             (if (eql letter #\A) 'car 'cdr)))
      `(progn (fundoc ',function-name
                      ,(fmt "Lambda list: (OBJECT)~@
                             Equivalent to ~a"
                            (loop with form = 'object
                                  for letter in letters
                                  do (setf form
                                           (list (primitive letter) form))
                                  finally (return form))))
              (fundoc '(setf ,function-name)
                      ,(fmt "Lambda list: (NEW-VALUE LIST)~@
                             Equivalent to (SETF (C~aR ~a) NEW-VALUE)"
                            (car (last letters))
                            (loop with form = 'list
                                  for letter in (butlast letters)
                                  do (setf form
                                           (list (primitive letter) form))
                                  finally (return form))))
              (setf (documentation ',function-name 'setf)
                    ,(fmt "Syntax: (SETF (~a OBJECT) NEW-VALUE)~@
                           Equivalent to (SETF (C~aR ~a) NEW-VALUE)"
                          function-name
                          (car (last letters))
                          (loop with form = 'list
                                for letter in (butlast letters)
                                do (setf form
                                         (list (primitive letter) form))
                                finally (return form))))))))
                          
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
              When LIST is neither a CONS cell nor NIL,~@
              an error of type TYPE-ERROR is signaled."))

(defmacro make-nth-documentation (function-name number)
  `(progn (fundoc ',function-name
                  ,(fmt "Lambda list: (LIST)~@
                          Return the ~a element of the list LIST.~@
                          When LIST is a proper list with fewer than ~a element,~@
                          NIL is returned.~@
                          When LIST is not a proper list, and it has fewer than~@
                          ~a elements, an error of type TYPE-ERROR is signaled.~@
                          In particular, when LIST is neither a list nor NIL,~@
                          an error of type TYPE-ERROR is signaled."
                        (string-downcase (symbol-name function-name))
                        number
                        number))
          (fundoc '(setf ,function-name)
                  ,(fmt "Lambda list: (NEW-VALUE LIST)~@
                         Replace the ~a element of LIST by NEW-VALUE,~@
                         and return NEW-VALUE.~@
                         If list has fewer than ~a top-level CONS cells,~@
                         or if it an atom, then an error of type TYPE-ERROR~@
                         is signaled."
                        (string-downcase (symbol-name function-name))
                        number))
          (setf (documentation ',function-name 'setf)
                    ,(fmt "Syntax: (SETF (~a OBJECT) NEW-VALUE)~@
                          Replace the ~a element of LIST by NEW-VALUE,~@
                          and return NEW-VALUE.~@
                          If list has fewer than ~a top-level CONS cells,~@
                          or if it an atom, then an error of type TYPE-ERROR~@
                          is signaled."
                          function-name
                          (string-downcase (symbol-name function-name))
                          number))))

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

(fundoc '(setf nth)
        (fmt "Lambda list: (NEW-VALUE N LIST)~@
              Replace the Nth element of LIST by NEW-VALUE,~@
              where N=0 indicates the first element of the LIST.~@
              The LIST must have more than N top-level CONS cells.~@
              If not, an error of type TYPE-ERROR is signaled."))

(setf (documentation 'nth 'setf)
      (fmt "Syntax: (setf (nth N LIST) NEW-VALUE~@
            Replace the Nth element of LIST by NEW-VALUE,~@
            where N=0 indicates the first element of the LIST.~@
            The LIST must have more than N top-level CONS cells.~@
            If not, an error of type TYPE-ERROR is signaled."))

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

(fundoc 'subst
        (fmt "Lambda list: (NEW OLD TREE &key KEY TEST TEST-NOT)~@
              where NEW and OLD are any objects, TREE is a tree~@
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
              and then the resulting value is compared to OLD using TEST~@
              or TEST-NOT as appropriate.  If the test succeeds.~@
              NEW is substituted for the node in the tree.~@
              If not, and the node is a CONS cell, then its CAR and CDR are~@
              traversed recursively.  If it is an atom, the atom is returned.~@
              A new tree is returned, i.e., there are no modifications to~@
              the original TREE.~@
              An error is signaled if both TEST and TEST-NOT are given."))

(fundoc 'nsubst
        (fmt "Lambda list: (NEW OLD TREE &key KEY TEST TEST-NOT)~@
              where NEW and OLD are any objects, TREE is a tree~@
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
              and then the resulting value is compared to OLD using TEST~@
              or TEST-NOT as appropriate.  If the test succeeds.~@
              NEW is substituted for the node in the tree.~@
              If not, and the node is a CONS cell, then its CAR and CDR are~@
              traversed recursively.  If it is an atom, the atom is returned.~@
              The original TREE might be modified, i.e., the cons cells of~@
              TREE might be altered to reflect the substitutions.~@
              An error is signaled if both TEST and TEST-NOT are given."))

(fundoc 'subst-if 
        (fmt "Lambda list: (NEW PREDICATE TREE &key KEY)~@
              where NEW is any objects, PREDICATE is a designator for~@
              a function of one argument that returns a generalized boolean,~@
              TREE is a tree consisting of internal nodes in the form of 
              CONS cells, and leaves in the form of atoms.  KEY is a 
              designator for a function of one argument which is applied
              the nodes of the tree before the PREDICATE is applied,~@
              or KEY could be NIL which means IDENTITY.~@
              The tree is traversed top-down, and for each node~@
              (including internal nodes) the KEY function is applied,~@
              and then the PREDICATE is called with resulting value.~@
              If the PREDICATE returns true NEW is substituted for the node in the tree.~@
              If not, and the node is a CONS cell, then its CAR and CDR are~@
              traversed recursively.  If it is an atom, the atom is returned.~@
              A new tree is returned, i.e., there are no modifications to~@
              the original TREE."))

(fundoc 'nsubst-if 
        (fmt "Lambda list: (NEW PREDICATE TREE &key KEY)~@
              where NEW is any objects, PREDICATE is a designator for~@
              a function of one argument that returns a generalized boolean,~@
              TREE is a tree consisting of internal nodes in the form of 
              CONS cells, and leaves in the form of atoms.  KEY is a 
              designator for a function of one argument which is applied
              the nodes of the tree before the PREDICATE is applied,~@
              or KEY could be NIL which means IDENTITY.~@
              The tree is traversed top-down, and for each node~@
              (including internal nodes) the KEY function is applied,~@
              and then the PREDICATE is called with resulting value.~@
              If the PREDICATE returns true NEW is substituted for the node in the tree.~@
              If not, and the node is a CONS cell, then its CAR and CDR are~@
              traversed recursively.  If it is an atom, the atom is returned.~@
              TREE might be altered to reflect the substitutions."))

(fundoc 'subst-if-not
        (fmt "Lambda list: (NEW PREDICATE TREE &key KEY)~@
              where NEW is any objects, PREDICATE is a designator for~@
              a function of one argument that returns a generalized boolean,~@
              TREE is a tree consisting of internal nodes in the form of 
              CONS cells, and leaves in the form of atoms.  KEY is a 
              designator for a function of one argument which is applied
              the nodes of the tree before the PREDICATE is applied,~@
              or KEY could be NIL which means IDENTITY.~@
              The tree is traversed top-down, and for each node~@
              (including internal nodes) the KEY function is applied,~@
              and then the PREDICATE is called with resulting value.~@
              If the PREDICATE returns false NEW is substituted for the node in the tree.~@
              If not, and the node is a CONS cell, then its CAR and CDR are~@
              traversed recursively.  If it is an atom, the atom is returned.~@
              A new tree is returned, i.e., there are no modifications to~@
              the original TREE."))

(fundoc 'nsubst-if-not
        (fmt "Lambda list: (NEW PREDICATE TREE &key KEY)~@
              where NEW is any objects, PREDICATE is a designator for~@
              a function of one argument that returns a generalized boolean,~@
              TREE is a tree consisting of internal nodes in the form of 
              CONS cells, and leaves in the form of atoms.  KEY is a 
              designator for a function of one argument which is applied
              the nodes of the tree before the PREDICATE is applied,~@
              or KEY could be NIL which means IDENTITY.~@
              The tree is traversed top-down, and for each node~@
              (including internal nodes) the KEY function is applied,~@
              and then the PREDICATE is called with resulting value.~@
              If the PREDICATE returns false NEW is substituted for the node in the tree.~@
              If not, and the node is a CONS cell, then its CAR and CDR are~@
              traversed recursively.  If it is an atom, the atom is returned.~@
              TREE might be altered to reflect the substitutions."))

(fundoc 'tree-equal
        (fmt "Lambda list: (TREE-1 TREE-2 &key TEST TEST-NOT)~@
              where TREE-1 and TREE-2 are trees consisting of internal nodes~@
              in the form of CONS cells, and leaves in the form of atoms.~@
              TEST and TEST-NOT are designators for functions of two arguments~@
              that return a generalized boolean indicating whether the test passed.~@
              The default if neither TEST nor TEST-NOT is given is a TEST of EQL.~@
              TREE-1 and TREE-2 are traversed and compared.  If both TREE-1 and~@
              TREE-2 are atoms, then the test is applied, and if the test is~@
              satisfied, then true is returned, otherwise, false is returned.~@
              If both TREE-1 and TREE-2 are CONS cells, then the CAR and the CDR~@
              of both are compared recursively.  If both the CAR and the CDR are~@
              recursively equal, then true is returned.  Otherwise false is returned.~@
              The test is satisfied if the TEST function returns true, or the TEST-NOT~@
              function returns false.  These functions receive an atom of TREE-1 and~@
              and atom of TREE-2 in that order.~@
              The consequences are undefined if both TREE-1 and TREE-2 contain~@
              circular references.~@
              An error is signaled if both TEST and TEST-NOT are given."))

(fundoc 'copy-list
        (fmt "Lambda list: (LIST)~@
              where LIST is a proper list or a dotted list.~@
              The top-level CONS cells of LIST are copied, and the resulting~@
              copy is returned.~@
              The consequences are undefined if LIST is a circular list."))

(fundoc 'list-length
        (fmt "Lambda list: (LIST)~@
              where LIST is a proper list or a circular list.~@
              If LIST is a proper list, its length is returned, as if computed~@
              with the LENGTH function.  If LIST is a circular list, then NIL~@
              is returned instead.~@
              An error of type TYPE-ERROR is signaled if LIST is neither~@
              a proper list or a circular list."))

(fundoc 'listp
        (fmt "Lambda list: (OBJECT)~@
              where OBJECT is any object.~@
              Return true if OBJECT is of type LIST, i.e., either a CONS cell~@
              or NIL.  Return false otherwise."))

(fundoc 'make-list
        (fmt "Lambda list: (SIZE &key INITIAL-ELEMENT)~@
              where SIZE is a nonnegative integer, and INITIAL-ELEMENT is~@
              any object.  The default for INITIAL-ELEMENT is NIL.~@
              Return a proper list of length SIZE containing INITIAL-ELEMENT~@
              as every element.~@
              Signal an error of type TYPE-ERROR if SIZE is not a nonnegative ~@
              integer."))

(fundoc 'endp
        (fmt "Lambda list: (LIST)~@
              where LIST is a list which might be dotted or circular.~@
              Return true if LIST is NIL.  Return false if LIST is a CONS cell.~@
              Signal an error of type TYPE-ERROR if LIST is neither NIL, nor~@
              a CONS cell, (i.e, if LIST is not a list)."))

(fundoc 'null
        (fmt "Lambda list: (OBJECT)~@
              where OBJECT is any object.~@
              Return true if OBJECT is NIL.  Return false otherwise."))

(fundoc 'nconc
        (fmt "Lambda list: (&rest LISTS)~@
              where each object in LISTS must be a list, except possibly the last one~@
              which may be any object.  The others may be dotted lists, but may~@
              not be circular lists.~@
              The lists are desctructively concatenated as follows: The CDR~@
              of the last CONS cell of each list (except the last one) is ~@
              modified and made to point to the next list in LISTS.~@
              The result of the concatenation is finally returned.~@
              Any of the lists in LISTS might be NIL in which case it does not~@
              participate in the operation.  It is as if it were not given.~@
              The consequences are undefined if any object (except the last one)~@
              in LISTS is not a list, i.e, if it is an atom other than NIL.~@
              If no lists are given, NCONC returns NIL."))

(fundoc 'append
        (fmt "Lambda list: (&rest LISTS)~@
              where each object in LISTS must be a proper list, except possibly the~@
              last one, which may be any object.~@
              Return a new list which is the concatenation of the lists in LISTS, i.e.,~@
              a list which contains the elements of the lists in LISTS in the order~@
              that they appear.  The lists in LISTS are not modified.  Every list~@
              in LISTS is copied, except the last object in LISTS.  That last object~@
              will share structure with the return value. 
              Any of the lists in LISTS might be NIL in which case it does not~@
              participate in the operation.  It is as if it were not given.~@
              The consequences are undefined if any object (except the last one)~@
              in LISTS is not a proper list.~@
              If no lists are given, APPEND returns NIL."))

(fundoc 'revappend
        (fmt "Lambda list: (LIST TAIL)~@
              where LIST must be a proper list, and TAIL may be any object.~@
              Return a list that starts with the elements of LIST in reverse order,~@
              with the CDR of the last CONS cell containing TAIL.~@
              LIST is not modified.  The return value shares structure with TAIL.~@
              The consequences are undefined if LIST is not a proper list."))

(fundoc 'nreconc
        (fmt "Lambda list: (LIST TAIL)~@
              where LIST must be a proper list, and TAIL may be any object.~@
              Return a list that starts with the elements of LIST in reverse order,~@
              with the CDR of the last CONS cell containing TAIL.~@
              LIST may be destructively modified.  The return value shares structure~@
              with TAIL.~@
              The consequences are undefined if LIST is not a proper list."))

(fundoc 'butlast
        (fmt "Lambda list: (LIST &optional N)~@
              where LIST is list that may be dotted but not circular, and
              N is a nonnegative integer.~@
              The default value of N is 1.~@
              Return a list that is a copy of LIST except that the last N~@
              CONS cells of LIST are not copied.~@
              If N is greater than or equal to the number of CONS cells of LIST,~@
              then NIL is returned.~@
              If LIST is a dotted list and N is zero, BUTLAST returns a copy of LIST~@
              except that the CDR of the last CONS cell is NIL instead.~@
              An error of type TYPE-ERROR is signaled if LIST is not a proper list~@
              or a dotted list, including if LIST is a circular list.~@
              An error of type TYPE-ERROR is signaled if N is not a nonnegative~@
              integer."))

(fundoc 'nbutlast
        (fmt "Lambda list: (LIST &optional N)~@
              where LIST is list that may be dotted but not circular, and
              N is a nonnegative integer.~@
              The default value of N is 1.~@
              NBUTLAST changes the CDR of the Nth CONS cell from the end of LIST~@
              to NIL (where the last CONS cell is the 0th from the end) and then~@
              returns LIST.~@
              If N is greater than or equal to the number of CONS cells of LIST,~@
              then NIL is returned, and LIST is not modified~@
              If LIST is a dotted list and N is zero, NBUTLAST changes the CDR~@
              of the last CONS cell of LIST to NIL~@
              An error of type TYPE-ERROR is signaled if LIST is not a proper list~@
              or a dotted list, including if LIST is a circular list.~@
              An error of type TYPE-ERROR is signaled if N is not a nonnegative~@
              integer."))

(fundoc 'last
        (fmt "Lambda list: (LIST &optional N)~@
              where LIST is a proper list or a dotted list, but must not be~@
              a circular list, and N is a nonnegative integer.~@
              The default value of N is 1.~@
              LAST returns the last N CONS cells of LIST.  If N is 0,~@
              the contents of the CDR of the last CONS cell is returned.~@
              If N is greater than or equal to the number of CONS cells of LIST,~@
              then LIST is returned.~@
              The consequences are undefined if LIST is a circular list.~@
              An error of type TYPE-ERROR is signaled if N is not a nonnegative~@
              integer"))

(fundoc 'ldiff
        (fmt "Lambda list: (LIST OBJECT)~@
              where LIST is a proper list, a dotted list, or a circular list,~@
              and OBJECT is any object.~@
              If OBJECT is EQL to the contents of the CDR of any CONS cell of~@
              LIST, then LDIFF returns a proper list that is a copy of the CONS~@
              cells of LIST up to and including the one containing OBJECT in its~@
              CDR, except that the copy is terminated by NIL.~@
              If OBJECT is not EQL to the contents of the CDR of any CONS cell of~@
              LIST, then if LIST is a proper list or a dotted list, LDIFF returns~@
              a copy of LIST, and if LIST is a circular list, the consequences~@
              are unspecified.~@
              An error of type TYPE-ERROR might be signaled if LIST is not a~@
              proper or a dotted list."))

(fundoc 'tailp
        (fmt "Lambda list: (LIST OBJECT)~@
              where LIST is a proper list, a dotted list, or a circular list,~@
              and OBJECT is any object.~@
              If OBJECT is EQL to the contents of the CDR of any CONS cell of~@
              LIST, then TAILP returns true.~@
              If OBJECT is not EQL to the contents of the CDR of any CONS cell of~@
              LIST, then if LIST is a proper list or a dotted list, TAILP returns~@
              false, and if LIST is a circular list, the consequences~@
              are unspecified.~@
              An error of type TYPE-ERROR might be signaled if LIST is not a~@
              proper or a dotted list."))

(fundoc 'nthcdr
        (fmt "Lambda list: (N LIST)~@
              where N is a nonnegative integer and LIST is a list that might~@
              be a dotted list or a circular list.~@
              Return the result of applying CDR N times to LIST.~@
              If LIST contains fewer than N CONS cells, then if LIST is a proper~@
              list, then NIL is returned, otherwise (i.e., LIST is a dotted list~@
              with fewer than N CONS cells) an error of type TYPE-ERROR is signaled.~@
              An error of type TYPE-ERROR is signaled if N is not a nonnegative~@
              integer."))

(fundoc 'rest
        (fmt "Lambda list: (LIST)~@
              where LIST is a CONS cell or NIL.~@
              If LIST is a CONS cell, then the CDR of that cell is returned.~@
              If LIST is NIL, then NIL is returned.~@
              If LIST is an atom other than NIL, then an error of type TYPE-ERROR~@
              is signaled."))

(fundoc '(setf rest)
        (fmt "Lambda list: (NEW-VALUE LIST)~@
              When LIST is a CONS cell, replace the CDR of that CONS cell~@
              by NEW-VALUE, and return NEW-VALUE.~@
              Otherwise signal an error of type TYPE-ERROR."))

(setf (documentation 'rest 'setf)
      (fmt "Syntax: (SETF (CAR LIST) NEW-VALUE)~@
            When LIST is a CONS cell, replace the CAR of that CONS cell~@
            by NEW-VALUE, and return NEW-VALUE.~@
            Otherwise signal an error of type TYPE-ERROR."))

(fundoc 'member
        (fmt "Lambda list: (ITEM LIST &key KEY TEST TEST-NOT)~@
              where ITEM is any object and LIST is a proper list.~@
              KEY is a designator for a function of one argument which is~@
              applied the elements of LIST before the test is applied,~@
              or KEY could be NIL which means IDENTITY.  TEST and TEST-NOT~@
              are designators for functions of two arguments that return a~@
              generalized boolean indicating whether the test passed.  The~@
              default if neither TEST nor TEST-NOT is given is a TEST of EQL.~@
              The LIST is search and successive elements are compared to ITEM.~@
              Before the comparison is made, the KEY function is applied to the~@
              element.  Then the TEST or TEST-NOT is applied to ITEM and the~@
              result of applying the KEY function in that order.  In the case~@
              of TEST, if the result is true, then the remaining tail of LIST~@
              starting with the element that made the TEST pass is returned.~@
              Since this value is a CONS cell, it is usable as a generalized~@
              boolean true value.  If no element passes the test, NIL is returned.~@
              In the case of TEST-NOT, if the result is false, then the remaining~@
              tail of LIST starting with the element that made the TEST pass is returned.~@
              Since this value is a CONS cell, it is usable as a generalized~@
              boolean true value.  If TEST-NOT yields true for evey element in~@
              LIST, then NIL is returned.~@
              An error of type TYPE-ERROR might be signaled if LIST is not a~@
              proper list."))

(fundoc 'member-if
        (fmt "Lambda list: (PREDICATE LIST &key KEY)~@
              where PREDICATE is a designator for a function of one argument~@
              returning a generalized boolean, and LIST is a proper list.~@
              KEY is a designator for a function of one argument which is~@
              applied the elements of LIST before the PREDICATE is applied,~@
              or KEY could be NIL which means IDENTITY.~@
              The LIST is search and PREDICATE is applied to successive elements.~@
              Before the PREDICATE is applied the KEY function is applied to the~@
              element.  If the PREDICATE returns true then the remaining tail of LIST~@
              starting with the element that made the PREDICATE return true is returned.~@
              Since this value is a CONS cell, it is usable as a generalized~@
              boolean true value.  If PREDICATE does not return true for any element,~@
              Then NIL is returned.~@
              An error of type TYPE-ERROR might be signaled if LIST is not a~@
              proper list."))

(fundoc 'member-if-not
        (fmt "Lambda list: (PREDICATE LIST &key KEY)~@
              where PREDICATE is a designator for a function of one argument~@
              returning a generalized boolean, and LIST is a proper list.~@
              KEY is a designator for a function of one argument which is~@
              applied the elements of LIST before the PREDICATE is applied,~@
              or KEY could be NIL which means IDENTITY.~@
              The LIST is search and PREDICATE is applied to successive elements.~@
              Before the PREDICATE is applied the KEY function is applied to the~@
              element.  If the PREDICATE returns false then the remaining tail of LIST~@
              starting with the element that made the PREDICATE return false is returned.~@
              Since this value is a CONS cell, it is usable as a generalized~@
              boolean true value.  If PREDICATE does not return false for any element,~@
              Then NIL is returned.~@
              An error of type TYPE-ERROR might be signaled if LIST is not a~@
              proper list."))

(fundoc 'mapc
        (fmt "Lambda list: (FUNCTION &rest LISTS)~@
              were function is a designator for a function that must accept~@
              as many arguments as there are lists in LISTS.~@
              Each list in LISTS must be a proper list, and there must be at~@
              least one such list in LISTS.~@
              The FUNCTION is applied successive elements of the LISTS~@
              in such a way that the first argument to the FUNCTION is the CAR~@
              of the first list in LISTS, the second argument to the FUNCTION is~@
              the CAR of the second list is LISTS, and so on.~@
              After each application of the FUNCTION, each list in LISTS is~@
              replaced by its CDR, and the process is repeated.~@
              The process stops when at least one of the lists in LISTS is NIL.~@
              The return value of the application of the FUNCTION is discarded.~@
              Finally, MAPC returns the first list in LISTS.~@
              An error of type TYPE-ERROR might be signaled if any of the lists~@
              in LISTS is not a proper list."))

(fundoc 'mapcar
        (fmt "Lambda list: (FUNCTION &rest LISTS)~@
              were function is a designator for a function that must accept~@
              as many arguments as there are lists in LISTS.~@
              Each list in LISTS must be a proper list, and there must be at~@
              least one such list in LISTS.~@
              The FUNCTION is applied successive elements of the LISTS~@
              in such a way that the first argument to the FUNCTION is the CAR~@
              of the first list in LISTS, the second argument to the FUNCTION is~@
              the CAR of the second list is LISTS, and so on.~@
              After each application of the FUNCTION, each list in LISTS is~@
              replaced by its CDR, and the process is repeated.~@
              The process stops when at least one of the lists in LISTS is NIL.~@
              The return values of the applications of the FUNCTION are collected~@
              into a list, and this list is returned by MAPCAR.~@
              An error of type TYPE-ERROR might be signaled if any of the lists~@
              in LISTS is not a proper list."))

(fundoc 'maplist
        (fmt "Lambda list: (FUNCTION &rest LISTS)~@
              were function is a designator for a function that must accept~@
              as many arguments as there are lists in LISTS.~@
              Each list in LISTS must be a proper list, and there must be at~@
              least one such list in LISTS.~@
              The FUNCTION is applied successive sublists of the LISTS~@
              in such a way that the first argument to the FUNCTION is the~@
              first list in LISTS, the second argument to the FUNCTION is~@
              the the second list is LISTS, and so on.~@
              After each application of the FUNCTION, each list in LISTS is~@
              replaced by its CDR, and the process is repeated.~@
              The process stops when at least one of the lists in LISTS is NIL.~@
              The return values of the applications of the FUNCTION are collected~@
              into a list, and this list is returned by MAPLIST.~@
              An error of type TYPE-ERROR might be signaled if any of the lists~@
              in LISTS is not a proper list."))

(fundoc 'mapl
        (fmt "Lambda list: (FUNCTION &rest LISTS)~@
              were function is a designator for a function that must accept~@
              as many arguments as there are lists in LISTS.~@
              Each list in LISTS must be a proper list, and there must be at~@
              least one such list in LISTS.~@
              The FUNCTION is applied successive sublists of the LISTS~@
              in such a way that the first argument to the FUNCTION is the~@
              first list in LISTS, the second argument to the FUNCTION is~@
              the the second list is LISTS, and so on.~@
              After each application of the FUNCTION, each list in LISTS is~@
              replaced by its CDR, and the process is repeated.~@
              The process stops when at least one of the lists in LISTS is NIL.~@
              The return value of the application of the FUNCTION is discarded.~@
              Finally, MAPL returns the first list in LISTS.~@
              An error of type TYPE-ERROR might be signaled if any of the lists~@
              in LISTS is not a proper list."))

(fundoc 'mapcan
        (fmt "Lambda list: (FUNCTION &rest LISTS)~@
              were function is a designator for a function that must accept~@
              as many arguments as there are lists in LISTS.~@
              Each list in LISTS must be a proper list, and there must be at~@
              least one such list in LISTS.~@
              The FUNCTION is applied successive elements of the LISTS~@
              in such a way that the first argument to the FUNCTION is the CAR~@
              of the first list in LISTS, the second argument to the FUNCTION is~@
              the CAR of the second list is LISTS, and so on.~@
              After each application of the FUNCTION, each list in LISTS is~@
              replaced by its CDR, and the process is repeated.~@
              The process stops when at least one of the lists in LISTS is NIL.~@
              The return values of the applications of the FUNCTION are~@
              concatenated into a list as if NCONC were appled to them~@
              and this list is returned by MAPCAN.~@
              An error of type TYPE-ERROR might be signaled if any of the lists~@
              in LISTS is not a proper list."))

(fundoc 'mapcon
        (fmt "Lambda list: (FUNCTION &rest LISTS)~@
              were function is a designator for a function that must accept~@
              as many arguments as there are lists in LISTS.~@
              Each list in LISTS must be a proper list, and there must be at~@
              least one such list in LISTS.~@
              The FUNCTION is applied successive sublists of the LISTS~@
              in such a way that the first argument to the FUNCTION is the~@
              first list in LISTS, the second argument to the FUNCTION is~@
              the the second list is LISTS, and so on.~@
              After each application of the FUNCTION, each list in LISTS is~@
              replaced by its CDR, and the process is repeated.~@
              The process stops when at least one of the lists in LISTS is NIL.~@
              The return values of the applications of the FUNCTION are~@
              concatenated into a list as if NCONC were appled to them~@
              and this list is returned by MAPCON.~@
              An error of type TYPE-ERROR might be signaled if any of the lists~@
              in LISTS is not a proper list."))

(fundoc 'acons
        (fmt "Lambda list: (KEY DATUM ALIST)~@
              where KEY and DATUM are any objects, and ALIST is an association list.~@
              ACONS allocates two CONS cells.  One contains KEY in its CAR and~@
              DATUM in its CDR.  The second one contains the first one in its CAR~@
              and ALIST in its CDR, this second CONS cell is return by ACONS.~@
              (acons key datum alist) is equivalent to (cons (cons key datum) alist)."))

(fundoc 'assoc
        (fmt "Lambda list: (ITEM ALIST &key KEY TEST TEST-NOT)~@
              where ITEM is any object and ALIST is an association list.~@
              KEY is a designator for a function of one argument which is~@
              applied to the CAR of the elements of ALIST before the test is applied,~@
              or KEY could be NIL which means IDENTITY.  TEST and TEST-NOT~@
              are designators for functions of two arguments that return a~@
              generalized boolean indicating whether the test passed.  The~@
              default if neither TEST nor TEST-NOT is given is a TEST of EQL.~@
              The elements of ALIST are searched.  If an element is NIL, it is~@
              ignored.  If the element is a CONS, then KEY is applied to its CAR~@
              and then, in the case of TEST, TEST is applied to ITEM and the result~@
              of applying KEY, in that order.  If the result is true, then the~@
              element (which is a CONS cell) of ALIST is returned.  
              If TEST returns false for all of the elements of ALIST then NIL is returned.~@
              In case of TEST-NOT, TEST-NOT is applied to ITEM and the result~@
              of applying KEY, in that order.  If the result is false, then the~@
              element (which is a CONS cell) of ALIST is returned.~@
              If TEST-NOT returns true for all of the elements of ALIST the~@
              NIL is returned.~@
              An error of type TYPE-ERROR might be signaled if ALIST is not~@
              an association list."))

(fundoc 'assoc-if
        (fmt "Lambda list: (PREDICATE ALIST &key KEY)~@
              where PREDICATE is a designator for a function of one argument~@
              returning a generalized boolean, and ALIST is an association list.~@
              KEY is a designator for a function of one argument which is~@
              applied the elements of LIST before the PREDICATE is applied,~@
              or KEY could be NIL which means IDENTITY.~@
              The elements of ALIST are searched.  If an element is NIL, it is~@
              ignored.  If the element is a CONS, then KEY is applied to its CAR~@
              and then, the PREDICATE is applied to the result.  If the PREDICATE~@
              returns true then the element (which is a CONS cell) of ALIST~@
              is returned.  If the PREDICATE returns false for all of the elements~@
              of ALIST then NIL is returned.~@
              An error of type TYPE-ERROR might be signaled if ALIST is not~@
              an association list."))

(fundoc 'assoc-if-not
        (fmt "Lambda list: (PREDICATE ALIST &key KEY)~@
              where PREDICATE is a designator for a function of one argument~@
              returning a generalized boolean, and ALIST is an association list.~@
              KEY is a designator for a function of one argument which is~@
              applied the elements of LIST before the PREDICATE is applied,~@
              or KEY could be NIL which means IDENTITY.~@
              The elements of ALIST are searched.  If an element is NIL, it is~@
              ignored.  If the element is a CONS, then KEY is applied to its CAR~@
              and then, the PREDICATE is applied to the result.  If the PREDICATE~@
              returns false then the element (which is a CONS cell) of ALIST~@
              is returned.  If the PREDICATE returns true for all of the elements~@
              of ALIST then NIL is returned.~@
              An error of type TYPE-ERROR might be signaled if ALIST is not~@
              an association list."))

(fundoc 'copy-alist
        (fmt "Lambda list: (ALIST)~@
              where ALIST is an association list.~@
              The top-level CONS cells are copied, as are any elements that~@
              are CONS cells.  Items that are the CAR or CDR of top-level CONS~@
              cells are not copied, but share structure with ALIST.  COPY-ALIST~@
              returns the copy created.  Any top-level elements of ALIST that are~@
              not CONS cells are included in the copy as-is."))

(fundoc 'pairlis
        (fmt "Lambda list: (KEYS DATA &optional ALIST)~@
              where KEYS and DATA are proper lists of the same length, and~@
              ALIST is an association list.  The default of ALIST is NIL.~@
              Elements of KEYS and DATA are paired up to form entries of~@
              a new association list, which is then prepended to ALIST and returned.~@
              An error of type type-error might be signaled if KEYS and DATA are~@
              not both proper lists.~@
              The consequences are undefined if ALIST is not an association list.~@
              The consequences are undefined if KEYS and DATA are not the same length."))

(fundoc 'rassoc
        (fmt "Lambda list: (ITEM ALIST &key KEY TEST TEST-NOT)~@
              where ITEM is any object and ALIST is an association list.~@
              KEY is a designator for a function of one argument which is~@
              applied to the CDR of the elements of ALIST before the test is applied,~@
              or KEY could be NIL which means IDENTITY.  TEST and TEST-NOT~@
              are designators for functions of two arguments that return a~@
              generalized boolean indicating whether the test passed.  The~@
              default if neither TEST nor TEST-NOT is given is a TEST of EQL.~@
              The elements of ALIST are searched.  If an element is NIL, it is~@
              ignored.  If the element is a CONS, then KEY is applied to its CDR~@
              and then, in the case of TEST, TEST is applied to ITEM and the result~@
              of applying KEY, in that order.  If the result is true, then the~@
              element (which is a CONS cell) of ALIST is returned.  
              If TEST returns false for all of the elements of ALIST then NIL is returned.~@
              In case of TEST-NOT, TEST-NOT is applied to ITEM and the result~@
              of applying KEY, in that order.  If the result is false, then the~@
              element (which is a CONS cell) of ALIST is returned.~@
              If TEST-NOT returns true for all of the elements of ALIST the~@
              NIL is returned.~@
              An error of type TYPE-ERROR might be signaled if ALIST is not~@
              an association list."))

(fundoc 'rassoc-if
        (fmt "Lambda list: (PREDICATE ALIST &key KEY)~@
              where PREDICATE is a designator for a function of one argument~@
              returning a generalized boolean, and ALIST is an association list.~@
              KEY is a designator for a function of one argument which is~@
              applied the elements of LIST before the PREDICATE is applied,~@
              or KEY could be NIL which means IDENTITY.~@
              The elements of ALIST are searched.  If an element is NIL, it is~@
              ignored.  If the element is a CONS, then KEY is applied to its CDR~@
              and then, the PREDICATE is applied to the result.  If the PREDICATE~@
              returns true then the element (which is a CONS cell) of ALIST~@
              is returned.  If the PREDICATE returns false for all of the elements~@
              of ALIST then NIL is returned.~@
              An error of type TYPE-ERROR might be signaled if ALIST is not~@
              an association list."))

(fundoc 'rassoc-if-not
        (fmt "Lambda list: (PREDICATE ALIST &key KEY)~@
              where PREDICATE is a designator for a function of one argument~@
              returning a generalized boolean, and ALIST is an association list.~@
              KEY is a designator for a function of one argument which is~@
              applied the elements of LIST before the PREDICATE is applied,~@
              or KEY could be NIL which means IDENTITY.~@
              The elements of ALIST are searched.  If an element is NIL, it is~@
              ignored.  If the element is a CONS, then KEY is applied to its CDR~@
              and then, the PREDICATE is applied to the result.  If the PREDICATE~@
              returns false then the element (which is a CONS cell) of ALIST~@
              is returned.  If the PREDICATE returns true for all of the elements~@
              of ALIST then NIL is returned.~@
              An error of type TYPE-ERROR might be signaled if ALIST is not~@
              an association list."))

(fundoc 'get-properties
        (fmt "Lambda list: (PLIST INDICATORs)~@
              where PLIST is a property list, and INDICATORS is a proper list.~@
              PLIST is searched for an indicator that matches one of the indicators~@
              in INDICATORS.  The function EQ is used to determin whether the~@
              indicator matches.  If such an indicator is found, GET-PROPERTIES~@
              returns three values, the indicator, the value (i.e., the object~@
              immediately after the indicator in PLIST), and the tail of PLIST~@
              whose first element is the indicator found.  If no such entry is found~@
              then tree NIL values are returned.~@
              The consequences are undefined if PLIST is not a property list.~@
              The consequences are undefined if INDICATORS is not a proper list."))

(fundoc 'getf
        (fmt "Lambda list: (PLIST INDICATOR &default DEFAULT)~@
              where PLIST is a property list, and INDICATOR is any object.~@
              PLIST is searched for an indicator that matches INDICATOR.~@
              The function EQ is used to determine whether the indicator matches.~@
              If such an indicator exists, the first one is used, and its~@
              corresponding value (i.e. the object immediately following the~@
              indicator in PLIST) is returned.  If no such indicator exists,~@
              then the value of DEFAULT is returned.  The default value for~@
              DEFAULT is NIL.~@
              The consequences are undefined if PLIST is not a property list."))

(fundoc 'intersection
        (fmt "Lambda list: (LIST-1 LIST-2 &key KEY TEST TEST-NOT)~@
              where LIST-1 and LIST-2 are proper lists,~@
              KEY is is a designator of a function of one argument~@
              or NIL, which means IDENTITY.~@
              TEST and TEST-NOT are designators for functions of two~@
              arguments that return a generalized boolean.  The default~@
              if neither TEST nor TEST-NOT is given is a TEST of EQL.~@
              INTERSECTION treats LIST-1 and LIST-2 as sets.~@
              Every element that occurs both in LIST-1 and LIST-2 is~@
              returned in the result, which is a proper list.~@
              The semantics are as if every element of LIST-1 were compared~@
              to every element of LIST-2.  Before any test is made~@
              between two elements, the KEY function is applied to both~@
              elements.~@
              Then, if TEST is given, it is applied to the result of the~@
              application of the KEY function in such a way that the first~@
              argument to the TEST comes from LIST-1 and the second argument~@
              comes from LIST-2.  If the TEST returns true, then exactly~@
              one of the elements are kept in the resulting list.~@
              Else, if TEST-NOT is given, it is applied to the result of the~@
              application of the KEY function in such a way that the first~@
              argument to the TEST-NOT comes from LIST-1 and the second argument~@
              comes from LIST-2.  If the TEST-NOT returns false, then exactly~@
              one of the elements are kept in the resulting list.~@
              The order of the elements in the resulting list is not~@
              specified.~@
              If either LIST-1 or LIST-2 contains duplicate elements (as~@
              defined by KEY and TEST or TEST-NOT), then the result may~@
              also contain duplicate elements.~@
              Neither LIST-1 nor LIST-2 is modified.
              The result may share structure with LIST-1 and/or LIST-2."))

(fundoc 'nintersection
        (fmt "Lambda list: (LIST-1 LIST-2 &key KEY TEST TEST-NOT)~@
              where LIST-1 and LIST-2 are proper lists,~@
              KEY is is a designator of a function of one argument~@
              or NIL, which means IDENTITY.~@
              TEST and TEST-NOT are designators for functions of two~@
              arguments that return a generalized boolean.  The default~@
              if neither TEST nor TEST-NOT is given is a TEST of EQL.~@
              INTERSECTION treats LIST-1 and LIST-2 as sets.~@
              Every element that occurs both in LIST-1 and LIST-2 is~@
              returned in the result, which is a proper list.~@
              The semantics are as if every element of LIST-1 were compared~@
              to every element of LIST-2.  Before any test is made~@
              between two elements, the KEY function is applied to both~@
              elements.~@
              Then, if TEST is given, it is applied to the result of the~@
              application of the KEY function in such a way that the first~@
              argument to the TEST comes from LIST-1 and the second argument~@
              comes from LIST-2.  If the TEST returns true, then exactly~@
              one of the elements are kept in the resulting list.~@
              Else, if TEST-NOT is given, it is applied to the result of the~@
              application of the KEY function in such a way that the first~@
              argument to the TEST-NOT comes from LIST-1 and the second argument~@
              comes from LIST-2.  If the TEST-NOT returns false, then exactly~@
              one of the elements are kept in the resulting list.~@
              The order of the elements in the resulting list is not~@
              specified.~@
              If either LIST-1 or LIST-2 contains duplicate elements (as~@
              defined by KEY and TEST or TEST-NOT), then the result may~@
              also contain duplicate elements.~@
              LIST-2 is not modified, but LIST-1 may be destroyed.  It's CONS~@
              cells may be used to build the result.~@
              The result may share structure with LIST-1 and/or LIST-2."))

(fundoc 'adjoin
        (fmt "Lambda list: (ITEM LIST &key KEY TEST TEST-NOT)~@
              where ITEM is any object and LIST is a proper list.~@
              KEY is is a designator of a function of one argument~@
              or NIL, which means IDENTITY.~@
              TEST and TEST-NOT are designators for functions of two~@
              arguments that return a generalized boolean.  The default~@
              if neither TEST nor TEST-NOT is given is a TEST of EQL.~@
              ADJOIN treats LIST as a set.~@
              The ITEM is compared to every element of LIST.  If ITEM already~@
              occurs in LIST, then LIST is returned.  If not the equivalent of~@
              (CONS ITEM LIST) is returned.~@
              Before any test is made between the ITEM and elements of LIST,~@
              the KEY function is applied to both of them.~@
              Then, if TEST is given, it is applied to the result of the~@
              application of the KEY function in such a way that the first~@
              argument to the TEST comes from the ITEM and the second argument~@
              comes from LIST.  If the TEST returns true, then LIST is returned,~@
              Otherwise the equivalent of (CONS ITEM LIST) is returned.~@
              Else, if TEST-NOT is given, it is applied to the result of the~@
              application of the KEY function in such a way that the first~@
              argument to the TEST-NOT comes from ITEM and the second argument~@
              comes from LIST.  If the TEST-NOT returns false, then LIST is returned,~@
              Otherwise the equivalent of (CONS ITEM LIST) is returned."))

(fundoc 'set-difference
        (fmt "Lambda list: (LIST-1 LIST-2 &key KEY TEST TEST-NOT)~@
              where LIST-1 and LIST-2 are proper lists,~@
              KEY is is a designator of a function of one argument~@
              or NIL, which means IDENTITY.~@
              TEST and TEST-NOT are designators for functions of two~@
              arguments that return a generalized boolean.  The default~@
              if neither TEST nor TEST-NOT is given is a TEST of EQL.~@
              INTERSECTION treats LIST-1 and LIST-2 as sets.~@
              Only elements that occur in in LIST-1 that do not match~@
              any element is LIST-2 are returned in the result,~@
              which is a proper list.~@
              The semantics are as if every element of LIST-1 were compared~@
              to every element of LIST-2.  Before any test is made~@
              between two elements, the KEY function is applied to both~@
              elements.~@
              Then, if TEST is given, it is applied to the result of the~@
              application of the KEY function in such a way that the first~@
              argument to the TEST comes from LIST-1 and the second argument~@
              comes from LIST-2.  If the TEST returns true, then the~@
              corresponding element of LIST-1 is discared from the result.~@
              Else, if TEST-NOT is given, it is applied to the result of the~@
              application of the KEY function in such a way that the first~@
              argument to the TEST-NOT comes from LIST-1 and the second argument~@
              comes from LIST-2.  If the TEST-NOT returns false, then the~@
              corresponding element of LIST-1 is discared from the result.~@
              The order of the elements in the resulting list is not~@
              specified.~@
              If LIST-1 contains duplicate elements (as~@
              defined by KEY and TEST or TEST-NOT), then the result may~@
              also contain duplicate elements.~@
              Neither LIST-1 nor LIST-2 is modified.
              The result may share structure with LIST-1 and/or LIST-2."))

(fundoc 'nset-difference
        (fmt "Lambda list: (LIST-1 LIST-2 &key KEY TEST TEST-NOT)~@
              where LIST-1 and LIST-2 are proper lists,~@
              KEY is is a designator of a function of one argument~@
              or NIL, which means IDENTITY.~@
              TEST and TEST-NOT are designators for functions of two~@
              arguments that return a generalized boolean.  The default~@
              if neither TEST nor TEST-NOT is given is a TEST of EQL.~@
              INTERSECTION treats LIST-1 and LIST-2 as sets.~@
              Only elements that occur in in LIST-1 that do not match~@
              any element is LIST-2 are returned in the result,~@
              which is a proper list.~@
              The semantics are as if every element of LIST-1 were compared~@
              to every element of LIST-2.  Before any test is made~@
              between two elements, the KEY function is applied to both~@
              elements.~@
              Then, if TEST is given, it is applied to the result of the~@
              application of the KEY function in such a way that the first~@
              argument to the TEST comes from LIST-1 and the second argument~@
              comes from LIST-2.  If the TEST returns true, then the~@
              corresponding element of LIST-1 is discared from the result.~@
              Else, if TEST-NOT is given, it is applied to the result of the~@
              application of the KEY function in such a way that the first~@
              argument to the TEST-NOT comes from LIST-1 and the second argument~@
              comes from LIST-2.  If the TEST-NOT returns false, then the~@
              corresponding element of LIST-1 is discared from the result.~@
              The order of the elements in the resulting list is not~@
              specified.~@
              If LIST-1 contains duplicate elements (as~@
              defined by KEY and TEST or TEST-NOT), then the result may~@
              also contain duplicate elements.~@
              NSET-DIFFERENCE may destroy LIST-1.~@
              The result may share structure with LIST-1 and/or LIST-2."))

(fundoc 'set-exclusive-or
        (fmt "Lambda list: (LIST-1 LIST-2 &key KEY TEST TEST-NOT)~@
              where LIST-1 and LIST-2 are proper lists,~@
              KEY is is a designator of a function of one argument~@
              or NIL, which means IDENTITY.~@
              TEST and TEST-NOT are designators for functions of two~@
              arguments that return a generalized boolean.  The default~@
              if neither TEST nor TEST-NOT is given is a TEST of EQL.~@
              INTERSECTION treats LIST-1 and LIST-2 as sets.~@
              Every element that occurs in LIST-1 but not in LIST-2~@
              and every element that occurs in LIST-2 but not in LIST-1~@
              is returned in the result, which is a proper list.~@
              The semantics are as if every element of LIST-1 were compared~@
              to every element of LIST-2.  Before any test is made~@
              between two elements, the KEY function is applied to both~@
              elements.~@
              Then, if TEST is given, it is applied to the result of the~@
              application of the KEY function in such a way that the first~@
              argument to the TEST comes from LIST-1 and the second argument~@
              comes from LIST-2.  If the TEST returns true, then both~@
              elements are discarded from the result.~@
              Else, if TEST-NOT is given, it is applied to the result of the~@
              application of the KEY function in such a way that the first~@
              argument to the TEST-NOT comes from LIST-1 and the second argument~@
              comes from LIST-2.  If the TEST-NOT returns false, then both~@
              elements are discarded from the result.~@
              The order of the elements in the resulting list is not~@
              specified.~@
              If either LIST-1 or LIST-2 contains duplicate elements (as~@
              defined by KEY and TEST or TEST-NOT), then the result may~@
              also contain duplicate elements.~@
              Neither LIST-1 nor LIST-2 is modified.
              The result may share structure with LIST-1 and/or LIST-2."))

(fundoc 'nset-exclusive-or
        (fmt "Lambda list: (LIST-1 LIST-2 &key KEY TEST TEST-NOT)~@
              where LIST-1 and LIST-2 are proper lists,~@
              KEY is is a designator of a function of one argument~@
              or NIL, which means IDENTITY.~@
              TEST and TEST-NOT are designators for functions of two~@
              arguments that return a generalized boolean.  The default~@
              if neither TEST nor TEST-NOT is given is a TEST of EQL.~@
              INTERSECTION treats LIST-1 and LIST-2 as sets.~@
              Every element that occurs in LIST-1 but not in LIST-2~@
              and every element that occurs in LIST-2 but not in LIST-1~@
              is returned in the result, which is a proper list.~@
              The semantics are as if every element of LIST-1 were compared~@
              to every element of LIST-2.  Before any test is made~@
              between two elements, the KEY function is applied to both~@
              elements.~@
              Then, if TEST is given, it is applied to the result of the~@
              application of the KEY function in such a way that the first~@
              argument to the TEST comes from LIST-1 and the second argument~@
              comes from LIST-2.  If the TEST returns true, then both~@
              elements are discarded from the result.~@
              Else, if TEST-NOT is given, it is applied to the result of the~@
              application of the KEY function in such a way that the first~@
              argument to the TEST-NOT comes from LIST-1 and the second argument~@
              comes from LIST-2.  If the TEST-NOT returns false, then both~@
              elements are discarded from the result.~@
              The order of the elements in the resulting list is not~@
              specified.~@
              If either LIST-1 or LIST-2 contains duplicate elements (as~@
              defined by KEY and TEST or TEST-NOT), then the result may~@
              also contain duplicate elements.~@
              NSET-EXCLUSIVE-OR may destroy LIST-1 or LIST-2 or both.~@
              The result may share structure with LIST-1 and/or LIST-2."))

(fundoc 'union
        (fmt "Lambda list: (LIST-1 LIST-2 &key KEY TEST TEST-NOT)~@
              where LIST-1 and LIST-2 are proper lists,~@
              KEY is is a designator of a function of one argument~@
              or NIL, which means IDENTITY.~@
              TEST and TEST-NOT are designators for functions of two~@
              arguments that return a generalized boolean.  The default~@
              if neither TEST nor TEST-NOT is given is a TEST of EQL.~@
              INTERSECTION treats LIST-1 and LIST-2 as sets.~@
              For every element that occurs both in LIST-1 and LIST-2,~@
              Only one of the two occurrences is returned in the result,~@
              which is a proper list.  Every element that occures only in~@
              LIST-1 and every element that occurs only in LIST-2 will be~@
              returned in the result.~@
              The semantics are as if every element of LIST-1 were compared~@
              to every element of LIST-2.  Before any test is made~@
              between two elements, the KEY function is applied to both~@
              elements.~@
              Then, if TEST is given, it is applied to the result of the~@
              application of the KEY function in such a way that the first~@
              argument to the TEST comes from LIST-1 and the second argument~@
              comes from LIST-2.  If the TEST returns true, then~@
              one of the elements is discared from the result.~@
              Else, if TEST-NOT is given, it is applied to the result of the~@
              application of the KEY function in such a way that the first~@
              argument to the TEST-NOT comes from LIST-1 and the second argument~@
              comes from LIST-2.  If the TEST-NOT returns false, then~@
              one of the elements is discared from the result.~@
              The order of the elements in the resulting list is not~@
              specified.~@
              If either LIST-1 or LIST-2 contains duplicate elements (as~@
              defined by KEY and TEST or TEST-NOT), then the result may~@
              also contain duplicate elements.~@
              Neither LIST-1 nor LIST-2 is modified.
              The result may share structure with LIST-1 and/or LIST-2."))

(fundoc 'subsetp
        (fmt "Lambda list: (LIST-1 LIST-2 &key KEY TEST TEST-NOT)~@
              where LIST-1 and LIST-2 are proper lists,~@
              KEY is is a designator of a function of one argument~@
              or NIL, which means IDENTITY.~@
              TEST and TEST-NOT are designators for functions of two~@
              arguments that return a generalized boolean.  The default~@
              if neither TEST nor TEST-NOT is given is a TEST of EQL.~@
              SUBSETP treats LIST-1 and LIST-2 as sets.~@
              The semantics are as if every element of LIST-1 were compared~@
              to every element of LIST-2.  Before any test is made~@
              between two elements, the KEY function is applied to both~@
              elements.~@
              Then, if TEST is given, it is applied to the result of the~@
              application of the KEY function in such a way that the first~@
              argument to the TEST comes from LIST-1 and the second argument~@
              comes from LIST-2.  If for some element of LIST-1 the TEST returns~@
              false for every element in LIST-2, then SUBSETP returns false.~@
              Else, if TEST-NOT is given, it is applied to the result of the~@
              application of the KEY function in such a way that the first~@
              argument to the TEST-NOT comes from LIST-1 and the second argument~@
              comes from LIST-2.  If for some element of LIST-1 the TEST-NOT returns~@
              true for every element in LIST-2, then SUBSETP returns false.~@
              Otherwise, SUBSETP returns true."))

(fundoc 'nunion
        (fmt "Lambda list: (LIST-1 LIST-2 &key KEY TEST TEST-NOT)~@
              where LIST-1 and LIST-2 are proper lists,~@
              KEY is is a designator of a function of one argument~@
              or NIL, which means IDENTITY.~@
              TEST and TEST-NOT are designators for functions of two~@
              arguments that return a generalized boolean.  The default~@
              if neither TEST nor TEST-NOT is given is a TEST of EQL.~@
              INTERSECTION treats LIST-1 and LIST-2 as sets.~@
              For every element that occurs both in LIST-1 and LIST-2,~@
              Only one of the two occurrences is returned in the result,~@
              which is a proper list.  Every element that occures only in~@
              LIST-1 and every element that occurs only in LIST-2 will be~@
              returned in the result.~@
              The semantics are as if every element of LIST-1 were compared~@
              to every element of LIST-2.  Before any test is made~@
              between two elements, the KEY function is applied to both~@
              elements.~@
              Then, if TEST is given, it is applied to the result of the~@
              application of the KEY function in such a way that the first~@
              argument to the TEST comes from LIST-1 and the second argument~@
              comes from LIST-2.  If the TEST returns true, then~@
              one of the elements is discared from the result.~@
              Else, if TEST-NOT is given, it is applied to the result of the~@
              application of the KEY function in such a way that the first~@
              argument to the TEST-NOT comes from LIST-1 and the second argument~@
              comes from LIST-2.  If the TEST-NOT returns false, then~@
              one of the elements is discared from the result.~@
              The order of the elements in the resulting list is not~@
              specified.~@
              If either LIST-1 or LIST-2 contains duplicate elements (as~@
              defined by KEY and TEST or TEST-NOT), then the result may~@
              also contain duplicate elements.~@
              NUNION may destroy any part of LIST-1 and any part of LIST-2~@
              The result may share structure with LIST-1 and/or LIST-2."))

(setf (documentation 'getf 'setf)
      (fmt "Syntax: (SETF (GETF PLACE INDICATOR &optional DEFAULT) NEW-VALUE)~@
            where PLACE is a place whose value is a property list,~@
            INDICATR, DEFAULT, and NEW-VALUE are any objects.~@
            The NEW-VALUE is associated with the INDICATOR of the property list~@
            that is the value of PLACE.  This can happen in two different ways.~@
            First, if INDICATOR is identical (as compared by EQ) to an existing~@
            indicator on the property list that is the value of PLACE,~@
            then NEW-VALUE becomes the new property value associated with that~@
            indicator.  If there are several such indicators on the property list,~@
            the NEW-VALUE gets associated with the first one.~@
            Second, if there is no indicator on the property list identical~@
            (as compared by EQ) to INDICATOR, then a new association between~@
            INDICATOR and NEW-VALUE is added to the property list that is the~@
            value of PLACE.  This might require the PLACE itsef to be modified.~@
            SETF of GETF may modify any part of the property list that is the~@
            value of PLACE, or it may modify the value of PLACE itself.~@
            The consequences are undefined if the value of PLACE is not a~@
            property list."))

(fundoc 'push
        (fmt "Lambda list: (ITEM PLACE)~@
              where ITEM is any object, and PLACE is a place whose value~@
              may be any object.  PLACE is not evaluated.~@
              PUSH replaces the value of PLACE by a CONS of the ITEM and the~@
              old value of PLACE, and then returns ITEM.~@
              (PUSH ITEM PLACE) is equivalent to (SETF PLACE (CONS ITEM PLACE),~@
              except that subforms of PLACE are evaluated only once."))

(fundoc 'pop
        (fmt "Lambda list: (PLACE)~@
              where PLACE is a place whose value must be a list,~@
              i.e, a CONS cell or NIL.  PLACE is not evaluated.~@
              POP remembers the CAR of the list that is the value of PLACE,~@
              replaces the value of PLACE by the CDR of that value, and finally~@
              returns the remembered value.~@
              If the value of PLACE is NIL, then NIL is returned, and the value~@
              of PLACE remains NIL.~@
              (POP ITEM PLACE) is equivalent to 
              (PROG1 (CAR PLACE) (SETF PLACE (CDR PLACE))), except that suforms~@
              of PLACE are evaluated only once.~@
              If the value of PLACE is not a LIST, then an error of type TYPE-ERROR~@
              is signaled."))

(fundoc 'remf
        (fmt "Lambda list: (PLACE INDICATOR)~@
              where PLACE whose value is a property list,~@
              and INDICATOR is any object.  PLACE is not evaluated.~@
              If the property list that is the value of PLACE contains an~@
              indicator that is identical (as compared by EQ) to INDICATOR,~@
              then that indicator and its associated value are destructively~@
              removed from the property list.  If there are several such indicators~@
              only the first one and its associated value are removed.~@
              REMF returns true if any any matching indicator was found,~@
              otherwise it returns false.~@
              REMF may modify any part of the property list that is the value~@
              of PLACE, or it may modify the value of PLACE itself.~@
              The consequences are undefined if the value of PLACE is not a~@
              property list."))

(fundoc 'pushnew
        (fmt "Lambda list: (ITEM PLACE &key KEY TEST TEST-NOT)~@
              where ITEM is any object, and PLACE is a PLACE~@
              whose value is a proper list.  PLACE is not evaluated.~@
              KEY is is a designator of a function of one argument~@
              or NIL, which means IDENTITY.~@
              TEST and TEST-NOT are designators for functions of two~@
              arguments that return a generalized boolean.  The default~@
              if neither TEST nor TEST-NOT is given is a TEST of EQL.~@
              The list that is the value of PLACE is checked for an occurrence~@
              of ITEM.  To determine whether the item is the same as an element~@
              of the list, KEY is first applied to both the ITEM and the element.~@
              Then, if TEST is given, the TEST is applied to the result of applying~@
              the KEY function to ITEM and to the element, in that order.~@
              If the TEST returns true for some element of the list,~@
              then the list is not altered. Otherwise the value of PLACE is replaced~@
              by the CONS of the ITEM and that value.~@
              If instead TEST-NOT is given, the TEST-NOT is applied to the result of~@
              applying the KEY function to ITEM and to the element, in that order.~@
              If the TEST-NOT returns false for some element of the list,~@
              then the list is not altered. Otherwise the value of PLACE is replaced~@
              by the CONS of the ITEM and that value.~@
              The consequences are undefined if the value of PLACE is not a~@
              proper list."))

(setf (documentation 'list 'type)
      (fmt "System class LIST.~@
            Class precedence list: LIST, SEQUENCE, T~@
            A LIST is a chain of CONS cells linked by their CDR,~@
            or NIL which is considered to be the empty LIST.~@
            The elements of the list are the contents of the CAR of each CONS cell.~@
            A LIST is a proper LIST if it is either the empty list (NIL),~@
            or a chain of CONS cells terminated by NIL.~@
            A LIST is a dotted LIST if it starts with a CONS cell, and is~@
            terminated by an atom other than NIL.~@
            A LIST is a circular LIST if it starts with a CONS cell, and~@
            none of its top-level CONS cells contains an atom in its CDR.~@
            As a type, LIST is equivalent to (or CONS NULL)."))

(setf (documentation 'null 'type)
      (fmt "System class NULL.~@
            Class precedence list: NULL, SYMBOL, LIST, SEQUENCE, T~@
            The NULL type contains only one object: NIL, which is also~@
            the empty list."))

(setf (documentation 'cons 'type)
      (fmt "System class CONS.~@
            Class precedence list: CONS LIST, SEQUENCE, T~@
            The CONS system class contains all CONS cells."))
