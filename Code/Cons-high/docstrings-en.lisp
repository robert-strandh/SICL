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
	(frm "Lambda list: (LIST)~@
              where LIST is a CONS cell or NIL.~@
              If LIST is a CONS cell, then the CDR of that cell is returned.~@
              If LIST is NIL, then NIL is returned.~@
              If LIST is an atom other than NIL, then an error of type TYPE-ERROR~@
              is signaled."))

