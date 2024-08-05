(cl:in-package #:sicl-cons-high)

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

(fundoc 'cons
        (fmt "Lambda list: (OBJECT-1 OBJECT-2)~@
              Return a new CONS cell with OBJECT-1 in the~@
              CAR field and OBJECT-2 in the CDR field."))
              
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

(fundoc 'listp
        (fmt "Lambda list: (OBJECT)~@
              where OBJECT is any object.~@
              Return true if OBJECT is of type LIST, i.e., either a CONS cell~@
              or NIL.  Return false otherwise."))

(fundoc 'null
        (fmt "Lambda list: (OBJECT)~@
              where OBJECT is any object.~@
              Return true if OBJECT is NIL.  Return false otherwise."))

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
