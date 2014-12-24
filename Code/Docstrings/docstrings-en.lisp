(in-package :sicl.documentation)

(defun fmt (&rest args)
  (apply #'format nil args))

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
              Return a list containing the objects in OBJECTS.~@"))

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
              
(fundoc '<
        (fmt "Lambda list: (&rest NUMBERS).~@
              At least one argument is required.~@
              Return true if the numbers in NUMBERS are in~@
              monotonically strictly increasing order.~@
              The consequences are undefined if some of the objects~@
              in numbers are not real numbers, but if that is the case~@
              and a condition is signaled, that condition is an error~@
              of type TYPE-ERROR.~@
              Might signal an error of type ARITHMETIC-ERROR if~@
              unable to fulfill its contract."))

(fundoc '<=
        (fmt "Lambda list: (&rest NUMBERS).~@
              At least one argument is required.~@
              Return true if the numbers in NUMBERS are in~@
              monotonically non-decreasing order.~@
              The consequences are undefined if some of the objects~@
              in numbers are not real numbers, but if that is the case~@
              and a condition is signaled, that condition is an error~@
              of type TYPE-ERROR.~@
              Might signal an error of type ARITHMETIC-ERROR if~@
              unable to fulfill its contract."))

(fundoc '=
        (fmt "Lambda list: (&rest NUMBERS).~@
              At least one argument is required.~@
              Return true if the numbers in NUMBERS have the same value.~@
              The consequences are undefined if some of the objects~@
              in numbers are not real numbers, but if that is the case~@
              and a condition is signaled, that condition is an error~@
              of type TYPE-ERROR.~@
              Might signal an error of type ARITHMETIC-ERROR if~@
              unable to fulfill its contract."))

(fundoc '/=
        (fmt "Lambda list: (&rest NUMBERS).~@
              At least one argument is required.~@
              Return true no two numbers in NUMBERS have the same value.~@
              The consequences are undefined if some of the objects~@
              in numbers are not real numbers, but if that is the case~@
              and a condition is signaled, that condition is an error~@
              of type TYPE-ERROR.~@
              Might signal an error of type ARITHMETIC-ERROR if~@
              unable to fulfill its contract."))

(fundoc '>
        (fmt "Lambda list: (&rest NUMBERS).~@
              At least one argument is required.~@
              Return true if the numbers in NUMBERS are in~@
              monotonically strictly decreasing order.~@
              The consequences are undefined if some of the objects~@
              in numbers are not real numbers, but if that is the case~@
              and a condition is signaled, that condition is an error~@
              of type TYPE-ERROR.~@
              Might signal an error of type ARITHMETIC-ERROR if~@
              unable to fulfill its contract."))

(fundoc '>=
        (fmt "Lambda list: (&rest NUMBERS).~@
              At least one argument is required.~@
              Return true if the numbers in NUMBERS are in~@
              monotonically non-increasing order.~@
              The consequences are undefined if some of the objects~@
              in numbers are not real numbers, but if that is the case~@
              and a condition is signaled, that condition is an error~@
              of type TYPE-ERROR.~@
              Might signal an error of type ARITHMETIC-ERROR if~@
              unable to fulfill its contract."))

(fundoc 'abort
	(fmt "Lambda list: (&optional CONDITION).~@
              Search for the most recently established restart named ABORT~@
              and transfer control to that restart.~@
              If no such restart exists, then an error of type CONTROL-ERROR~@
              is signaled.~@
              The argument CONDITION must be a condition object, or NIL.~@
              When CONDITION is given, the restarts that are searched are~@
              only the restarts that have CONDITION explicitly associated~@
              with them, or restarts having no conditions associated with them.~@
              When CONDITION is nil all restarts are searched."))            

(fundoc 'continue
	(fmt "Lambda list: (&optional CONDITION).~@
              Search for the most recently established restart named CONTINUE~@
              and transfer control to that restart.~@
              If no such restart exists, NIL is returned.~@
              The argument CONDITION must be a condition object, or NIL.~@
              When CONDITION is given, the restarts that are searched are~@
              only the restarts that have CONDITION explicitly associated~@
              with them, or restarts having no conditions associated with them.~@
              When CONDITION is nil all restarts are searched."))            

(fundoc 'muffle-warnings
	(fmt "Lambda list: (&optional CONDITION).~@
              Search for the most recently established restart named MUFFLE-WARNINGS~@
              and transfer control to that restart.~@
              If no such restart exists, then an error of type CONTROL-ERROR~@
              is signaled.~@
              The argument CONDITION must be a condition object, or NIL.~@
              When CONDITION is given, the restarts that are searched are~@
              only the restarts that have CONDITION explicitly associated~@
              with them, or restarts having no conditions associated with them.~@
              When CONDITION is nil all restarts are searched."))            

(fundoc 'store-value
	(fmt "Lambda list: (&optional CONDITION).~@
              Search for the most recently established restart named STORE-VALUE~@
              and transfer control to that restart.~@
              If no such restart exists, NIL is returned.~@
              The argument CONDITION must be a condition object, or NIL.~@
              When CONDITION is given, the restarts that are searched are~@
              only the restarts that have CONDITION explicitly associated~@
              with them, or restarts having no conditions associated with them.~@
              When CONDITION is nil all restarts are searched."))            

(fundoc 'use-value
	(fmt "Lambda list: (&optional CONDITION).~@
              Search for the most recently established restart named USE-VALUE~@
              and transfer control to that restart.~@
              If no such restart exists, NIL is returned.~@
              The argument CONDITION must be a condition object, or NIL.~@
              When CONDITION is given, the restarts that are searched are~@
              only the restarts that have CONDITION explicitly associated~@
              with them, or restarts having no conditions associated with them.~@
              When CONDITION is nil all restarts are searched."))            

(fundoc 'abs
        (fmt "Lambda list: (NUMBER).~@
              Return the absolute value of the number NUMBER.~@
              If given a real number, the result type is the same~@
              as that of the number given, so that if for instance~@
              a double float is given, then the result is a double
              float as well.~@
              If given a complex number, the result is a real number.~@
              In that case, the return value might be a floating point number,~@
              even if the result could be expressed as an exact rational number.~@
              The consequences are undefine if NUMBER is not a number."))

(fundoc 'acons
        (fmt "Lambda List: (KEY DATUM ALIST).~@
              Return a new association list with a CONS of the~@
              KEY and the DATUM as a first element, and ALIST as the~@
              rest.  It is entirely equivalent to~@
              (cons (cons KEY DATUM) ALIST."))

(fundoc 'acos 
        (fmt "Lambda list: (NUMBER).~@
              Return the arc cosine of the number NUMBER.~@
              If NUMBER is not a number, then an error of type~@
              TYPE-ERROR is signaled.~@
              Might signal an error of type ARITHMETIC-ERROR if~@
              unable to fulfill its contract."))
        
(fundoc 'asin 
        (fmt "Lambda list: (NUMBER).~@
              Return the arc sine of the number NUMBER.~@
              If NUMBER is not a number, then an error of type~@
              TYPE-ERROR is signaled.~@
              Might signal an error of type ARITHMETIC-ERROR if~@
              unable to fulfill its contract."))
        
(fundoc 'atan 
        (fmt "Lambda list: (NUMBER1 &optional NUMBER2).~@
              If NUMBER2 is not supplied, return the arc tangent~@
              of the number NUMBER1.  In that case, NUMBER1 can be~@
              any number.~@
              If NUMBER2 is supplied, return the arc tangent of~@
              NUMBER1/NUMBER2.  In that case, NUMBER1 and NUMBER2 must~@
              both be real numbers.~@
              If NUMBER2 is not given, the result is in the interval~@
              ]-pi/2,pi/2[~@
              If number2 is given, the result is in the interval~@
              [-pi,pi[ when minus zero is NOT supported, and in the interval~@
              [-pi,pi] when minus zero IS supported.~@
              If NUMBER2 is not supplied and NUMBER1 is not a number,~@
              then an error of type TYPE-ERROR is signaled.~@
              If NUMBER2 is supplied, and at least one of the two~@
              numbers is not real, an error of type TYPE-ERROR is signaled.~@
              Might signal an error of type ARITHMETIC-ERROR if~@
              unable to fulfill its contract."))
        
(fundoc 'asinh 
        (fmt "Lambda list: (NUMBER).~@
              Return the hyperbolic arc sine of the number NUMBER.~@
              If NUMBER is not a number, then an error of type~@
              TYPE-ERROR is signaled.~@
              Might signal an error of type ARITHMETIC-ERROR if~@
              unable to fulfill its contract."))
        
(fundoc 'acosh
        (fmt "Lambda list: (NUMBER).~@
              Return the hyperbolic arc cosine of the number NUMBER.~@
              If NUMBER is not a number, then an error of type~@
              TYPE-ERROR is signaled.~@
              Might signal an error of type ARITHMETIC-ERROR if~@
              unable to fulfill its contract."))
        
(fundoc 'atanh
        (fmt "Lambda list: (NUMBER).~@
              Return the hyperbolic arc tangent of the number NUMBER.~@
              If NUMBER is not a number, then an error of type~@
              TYPE-ERROR is signaled.~@
              Might signal an error of type ARITHMETIC-ERROR if~@
              unable to fulfill its contract."))
        
(fundoc 'add-method
        (fmt "Lambda list: (GENERIC-FUNCTION METHOD).~@
              Add a method to a generic function.~@
              If there is already a method of GENERIC-FUNCTION~@
              with the same parameter specializers, and the same~@
              qualifiers, then METHOD replaces the existing one.~@
              If the lambda list of the method function of METHOD~@
              is not congruent with that of GENERIC-FUNCTION,~@
              then an error of type TYPE-ERROR is signaled.~@
              If METHOD is a method of a generic function other than~@
              GENERIC-FUNCTION, then an error of type TYPE-ERROR is signaled.~@
              The consequences are undefined if GENERIC-FUNCTION~@
              is not a generic function, or of METHOD is not a method."))

(fundoc 'adjoin
        (fmt "Lambda list: (ITEM LIST &key KEY TEST TEST-NOT).~@
              If ITEM is already an element of LIST, then return LIST.~@
              Otherwise return LIST with ITEM as an additional element,~@
              as if (cons ITEM LIST) had been called.~@
              KEY is a designator for a function that takes one argument~@
              which is applied to ITEM and the elements of LIST before~@
              an equality test is applied.~@
              TEST and TEST-NOT are designators functions of functions~@
              that take two arguments and which return a true value if~@
              and only if their arguments are considered equal.~@
              TEST and TEST-NOT must not be given simultaneously.~@
              ADJOIN might signal an error of type TYPE-ERROR is LIST is~@
              not a proper list."))

(fundoc 'adjustable-array-p
        (fmt "Lambda list: (ARRAY).~@
              Return a true value if and only if ARRAY is an adjustable~@
              array, i.e., if passing ARRAY to ADJUST-ARRAY could return~@
              an array identical to ARRAY.~@
              If ARRAY is not an array, then an error of type TYPE-ERROR~@
              is signaled."))

(fundoc 'alpha-char-p
	(fmt "Lambda list: (CHARACTER).~@
              Return true if CHARACTER is alphabetic.  Return false otherwise.~@
              If CHARACTER is not a character, an error of type type-error~@
              is signaled."))

(fundoc 'alphanumericp
	(fmt "Lambda list: (CHARACTER).~@
              Return true if CHARACTER is alphabetic or numeric.~@
              Return false otherwise.~@
              If CHARACTER is not a character, an error of type type-error~@
              is signaled."))

(fundoc 'append
	(fmt "Lambda list: (&rest LISTS).~@
              Return a new list which is the concatenation of the lists in LISTS.~@
              All arguments except the last one must be proper lists.~@
              The last argument may be any object.~@
              Each of the argument except the last one is copied.~@
              The last argument is not copied, and shares structure with
              the return value.~@
              As a special case, when every argument except the last is NIL~@
              (which includes the case where there is only one argument),~@
              then the last argument is returned.~@"))

(fundoc 'array-dimension
	(fmt "Lambda list: (ARRAY AXIS-NUMBER).~@
              Return the dimension of the array for the given axis number.~@
              AXIS-NUMBER is an integer greater than or equal to zero~@
              and less than the rank of the array.~@
              If the array has a fill pointer, then it is ignored." ))

(fundoc 'array-dimensions
	(fmt "Lambda list: (ARRAY).~@
              Return a list of dimensions for the array.~@
              If the array has a fill pointer, then it is ignored.~@
              If ARRAY is not an array, then an error of type TYPE-ERROR~@
              is signaled."))

(fundoc 'array-displacement
	(fmt "Lambda list: (ARRAY).~@
              If ARRAY is displaced to another array, then~@
              return the array that ARRAY is displaced to, and~@
              displaced index offset of ARRAY.~@
              If ARRAY is not displaced to another array, then the~@
              values NIL and 0 are returned.~@
              If ARRAY is not an array, then an error of type TYPE-ERROR~@
              is signaled."))

(fundoc 'array-element-type
	(fmt "Lambda list: (ARRAY).~@
              Return a type specifier representing the type of object~@
              that ARRAY can hold.  This type specifier might be a supertype~@
              of the one used when the array was created, because the~@
              implementation may have upgraded the element type.
              If ARRAY is not an array, then an error of type TYPE-ERROR~@
              is signaled."))

(fundoc 'array-has-fill-pointer-p
	(fmt "Lambda list: (ARRAY).~@
              Return true if and only if ARRAY has a fill pointer.~@
              If ARRAY is not an array, then an error of type TYPE-ERROR~@
              is signaled."))

(fundoc 'array-in-bounds-p
	(fmt "Lambda list: (ARRAY &rest SUBSCRIPTS).~@
              Return true if and only if SUBSCRIPTS are valid subscripts~@
              for ARRAY, i.e. if those subscripts can be used to access~@
              an element of ARRAY.~@
              The number of subscripts given must be equal to the rank~@
              of ARRAY."))

(fundoc 'array-rank
	(fmt "Lambda list: (ARRAY).@
              Return the rank of the array, which is a nonnegative integer~@
              indicating the number of dimensions of the array.~@
              When ARRAY is not an array, an error of type TYPE-ERROR~@
              is signaled."))

(fundoc 'array-row-major-index
	(fmt "Lambda list: (ARRAY &rest SUBSCRIPTS).~@
              Return a valid array row-major index for the array, i.e.~@
              an index that indicates the position of an element of ARRAY ~@
              having the indicated SUBSCRIPTS, and that can be used ~@
              to access the corresponding element using ROW-MAJOR-AREF.~@
              The the number of SUBSCRIPS must be the same as the rank~@
              of ARRAY, and each one must be a nonnegative integer which~@
              is less than the size of the corresponing dimension."))

(fundoc 'array-total-size
	(fmt "Lambda list: (ARRAY).~@
              Return the total size of the array, which is a nonnegative~@
              integer indicating the total number of elements of ARRAY.~@
              Fill pointers are not taken into account.~@
              When ARRAY is not an array, an error of type TYPE-ERROR~@
              is signaled."))

(fundoc 'arrayp
	(fmt "Lambda list: (ARRAY).~@
              Return true if ARRAY is of type array, and fals otherwise."))

(fundoc 'assoc
	(fmt "Lambda list: (ITEM ALIST &key KEY TEST TEST-NOT).~@
              Return the first cons in ALIST that has a car that satisfies~@
              the test.~@
              When neither of KEY, TEST and TEST-NOT are given, the test is~@
              satisfied when the car of the entry in alist is EQL to ITEM.~@
              When TEST is given, it must be a designator for a function of~@
              two arguments that is used in place of EQL to determine wheter~@
              the test is satisified, i.e., if the function returns true when~@
              applied to ITEM and the car of the entry of ALIST, then that~@
              entry is returned. 
              When TEST-NOT is given, it must be a designator for a function of~@
              two arguments that is is applied to ITEM and the the car of the~@
              entries of ALIST, and if the function then returns false for some~@
              entry, then that entry is returned.~@
              At most one of TEST and TEST-NOT may be given.~@
              When KEY is given and not NIL, then it must be a designator for~@
              a function of one argument.  It is then applied to the car of each~@
              entry of ALIST before the test is applied.  When KEY is not given~@
              or NIL, it is as if IDENTITY were used."))

(fundoc 'assoc-if
	(fmt "Lambda list: (PREDICATE ALIST &key KEY).~@
              Return the first cons in ALIST that has a car that satisfies~@
              the test.~@
              PREDICATE is a designator for a function of one argument.~@
              When KEY is not given, the test is satisified when PREDICATE~@
              returns true when applied to the car of an entry of ALIST.~@
              When KEY is given, and not NIL, it must be a designator for~@
              a function of one argument, and it is then applied to the car~@
              of the entry of ALIST before PREDICATE is applied."))

(fundoc 'assoc-if-not
	(fmt "Lambda list: (PREDICATE ALIST &key KEY).~@
              Return the first cons in ALIST that has a car that satisfies~@
              the test.~@
              PREDICATE is a designator for a function of one argument.~@
              When KEY is not given, the test is satisified when PREDICATE~@
              returns false when applied to the car of an entry of ALIST.~@
              When KEY is given, and not NIL, it must be a designator for~@
              a function of one argument, and it is then applied to the car~@
              of the entry of ALIST before PREDICATE is applied."))

(fundoc 'atom
        (fmt "Lambda list: (OBJECT).~@
              Return true if OBJECT is an atom (i.e. anything other than~@
              a cons cell), otherwise false"))

(fundoc 'bit-and
	(fmt "Lambda list: (BIT-ARRAY-1 BIT-ARRAY-2 &optional OPTIONAL).~@
              Return a bit array that contains the logical AND~@
              of the bits in BIT-ARRAY-1 and those in BIT-ARRAY-2.~@
              BIT-ARRAY-1 and BIT-ARRAY-2 have the same rank and~@
              the same dimensions.~@
              If OPTIONAL is NIL or not or omitted, a new array is~@
              allocated and returned.~@
              If OPTIONAL is a bit array, its contents is replaced by~@
              the result of the operation, and this array is returned~@
              as the value of the function.~@
              If OPTIONAL is T, the contents of BIT-ARRAY-1 is replaced~@
              by the result of the operation."))

(fundoc 'bit-eqv
	(fmt "Lambda list: (BIT-ARRAY-1 BIT-ARRAY-2 &optional OPTIONAL).~@
              Return a bit array that contains the logical complement~@
              of the exclusive OR of the bits in BIT-ARRAY-1 and those~@
              in BIT-ARRAY-2.~@
              BIT-ARRAY-1 and BIT-ARRAY-2 have the same rank and~@
              the same dimensions.~@
              If OPTIONAL is NIL or not or omitted, a new array is~@
              allocated and returned.~@
              If OPTIONAL is a bit array, its contents is replaced by~@
              the result of the operation, and this array is returned~@
              as the value of the function.~@
              If OPTIONAL is T, the contents of BIT-ARRAY-1 is replaced~@
              by the result of the operation."))

(fundoc 'bit-ior
	(fmt "Lambda list: (BIT-ARRAY-1 BIT-ARRAY-2 &optional OPTIONAL).~@
              Return a bit array that contains the logical OR (inclusive)~@
              of the bits in BIT-ARRAY-1 and those in BIT-ARRAY-2.~@
              BIT-ARRAY-1 and BIT-ARRAY-2 have the same rank and~@
              the same dimensions.~@
              If OPTIONAL is NIL or not or omitted, a new array is~@
              allocated and returned.~@
              If OPTIONAL is a bit array, its contents is replaced by~@
              the result of the operation, and this array is returned~@
              as the value of the function.~@
              If OPTIONAL is T, the contents of BIT-ARRAY-1 is replaced~@
              by the result of the operation."))

(fundoc 'bit-xor
	(fmt "Lambda list: (BIT-ARRAY-1 BIT-ARRAY-2 &optional OPTIONAL).~@
              Return a bit array that contains the logical exclusive OR~@
              of the bits in BIT-ARRAY-1 and those in BIT-ARRAY-2.~@
              BIT-ARRAY-1 and BIT-ARRAY-2 have the same rank and~@
              the same dimensions.~@
              If OPTIONAL is NIL or not or omitted, a new array is~@
              allocated and returned.~@
              If OPTIONAL is a bit array, its contents is replaced by~@
              the result of the operation, and this array is returned~@
              as the value of the function.~@
              If OPTIONAL is T, the contents of BIT-ARRAY-1 is replaced~@
              by the result of the operation."))

(fundoc 'bit-nand
	(fmt "Lambda list: (BIT-ARRAY-1 BIT-ARRAY-2 &optional OPTIONAL).~@
              Return a bit array that contains the logical complement~@
              of the logical AND of the bits in BIT-ARRAY-1 and those~@
              in BIT-ARRAY-2.~@
              BIT-ARRAY-1 and BIT-ARRAY-2 have the same rank and~@
              the same dimensions.~@
              If OPTIONAL is NIL or not or omitted, a new array is~@
              allocated and returned.~@
              If OPTIONAL is a bit array, its contents is replaced by~@
              the result of the operation, and this array is returned~@
              as the value of the function.~@
              If OPTIONAL is T, the contents of BIT-ARRAY-1 is replaced~@
              by the result of the operation."))

(fundoc 'bit-nor
	(fmt "Lambda list: (BIT-ARRAY-1 BIT-ARRAY-2 &optional OPTIONAL).~@
              Return a bit array that contains the logical complement~@
              of the logical OR (inclusive) of the bits in BIT-ARRAY-1 and~@
              those in BIT-ARRAY-2.~@
              BIT-ARRAY-1 and BIT-ARRAY-2 have the same rank and~@
              the same dimensions.~@
              If OPTIONAL is NIL or not or omitted, a new array is~@
              allocated and returned.~@
              If OPTIONAL is a bit array, its contents is replaced by~@
              the result of the operation, and this array is returned~@
              as the value of the function.~@
              If OPTIONAL is T, the contents of BIT-ARRAY-1 is replaced~@
              by the result of the operation."))

(fundoc 'bit-andc1
	(fmt "Lambda list: (BIT-ARRAY-1 BIT-ARRAY-2 &optional OPTIONAL).~@
              Return a bit array that contains the logical AND~@
              of the complement of the bits in BIT-ARRAY-1 and the~@
              bits in BIT-ARRAY-2.~@
              BIT-ARRAY-1 and BIT-ARRAY-2 have the same rank and~@
              the same dimensions.~@
              If OPTIONAL is NIL or not or omitted, a new array is~@
              allocated and returned.~@
              If OPTIONAL is a bit array, its contents is replaced by~@
              the result of the operation, and this array is returned~@
              as the value of the function.~@
              If OPTIONAL is T, the contents of BIT-ARRAY-1 is replaced~@
              by the result of the operation."))

(fundoc 'bit-andc2
	(fmt "Lambda list: (BIT-ARRAY-1 BIT-ARRAY-2 &optional OPTIONAL).~@
              Return a bit array that contains the logical AND~@
              of the bits in BIT-ARRAY-1 and the complement of the~@
              bits in BIT-ARRAY-2.~@
              BIT-ARRAY-1 and BIT-ARRAY-2 have the same rank and~@
              the same dimensions.~@
              If OPTIONAL is NIL or not or omitted, a new array is~@
              allocated and returned.~@
              If OPTIONAL is a bit array, its contents is replaced by~@
              the result of the operation, and this array is returned~@
              as the value of the function.~@
              If OPTIONAL is T, the contents of BIT-ARRAY-1 is replaced~@
              by the result of the operation."))

(fundoc 'bit-orc1
	(fmt "Lambda list: (BIT-ARRAY-1 BIT-ARRAY-2 &optional OPTIONAL).~@
              Return a bit array that contains the logical OR~@
              of the complement of the bits in BIT-ARRAY-1 and the~@
              bits in BIT-ARRAY-2.~@
              BIT-ARRAY-1 and BIT-ARRAY-2 have the same rank and~@
              the same dimensions.~@
              If OPTIONAL is NIL or not or omitted, a new array is~@
              allocated and returned.~@
              If OPTIONAL is a bit array, its contents is replaced by~@
              the result of the operation, and this array is returned~@
              as the value of the function.~@
              If OPTIONAL is T, the contents of BIT-ARRAY-1 is replaced~@
              by the result of the operation."))

(fundoc 'bit-orc2
	(fmt "Lambda list: (BIT-ARRAY-1 BIT-ARRAY-2 &optional OPTIONAL).~@
              Return a bit array that contains the logical OR~@
              of the bits in BIT-ARRAY-1 and the complement of the~@
              bits in BIT-ARRAY-2.~@
              BIT-ARRAY-1 and BIT-ARRAY-2 have the same rank and~@
              the same dimensions.~@
              If OPTIONAL is NIL or not or omitted, a new array is~@
              allocated and returned.~@
              If OPTIONAL is a bit array, its contents is replaced by~@
              the result of the operation, and this array is returned~@
              as the value of the function.~@
              If OPTIONAL is T, the contents of BIT-ARRAY-1 is replaced~@
              by the result of the operation."))

(fundoc 'bit-not
	(fmt "Lambda list: (BIT-ARRAY &optional OPTIONAL).~@
              Return a bit array that contains the logical complement~@
              of the bits in BIT-ARRAY.~@
              If OPTIONAL is NIL or not or omitted, a new array is~@
              allocated and returned.~@
              If OPTIONAL is a bit array, its contents is replaced by~@
              the result of the operation, and this array is returned~@
              as the value of the function.~@
              If OPTIONAL is T, the contents of BIT-ARRAY is replaced~@
              by the result of the operation."))

;;; do something fancy with format to get a table.
(fundoc 'boole
	(fmt "Lambda list: (OPERATION INTEGER-1 INTEGER-2).~@
              Return the result of applying the bitwise locical~@
              operation indicated by OPERATION to INTEGER-1 and INTEGER-2.~@
              The two integers are treated as if represented in binary~@
              two's complement.  A nonnegative integer is considered starting~@
              with an infinite number of 0s ans a negative integer is considered~@
              starting with an infinite number of 1s.~@
              The operation is given by the value of 16 constant variables:~@

              boole-1      INTEGER-1 is returned, and INTEGER-2 is ignored.~@
              boole-2      INTEGER-2 is returned, and INTEGER-1 is ignored.~@
              boole-andc1  The AND of the complement of INTEGER-1 and of INTEGER-2@
                           is returned.~@
              boole-andc1  The AND of INTEGER-1 and the complement of INTEGER-2@
                           is returned.~@
              boole-and    The AND of INTEGER-1 and of INTEGER-2 is returned.~@
              boole-c1     The complement of INTEGER-1 is returned.~@
                           INTEGER-2 is ignored.~@
              boole-c2     The complement of INTEGER-2 is returned.~@
                           INTEGER-1 is ignored.~@
              boole-clr    The integer 0 is returned.  Both INTEGER-1 and~@
                           INTEGER-2 are ignored.~@
              boole-eqv    The complement of the exclusive OR of INTEGER-1 and~@
                           INTEGER-2 is returned.~@
              boole-ior    The (inclusive) OR of INTEGER-1 and INTEGER-2 is returned.~@
              boole-nand   The complement of the AND of INTEGER-1 and INTEGER-2~@
                           is returned.~@
              boole-nor    The complement of the (inclusive) OR of INTEGER-1 and~@
                           INTEGER-2 is returned.~@
              boole-orc1   The (inclusive) OR of the complement of INTEGER-1~@
                           and of INTEGER-2 is returned.~@
              boole-orc2   The (inclusive) OR of INTEGER-1 and of the complement of~@
                           INTEGER-2 is returned.~@
              boole-clr    The integer -1 is returned.  Both INTEGER-1 and~@
                           INTEGER-2 are ignored.~@
              boole-xor    The exclusive or of INTEGER-1 and of INTEGER-2 is returned.~@

              An error of type TYPE-ERROR is signaled if OPERATION is not a 
              bitwise logicial operation specifier as indicated above, or if
              any of INTEGER-1 and INTEGER-2 is not an integer."))

(fundoc 'both-case-p
	(fmt "Lambda list: (CHARACTER).~@
              Return true if CHARACTER is a character with case, i.e., if~@
              CHARACTER exists in both an upper-case and a lower-case version.~@
              An error of type TYPE-ERROR is signaled if CHARACTER is not a character."))

(fundoc 'lower-case-p
	(fmt "Lambda list: (CHARACTER).~@
              Return true if CHARACTER is a lower-case character, i.e., if it is the~@
              lower-case version of a character that has both upper and lower case~@
              versions.
              An error of type TYPE-ERROR is signaled if CHARACTER is not a character."))

(fundoc 'upper-case-p
	(fmt "Lambda list: (CHARACTER).~@
              Return true if CHARACTER is a upper-case character, i.e., if it is the~@
              upper-case version of a character that has both upper and lower case~@
              versions.
              An error of type TYPE-ERROR is signaled if CHARACTER is not a character."))

(fundoc 'boundp
	(fmt "Lambda list: (SYMBOL).~@
              Return true if and only if SYMBOL is bound in the null lexical environment,~@
              i.e., if it has a value as a value in the global environment.~@
              Lexical bindings are not taken into account.~@
              An error of type TYPE-ERROR is signaled if SYMBOL is not a symbol.~@"))

(fundoc 'consp
        (fmt "Lambda list: (OBJECT).~@
              Return true if OBJECT is a cons cell, false otherwise."))

(fundoc 'copy-tree
        (fmt "Lambda list: (TREE).~@
              Return a copy of the tree TREE.~@
              A deep copy of TREE is constructed, in that all the cons cells~@
              of TREE are recursively copied.  Only the leaves (i.e. atoms)~@
              of TREE are not copied.~@
              Circularity or partial substructure sharing in TREE~@
              are not preserved."))

(fundoc 'decode-float
	(fmt "Lambda list: (FLOAT).~@
              Return three values: the significand, the expnent, and the sign~@
              of the argument.  The return values are related to each other~@
              in that (* significand (expt (float b exponent)) sign) where~@
              b is the radix of the radix of the floating-point representation~@
              as reported by FLOAT-RADIX.
              The significand is a floating-point number of the same type as~@
              the argument, and it is scaled so that it is greater than~@
              or equal to 1/b where b again is the radix of the floating-point~@
              representation, and strictly less than 1.  If the argument is zero~@
              (positive or negative) the significand is positive zero.~@
              The exponent is an integer that makes the relation described~@
              above hold. If the argument is zero, then the exponent could~@
              be some arbitrary integer.~@
              The sign is a floating-point value of the same type as the argument~@
              and is equal to 1.0 if the argument is greater than or equal to 0~@
              and equal to -1.0 if the argument is negative."))

(fundoc 'float-digits
	(fmt "Lambda list: (FLOAT).~@
              Return the number of digits used in the representation of FLOAT.~@
              The return value is a nonnegative integer and represents the~@              
              number of radix-b digits, where b is the radix of the number,~@
              as reported by FLOAT-RADIX.~@
              The number includes digits that are not necessarily explicitly~@
              present in the representation of the floating-point number.~@
              In particular, if IEEE 754 arithmetic is used, the return-value~@
              is one plus the size of the field used to represent the mantissa.~@
              The return value does not change as a result of the number being~@
              represented with fewer significant digits, such as when IEEE 754~@
              denormalized numbers are used.  To detect such situations,~@
              use FLOAT-PRECISION instead."))

(fundoc 'float-radix
	(fmt "Lambda list: (FLOAT).~@
              Return the number of significant digits used in the representation~@
              of the argument.~@
              The return value is a nonnegative integer and represents the~@              
              number of radix-b digits, where b is the radix of the number,~@
              as reported by FLOAT-RADIX.~@
              If the argument is numerically equal to 0, then the return value~@
              is the integer 0.~@
              This function is different from FLOAT-DIGITS in that if the number~@
              is stored with fewer than the maximum number of digits possible,~@
              such as when IEEE 754 denormalized numbers are used, then the loss~@
              of significant digits is reflected in the return value of this function."))

(fundoc 'float-radix
	(fmt "Lambda list: (FLOAT).~@
              Return the radix of its argument.  
              The radix is an integer whose value must be taken into account~@
              in other floating-point functions, notably DECODE-FLOAT.~@"

(fundoc 'float-sign
	(fmt "Lambda list: (FLOAT-1 &optional FLOAT-2).~@
              Return a floating-point number that has the sign of FLOAT-1~@
              and the magnitude of FLOAT-2.  The default value of FLOAT-2~@
              is (float 1 FLOAT-1), that is, it is numerically equal to 1~@
              and it has the same type as FLOAT-1."))

(fundoc 'integer-decode-float
	(fmt "Lambda list: (FLOAT).@
              Return three values: the significand, the exponent, and the sign~@
              of the argument.  The return values are related to each other~@
              in that (scale-float (float significand FLOAT) exponent)~@
              is equal to (abs FLOAT).  However there are no restrictions~@
              on the magnitude of the significand and the exponent.  Some~@
              arbitrary scaling between the two are possible.~@
              The significand is an integer that represents the mantissa~@
              of the argument. If the argument is zero, then the value of the~@
              significand is 0. 
              The exponent is an integer that makes the relation describe~@
              above hold.  If the argument is zero, then the value of the~@
              exponent is some arbitrary integer.~@
              The sign is an integer equal to -1 if the argument is negative~@
              and equal to 1 if the argument is positive or 0."

(fundoc 'nsublis
        (fmt "Lambda list: (ALIST TREE &key KEY TEST TEST-NOT).~@
              Return a new tree, which is like TREE, except that occurrences~@
              of keys of the association list ALIST in TREE are replaced by~@
              the object associated with that key in ALIST.  When no substitutions~@
              are made, the original tree TREE is returned.~@
              Relevant parts of TREE may be destructively modified.~@
              When KEY is given and not NIL, it is a designator for a function of~@
              one argument which is applied to the elements of TREE before~@
              testing against the keys of ALIST.~@
              When TEST is given, it must be a designator for a function that~@
              returns a generalized Boolean, and it is used to determine whether~@
              elements of TREE correspond to a key of ALIST.~@
              When TEST-NOT is given, it must be a designator for a function that~@
              returns a generalized Boolean, and it is used to determine whether~@
              elements of TREE do not correspond to a key of ALIST.~@
              At most one of TEST and TEST-NOT can be given.~@
              If neither TEST nor TEST-NOT is given, EQL is used."))

(fundoc 'rationalp
	(fmt "Lambda list: (OBJECT).~@
              Return true if OBJECT is of type RATIONAL.~@
              Return falise otherwise."))

(fundoc 'realp
	(fmt "Lambda list: (OBJECT).~@
              Return true if OBJECT is of type REAL.~@
              Return falise otherwise."))

(fundoc 'row-major-aref
	(fmt "Lambda list: (ARRAY INDEX).~@
              Return the element of the array at the row-major index INDEX.~@
              INDEX must be a valid array row-major index for ARRAY.~@
              The row-major index is the position of the element of ARRAY~@
              when the elements are stored in row-major order."))

(fundoc 'rplaca
        (fmt "Lambda list: (CONS OBJECT).~@
              Replace the contents of the car cell of the cons cell CONS~@
              by OBJECT, and return the cons cell CONS.~@
              An error of type type-error is signaled if CONS is not~@
              a cons cell. "))

(fundoc 'rplacd
        (fmt "Lambda list: (CONS OBJECT).~@
              Replace the contents of the cdr cell of the cons cell CONS~@
              by OBJECT, and return the cons cell CONS.~@
              An error of type type-error is signaled if CONS is not~@
              a cons cell. "))

(fundoc 'scale-float
	(fmt "Lambda list: (FLOAT INTEGER).~@
              Return a floating-point number that is FLOAT scaled by INTEGER,~@
              i.e., (* FLOAT (expt (float b FLOAT) INTEGER)), where b is the~@
              radix of the floating-point representation as reported by~@
              FLOAT-RADIX.~@"))

(fundoc 'sublis
        (fmt "Lambda list: (ALIST TREE &key KEY TEST TEST-NOT).~@
              Return a new tree, which is like TREE, except that occurrences~@
              of keys of the association list ALIST in TREE are replaced by~@
              the object associated with that key in ALIST.  When no substitutions~@
              are made, the original tree TREE is returned.~@
              When KEY is given and not NIL, it is a designator for a function of~@
              one argument which is applied to the elements of TREE before~@
              testing against the keys of ALIST.~@
              When TEST is given, it must be a designator for a function that~@
              returns a generalized Boolean, and it is used to determine whether~@
              elements of TREE correspond to a key of ALIST.~@
              When TEST-NOT is given, it must be a designator for a function that~@
              returns a generalized Boolean, and it is used to determine whether~@
              elements of TREE do not correspond to a key of ALIST.~@
              At most one of TEST and TEST-NOT can be given.~@
              If neither TEST nor TEST-NOT is given, EQL is used."))

(fundoc 'subst
	(fmt "Lambda list: (NEW OLD TREE &key KEY TEST TEST-NOT).~@
              Return a new tree, which is like TREE, except that each occurrence~@
              of OLD in TREE is replaced by NEW.  If no substitutions are made,~@
              then the original TREE may be returned.~@
              When KEY is given and not NIL, it is a designator for a function of~@
              one argument which is applied to the elements of TREE before~@
              testing against OLD.~@
              When TEST is given, it must be a designator for a function that~@
              returns a generalized Boolean, and it is used to determine whether~@
              elements of TREE correspond to OLD.~@
              When TEST-NOT is given, it must be a designator for a function that~@
              returns a generalized Boolean, and it is used to determine whether~@
              elements of TREE do not correspond OLD.~@
              At most one of TEST and TEST-NOT can be given.~@
              If neither TEST nor TEST-NOT is given, EQL is used."))

(fundoc 'subst-if
	(fmt "Lambda list: (NEW PREDICATE &key KEY).~@
              Return a new tree, which is like TREE except that each node~@
              for which PREDICATE returns true is replaced by NEW.~@
              If no substitutions are made, then the original TREE may be returned.~@
              PREDICATE is a designator for a function of one argument~@
              returning a generalized Boolean.~@
              When KEY is given and not NIL, it is a designator for a function of~@
              one argument which is applied to the elements of TREE before~@
              PREDICATE is applied."))

(fundoc 'subst-if-not
	(fmt "Lambda list: (NEW PREDICATE &key KEY).~@
              Return a new tree, which is like TREE except that each node~@
              for which PREDICATE returns false is replaced by NEW.~@
              If no substitutions are made, then the original TREE may be returned.~@
              PREDICATE is a designator for a function of one argument~@
              returning a generalized Boolean.~@
              When KEY is given and not NIL, it is a designator for a function of~@
              one argument which is applied to the elements of TREE before~@
              PREDICATE is applied."))
