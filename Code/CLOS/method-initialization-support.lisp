(cl:in-package #:sicl-clos)

(defun initialize-instance-after-method
    (method
     &key
       qualifiers
       (lambda-list nil lambda-list-p)
       (specializers nil specializers-p)
       (function nil function-p)
       documentation
     &allow-other-keys)
  ;; Start by checking the QUALIFIERS.  The AMOP says it should be a
  ;; proper list, so check that first.
  (unless (proper-list-p qualifiers)
    (error "qualifiers must be a proper list"))
  ;; FIXME: the AMOP says that the elements of the list of qualifiers
  ;; should be "non-null atoms".  I wonder if they really mean
  ;; "non-null SYMBOLS"?  In the section "Readeres for Method
  ;; Metaobjects", they say that the list is a "list of non-nil
  ;; atoms". 
  (unless (every (lambda (qualifier)
		   (and (atom qualifier) (not (null qualifier))))
		 qualifiers)
    (error "a qualifier must be a non-nil atom"))
  ;; Next do the LAMBDA-LIST.  The AMOP also says that an error is
  ;; signaled if this value is not supplied, so we start by checking
  ;; that.
  (unless lambda-list-p
    (error "the lambda-list argument must be supplied"))
  ;; The AMOP says it should be an "unspecialized lambda list".  The
  ;; HyperSpec says that a specialized lambda list is "syntactically
  ;; the same as an ordinary lambda list, except [for the
  ;; specializers]".  In other words, an "unspecialized" lambda list
  ;; is the same as an ordinary lambda list.  We call
  ;; PARSE-ORDINARY-LAMBDA-LIST relying on the fact that an error will
  ;; be signaled if it is not syntactically correct, but we also keep
  ;; the return value because we need to check the number of required
  ;; parameters against the specializers, below.
  (let ((parsed-lambda-list (parse-ordinary-lambda-list lambda-list)))
    ;; Everything is OK.
    ;; Next, we do the SPECIALIZERS.  Again, the AMOP says that an error
    ;; is signaled if this argument is not supplied.
    (unless specializers-p
      (error "the specializers argument must be supplied"))
    ;; Furthermore, the AMOP requires the list of specializers to be a
    ;; proper list.
    (unless (proper-list-p specializers)
      (error "specializers must be a proper list"))
    ;; The AMOP says that list of specializers must have the same number
    ;; of elements as there are required parameters in the lambda list.
    (unless (= (length specializers) (length (required parsed-lambda-list)))
      (error "there must be as many specializers as required parameters")))
  ;; Finally, the AMOP requires every specializer to be a specialier
  ;; metaobject.
  (unless (every #'specializerp specializers)
    (error "a specializer must be a specializer metaobject"))
  ;; Now do the FUNCTION.  The AMOP also says that an error is
  ;; signaled if this value is not supplied, so we start by checking
  ;; that.
  (unless function-p
    (error "the function argument must be supplied"))
  ;; The AMOP says that the function must be compatible with the
  ;; methods on COMPUTE-EFFECTIVE-METHODS defined for this class of
  ;; method and generic function with which it will be used.  But I
  ;; don't see how to check that here.  For now, just check that it
  ;; is a function.
  (unless (functionp function)
    (error "the function argument must be a function."))
  ;; Everything is OK.
  ;; Next check the documentation.  Strangely, there is no mention
  ;;of the documentation in the section "Readers for Method
  ;;Metaobjects" in the AMOP, so we added it.  According to the AMOP
  ;;it must be a string or NIL, and it defaults to NIL.
  (unless (or (null documentation) (stringp documentation))
    (error "the documentation argument must be NIL or a string"))
  (setf (method-documentation method) documentation))

(defun initialize-instance-after-standard-accessor-method
    (method
     &key
       (slot-definition nil slot-definition-p)
     &allow-other-keys)
  (unless slot-definition-p
    (error "the slot-definition argument must be supplied"))
  (unless (typep slot-definition 'direct-slot-definition)
    (error "the slot-definition argument must be a direct-slot-definition")))
