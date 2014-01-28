(cl:in-package #:sicl-clos)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic functions specified in the AMOP.

;;; For the specification of this generic function, see
;;; http://metamodular.com/CLOS-MOP/add-dependent.html
(defgeneric add-dependent (metaobject dependent))

;;; For the specification of this generic function, see
;;; http://metamodular.com/CLOS-MOP/add-direct-method.html
(defgeneric add-direct-method (specializer method))

;;; For the specification of this generic function, see
;;; http://metamodular.com/CLOS-MOP/add-direct-subclass.html
(defgeneric add-direct-subclass (superclass subclass))

;;; For the specification of this generic function, see
;;; http://metamodular.com/CLOS-MOP/add-method.html
;;;
;;; This generic function is also specified in the CLHS.
;;; http://www.lispworks.com/documentation/HyperSpec/Body/f_add_me.htm#add-method
(defgeneric add-method (generic-function method))

;;; For the specification of this generic function, see
;;; http://metamodular.com/CLOS-MOP/allocate-instance.html
;;;
;;; This generic function is also specified in the CLHS.
;;; http://www.lispworks.com/documentation/HyperSpec/Body/f_alloca.htm#allocate-instance
(defgeneric allocate-instance (class &rest initargs))

;;; For the specification of this generic function, see
;;; http://metamodular.com/CLOS-MOP/compute-applicable-methods.html
;;;
;;; This generic function is also specified in the CLHS.
;;; http://www.lispworks.com/documentation/HyperSpec/Body/f_comput.htm#compute-applicable-methods
(defgeneric compute-applicable-methods (generic-function arguments))

;;; For the specification of this generic function, see
;;; http://metamodular.com/CLOS-MOP/compute-applicable-methods-using-classes.html
(defgeneric compute-applicable-methods-using-classes
    (generic-function classes-of-arguments))

;;; For the specification of this generic function, see
;;; http://metamodular.com/CLOS-MOP/compute-class-precedence-list.html
(defgeneric compute-class-precedence-list (class))

;;; For the specification of this generic function, see
;;; http://metamodular.com/CLOS-MOP/compute-default-initargs.html
(defgeneric compute-default-initargs (class))

;;; For the specification of this generic function, see
;;; http://metamodular.com/CLOS-MOP/compute-discriminating-function.html
(defgeneric compute-discriminating-function (generic-function))

;;; For the specification of this generic function, see
;;; http://metamodular.com/CLOS-MOP/compute-effective-method.html
(defgeneric compute-effective-method
    (generic-function method-combination methods))

;;; For the specification of this generic function, see
;;; http://metamodular.com/CLOS-MOP/compute-effective-slot-definition.html
(defgeneric compute-effective-slot-definition
    (class name direct-slot-definitions))

;;; For the specification of this generic function, see
;;; http://metamodular.com/CLOS-MOP/compute-slots.html
(defgeneric compute-slots (class))

;;; For the specification of this generic function, see
;;; http://metamodular.com/CLOS-MOP/direct-slot-definition-class.html
(defgeneric direct-slot-definition-class (class &rest initargs))

;;; For the specification of this generic function, see
;;; http://metamodular.com/CLOS-MOP/effective-slot-definition-class.html
(defgeneric effective-slot-definition-class (class &rest initargs))

;;; For the specification of this generic function, see
;;; http://metamodular.com/CLOS-MOP/ensure-class.html
(defgeneric ensure-class (name &key &allow-other-keys))

;;; For the specification of this generic function, see
;;; http://metamodular.com/CLOS-MOP/ensure-class-using-class.html
(defgeneric ensure-class-using-class
    (class
     name
     &rest keys
     &key
       direct-default-initargs
       direct-slots
       direct-superclasses
       metaclass
       &allow-other-keys))

;;; For the specification of this generic function, see
;;; http://metamodular.com/CLOS-MOP/ensure-generic-function.html
;;;
;;; This generic function is also specified in the CLHS.
;;; http://www.lispworks.com/documentation/HyperSpec/Body/f_ensure.htm#ensure-generic-function
(defgeneric ensure-generic-function (function-name &key &allow-other-keys))

;;; For the specification of this generic function, see
;;; http://metamodular.com/CLOS-MOP/ensure-generic-function-using-class.html
(defgeneric ensure-generic-function-using-class
    (generic-function
     function-name
     &key
       argument-precedence-order
       declarations
       documentation
       generic-function-class
       lambda-list
       method-class
       method-combination
       name
     &allow-other-keys))

;;; For the specification of this generic function, see
;;; http://metamodular.com/CLOS-MOP/extract-lambda-list.html
(defgeneric extract-lambda-list (specialized-lambda-list))

;;; For the specification of this generic function, see
;;; http://metamodular.com/CLOS-MOP/extract-specializer-names.html
(defgeneric extract-specializer-names (specialized-lambda-list))

;;; For the specification of this generic function, see
;;; http://metamodular.com/CLOS-MOP/finalize-inheritance.html
(defgeneric finalize-inheritance (class))

;;; For the specification of this generic function, see
;;; http://metamodular.com/CLOS-MOP/find-method-combination.html
(defgeneric find-method-combination (generic-function
				     method-combination-type-name
				     method-combination-options))

;;; For the specification of this generic function, see
;;; http://metamodular.com/CLOS-MOP/funcallable-standard-instance-access.html
(defgeneric funcallable-standard-instance-access (instance location))

;;; For the specification of this generic function, see
;;; http://metamodular.com/CLOS-MOP/intern-eql-specializer.html
(defgeneric intern-eql-specializer (object))

;;; For the specification of this generic function, see
;;; http://metamodular.com/CLOS-MOP/make-instance.html
;;;
;;; This generic function is also specified in the CLHS.
;;; http://www.lispworks.com/documentation/HyperSpec/Body/f_mk_ins.htm#make-instance
(defgeneric make-instance (class &rest initargs))

;;; For the specification of this generic function, see
;;; http://metamodular.com/CLOS-MOP/make-method-lambda.html
(defgeneric make-method-lambda
    (generic-function method lambda-expression environment))

;;; For the specification of this generic function, see
;;; http://metamodular.com/CLOS-MOP/map-dependents.html
(defgeneric map-dependents (metaobject function))

;;; For the specification of this generic function, see
;;; http://metamodular.com/CLOS-MOP/reader-method-class.html
(defgeneric reader-method-class (class direct-slot &rest initargs))

;;; For the specification of this generic function, see
;;; http://metamodular.com/CLOS-MOP/remove-dependent.html
(defgeneric remove-dependent (metaobject dependent))

;;; For the specification of this generic function, see
;;; http://metamodular.com/CLOS-MOP/remove-direct-method.html
(defgeneric remove-direct-method (specializer method))

;;; For the specification of this generic function, see
;;; http://metamodular.com/CLOS-MOP/remove-direct-subclass.html
(defgeneric remove-direct-subclass (superclass subclass))

;;; For the specification of this generic function, see
;;; http://metamodular.com/CLOS-MOP/remove-method.html
;;;
;;; This generic function is also specified in the CLHS.
;;; http://www.lispworks.com/documentation/HyperSpec/Body/f_rm_met.htm#remove-method
(defgeneric remove-method (generic-function method))

;;; For the specification of this generic function, see
;;; http://metamodular.com/CLOS-MOP/set-funcallable-instance-function.html
(defgeneric set-funcallable-instance-function (funcallable-instance function))

;;; Contrary to appearance, this function is not a slot writer.
;;; Instead, according to the AMOP, this function should call
;;; REINITIALIZE-INSTANCE with three arguments: CLASS, :NAME, and
;;; NEW-NAME.
;;;
;;; For the specification of this generic function, see
;;; http://metamodular.com/CLOS-MOP/setf-class-name.html
(defgeneric (setf class-name) (new-name class))

;;; Contrary to appearance, this function is not a slot writer.
;;; Instead, according to the AMOP, this function should call
;;; REINITIALIZE-INSTANCE with three arguments: CLASS, :NAME, and
;;; NEW-NAME.
;;;
;;; For the specification of this generic function, see
;;; http://metamodular.com/CLOS-MOP/setf-generic-function-name.html
(defgeneric (setf generic-function-name) (new-name generic-function))

;;; For the specification of this generic function, see
;;; http://metamodular.com/CLOS-MOP/slot-boundp-using-class.html
(defgeneric slot-boundp-using-class (class object slot))

;;; For the specification of this generic function, see
;;; http://metamodular.com/CLOS-MOP/setf-slot-value-using-class.html
(defgeneric (setf slot-value-using-class) (new-value class object slot))

;;; For the specification of this generic function, see
;;; http://metamodular.com/CLOS-MOP/slot-makunbound-using-class.html
(defgeneric slot-makunbound-using-class (class object slot))

;;; For the specification of this generic function, see
;;; http://metamodular.com/CLOS-MOP/slot-value-using-class.html
(defgeneric slot-value-using-class (class object slot))

;;; For the specification of this generic function, see
;;; http://metamodular.com/CLOS-MOP/specializer-direct-generic-functions.html
(defgeneric specializer-direct-generic-functions (specializer))

;;; For the specification of this generic function, see
;;; http://metamodular.com/CLOS-MOP/standard-instance-access.html
(defgeneric standard-instance-access (instance location))

;;; For the specification of this generic function, see
;;; http://metamodular.com/CLOS-MOP/update-dependent.html
(defgeneric update-dependent (metaobject dependent &rest initargs))

;;; For the specification of this generic function, see
;;; http://metamodular.com/CLOS-MOP/validate-superclass.html
(defgeneric validate-superclass (class superclass))

;;; For the specification of this generic function, see
;;; http://metamodular.com/CLOS-MOP/writer-method-class.html
(defgeneric writer-method-class (class direct-slot &rest initargs))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic functions that are not specified in the AMOP, but that
;;; are specified in the CLHS and that have to do with CLOS. 

;;; For the specification of this generic function, see
;;; http://www.lispworks.com/documentation/HyperSpec/Body/f_chg_cl.htm#change-class
(defgeneric change-class (instance new-class &key &allow-other-keys))

;;; For the specification of this generic function, see
;;; http://www.lispworks.com/documentation/HyperSpec/Body/f_init_i.htm#initialize-instance
(defgeneric initialize-instance (instance
				 &rest initargs
				 &key &allow-other-keys))

;;; For the specification of this generic function, see
;;; http://www.lispworks.com/documentation/HyperSpec/Body/f_find_c.htm#find-class
(defgeneric find-class (symbol &optional errorp environment))

;;; For the specification of this generic function, see
;;; http://www.lispworks.com/documentation/HyperSpec/Body/f_find_m.htm#find-method
(defgeneric find-method
    (generic-function method-qualifiers specializers &optional errorp))

;;; For the specification of this generic function, see
;;; http://www.lispworks.com/documentation/HyperSpec/Body/f_no_app.htm#no-applicable-method
(defgeneric no-applicable-method (generic-function &rest function-arguments))

;;; For the specification of this generic function, see
;;; http://www.lispworks.com/documentation/HyperSpec/Body/f_no_nex.htm#no-next-method
(defgeneric no-next-method (generic-function method &rest args))

;;; For the specification of this generic function, see
;;; http://www.lispworks.com/documentation/HyperSpec/Body/f_reinit.htm#reinitialize-instance
(defgeneric reinitialize-instance (instance
				   &rest initargs
				   &key &allow-other-keys))

;;; For the specification of this generic function, see
;;; http://www.lispworks.com/documentation/HyperSpec/Body/f_find_c.htm#find-class
(defgeneric (setf find-class) (new-class symbol &optional errorp environment))

;;; For the specification of this generic function, see
;;; http://www.lispworks.com/documentation/HyperSpec/Body/f_shared.htm#shared-initialize
(defgeneric shared-initialize (instance
			       slot-names
			       &rest initargs
			       &key &allow-other-keys))
;;; For the specification of this generic function, see
;;; http://www.lispworks.com/documentation/HyperSpec/Body/f_slt_mi.htm#slot-missing
(defgeneric slot-missing
    (class object slot-name operation &optional new-value))

;;; For the specification of this generic function, see
;;; http://www.lispworks.com/documentation/HyperSpec/Body/f_slt_un.htm#slot-unbound
(defgeneric slot-unbound (class object slot-name))

;;; For the specification of this generic function, see
;;; http://www.lispworks.com/documentation/HyperSpec/Body/f_update.htm#update-instance-for-different-class
(defgeneric update-instance-for-different-class (previous
						 current
						 &rest initargs
						 &key &allow-other-keys))

;;; For the specification of this generic function, see
;;; http://www.lispworks.com/documentation/HyperSpec/Body/f_upda_1.htm#update-instance-for-redefined-class
(defgeneric update-instance-for-redefined-class (instance
						 added-slots
						 discarded-slots
						 property-list
						 &rest initargs
						 &key &allow-other-keys))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric initialize-built-in-instance
    (instance &rest initargs &key &allow-other-keys))

(defgeneric built-in-slot-value-using-class (class object slot))

(defgeneric (setf built-in-slot-value-using-class) (new-value class object slot))

(defgeneric built-in-slot-boundp-using-class (class object slot))

(defgeneric classp (object))

(defgeneric compute-singleton-effective-method-function (method))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function METHOD-COMBINATION-COMPUTE-EFFECTIVE-METHOD.
;;;
;;; This function is not specified in the AMOP.  The AMOP stipulates
;;; that there is a single specified method on the generic function
;;; COMPUTE-EFFECTIVE-METHOD specialized only for a generic function
;;; of STANDARD-GENERIC-FUNCTION.  There is no specialization on the
;;; second argument, which is METHOD-COMBINATION.  For that reason, we
;;; can not add methods on COMPUTE-EFFECTIVE-METHOD for each method
;;; combination in the Common Lisp HyperSpec.  Instead, the only
;;; specified method on COMPUTE-EFFECTIVE-METHOD calls this generic
;;; function, which will dispatch on the class of the method
;;; combination.
;;;
;;; The macro DEFINE-METHOD-COMBINATION will use some unspecified
;;; mechanism to add methods to this generic function. 
;;;
;;; Furthermore, there is a potential metastability problem in
;;; COMPUTE-EFFECTIVE-METHOD, namely when the first argument is the
;;; generic function named COMPUTE-EFFECTIVE-METHOD, which is a
;;; standard generic function using the standard method combination.
;;; We avoid the metastability problem by making the discriminating
;;; function of COMPUTE-EFFECTIVE-METHOD recognize the special case
;;; when the class of the first argument is STANDARD-GENERIC-FUNCTION
;;; and the class of the second argument is
;;; METHOD-COMBINATION-STANDARD and by having it call a default
;;; function in that case.  Thus, this generic function will never be
;;; called in that case.
(defgeneric method-combination-compute-effective-method
    (method-combination methods))
