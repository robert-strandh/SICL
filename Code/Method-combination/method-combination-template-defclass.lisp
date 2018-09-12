(cl:in-package #:sicl-method-combination)

(defclass method-combination-template ()
  ((%name :initarg :name)
   ;; This slot contains a function that, when applied to the list of
   ;; method-combination options (as given after the
   ;; :METHOD-COMBINATION option to DEFGENERIC), returns a SIGNATURE
   ;; which is a list of values of all the local variables that are
   ;; introduced by the ordinary lambda list in the
   ;; DEFINE-METHOD-COMBINATION form.  This signature is used to
   ;; determine whether there is an existing method-combination
   ;; variant that corresponds to those options, or whether a new
   ;; variant must be created.
   (%variant-signature-determiner :initarg :variant-signature-determiner
                                  :reader variant-signature-determiner)
   ;; This slot contains a list of all the existing variants.  Each
   ;; variant is an instance of the class METHOD-COMBINATION.
   (%variants :initform '() :accessor variants)
   ;; This slot contains the function that computes the "expansion"
   ;; defined by the DEFINE-METHOD-COMBINATION form.  This expansion
   ;; is a form that describes the effective method.  This form
   ;; contains calls to the local macros CALL-METHOD and MAKE-METHOD
   ;; the definitions of which need to wrap the form before it can be
   ;; compiled.  The expansion function in this slots should be
   ;; applied to a list of method/qualifier pairs and the signature of
   ;; the variant.  This function will bind the local variables in the
   ;; DEFINE-METHOD-COMBINATION form to the values in the signature
   ;; and then run the code for categorizing the methods and the code
   ;; in the body of the DEFINE-METHOD-COMBINATION form.
   (%effective-method-form-function :initarg :effective-method-form-function
                                    :reader effective-method-form-function)
   (%documentation :initarg :documentation)))
