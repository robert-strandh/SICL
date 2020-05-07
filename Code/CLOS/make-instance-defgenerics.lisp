(cl:in-package #:sicl-clos)

;;; Because of the way bootstrapping works, we must unfortunately give
;;; a temporary name to MAKE-INSTANCE.  The basic reason is that,
;;; during bootstrapping, we must have a special version of it so that
;;; it can call both ALLOCATE-INSTANCE and INITIALIZE-INSTANCE.  But
;;; then, we can not redefine it later by loading FASL files, because
;;; creating generic functions requires a working version of
;;; MAKE-INSTANCE.  Our solution is to define MAKE-INSTANCE as a
;;; generic function, but with a different name.  Then, once we have a
;;; circular graph of CLOS metaobjects, we simply rename it, which
;;; does not require any object creation.

;;; For the specification of this generic function, see
;;; http://metamodular.com/CLOS-MOP/make-instance.html
;;;
;;; This generic function is also specified in the CLHS.
;;; http://www.lispworks.com/documentation/HyperSpec/Body/f_mk_ins.htm#make-instance
(defgeneric make-instance-temp (class &rest initargs))
