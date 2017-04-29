(cl:in-package #:sicl-clos)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function ADD-DIRECT-SUBCLASS.
;;;
;;; For the specification of this generic function, see
;;; http://metamodular.com/CLOS-MOP/add-direct-subclass.html
;;;
;;; The specification includes a single method on this generic
;;; function, specialized for CLASS and CLASS.  The default action
;;; below is valid for that method.

(defun add-direct-subclass-default (superclass subclass)
  (when (member subclass (class-direct-subclasses superclass))
    (error 'attempt-to-add-existing-subclass
           :subclass subclass
           :superclass superclass))
  (setf (class-direct-subclasses superclass)
	(cons subclass (class-direct-subclasses superclass))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function REMOVE-DIRECT-SUBCLASS.
;;;
;;; For the specification of this generic function, see
;;; http://metamodular.com/CLOS-MOP/remove-direct-subclass.html
;;;
;;; The specification includes a single method on this generic
;;; function, specialized for CLASS and CLASS.  The default action
;;; below is valid for that method.

(defun remove-direct-subclass-default (superclass subclass)
  (setf (class-direct-subclasses superclass)
	(remove subclass (class-direct-subclasses superclass)
		:test #'eq)))
