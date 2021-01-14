(cl:in-package #:sicl-environment)

;;; In this file, we define additional generic functions, not provided
;;; by Clostrum.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function FIND-METHOD-COMBINATION-TEMPLATE.
;;;
;;; If SYMBOL has a definition as a method-combination template, then
;;; that template is returned.  Otherwise NIL is returned.

(defgeneric find-method-combination-template (symbol environment))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function (SETF FIND-METHOD-COMBINATION-TEMPLATE).
;;;
;;; This function is used in order to associate a method-combination
;;; template with a method-combination name in ENVIRONMENT.
;;;
;;; If NEW-TEMPLATE is a method-combination template object, then that
;;; object is associated with the name SYMBOL in ENVIRONMENT.  If
;;; SYMBOL already names a method-combination template in ENVIRONMENT
;;; than that association is lost.
;;;
;;; If NEW-CLASS is NIL, then SYMBOL is no longer associated with a
;;; method-combination class in ENVIRONMENT.

(defgeneric (setf find-method-combination-template) (new-template symbol environment))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function STRUCTURE-DESCRIPTION.
;;;
;;; Return the structure description of a defstruct definition for
;;; which a :type was specified.
;;; If no typed structure with the given name exists, NIL is returned.
;;;
;;; (Defstruct definitions without :type, i.e. structure-objects,
;;;  shouldn't need this accessor, as the information is apparent
;;;  in the class.)

(defgeneric structure-description (name environment))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function (SETF STRUCTURE-DESCRIPTION).
;;;
;;; Set the structure description (explained above).
;;;
;;; FIXME: Should redefinition be banned? CLHS leaves undefined.

(defgeneric (setf structure-description) (description name environment))
