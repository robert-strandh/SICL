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
