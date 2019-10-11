(cl:in-package #:cleavir-cst-to-ast)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Managing top-level forms.
;;;
;;; We need to be able to test whether a form is a top-level form or
;;; not.  A few special forms (LOCALLY, MACROLET, SYMBOL-MACROLET)
;;; preserve this property in that if the special form itself is a
;;; top-level form, then the body of the special form is also a
;;; top-level form.  For all other forms, any subform of the form is
;;; not considered a top-level form.

;;; The reason for the following somewhat twisted logic is that we
;;; want to avoid having to explicitly set *SUBFORMS-ARE-TOP-LEVEL-P*
;;; to false in every method EXCEPT the ones for LOCALLY, MACROLET,
;;; and SYMBOL-MACROLET.  This logic allows us to add some code ONLY
;;; to these special forms in order to indicate that they preserve the
;;; top-level property.
;;;
;;; The way this logic works is as follows: We define a second
;;; variable named *CURRENT-FORM-IS-TOP-LEVEL-P*.  This variable holds
;;; the value of *SUBFORMS-ARE-TOP-LEVEL-P* as it was before CONVERT was
;;; called, and this is the variable that we actually test in order to
;;; determine whether a form is a top-level form.  To obtain that, we
;;; define an :AROUND method on CONVERT that binds
;;; *CURRENT-FORM-IS-TOP-LEVEL-P* to the value of *SUBFORMS-ARE-TOP-LEVEL-P*
;;; for the duration of the invocation of the primary method on
;;; CONVERT, and that also binds *SUBFORMS-ARE-TOP-LEVEL-P* to false.  Any
;;; recursive invocation of CONVERT will thus automatically see the
;;; value of *CURRENT-FORM-IS-TOP-LEVEL-P* as false.  The methods for
;;; LOCALLY, MACROLET, and SYMBOL-MACROLET set
;;; *CURRENT-FORM-IS-TOP-LEVEL-P* to true so that when they
;;; recursively call CONVERT, then this true value will be the value
;;; of *CURRENT-FORM-IS-TOP-LEVEL-P*.  I hope this explanation makes
;;; sense.

;;; This variable is true if and only if the current form is a
;;; top-level form.
(defvar *current-form-is-top-level-p*)

;;; This variable is true if and only if the subforms of the current
;;; form are top-level forms.
(defvar *subforms-are-top-level-p*)

(defmacro with-preserved-toplevel-ness (&body body)
  `(progn (setf *subforms-are-top-level-p* *current-form-is-top-level-p*)
          ,@body))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Variables that control certain behavior of the compiler.

;;; This variable should be bound to a true value if this system is
;;; called from the file compiler.
(defvar *use-file-compilation-semantics-p*)

;;; This variable indicates whether a form should be evaluated in
;;; addition to be being processed by the compiler.
(defvar *compile-time-too*)

;;; During the conversion of a single CST, the value of this variable
;;; is the source location of that CST.
(defvar *origin*)

;;; This variable is bound by CST-TO-AST:CST-TO-AST and is used to track
;;; that literal objects are only processed once, and that similar literal
;;; objects are suitably coalesced.
(defvar *similarity-table*)

;;; A list of ASTs - one for each occurrence of MAKE-LOAD-FORM, and for
;;; each creation form and initialization form of a literal object.
(defvar *prologue*)
