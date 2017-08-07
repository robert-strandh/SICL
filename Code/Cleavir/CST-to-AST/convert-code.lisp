(cl:in-package #:cleavir-cst-to-ast)

;;; The general method for processing the lambda list is as follows:
;;; We use recursion to process the remaining lambda list.  Before the
;;; recursive call, we add the current parameters to the environment
;;; that we pass to the recursive call.  The call returns two values:
;;; the AST that was built and a modified lambda list, containing the
;;; lambda list keywords, and the lexical variables that were
;;; introduced.  The exception is that processing &AUX entries does
;;; not return any lambda list, because it will always be empty.
;;;
;;; The reason we do it this way is that, if a parameter turns out to
;;; be a special variable, the entire rest of the lambda list and
;;; function body must be executed with this variable bound.  The AST
;;; configuration for expressing that situation is that the AST for
;;; computing the rest of the lambda list and the body must be a child
;;; of a BIND-AST that indicates that the special variable should be
;;; bound.  This recursive method makes sure that the child exists
;;; before the BIND-AST is created.
;;;
;;; The parameter DSPECS that is used in several functions is a list
;;; of canonicalized declaration specifiers.  This list is used to
;;; determine whether a variable is declared special.

;;; Process a single parameter.  REMAINING-PARAMETERS-IN-GROUP
;;; contains a list of remaining parameters in the same group as
;;; PARAMETER to be processed after PARAMETER has been processed.
;;; REMAINING-PARAMETER-GROUPS contains a list of the remaining
;;; parameter groups to be processed after all parameters in the
;;; current group have been processed.
(defgeneric process-parameter
    (parameter
     remaining-parameters-in-group
     remaining-parameter-groups
     idspecs
     body
     env
     system))

(defgeneric process-parameters-in-group
    (parameters-in-group
     remaining-parameter-groups
     idspecs
     body
     environment
     system))

(defmethod process-parameters-in-group
    ((parameters-in-group null)
     remaining-parameter-groups
     idspecs
     body
     environment
     system)
  (process-parameter-groups remaining-parameter-groups
                            idspecs
                            body
                            environment
                            system))

(defmethod process-parameters-in-group
    ((parameters-in-group cons)
     remaining-parameter-groups
     idspecs
     body
     environment
     system)
  (process-parameter (car parameters-in-group)
                     (cdr parameters-in-group)
                     remaining-parameter-groups
                     idspecs
                     body
                     environment
                     system))

(defgeneric new-environment-from-parameter-group
    (parameter-group idspecs environment system))

(defgeneric new-environment-from-parameter
    (parameter idspecs environment system))

;;; This class is used to describe the body of a function.  It
;;; contains the declaration specifiers that apply to the body as a
;;; whole, the forms of the body and information about a possible
;;; BLOCK that the body code should be wrapped in.  The main reason
;;; for the existence of this class is to keep the number of arguments
;;; down of several functions below, not for the purpose of
;;; performance, but simply to avoid very long lambda lists in the
;;; source code.
(defclass body ()
  ((%dspecs :initarg :dspecs :accessor dspecs)
   (%forms :initarg :forms :accessor forms)
   (%block-name :initarg :block-name :reader block-name)
   (%block-name-p :initarg :block-name-p :reader block-name-p)))

(defun make-body (dspecs forms block-name block-name-p)
  (make-instance 'body
    :dspecs dspecs
    :forms forms
    :block-name block-name
    :block-name-p block-name-p))

(defgeneric process-parameter-groups
    (parameter-groups
     idspecs
     body
     envrironment
     system))

(defmethod process-parameter-groups
    ((parameter-groups null)
     idspecs
     body
     environment
     system)
  (values (convert-body body environment system) '()))

(defmethod process-parameter-groups
    ((parameter-groups cons)
     idspecs
     body
     environment
     system)
  (process-parameter-group (car parameter-groups)
                           (cdr parameter-groups)
                           idspecs
                           body
                           environment
                           system))

(defun new-environment-from-parameters (parameters idspecs environment system)
  (let ((result environment))
    (loop for parameter in parameters
          do (setf result
                   (new-environment-from-parameter (car parameters)
                                                   idspecs
                                                   result
                                                   system)))
    result))

(defmethod new-environment-from-parameter-group
    ((parameter-group cst:multi-parameter-group-mixin)
     idspecs
     environment
     system)
  (new-environment-from-parameters (cst:parameters parameter-group)
                                   idspecs
                                   environment
                                   system))

(defmethod new-environment-from-parameter-group
    ((parameter-group cst:ordinary-rest-parameter-group)
     idspecs
     environment
     system)
  (augment-environment-with-variable
   (cst:raw (cst:name (cst:parameter parameter-group)))
   (first idspecs)
   environment
   environment))

(defmethod new-environment-from-parameter
    ((parameter cst:simple-variable) idspecs environment system)
  (augment-environment-with-variable (cst:raw (cst:name parameter))
                                     (first idspecs)
                                     environment
                                     environment))

(defmethod new-environment-from-parameter
    ((parameter cst:ordinary-key-parameter) idspecs environment system)
  (augment-environment-with-parameter (cst:raw (cst:name parameter))
                                      (cst:raw (cst:supplied-p parameter))
                                      (first idspecs)
                                      environment))

(defmethod new-environment-from-parameter
    ((parameter cst:ordinary-optional-parameter) idspecs environment system)
  (augment-environment-with-parameter (cst:raw (cst:name parameter))
                                      (cst:raw (cst:supplied-p parameter))
                                      (first idspecs)
                                      environment))
