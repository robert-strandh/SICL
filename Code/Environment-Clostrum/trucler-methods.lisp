(cl:in-package #:sicl-environment)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Trucler Methods

#+(or)
(defmethod trucler:describe-variable
    (client (environment compilation-environment) name)
  ;; FIXME: check the compilation environment for information about
  ;; the variable.
  (trucler:describe-variable
   client (parent environment) name))

#+(or)
(defmethod trucler:describe-function
    (client (environment compilation-environment) name)
  ;; FIXME: check the compilation environment for information about
  ;; the function.
  (trucler:describe-function
   client (parent environment) name))

;;; FIXME: do this better
(defmethod trucler:describe-optimize
    (client (environment base-run-time-environment))
  (make-instance  'trucler:optimize-description
    :speed 0
    :compilation-speed 0
    :debug 3
    :space 0
    :safety 3))

;;; Augmentation Functions

(defun make-trucler-environment (global-environment)
  (make-instance 'trucler-reference:environment
    :global-environment global-environment))

(macrolet
    ((def (name &rest lambda-list)
       `(progn (defmethod ,name (client (e compilation-environment) ,@lambda-list)
                 (,name client (make-trucler-environment e)
                              ,@(loop for item in lambda-list
                                      append
                                      (if (member item lambda-list-keywords)
                                          '()
                                          (list item)))))
               (defmethod ,name (client (e base-run-time-environment) ,@lambda-list)
                 (,name client (make-trucler-environment e)
                              ,@(loop for item in lambda-list
                                      append
                                      (if (member item lambda-list-keywords)
                                          '()
                                          (list item))))))))
  (def trucler:augment-with-variable-description variable-description)
  (def trucler:augment-with-function-description function-description)
  (def trucler:augment-with-block-description block-description)
  (def trucler:augment-with-tag-description tag-description)
  (def trucler:augment-with-optimize-description optimize-description)
  (def trucler:add-lexical-variable symbol &optional identity)
  (def trucler:add-local-special-variable symbol)
  (def trucler:add-local-symbol-macro symbol expansion)
  (def trucler:add-local-function function-name &optional identity)
  (def trucler:add-local-macro symbol expander)
  (def trucler:add-block symbol &optional identity)
  (def trucler:add-tag tag &optional identity)
  (def trucler:add-variable-type symbol type)
  (def trucler:add-function-type function-name type)
  (def trucler:add-variable-ignore symbol ignore)
  (def trucler:add-function-ignore function-name ignore)
  (def trucler:add-variable-dynamic-extent symbol)
  (def trucler:add-function-dynamic-extent function-name)
  (def trucler:add-inline function-name inline)
  (def trucler:add-inline-data function-name inline-data)
  (def trucler:add-speed value)
  (def trucler:add-compilation-speed value)
  (def trucler:add-debug value)
  (def trucler:add-safety value)
  (def trucler:add-space value))

;;; Miscellaneous Functions

(defmethod trucler:macro-function (name (env compilation-environment))
  (macro-function (client env) env name))

(defmethod trucler:macro-function (name (env base-run-time-environment))
  (macro-function (client env) env name))

(defmethod trucler:compiler-macro-function (name (env compilation-environment))
  (compiler-macro-function (client env) env name))

(defmethod trucler:compiler-macro-function (name (env base-run-time-environment))
  (compiler-macro-function (client env) env name))

(defmethod trucler:symbol-macro-expansion (name (env compilation-environment))
  (multiple-value-bind (expander expansion)
      (symbol-macro (client env) env name)
    (if (null expander)
        name
        expansion)))

(defmethod trucler:symbol-macro-expansion (name (env base-run-time-environment))
  (multiple-value-bind (expander expansion)
      (symbol-macro (client env) env name)
    (if (null expander)
        name
        expansion)))
