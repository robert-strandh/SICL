(cl:in-package #:sicl-environment)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Trucler Methods

(defmethod trucler:describe-variable
    (client (environment base-run-time-environment) name)
  (if (boundp client environment name)
      (or (special-variable-description client environment name)
          (constant-variable-description client environment name)
          (symbol-macro-description client environment name))
      nil))

(defmethod trucler:describe-variable
    (client (environment compilation-environment) name)
  ;; FIXME: check the compilation environment for information about
  ;; the variable.
  (trucler:describe-variable
   client (parent environment) name))

(defun special-variable-description (client environment name)
  (multiple-value-bind (special-variable-p value)
      (special-variable client environment name)
    (declare (ignore value))
    (if (not special-variable-p)
        nil
        (make-instance 'trucler:global-special-variable-description
          :name name
          :type (variable-type client environment name)))))

(defun constant-variable-description (client environment name)
  (multiple-value-bind (constant-variable-p value)
      (constant-variable client environment name)
    (if (not constant-variable-p)
        nil
        (make-instance 'trucler:constant-variable-description
          :name name
          :value value))))

(defun symbol-macro-description (client environment name)
  (multiple-value-bind (expansion symbol-macro-p)
      (symbol-macro client environment name)
    (if (not symbol-macro-p)
        nil
        (make-instance 'trucler:symbol-macro-description
          :name name
          :type (variable-type client environment name)
          :expansion expansion))))

(defmethod trucler:describe-function
    (client (environment base-run-time-environment) name)
  (cond ((not (fboundp client  environment name))
         nil)
        ((symbolp name)
         (let ((macro-function (macro-function client environment name)))
           (if (not (null macro-function))
               (make-instance 'trucler:global-macro-description
                 :name name
                 :expander macro-function
                 :compiler-macro (compiler-macro-function client environment name))
               (if (special-operator client environment name)
                   (make-instance 'trucler:special-operator-description
                     :name name)
                   (make-instance 'trucler:global-function-description
                     :name name
                     :compiler-macro (compiler-macro-function client environment name))))))
        (t
         (make-instance 'trucler:global-function-description
           :name name
           :compiler-macro (compiler-macro-function client environment name)))))

(defmethod trucler:describe-function
    (client (environment compilation-environment) name)
  ;; FIXME: check the compilation environment for information about
  ;; the function.
  (trucler:describe-function
   client (parent environment) name))

(defmethod trucler:describe-block
    (client (environment compilation-environment) name)
  nil)

(defmethod trucler:describe-tag
    (client (environment compilation-environment) tag)
  nil)

;; (defmethod trucler:describe-optimize
;;     (client (environment compilation-environment))
;;   (let ((policy (policy environment)))
;;     (make-instance 'trucler:optimize-description
;;       :speed (cleavir-policy:policy-value policy 'speed)
;;       :compilation-speed (cleavir-policy:policy-value policy 'compilation-speed)
;;       :debug (cleavir-policy:policy-value policy 'debug)
;;       :space (cleavir-policy:policy-value policy 'space)
;;       :safety (cleavir-policy:policy-value policy 'safety))))

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
  (def trucler:add-special-variable symbol)
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

(defmethod trucler:global-environment
    (client (environment compilation-environment))
  environment)

(defmethod trucler:global-environment
    (client (environment base-run-time-environment))
  environment)

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
