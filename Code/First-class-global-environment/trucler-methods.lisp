(cl:in-package #:sicl-global-environment)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Trucler Methods

(defmethod trucler:describe-variable
    (client (environment environment) name)
  (if (boundp name environment)
      (or (special-variable-description name environment)
          (constant-variable-description name environment)
          (symbol-macro-description name environment))
      nil))

(defun special-variable-description (name environment)
  (multiple-value-bind (value special-variable-p)
      (special-variable name environment)
    (declare (ignore value))
    (if (not special-variable-p)
        nil
        (make-instance 'trucler:global-special-variable-description
          :name name
          :type (variable-type name environment)))))

(defun constant-variable-description (name environment)
  (multiple-value-bind (value constant-variable-p)
      (constant-variable name environment)
    (if (not constant-variable-p)
        nil
        (make-instance 'trucler:constant-variable-description
          :name name
          :value value))))

(defun symbol-macro-description (name environment)
  (multiple-value-bind (expansion symbol-macro-p)
      (symbol-macro name environment)
    (if (not symbol-macro-p)
        nil
        (make-instance 'trucler:symbol-macro-description
          :name name
          :type (variable-type name environment)
          :expansion expansion))))

(defmethod trucler:describe-function
    (client (environment environment) name)
  (if (not (fboundp name environment))
      nil
      (let ((fdefinition (fdefinition name environment)))
        (etypecase fdefinition
          (function
           (make-instance 'trucler:global-function-description
             :name name
             :compiler-macro (compiler-macro-function name environment)))
          (cons
           (ecase (first fdefinition)
             (cl:macro-function
              (make-instance 'trucler:global-macro-description
                :name name
                :expander (second fdefinition)
                :compiler-macro (compiler-macro-function name environment)))
             (cl:special
              (make-instance 'trucler:special-operator-description
                :name name))))))))

(defmethod trucler:describe-block
    (client (environment environment) name)
  nil)

(defmethod trucler:describe-tag
    (client (environment environment) tag)
  nil)

(defmethod trucler:describe-optimize
    (client (environment environment))
  (let ((policy (policy environment)))
    (make-instance 'trucler:optimize-description
      :speed (cleavir-policy:policy-value policy 'speed)
      :compilation-speed (cleavir-policy:policy-value policy 'compilation-speed)
      :debug (cleavir-policy:policy-value policy 'debug)
      :space (cleavir-policy:policy-value policy 'space)
      :safety (cleavir-policy:policy-value policy 'safety))))

;;; Augmentation Functions

(defun make-trucler-environment (global-environment)
  (make-instance 'trucler-reference:environment
    :global-environment global-environment))

(macrolet ((def (name lambda-list)
             `(defmethod ,name ,lambda-list
                (,name
                 ,@(loop for item in lambda-list
                         append
                         (cond ((equal item '(environment environment))
                                '((make-trucler-environment environment)))
                               ((member item lambda-list-keywords)
                                '())
                               ((symbolp item)
                                `(,item))))))))
  (def trucler:augment-with-variable-description
      (client (environment environment) variable-description))
  (def trucler:augment-with-function-description
      (client (environment environment) function-description))
  (def trucler:augment-with-block-description
      (client (environment environment) block-description))
  (def trucler:augment-with-tag-description
      (client (environment environment) tag-description))
  (def trucler:augment-with-optimize-description
      (client (environment environment) optimize-description))
  (def trucler:add-lexical-variable
      (client (environment environment) symbol &optional identity))
  (def trucler:add-special-variable
      (client (environment environment) symbol))
  (def trucler:add-local-symbol-macro
      (client (environment environment) symbol expansion))
  (def trucler:add-local-function
      (client (environment environment) function-name &optional identity))
  (def trucler:add-local-macro
      (client (environment environment) symbol expander))
  (def trucler:add-block
      (client (environment environment) symbol &optional identity))
  (def trucler:add-tag
      (client (environment environment) tag &optional identity))
  (def trucler:add-variable-type
      (client (environment environment) symbol type))
  (def trucler:add-function-type
      (client (environment environment) function-name type))
  (def trucler:add-variable-ignore
      (client (environment environment) symbol ignore))
  (def trucler:add-function-ignore
      (client (environment environment) function-name ignore))
  (def trucler:add-variable-dynamic-extent
      (client (environment environment) symbol))
  (def trucler:add-function-dynamic-extent
      (client (environment environment) function-name))
  (def trucler:add-inline
      (client (environment environment) function-name inline))
  (def trucler:add-inline-data
      (client (environment environment) function-name inline-data))
  (def trucler:add-speed
      (client (environment environment) value))
  (def trucler:add-compilation-speed
      (client (environment environment) value))
  (def trucler:add-debug
      (client (environment environment) value))
  (def trucler:add-safety
      (client (environment environment) value))
  (def trucler:add-space
      (client (environment environment) value)))

;;; Miscellaneous Functions

(defmethod trucler:global-environment
    (client (environment environment))
  environment)

(defmethod trucler:macro-function (name (env environment))
  (macro-function name env))

(defmethod trucler:compiler-macro-function (name (env environment))
  (compiler-macro-function name env))

(defmethod trucler:symbol-macro-expansion (name (env environment))
  (multiple-value-bind (expander expansion)
      (symbol-macro name env)
    (if (null expander)
        name
        expansion)))
