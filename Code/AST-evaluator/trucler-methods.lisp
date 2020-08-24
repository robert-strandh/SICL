(cl:in-package #:sicl-ast-evaluator)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Trucler Methods

(defmethod trucler:describe-variable
    (client (environment evaluation-environment) name)
  (if (env:boundp client environment name)
      (or (special-variable-description client environment name)
          (constant-variable-description client environment name)
          (symbol-macro-description client environment name))
      nil))

(defmethod trucler:describe-variable
    (client (environment compilation-environment) name)
  ;; FIXME: check the compilation environment for information about
  ;; the variable.
  (trucler:describe-variable
   client (env:parent environment) name))

(defun special-variable-description (client environment name)
  (multiple-value-bind (value special-variable-p)
      (env:special-variable client environment name)
    (declare (ignore value))
    (if (not special-variable-p)
        nil
        (make-instance 'trucler:global-special-variable-description
          :name name
          :type (env:variable-type client environment name)))))

(defun constant-variable-description (client environment name)
  (multiple-value-bind (value constant-variable-p)
      (env:constant-variable client environment name)
    (if (not constant-variable-p)
        nil
        (make-instance 'trucler:constant-variable-description
          :name name
          :value value))))

(defun symbol-macro-description (client environment name)
  (multiple-value-bind (expansion symbol-macro-p)
      (env:symbol-macro client environment name)
    (if (not symbol-macro-p)
        nil
        (make-instance 'trucler:symbol-macro-description
          :name name
          :type (env:variable-type client environment name)
          :expansion expansion))))

(defmethod trucler:describe-function
    (client (environment evaluation-environment) name)
  (if (not (env:fboundp client  environment name))
      nil
      (let ((fdefinition (env:fdefinition client environment name)))
        (etypecase fdefinition
          (function
           (make-instance 'trucler:global-function-description
             :name name
             :compiler-macro (env:compiler-macro-function client environment name)))
          (cons
           (ecase (first fdefinition)
             (cl:macro-function
              (make-instance 'trucler:global-macro-description
                :name name
                :expander (second fdefinition)
                :compiler-macro (env:compiler-macro-function client environment name)))
             (cl:special
              (make-instance 'trucler:special-operator-description
                :name name))))))))

(defmethod trucler:describe-function
    (client (environment compilation-environment) name)
  ;; FIXME: check the compilation environment for information about
  ;; the function.
  (trucler:describe-function
   client (env:parent environment) name))

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

(macrolet ((def (name lambda-list)
             `(defmethod ,name ,lambda-list
                (,name
                 ,@(loop for item in lambda-list
                         append
                         (cond ((equal item '(environment compilation-environment))
                                '((make-trucler-environment environment)))
                               ((member item lambda-list-keywords)
                                '())
                               ((symbolp item)
                                `(,item))))))))
  (def trucler:augment-with-variable-description
      (client (environment compilation-environment) variable-description))
  (def trucler:augment-with-function-description
      (client (environment compilation-environment) function-description))
  (def trucler:augment-with-block-description
      (client (environment compilation-environment) block-description))
  (def trucler:augment-with-tag-description
      (client (environment compilation-environment) tag-description))
  (def trucler:augment-with-optimize-description
      (client (environment compilation-environment) optimize-description))
  (def trucler:add-lexical-variable
      (client (environment compilation-environment) symbol &optional identity))
  (def trucler:add-special-variable
      (client (environment compilation-environment) symbol))
  (def trucler:add-local-symbol-macro
      (client (environment compilation-environment) symbol expansion))
  (def trucler:add-local-function
      (client (environment compilation-environment) function-name &optional identity))
  (def trucler:add-local-macro
      (client (environment compilation-environment) symbol expander))
  (def trucler:add-block
      (client (environment compilation-environment) symbol &optional identity))
  (def trucler:add-tag
      (client (environment compilation-environment) tag &optional identity))
  (def trucler:add-variable-type
      (client (environment compilation-environment) symbol type))
  (def trucler:add-function-type
      (client (environment compilation-environment) function-name type))
  (def trucler:add-variable-ignore
      (client (environment compilation-environment) symbol ignore))
  (def trucler:add-function-ignore
      (client (environment compilation-environment) function-name ignore))
  (def trucler:add-variable-dynamic-extent
      (client (environment compilation-environment) symbol))
  (def trucler:add-function-dynamic-extent
      (client (environment compilation-environment) function-name))
  (def trucler:add-inline
      (client (environment compilation-environment) function-name inline))
  (def trucler:add-inline-data
      (client (environment compilation-environment) function-name inline-data))
  (def trucler:add-speed
      (client (environment compilation-environment) value))
  (def trucler:add-compilation-speed
      (client (environment compilation-environment) value))
  (def trucler:add-debug
      (client (environment compilation-environment) value))
  (def trucler:add-safety
      (client (environment compilation-environment) value))
  (def trucler:add-space
      (client (environment compilation-environment) value)))

;;; Miscellaneous Functions

(defmethod trucler:global-environment
    (client (environment compilation-environment))
  environment)

(defmethod trucler:macro-function (name (env compilation-environment))
  (env:macro-function (client env) env name))

(defmethod trucler:compiler-macro-function (name (env compilation-environment))
  (env:compiler-macro-function (client env) env name))

(defmethod trucler:symbol-macro-expansion (name (env compilation-environment))
  (multiple-value-bind (expander expansion)
      (env:symbol-macro (client env) env name)
    (if (null expander)
        name
        expansion)))
