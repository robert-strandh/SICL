(cl:in-package #:sicl-expression-to-ast)

(defclass environment ()
  ())

(defclass client (trucler-reference:client)
  ())

(defmethod trucler:describe-variable
    (client (environment environment) name)
  (cond ((sb-eval::specialp name nil)
         (make-instance 'trucler:global-special-variable-description
           :name name
           :type t))
        ((constantp name)
         (make-instance 'trucler:constant-variable-description
           :name name
           :value (symbol-value name)))
        ;; FIXME: check for symbol macro
        (t
         nil)))

(defmethod trucler:describe-function
    (client (environment environment) name)
  (let ((macro-function (macro-function name)))
    (cond ((not (null macro-function))
           (make-instance 'trucler:global-macro-description
             :name name
             :expander macro-function
             :compiler-macro nil))
          ((special-operator-p name)
           (make-instance 'trucler:special-operator-description
             :name name))
          ((null (fdefinition name))
           nil)
          (t
           (make-instance 'trucler:global-function-description
             :type t
             :name name
             :compiler-macro nil)))))

(defmethod trucler:describe-block
    (client (environment environment) name)
  nil)

(defmethod trucler:describe-tag
    (client (environment environment) tag)
  nil)

;;; FIXME: do this better
(defmethod trucler:describe-optimize
    (client (environment environment))
  (make-instance  'trucler:optimize-description
    :speed 0
    :compilation-speed 0
    :debug 3
    :space 0
    :safety 3))
