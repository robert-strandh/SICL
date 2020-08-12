(cl:in-package #:sicl-clos)

(defun ensure-generic-function-using-class-null
    (generic-function
     function-name
     &rest
       all-keyword-arguments
     &key
       (environment (sicl-genv:global-environment))
       (generic-function-class
        (find-class 'standard-generic-function t environment))
       (method-class nil method-class-p)
       (method-combination nil method-combination-p)
     &allow-other-keys)
  (declare (ignore generic-function))
  (cond ((symbolp generic-function-class)
         (let ((class (find-class generic-function-class t environment)))
           (when (null class)
             (error 'no-such-generic-function-class
                    :class-name generic-function-class))
           (setf generic-function-class class)))
        ((member (find-class 'generic-function)
                 (class-precedence-list generic-function-class))
         nil)
        (t
         (error 'generic-function-class-must-be-class-or-name
                :object generic-function-class)))
  (when method-class-p
    (cond ((symbolp method-class)
           (let ((class (find-class method-class t environment)))
             (when (null class)
               (error 'no-such-method-class :class-name method-class))
             (setf method-class class)))
          ((member (find-class 'method)
                   (class-precedence-list method-class))
           nil)
          (t
           (error "method class must be a class or a name"))))
  (unless method-combination-p
    ;; Neither the Common Lisp standard nor the AMOP indicates where
    ;; this keyword argument is defaulted, but it has to be here,
    ;; because, this is where we find out that there is no generic
    ;; function with the name given as an argument.
    (setf method-combination
          (sicl-method-combination:find-method-combination
           'standard '() environment)))
  (let ((remaining-keys (copy-list all-keyword-arguments)))
    (loop while (remf remaining-keys :generic-function-class))
    (loop while (remf remaining-keys :environment))
    (setf (sicl-genv:fdefinition function-name environment)
          (if method-class-p
              (apply #'make-instance generic-function-class
                     ;; The AMOP does
                     :name function-name
                     :method-class method-class
                     :method-combination method-combination
                     remaining-keys)
              (apply #'make-instance generic-function-class
                     :name function-name
                     :method-combination method-combination
                     remaining-keys)))))

(defun ensure-generic-function-using-class-generic-function
    (generic-function
     function-name
     &rest
       all-keyword-arguments
     &key
       (environment (sicl-genv:global-environment))
       (generic-function-class
        (find-class 'standard-generic-function t environment))
       (method-class nil method-class-p)
     &allow-other-keys)
  (declare (ignore function-name))
  (cond ((symbolp generic-function-class)
         (let ((class (find-class generic-function-class t environment)))
           (when (null class)
             (error "no such generic-function-class ~s"
                    generic-function-class))
           (setf generic-function-class class)))
        ((member (find-class 'generic-function)
                 (class-precedence-list generic-function-class))
         nil)
        (t
         (error "generic function class must be a class or a name")))
  (unless (eq generic-function-class (class-of generic-function))
    (error "classes don't agree"))
  (when method-class-p
    (cond ((symbolp method-class)
           (let ((class (find-class method-class t environment)))
             (when (null class)
               (error "no such method class ~s" method-class))
             (setf method-class class)))
          ((member (find-class 'method)
                   (class-precedence-list method-class))
           nil)
          (t
           (error "method class must be a class or a name"))))
  (let ((remaining-keys (copy-list all-keyword-arguments)))
    (loop while (remf remaining-keys :generic-function-class))
    (loop while (remf remaining-keys :environment))
    (if method-class-p
        (apply #'reinitialize-instance generic-function
               :method-class method-class
               remaining-keys)
        (apply #'reinitialize-instance generic-function
               remaining-keys)))
  generic-function)
