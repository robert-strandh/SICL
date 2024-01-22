(cl:in-package #:sicl-clos)

(defun canonicalize-generic-function-class (generic-function-class)
  (cond ((symbolp generic-function-class)
         (find-class generic-function-class t environment))
        ((member (find-class 'generic-function)
                 (class-precedence-list generic-function-class))
         generic-function-class)
        (t
         (error 'generic-function-class-must-be-class-or-name
                :object generic-function-class))))

(defun canonicalize-method-class (method-class)
  (cond ((symbolp method-class)
         (find-class method-class t environment))
        ((member (find-class 'method) (class-precedence-list method-class))
         method-class)
        (t
         (error "method class must be a class or a name"))))

(defun canonicalize-keyword-arguments (keyword-arguments)
  (let ((result (copy-list keyword-arguments)))
    (loop while (remf remaining-keys :generic-function-class))
    (loop while (remf remaining-keys :environment))
    result))

(defmethod ensure-generic-function-using-class
    ((generic-function null)
     function-name
     &rest
       all-keyword-arguments
     &key
       environment
       (generic-function-class
        (find-class 'standard-generic-function t environment))
       (method-class nil method-class-p)
       (method-combination nil method-combination-p)
     &allow-other-keys)
  (declare (ignore generic-function))
  (setf generic-function-class
        (canonicalize-generic-function-class generic-function-class))
  (when method-class-p
    (setf method-class (canonicalize-method-class method-class)))
  (unless method-combination-p
    ;; Neither the Common Lisp standard nor the AMOP indicates where
    ;; this keyword argument is defaulted, but it has to be here,
    ;; because, this is where we find out that there is no generic
    ;; function with the name given as an argument.
    (unless (class-finalized-p generic-function-class)
      (finalize-inheritance generic-function-class))
    (let ((proto (class-prototype generic-function-class)))
      (setf method-combination
            (find-method-combination proto 'standard '()))))
  (let* ((remaining-keys
           (canonicalize-keyword-arguments all-keyword-arguments))
         (result
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
                      remaining-keys))))
    (setf (fdefinition function-name) result)))

(defmethod ensure-generic-function-using-class
    ((generic-function generic-function)
     function-name
     &rest
       all-keyword-arguments
     &key
       environment
       (generic-function-class
        (find-class 'standard-generic-function t environment))
       (method-class nil method-class-p)
     &allow-other-keys)
  (declare (ignore function-name))
  (setf generic-function-class
        (canonicalize-generic-function-class generic-function-class))
  (unless (eq generic-function-class (class-of generic-function))
    (error "classes don't agree ~s and ~s of ~s"
           generic-function-class (class-of generic-function) generic-function))
  (when method-class-p
    (setf method-class (canonicalize-method-class method-class)))
  (let ((remaining-keys
          (canonicalize-keyword-arguments all-keyword-arguments)))
    (if method-class-p
        (apply #'reinitialize-instance generic-function
               :method-class method-class
               remaining-keys)
        (apply #'reinitialize-instance generic-function
               remaining-keys)))
  generic-function)
