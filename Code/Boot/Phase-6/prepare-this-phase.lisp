(cl:in-package #:sicl-boot-phase-6)

(defun finalize-classes (e4 e5)
  (format *trace-output* "Finalizing all classes in ~a..." (sicl-boot:name e5))
  (finish-output *trace-output*)
  (let ((visited (make-hash-table :test #'eq))
        (finalized-p (env:fdefinition (env:client e4) e4 'sicl-clos::class-finalized-p))
        (finalize (env:fdefinition (env:client e4) e4 'sicl-clos:finalize-inheritance)))
    (do-all-symbols (symbol)
      (unless (gethash symbol visited)
        (setf (gethash symbol visited) t)
        (let ((class (env:find-class (env:client e5) e5 symbol)))
          (unless (or (null class) (funcall finalized-p class))
            (funcall finalize class))))))
  (format *trace-output* "done~%")
  (finish-output *trace-output*))

(defun satiate-generic-functions (e4 e5)
  (format *trace-output* "Satiating all generic functions in ~a..." (sicl-boot:name e5))
  (finish-output *trace-output*)
  (let ((processed (make-hash-table :test #'eq))
        (client (env:client e5))
        (satiation-function
          (env:fdefinition (env:client e4) e4 'sicl-clos::satiate-generic-function))
        (generic-function-class
          (env:find-class (env:client e4) e4 'standard-generic-function)))
    (do-all-symbols (symbol)
      (unless (gethash symbol processed)
        (setf (gethash symbol processed) t)
        (when (and (env:fboundp client e5 symbol)
                   (not (env:special-operator client e5 symbol))
                   (null (env:macro-function client e5 symbol)))
          (let ((fun (env:fdefinition client e5 symbol)))
            (when (and (typep fun 'sicl-boot::header)
                       (eq (slot-value fun 'sicl-boot::%class)
                           generic-function-class))
              (funcall satiation-function fun))))
        (when (env:fboundp client e5 `(setf ,symbol))
          (let ((fun (env:fdefinition client e5 `(setf ,symbol))))
            (when (and (typep fun 'sicl-boot::header)
                       (eq (slot-value fun 'sicl-boot::%class)
                           generic-function-class))
              (funcall satiation-function fun)))))))
  (format *trace-output* "done~%")
  (finish-output *trace-output*))

(defun prepare-this-phase (e3 e4 e5)
  (enable-compute-discriminating-function e5)
  (enable-generic-function-creation e3 e5)
  (enable-defmethod e5)
  (enable-defclass e5)
  ;; (enable-printing e5)
  (finalize-classes e4 e5)
  (define-error-functions '(sicl-clos::all-descendants sicl-clos::cartesian-product) e4)
  (load-source-file "CLOS/satiation.lisp" e4)
  (load-source-file "CLOS/standard-instance-access.lisp" e4)
  (satiate-generic-functions e4 e5)
  (update-all-objects e4 e5)
  (load-source-file "CLOS/ensure-generic-function-defun.lisp" e5)
  (load-source-file "CLOS/ensure-method-defun.lisp" e5)
  (load-source-file "CLOS/ensure-class.lisp" e5)
  (define-error-functions '(sicl-clos::all-descendants sicl-clos::cartesian-product) e5)
  (load-source-file "CLOS/satiation.lisp" e5)
  (satiate-generic-functions e5 e5)
  (setf (env:special-variable (env:client e5) e5 'sicl-clos::*class-unique-number* t)
        (nth-value 1 (env:special-variable (env:client e4) e4 'sicl-clos::*class-unique-number*))))

