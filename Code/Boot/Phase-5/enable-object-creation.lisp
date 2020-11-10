(cl:in-package #:sicl-boot-phase-5)

(defun enable-object-allocation (e4)
  (setf (env:fdefinition (env:client e4) e4 'sicl-clos::allocate-general-instance)
        (lambda (class size)
          (make-instance 'sicl-boot:header
            :class class
            :rack (make-array size :initial-element 10000000))))
  (load-source-file "CLOS/stamp-offset-defconstant.lisp" e4)
  (load-source-file "CLOS/effective-slot-definition-class-support.lisp" e4)
  (load-source-file "CLOS/effective-slot-definition-class-defgeneric.lisp" e4)
  (load-source-file "CLOS/effective-slot-definition-class-defmethods.lisp" e4)
  ;; These were already loaded in phase 3 because they were needed for
  ;; the finalization of built-in classes.
  ;; (load-source-file "CLOS/class-finalization-defgenerics.lisp" e4)
  ;; (load-source-file "CLOS/class-finalization-defmethods.lisp" e4)
  (load-source-file "CLOS/allocate-instance-support.lisp" e4)
  (load-source-file "CLOS/allocate-instance-defgenerics.lisp" e4)
  (load-source-file "CLOS/allocate-instance-defmethods.lisp" e4))

(defun finalize-inheritance (e4)
  (let ((fun (env:fdefinition (env:client e4) e4 'sicl-clos:finalize-inheritance)))
    (do-all-symbols (symbol)
      (let ((class (env:find-class (env:client e4) e4 symbol)))
        (unless (or (null class)
                    (funcall (env:fdefinition (env:client e4) e4 'sicl-clos::class-finalized-p)
                             class))
          (funcall fun class))))))

(defun enable-object-creation (e4 e5)
  (declare (ignore e5))
  (enable-object-allocation e4)
  (finalize-inheritance e4))
