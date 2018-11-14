(cl:in-package #:sicl-new-boot-phase-4)

(defclass header (closer-mop:funcallable-standard-object)
  ((%class :initarg :class)
   (%rack :initarg :rack))
  (:metaclass closer-mop:funcallable-standard-class))

(defun enable-allocate-instance-in-e3 (e2 e3)
  ;; (setf (sicl-genv:fdefinition 'sicl-clos::allocate-general-instance e3)
  ;;       (lambda (class size)
  ;;         (make-instance 'header
  ;;           :class class
  ;;           :rack (make-array size :initial-element 10000000))))
  ;; (setf (sicl-genv:fdefinition 'sicl-clos::general-instance-access e3)
  ;;       (lambda (object location)
  ;;         (aref (slot-value object '%rack) location)))
  ;; (setf (sicl-genv:fdefinition '(setf sicl-clos::general-instance-access) e3)
  ;;       (lambda (value object location)
  ;;         (setf (aref (slot-value object '%rack) location) value)))
  ;; (import-functions-from-host
  ;;  '((setf sicl-genv:constant-variable) sort every
  ;;    mapc 1+ 1- subseq butlast position identity nthcdr equal
  ;;    remove-if-not mapcar reverse find compile)
  ;;  e3)
  (setf (sicl-genv:fdefinition 'allocate-instance e3)
        (sicl-genv:fdefinition 'allocate-instance e2)))
