(cl:in-package #:sicl-boot-phase-7)

(defun find-classes (e5)
  (let ((ht (make-hash-table :test #'eq))
        (result '()))
    (do-all-symbols (var)
      (unless (gethash var ht)
        (setf (gethash var ht) t)
        (let ((potential-class (sicl-genv:find-class var e5)))
          (unless (null potential-class)
            (push potential-class result)))))
    result))

(defun patch-class (class e4 e5)
  (let ((standard-class-e4
          (sicl-genv:find-class 'standard-class e4))
        (funcallable-standard-class-e4
          (sicl-genv:find-class 'sicl-clos:funcallable-standard-class e4))
        (built-in-class-e4
          (sicl-genv:find-class 'built-in-class e4))
        (standard-class-e5
          (sicl-genv:find-class 'standard-class e5))
        (funcallable-standard-class-e5
          (sicl-genv:find-class 'sicl-clos:funcallable-standard-class e5))
        (built-in-class-e5
          (sicl-genv:find-class 'built-in-class e5))
        (dsfun (sicl-genv:fdefinition 'sicl-clos:class-direct-slots e5))
        (esfun (sicl-genv:fdefinition 'sicl-clos:class-slots e5)))
    (loop for ds in (funcall dsfun class)
          do (patch-direct-slot-definition ds e5))
    (loop for es in (funcall esfun class)
          do (patch-effective-slot-definition es e5))
    (loop for es in (aref (slot-value class 'sicl-boot::%rack) 1)
          do (patch-effective-slot-definition es e5))
    (setf (slot-value class 'sicl-boot::%class)
          (let ((current-class (slot-value class 'sicl-boot::%class)))
            (cond ((eq current-class standard-class-e4)
                   standard-class-e5)
                  ((eq current-class funcallable-standard-class-e4)
                   funcallable-standard-class-e5)
                  ((eq current-class built-in-class-e4)
                   built-in-class-e5)
                  (t (error "Not a valid class")))))))

(defun patch-classes (e4 e5)
  (loop for class in (find-classes e5)
        do (patch-class class e4 e5)))
