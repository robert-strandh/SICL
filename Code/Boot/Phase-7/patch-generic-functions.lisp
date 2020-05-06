(cl:in-package #:sicl-boot-phase-7)

(defun find-generic-functions (e3 e5)
  (let ((ht (make-hash-table :test #'eq))
        (result '())
        (generic-function-class-e3
          (sicl-genv:find-class 'standard-generic-function e3)))
    (do-all-symbols (var)
      (unless (gethash var ht)
        (setf (gethash var ht) t)
        (when (sicl-genv:fboundp var e5)
          (let ((fun (sicl-genv:fdefinition var e5)))
            (when (and (typep fun 'sicl-boot::header)
                       (eq (slot-value fun 'sicl-boot::%class)
                           generic-function-class-e3))
              (push fun result))))
        (when (sicl-genv:fboundp `(setf ,var) e5)
          (let ((fun (sicl-genv:fdefinition `(setf ,var) e5)))
            (when (and (typep fun 'sicl-boot::header)
                       (eq (slot-value fun 'sicl-boot::%class)
                           generic-function-class-e3))
              (push fun result))))))
    result))

(defun patch-generic-functions (e3 e4 e5)
  (let ((standard-method-class-e3
          (sicl-genv:find-class 'standard-method e3))
        (standard-reader-method-class-e3
          (sicl-genv:find-class 'sicl-clos:standard-reader-method e3))
        (standard-writer-method-class-e3
          (sicl-genv:find-class 'sicl-clos:standard-writer-method e3))
        (standard-generic-function-class-e5
          (sicl-genv:find-class 'standard-generic-function e5))
        (standard-method-class-e5
          (sicl-genv:find-class 'standard-method e5))
        (standard-reader-method-class-e5
          (sicl-genv:find-class 'sicl-clos:standard-reader-method e5))
        (standard-writer-method-class-e5
          (sicl-genv:find-class 'sicl-clos:standard-writer-method e5))
        (mfun (sicl-genv:fdefinition 'sicl-clos:generic-function-methods e4))
        (standard-method-combination-class-e5
          ;; FIXME, make this STANDARD-METHOD-COMBINATION once we have it.
          (sicl-genv:find-class 'method-combination e5))
        (functions (find-generic-functions e3 e5)))
    (loop for function in functions
          do (loop for method in (funcall mfun function)
                   for class = (slot-value method 'sicl-boot::%class)
                   do (setf (slot-value method 'sicl-boot::%class)
                            (cond ((eq class standard-method-class-e3)
                                   standard-method-class-e5)
                                  ((eq class standard-reader-method-class-e3)
                                   standard-reader-method-class-e5)
                                  ((eq class standard-writer-method-class-e3)
                                   standard-writer-method-class-e5)
                                  (t
                                   (error "unknown method class")))))
             (let ((method-combination
                     (funcall (sicl-genv:fdefinition
                               'sicl-clos:generic-function-method-combination e4)
                              function)))
               (setf (slot-value method-combination 'sicl-boot::%class)
                     standard-method-combination-class-e5))
             ;; FIXME: this is not great.  We should try to use
             ;; REINITIALIZE-INSTANCE instead
             (setf (aref (slot-value function 'sicl-boot::%rack) 10)
                   standard-method-class-e5)
             (setf (slot-value function 'sicl-boot::%class)
                   standard-generic-function-class-e5))))
