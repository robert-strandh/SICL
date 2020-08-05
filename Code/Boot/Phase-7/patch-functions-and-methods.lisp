(cl:in-package #:sicl-boot-phase-7)

(defun patch-simple-function (function e3 e4 e5)
  ;; We may need E3 and E4 later.
  (declare (ignore e3 e4))
  (setf (slot-value function 'sicl-boot::%class)
        (sicl-genv:find-class 'sicl-clos:simple-function e5)))

(defun patch-method (method e3 e4 e5)
  (declare (ignore e4))
  (let ((standard-method-class-e3
          (sicl-genv:find-class 'standard-method e3))
        (standard-reader-method-class-e3
          (sicl-genv:find-class 'sicl-clos:standard-reader-method e3))
        (standard-writer-method-class-e3
          (sicl-genv:find-class 'sicl-clos:standard-writer-method e3))
        (standard-method-class-e5
          (sicl-genv:find-class 'standard-method e5))
        (standard-reader-method-class-e5
          (sicl-genv:find-class 'sicl-clos:standard-reader-method e5))
        (standard-writer-method-class-e5
          (sicl-genv:find-class 'sicl-clos:standard-writer-method e5))
        (current-class (slot-value method 'sicl-boot::%class)))
    (setf (slot-value method 'sicl-boot::%class)
          (cond ((eq current-class standard-method-class-e3)
                 standard-method-class-e5)
                ((eq current-class standard-reader-method-class-e3)
                 standard-reader-method-class-e5)
                ((eq current-class standard-writer-method-class-e3)
                 standard-writer-method-class-e5)
                (t
                 (error "unknown method class"))))))

(defun patch-generic-function (function e3 e4 e5)
  (loop with mfun = (sicl-genv:fdefinition 'sicl-clos:generic-function-methods e4)
        for method in (funcall mfun function)
        do (patch-method method e3 e4 e5))
  (let ((method-combination
          (funcall (sicl-genv:fdefinition
                    'sicl-clos:generic-function-method-combination e4)
                   function)))
    (patch-method-combination method-combination e5))
  ;; FIXME: this is not great.  We should try to use
  ;; REINITIALIZE-INSTANCE instead
  (setf (aref (slot-value function 'sicl-boot::%rack) 10)
        (sicl-genv:find-class 'standard-method e5))
  (setf (slot-value function 'sicl-boot::%class)
        (sicl-genv:find-class 'standard-generic-function e5)))

(defun patch-function (function e3 e4 e5)
  (let ((simple-function-class-e3
          (sicl-genv:find-class 'sicl-clos:simple-function e3))
        ;; We moved some simple functions from E6 to E5, and the class
        ;; of those functions is the class SIMPLE-FUNCTION in E4,
        ;; rather than E4, so we must check for that as well.
        (simple-function-class-e4
          (sicl-genv:find-class 'sicl-clos:simple-function e4))
        (generic-function-class-e3
          (sicl-genv:find-class 'sicl-clos:standard-generic-function e3)))
    (if (not (typep function 'sicl-boot::header))
        'not-a-sicl-function
        (let ((current-class (slot-value function 'sicl-boot::%class)))
          (cond ((or (eq current-class simple-function-class-e3)
                     (eq current-class simple-function-class-e4))
                 (patch-simple-function function e3 e4 e5))
                ((eq current-class generic-function-class-e3)
                 (patch-generic-function function e3 e4 e5))
                (t
                 'unknown-function-class))))))

(defun find-functions (e5)
  (let ((ht (make-hash-table :test #'eq))
        (result '()))
    (do-all-symbols (var)
      (unless (gethash var ht)
        (setf (gethash var ht) t)
        (when (sicl-genv:fboundp var e5)
          (push (cons var (sicl-genv:fdefinition var e5))
                result))
        (when (sicl-genv:fboundp `(setf ,var) e5)
          (push (cons `(setf ,var) (sicl-genv:fdefinition `(setf ,var) e5))
                result))))
    result))

(defun patch-functions (e3 e4 e5)
  (loop for (name . function) in (find-functions e5)
        do (case (patch-function function e3 e4 e5)
             (not-a-sicl-function
              (if (consp function)
                  (when (eq (car function) 'macro-function)
                    (unless (typep (cadr function) 'sicl-boot::header)
                      (format *trace-output*
                              "Macro function for ~s is not a SICL function~%"
                              name)))
                  (format *trace-output*
                          "~s is not a SICL function~%"
                          name)))
             (unknown-function-class
              (format *trace-output*
                      "~s has an unknown class~%"
                      name)))))
