(cl:in-package #:sicl-boot-phase-8)

(defun load-source (relative-pathname environment)
  (let* ((dot-pos (position #\. relative-pathname))
         (prefix (subseq relative-pathname 0 dot-pos))
         (relative-fasl-pathname (concatenate 'string prefix ".fasl")))
    (sicl-boot:compile-file
     (sicl-genv:client sicl-boot:*e0*)
     relative-pathname
     environment)
    (load-fasl relative-fasl-pathname environment)))

(defun find-non-sicl-functions (environment)
  (let ((result '())
        (table (make-hash-table :test #'eq))
        (simple-function-class (sicl-genv:find-class 'sicl-clos:simple-function environment))
        (standard-generic-function-class (sicl-genv:find-class 'sicl-clos:standard-generic-function environment)))
    (do-all-symbols (symbol)
      (unless (gethash symbol table)
        (setf (gethash symbol table) t)
        (unless (eq (symbol-package symbol) (find-package '#:sicl-genv))
          (when (sicl-genv:fboundp symbol environment)
            (let ((fun (sicl-genv:fdefinition symbol environment)))
              (unless (or (consp fun)
                          (and (typep fun 'sicl-boot::header)
                               (member (slot-value fun 'sicl-boot::%class)
                                       (list simple-function-class
                                             standard-generic-function-class))))
                (push symbol result))))
          (when (sicl-genv:fboundp `(setf ,symbol) environment)
            (let ((fun (sicl-genv:fdefinition `(setf ,symbol) environment)))
              (unless (or (consp fun)
                          (and (typep fun 'sicl-boot::header)
                               (member (slot-value fun 'sicl-boot::%class)
                                       (list simple-function-class
                                             standard-generic-function-class))))
                (push `(setf ,symbol) result)))))))
    result))

(defun load-asdf-system-components (name environment)
  (loop for name in (sicl-boot:asdf-system-components name)
        do (load-source name environment)))

;;; This variable holds an alist where keys are function names and
;;; values are function definitions or NIL.
(defparameter *intercepted-functions* '())

(defmethod sicl-genv:fboundp :around
    (function-name (environment sicl-boot-phase-5::environment))
  (if (find function-name *intercepted-functions*
            :test #'equal
            :key #'car)
      nil
      (call-next-method)))

(defmethod (setf sicl-genv:fdefinition) :around
    (new-definition function-name (environment sicl-boot-phase-5::environment))
  (let ((entry (find function-name *intercepted-functions*
                     :test #'equal
                     :key #'car)))
    (if (null entry)
        (call-next-method)
        (setf (cdr entry) new-definition))))

(defun invoke-with-intercepted-function-names
    (thunk intercepted-function-names environment)
  (let ((intercepted-functions (mapcar #'list intercepted-function-names)))
    (let ((*intercepted-functions* intercepted-functions))
      (funcall thunk))
    (loop for (name . definition) in intercepted-functions
          do (setf (sicl-genv:fdefinition name environment) definition))))

(defmacro with-intercepted-function-names ((names environment) &body body)
  `(invoke-with-intercepted-function-names
    (lambda () ,@body)
    ',names
    ,environment))
