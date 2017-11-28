(cl:in-package #:sicl-extrinsic-environment)

(defun package-relevant-p (package)
  (or (eq package (find-package '#:common-lisp))
      (eq package (find-package '#:closer-mop))
      (let* ((name (package-name package))
             (name-length (length name)))
        (or (and (>= name-length 8)
                 (string-equal (subseq name 0 8) "cleavir-"))
            (and (>= name-length 5)
                 (string-equal (subseq name 0 5) "sicl-"))))))

(defun import-function-from-host (function-name environment)
  (setf (sicl-genv:fdefinition function-name environment)
        (fdefinition function-name)))

(defun import-functions-from-package-symbols (package environment)
  (do-symbols (symbol package)
    (when (eq (symbol-package symbol) (find-package package))
      (when (and (fboundp symbol)
                 (not (special-operator-p symbol))
                 (null (macro-function symbol)))
        (import-function-from-host symbol environment))
      (when (fboundp `(setf ,symbol))
        (import-function-from-host `(setf ,symbol) environment)))))

(defun import-functions-from-host (environment)
  (loop for package in '(sicl-genv
                         cleavir-environment
                         sicl-loop
                         sicl-conditionals
                         cleavir-code-utilities
                         sicl-cons
                         sicl-iteration
                         sicl-clos
                         sicl-environment)
        do (import-functions-from-package-symbols package environment))
  (loop for name in '(symbol-value
                      (setf symbol-value))
        do (import-function-from-host name environment)))

(defun import-from-closer-mop (environment)
  ;; We look at symbols in the package CLOSER-MOP.  If they have some
  ;; interesting definition, we import that definition associated with
  ;; a symbol with the same name but interned in the package
  ;; SICL-CLOS.  But we only do that if it doesn't already have a
  ;; definition associated with the symbol in the SICL-CLOS package.
  (do-symbols (symbol (find-package '#:closer-mop))
    (when (eq (symbol-package symbol) (find-package '#:closer-mop))
      (let ((new (intern (symbol-name symbol) (find-package '#:sicl-clos))))
        ;; Import available functions.
        (when (and (fboundp symbol)
                   (not (special-operator-p symbol))
                   (null (macro-function symbol))
                   (not (sicl-genv:fboundp new environment)))
          (setf (sicl-genv:fdefinition new environment)
                (fdefinition symbol)))
        (when (and (fboundp `(setf ,symbol))
                   (not (sicl-genv:fboundp `(setf ,new) environment)))
          (setf (sicl-genv:fdefinition `(setf ,new) environment)
                (fdefinition `(setf ,symbol))))
        ;; Import constant variables.
        (when (and (constantp symbol)
                   (not (sicl-genv:boundp new environment)))
          (setf (sicl-genv:constant-variable new environment)
                (cl:symbol-value symbol)))
        ;; Import classes.
        (let ((class (find-class symbol nil)))
          (unless (or (null class)
                      (sicl-genv:find-class new environment))
            (setf (sicl-genv:find-class new environment)
                  class)))
        ;; Import special variables.
        (let* ((name (symbol-name symbol))
               (length (length name))
               (boundp (boundp symbol)))
          (when (and (>= length 3)
                     (eql (char name 0) #\*)
                     (eql (char name (1- length)) #\*)
                     (not (constantp symbol))
                     (not (sicl-genv:boundp new environment)))
            (setf (sicl-genv:special-variable new environment boundp)
                  (if boundp (cl:symbol-value symbol) nil))))))))

(defun import-cons-related-functions (environment)
  (loop for name in '(cons car cdr
                      consp list append
                      first second third fourth fifth
                      rest last butlast nth
                      rplaca rplacd cadr cddr reverse member-if-not
                      set-difference)
        do (import-function-from-host name environment)))

(defun import-hash-table-related-functions (environment)
  (loop for name in '(gethash (setf gethash) remhash maphash make-hash-table)
        do (import-function-from-host name environment)))

(defun import-number-related-functions (environment)
  (loop for name in '(+ - * / = < <= > >= evenp zerop)
        do (import-function-from-host name environment)))

(defun import-sequence-related-functions (environment)
  (loop for name in '(count remove-duplicates remove-if-not)
        do (import-function-from-host name environment)))

(defun import-from-common-lisp (environment)
  (import-cons-related-functions environment)
  (import-hash-table-related-functions environment)
  (import-number-related-functions environment)
  (import-sequence-related-functions environment)
  (loop for name in '(find-package funcall gensym
                      not null coerce symbolp atom stringp
                      apply vector eq eql equal values mapcar keywordp
                      endp length
                      member
                      getf
                      print warn proclaim
                      ensure-generic-function)
        do (import-function-from-host name environment)))

(defun import-from-host (environment)
  ;; Import available packages in the host to ENVIRONMENT.
  (setf (sicl-genv:packages environment)
        (remove-if-not #'package-relevant-p (list-all-packages)))
  (import-from-common-lisp environment)
  (import-functions-from-host environment)
  (do-all-symbols (symbol)
    (when (package-relevant-p (symbol-package symbol))
      ;; Import all constant variables in the host to ENVIRONMENT.
      (when (constantp symbol)
        (setf (sicl-genv:constant-variable symbol environment)
              (cl:symbol-value symbol)))
      ;; Import all special operators in the host to ENVIRONMENT
      (when (special-operator-p symbol)
        (setf (sicl-genv:special-operator symbol environment) t))
      ;; Import all classes in the host to ENVIRONMENT
      (let ((class (find-class symbol nil)))
        (unless (null class)
          (setf (sicl-genv:find-class symbol environment)
                class)))
      ;; Import special variables.  There is no predicate for special
      ;; variables in Common Lisp, so we must settle for an
      ;; approximation.  We consider all symbols with earmuffs to be
      ;; special, and if they are bound, we initialize them with that
      ;; value.  We also exclude constant variables that happen to have
      ;; earmuffs.
      (let* ((name (symbol-name symbol))
             (length (length name))
             (boundp (boundp symbol)))
        (when (and (>= length 3)
                   (eql (char name 0) #\*)
                   (eql (char name (1- length)) #\*)
                   (not (constantp symbol)))
          (setf (sicl-genv:special-variable symbol environment boundp)
                (if boundp (cl:symbol-value symbol) nil))))))
  (import-from-closer-mop environment))
