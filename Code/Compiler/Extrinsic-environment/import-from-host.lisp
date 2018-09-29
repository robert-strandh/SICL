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
                         sicl-data-and-control-flow
                         sicl-cons
                         sicl-iteration
                         sicl-clos
                         sicl-evaluation-and-compilation
                         sicl-standard-environment-macros
                         sicl-environment)
        do (import-functions-from-package-symbols package environment))
  (loop for name in '(symbol-value
                      (setf symbol-value))
        do (import-function-from-host name environment)))

(defun import-cons-related-functions (environment)
  (loop for name in '(cons car cdr (setf car) (setf cdr)
                      consp listp list list* append make-list
                      first second third fourth fifth
                      rest last butlast nth
                      rplaca rplacd cadr cddr reverse member-if-not
                      (setf cadr) (setf cddr) caddr
                      cdddr reduce union assoc mapc intersection
                      get-properties adjoin
                      copy-list set-difference set-exclusive-or)
        do (import-function-from-host name environment)))

(defun import-hash-table-related-functions (environment)
  (loop for name in '(gethash (setf gethash) remhash maphash make-hash-table)
        do (import-function-from-host name environment)))

(defun import-number-related-functions (environment)
  (loop for name in '(+ - * / = < <= > >= /= evenp zerop 1+ 1-
                      floor ceiling)
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
                      endp length every find-if-not remove find find-if
                      position subseq
                      member sort
                      getf typep class-of format
                      print warn proclaim compile
                      ensure-generic-function)
        do (import-function-from-host name environment)))

(defun import-from-host (environment)
  (host-load "../../Data-and-control-flow/defun-support.lisp")
  (host-load "../../Data-and-control-flow/shiftf-support.lisp")
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
      ;; Import special variables.
      (loop for symbol in '(*package* *macroexpand-hook*)
            for boundp = (boundp symbol)
            do (setf (sicl-genv:special-variable symbol environment boundp)
                     (if boundp (cl:symbol-value symbol) nil))))))
