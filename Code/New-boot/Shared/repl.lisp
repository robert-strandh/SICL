(cl:in-package #:sicl-new-boot)

(defun repl (client global-environment)
  (let* ((environment
           (make-instance 'trucler-reference:environment
             :global-environment global-environment))
         (eclector.base:*client* client)
         (abp:*builder* (cb::make-builder client environment)))
    (loop for prompt = (progn (princ ">>> ") (finish-output))
          for form = (eclector.reader:read)
          for cst = (cst:cst-from-expression form)
          for values
            = (multiple-value-list (eval-cst client cst environment))
          do (loop for value in values
                   do (format t "~s~%" value))
             (finish-output))))

(defun repl1 (boot)
  (let* ((environment (e1 boot))
         (package (find-package '#:sicl-new-boot-phase-1))
         (client-symbol (find-symbol "CLIENT" package))
         (client-object (make-instance client-symbol
                          :environment environment)))
    (repl client-object environment)))

(defun repl2 (boot)
  (let* ((environment (e2 boot))
         (package (find-package '#:sicl-new-boot-phase-2))
         (client-symbol (find-symbol "CLIENT" package))
         (client-object (make-instance client-symbol
                          :environment environment)))
    (repl client-object environment)))

(defun repl3 (boot)
  (let* ((environment (e3 boot))
         (package (find-package '#:sicl-new-boot-phase-3))
         (client-symbol (find-symbol "CLIENT" package))
         (client-object (make-instance client-symbol
                          :environment environment)))
    (repl client-object environment)))

(defun repl4 (boot)
  (let* ((*boot* boot)
         (environment (e4 boot))
         (package (find-package '#:sicl-new-boot-phase-4))
         (client-symbol (find-symbol "CLIENT" package))
         (client-object (make-instance client-symbol
                          :environment environment))
         (env:*client* client-object))
    (repl client-object environment)))
