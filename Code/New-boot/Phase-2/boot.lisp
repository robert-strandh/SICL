(cl:in-package #:sicl-new-boot-phase-2)

;;; We program the reader so that we can write:
;;; @package-name symbol-name to intern a symbol in a
;;; Parcl package at run time.
(eval-when (:compile-toplevel)
  (setf *readtable* (copy-readtable))
  (set-macro-character
   #\@ (lambda (stream character)
         (declare (ignore character))
         (multiple-value-bind (package-name symbol-name)
             (sb:read-symbol-components stream)
           `(sb:intern-parcl-symbol client ,package-name ,symbol-name)))))

(defun boot (boot)
  (format *trace-output* "**************** Phase 2~%")
  (let* ((client (make-instance 'client))
         (environment (create-environment client))
         (global-environment
           (trucler:global-environment client environment))
         (env:*client* client)
         (env:*environment* global-environment))
    (setf (sb:e2 boot) global-environment
          (sb:c2 boot) client)
    (reinitialize-instance client
      :environment global-environment)
    (clo:make-variable
     client global-environment '*package* (find-package '#:common-lisp-user))
    (sb:define-package-functions client global-environment)
    (sb:define-backquote-macros client global-environment)
    (import-from-host client global-environment)
    (sb:import-khazern client global-environment)
    (sb:define-environment-functions client global-environment)
    (clo:make-variable client global-environment
                       @sicl-environment:*environment*
                       global-environment)
    (clo:make-variable client global-environment
                       @sicl-environment:*client*
                       client)
    (define-ensure-method-combination-template
        client (sb:e1 boot) global-environment)
    (define-^ensure-method-combination
        client (sb:e1 boot) global-environment)
    (define-find-method-combination-template client global-environment)
    (setf (clo:fdefinition
           client global-environment
           @sicl-clos:^ensure-generic-function-using-class)
          (clo:fdefinition
           client (sb:e1 boot)
           @sicl-clos:ensure-generic-function-using-class))
    (setf (clo:fdefinition
           client global-environment @sicl-clos:^ensure-class-using-class)
          (clo:fdefinition
           client (sb:e1 boot) @sicl-clos:ensure-class-using-class))
    (sb:ensure-asdf-system client environment "sicl-clos-ensure-metaobject")
    (define-ecclesia-functions client (sb:e1 boot) global-environment)
    (sb:ensure-asdf-system
     client environment "clostrophilia-method-combination"))
  boot)
