(cl:in-package #:sicl-new-boot)

;;; Return a list of pathnames of all the Lisp source files
;;; of the ASDF system with the name given as an argument.
(defun source-file-path-names (asdf-system)
  (loop for component in (asdf/plan:required-components asdf-system :other-systems nil)
        when (typep component 'asdf/lisp-action:cl-source-file)
          append (asdf:input-files 'asdf:compile-op component)))

(defun load-asdf-system-files (client environment asdf-system)
  (loop for path in (source-file-path-names asdf-system)
        do (load-source-file client path environment)))

;; (defun check-dependencies (asdf-system environment)
;;   (loop for dependency in (asdf/system:system-depends-on asdf-system)
;;         for system = (asdf:find-system dependency)
;;         unless (member system (loaded-asdf-systems environment))
;;           do (warn "System ~s depends on system ~s which is not loaded."
;;                    asdf-system system)))

;;; Ensure that ASDF-SYSTEM is loaded into ENVIRONMENT.
;;; LOAD-ASDF-SYSTEM-FILES is called for each Lisp file of
;;; ASDF-SYSTEM, in the order determined by dependencies.  ASDF-SYSTEM
;;; is either an instance of ASDF/SYSTEM:SYSTEM or a name of an ASDF
;;; system.
;;;
;;; If the keyword argument LOAD-SYSTEM-FILE is true, then the system
;;; file itself is loaded as well.  We use this possibility when the
;;; system file contains code that must be loaded into the
;;; environment, like when it contains package definitions or other
;;; code.
(defun ensure-asdf-system
    (client environment asdf-system &key load-system-file)
  (let ((asdf-system (asdf:find-system asdf-system)))
    ;; (check-dependencies asdf-system environment)
    (format *trace-output*
            "Loading ASDF system ~s into environment ~a~%"
            (asdf/system:primary-system-name asdf-system)
            (name (trucler:global-environment client environment)))
    (when load-system-file
      (let ((path (asdf/system:system-source-file asdf-system)))
        (load-source-file client path environment)))
    (load-asdf-system-files client environment asdf-system)
    ;; (push asdf-system (loaded-asdf-systems environment))
    (format *trace-output*
            "Done loading ASDF system ~s into environment ~a~%"
            (asdf/system:primary-system-name asdf-system)
            (name (trucler:global-environment client environment)))))
