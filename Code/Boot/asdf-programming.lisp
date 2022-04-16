(cl:in-package #:sicl-boot)

;;; Return a list of pathnames of all the Lisp source files
;;; of the ASDF system with the name given as an argument.
(defun source-file-path-names (asdf-system)
  (loop for component in (asdf/plan:required-components asdf-system :other-systems nil)
        when (typep component 'asdf/lisp-action:cl-source-file)
          append (asdf:input-files 'asdf:compile-op component)))

(defun load-asdf-system-files (asdf-system environment)
  (loop for path in (source-file-path-names asdf-system)
        do (load-source-file-common path environment)))

;;; Ensure that ASDF-SYSTEM is loaded into ENVIRONMENT.  If
;;; ASDF-SYSTEM is already loaded into ENVIRONMENT, this function does
;;; nothing.  If not, then LOAD-ASDF-SYSTEM-FILES is called for each
;;; Lisp file of ASDF-SYSTEM, in the order determined by dependencies.
;;; ASDF-SYSTEM is either an instance of ASDF/SYSTEM:SYSTEM or a name
;;; of an ASDF system.
(defun ensure-asdf-system (asdf-system environment)
  (let ((asdf-system (asdf:find-system asdf-system)))
    (unless (member asdf-system (loaded-asdf-systems environment) :test #'eq)
      (format *trace-output*
              "Loading ASDF system ~s into environment ~a~%"
              (asdf/system:primary-system-name asdf-system)
              (name environment))
      (load-asdf-system-files asdf-system environment)
      (push asdf-system (loaded-asdf-systems environment))
      (format *trace-output*
              "Done loading ASDF system ~s into environment ~a~%"
              (asdf/system:primary-system-name asdf-system)
              (name environment)))))
