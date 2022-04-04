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
;;; nothing.  If not, the systems that ASDF-SYSTEM depends on are
;;; first considered recursively.  Then LOAD-ASDF-SYSTEM-FILES is
;;; called for each Lisp file of ASDF-SYSTEM, in the order determined
;;; by dependencies.  ASDF-SYSTEM is either an instance of
;;; ASDF/SYSTEM:SYSTEM or a name of an ASDF system.
(defun ensure-asdf-system (asdf-system environment)
  (let ((asdf-system (asdf:find-system asdf-system)))
    (unless (member asdf-system (loaded-asdf-systems environment) :test #'eq)
      (format *trace-output*
              "Prepare loading ASDF system ~s into environment ~a~%"
              (asdf/system:primary-system-name asdf-system)
              (name environment))
      (loop with dependencies = (asdf/system:system-depends-on asdf-system)
            for dependency in dependencies
            do (ensure-asdf-system dependency environment))
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

(defclass load-op (asdf/action:downward-operation asdf/action:selfward-operation)
  ((asdf:selfward-operation :initform '(prepare-op) :allocation :class)))

(defmethod print-object ((object load-op) stream)
  (print-unreadable-object (object stream :type t :identity t)
    nil))

(defclass prepare-op (asdf/action:upward-operation asdf/action:sideway-operation)
  ((asdf:sideway-operation :initform 'load-op :allocation :class)))

(defmethod asdf:perform ((operation load-op) component)
  (format *trace-output*
          "Not acting upon component ~s~%"
          component))

(defvar *environment*)

(defmethod asdf:perform
    ((operation load-op) (component asdf/lisp-action:cl-source-file))
  (load-source-file-common
   (asdf/component:component-pathname component)
   *environment*))

(defmethod asdf:perform ((operation load-op) (component asdf/system:system))
  (format *trace-output*
          "Done loading system ~s~%"
          component))

(defmethod asdf:perform ((operation prepare-op) component)
  (declare (ignore component))
  nil)

(defmethod asdf:perform ((operation prepare-op) (component asdf/system:system))
  (format *trace-output*
          "Loading system ~s~%"
          component))

(defmethod asdf/action:compute-action-stamp
    (plan (operation prepare-op) component &key just-done)
  (if just-done
      (values (get-universal-time) t)
      (values nil nil)))

(defmethod asdf/action:compute-action-stamp
    (plan (operation load-op) component &key just-done)
  (if just-done
      (values (get-universal-time) t)
      (values nil nil)))

(defmethod asdf/action:compute-action-stamp
    (plan (operation load-op) (component asdf/lisp-action:cl-source-file) &key just-done)
  (if just-done
      (values (get-universal-time) t)
      (let* ((files (loaded-files *environment*))
             (path (asdf/component:component-pathname component))
             (entry (assoc path files :test #'equal)))
        (if (null entry)
            (values nil nil)
            (values (cdr entry) t)))))

(defun load-asdf-system (asdf-system-name environment)
  (let ((*environment* environment))
    (asdf:operate (asdf:make-operation 'load-op)
                  (asdf:find-system asdf-system-name))))
