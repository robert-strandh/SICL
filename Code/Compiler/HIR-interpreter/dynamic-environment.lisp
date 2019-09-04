(cl:in-package #:sicl-hir-interpreter)

(defparameter *dynamic-environment* '())

(defclass entry () ())

(defclass exit-point (entry)
  ((%valid-p :initform t :accessor valid-p)))

(defclass block/tagbody-entry (exit-point)
  ((%transfer-tag :initarg :transfer-tag :reader transfer-tag)
   (%abandoon-tag :initarg :abandoon-tag :reader abandoon-tag)))

(defmethod print-object ((object block/tagbody-entry) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~s ~s"
            (abandoon-tag object))))

(defclass catch-entry (exit-point)
  ((%tag :initarg :tag :reader tag)))

(defmethod print-object ((object catch-entry) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~s" (tag object))))

(defclass special-variable-entry (entry)
  ((%name :initarg :name :reader name)
   (%value :initarg :value :accessor value)))

(defmethod print-object ((object special-variable-entry) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~s ~s" (name object) (value object))))

(defclass unwind-protect-entry (entry)
  ((%thunk :initarg :thunk :reader thunk)))

(defmethod print-object ((object unwind-protect-entry) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~s" (thunk object))))

(defun find-entry (identifier dynamic-environment)
  (loop for entry in dynamic-environment
        when (and (typep entry 'block/tagbody-entry)
                  (eq (identifier entry) identifier))
          return entry))

(defun compute-source-info (old new)
  (if (null new) old new))

(declaim (notinline compute-source-info))

(defparameter *values-environment* '())

(defvar *global-values-location*)

(defvar *previous-instruction*)

(defmethod interpret-instruction :before
    (client instruction lexical-environment)
  (let ((env1 (gethash (cleavir-ir:dynamic-environment-location
                        *previous-instruction*)
                       lexical-environment))
        (env2 (gethash (cleavir-ir:dynamic-environment-location
                        instruction)
                       lexical-environment)))
    (loop for env = env1 then (rest env)
          for entry = (first env)
          until (eq env env2)
          do (setf (valid-p entry) nil))
    (let ((last-block/tagbody
            (loop with result = nil
                  for env = env1 then (rest env)
                  for entry = (first env)
                  until (eq env env2)
                  when (typep entry 'unwind-protect-entry)
                    do (funcall (thunk entry))
                  when (typep entry 'block/tagbody-entry)
                    do (setf result entry)
                  finally (return result))))
      (unless (null last-block/tagbody)
        (throw (abandon-tag last-block/tagbody)
          instruction)))))
