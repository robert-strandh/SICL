(cl:in-package #:sicl-global-environment)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compiler macros.

(defun (setf compiler-macro-function) (new-function name &optional environment)
  (unless (null environment)
    (error "Environment object must be nil."))
  (let ((base-entry (find-base-entry name *global-environment*)))
    (when (null base-entry)
      (error "A global macro or a global function must already exist."))
    (let ((c-m-entry (find-if (lambda (entry)
				(eq (base-entry entry) base-entry))
			      (compiler-macros *global-environment*))))
      ;; Remove the old entry if there was one.
      (unless (null c-m-entry)
	(setf (compiler-macros *global-environment*)
	      (delete c-m-entry (compiler-macros *global-environment*)
		      :test #'eq)))
      ;; Add a new entry unless the new function is NIL.
      (unless (null new-function)
	(push (make-compiler-macro-entry base-entry new-function)
	      (compiler-macros *global-environment*)))))
  ;; Return the new value as required by the HyperSpec.
  new-function)
