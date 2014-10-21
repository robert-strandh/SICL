(cl:in-package #:sicl-global-environment)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function (SETF MACRO-FUNCTION).
;;;
;;; The HyperSpec says that the consequences are undefined if a
;;; non-nil environment is given.  We define those consequences to be
;;; that an error is signaled.
;;;
;;; The HyperSpec further says that "Performing this operation causes
;;; symbol to have only that macro definition as its global function
;;; definition; any previous definition, whether as a macro or as a
;;; function, is lost."
;;;
;;; The HyperSpec also says that it is possible for a symbol to be
;;; defined both as a special operator and as a macro.  For that
;;; reason, we want to find out whether there is an entry that is
;;; either a global function entry or a global macro entry, but we
;;; want to ignore the possibility of there being a special operator
;;; entry.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun (setf macro-function) (new-function symbol &optional environment)
    (declare (cl:type null environment)
	     (cl:type symbol symbol)
	     (cl:type function new-function)
	     (ignore environment))
    ;; First check whether there is a global function entry with
    ;; this name.
    (let ((function-entry
	    (find symbol (functions *global-environment*)
		  :key #'name :test #'eq)))
      (if (not (null function-entry))
	  (progn
	    ;; Make it unbound.
	    (setf (car (storage (location function-entry))) +funbound+)
	    ;; Remove any compiler macro that might be present referring
	    ;; to this entry.
	    (setf (compiler-macros *global-environment*)
		  (remove function-entry (compiler-macros *global-environment*)
			  :key #'base-entry :test #'eq))
	    ;; Remove any proclamations referring to this entry.
	    (setf (proclamations *global-environment*)
		  (remove-if (lambda (entry)
			       (and (typep entry 'auxiliary-entry)
				    (eq (base-entry entry) function-entry)))
			     (proclamations *global-environment*)))
	    (push (make-instance 'macro-entry
		    :name symbol
		    :definition new-function)
		  (macros *global-environment*)))
	  ;; No function entry exists.  check whether there is a
	  ;; global macro entry with this name.
	  (let ((macro-entry
		  (find symbol (macros *global-environment*)
			:key #'name :test #'eq)))
	    (if (not (null macro-entry))
		;; Then just replace the old definition.  This
		;; way, we preserve any compiler macro that refers
		;; to this entry.
		(setf (definition macro-entry) new-function)
		;; Otherwise, we must create a new entry
		(push (make-instance 'macro-entry
			:name symbol
			:definition new-function)
		      (macros *global-environment*))))))
    ;; Return the new value as required by the HyperSpec.
    new-function))
