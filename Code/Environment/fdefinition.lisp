(cl:in-package #:sicl-global-environment)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function (SETF FDEFINITION).
;;;
;;; They HyperSpec says that this function can be used "to replace a
;;; global function definition when the function-name's function
;;; definition does not represent a special form.  [it] requires a
;;; function as the new value."
;;;
;;; We take this to mean: If we find an existing
;;; special-operator-entry for the name, then we signal an error.  If
;;; we find an existing macro-entry for the name, we replace it
;;; with a global-function entry.  If we find an existing
;;; global-function entry, we replace the definition.  If no existing
;;; entry is found, we create one.
;;;
;;; In the case of an existing macro-entry, we must remove it.
;;; If in addition, it has a compiler macro entry referring to it, we
;;; must remove that compiler macro entry as well.

(defun (setf fdefinition) (new-definition function-name)
  (declare (cl:type function-name function-name)
	   (cl:type function new-definition))
  ;; First see whether there is a special operator entry for the name.
  (let ((specop-entry
	  (find function-name (special-operators *global-environment*)
		:key #'name :test #'eq)))
    (if (not (null specop-entry))
	;; We found a special operator entry.  In this situation we
	;; signal an error.
	(error "can't replace a special operator")
	(progn
	  ;; If there was no special operator entry, then check whether
	  ;; there might be a global macro entry. 
	  (let ((macro-entry
		  (find function-name (macros *global-environment*)
			:key #'name :test #'eq)))
	    (unless (null macro-entry)
	      ;; We found a global macro entry.  We must remove it.
	      (setf (macros *global-environment*)
		    (delete macro-entry
			    (macros *global-environment*)
			    :test #'eq))
	      ;; There might be a compiler macro entry that refers to
	      ;; the global macro entry we just removed, because
	      ;; compiler macro entries are auxiliary entries.  If so we
	      ;; remove that one too.
	      (setf (compiler-macros *global-environment*)
		    (delete macro-entry (compiler-macros *global-environment*)
			    :key #'base-entry :test #'eq))))
	  ;; When we come here, we know that there is no special
	  ;; operator entry with the name we are defining, and if
	  ;; there was a global macro entry for it, then that entry
	  ;; has been removed.  Next, we check whether there is an
	  ;; existing global function entry.
	  (let ((function-entry
		  (find function-name (functions *global-environment*)
			:key #'name :test #'equal)))
	    (when (null function-entry)
	      ;; No function entry found.  Create one.
	      (setf function-entry (make-function-entry function-name))
	      (push function-entry (functions *global-environment*)))
	    ;; Now, we have a global function entry for the name,
	    ;; whether it already existed, or we just created one.
	    ;; All we need to do is assign the new defintion to the
	    ;; storage cell of the entry.
	    (setf (car (storage function-entry)) new-definition)))))
  ;; The HyperSpec says that any SETF function must return the new
  ;; value that was assigned.
  new-definition)
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function SYMBOL-FUNCTION.
;;;
;;; According to the HyperSpec, SYMBOL-FUNCTION is just like
;;; FDEFINITION, except that it only accepts a symbol as its argument.
;;; I am guessing that SYMBOL-FUNCTION existed before (SETF <mumble>)
;;; were legal function names, and that FDEFINITION was introduced to
;;; make such names possible.  In fact, on the SYMBOL-FUNCTION page,
;;; the HyperSpec says: (symbol-function symbol) == (fdefinition symbol)
;;; It suffices thus to check that the argument is a symbol, and then 
;;; to call FDEFINITION to do the work. 

(defun symbol-function (symbol)
  (declare (cl:type symbol symbol))
  (fdefinition symbol))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function (SETF SYMBOL-FUNCTION).
;;;
;;; According to the HyperSpec, (SETF SYMBOL-FUNCTION) is just like
;;; (SETF FDEFINITION), except that it only accepts a symbol as its
;;; argument.  It suffices thus to check that the argument is a
;;; symbol, and then to call (SETF FDEFINITION) to do the work.

(defun (setf symbol-function) (new-definition symbol)
  (declare (cl:type function new-definition)
	   (cl:type symbol symbol))
  (setf (fdefinition symbol) new-definition))

