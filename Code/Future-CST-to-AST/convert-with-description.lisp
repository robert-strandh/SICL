(cl:in-package #:sicl-expression-to-ast)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting a symbol that has a definition as a symbol macro.

(defmethod convert-with-description
    (client
     cooked-form
     (description trucler:symbol-macro-description)
     environment)
  (let* ((expansion (trucler:expansion description))
         (expander (symbol-macro-expander expansion))
         (raw-expanded-form
           (expand-macro expander cooked-form environment))
         (cooked-expanded-form
           (c:reconstruct raw-expanded-form cooked-form)))
    (setf (c:origin cooked-expanded-form) (c:origin cooked-form))
    (with-preserved-toplevel-ness
      (convert client cooked-expanded-form environment))))
