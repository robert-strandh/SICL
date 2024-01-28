(cl:in-package #:sicl-clos)

;;; For the specification of this function, see
;;; http://metamodular.com/CLOS-MOP/ensure-generic-function.html
;;;
;;; This function is also specified in the CLHS.
;;; http://www.lispworks.com/documentation/HyperSpec/Body/f_ensure.htm#ensure-generic-function

(defun ensure-generic-function
    (name
     &rest keys
     &key
     &allow-other-keys)
  (let ((generic-function
          (if (fboundp name)
              (if (consp name)
                  ;; Then it must be a function and not a macro or a
                  ;; special operator.  It could, of course be an
                  ;; ordinary function.  In that case, we signal an
                  ;; error in the method specialized to FUNCTION on
                  ;; ENSURE-GENERIC-FUNCTION-USING-CLASS.
                  (fdefinition name)
                  ;; If it is not a CONS, then it might be a macro or
                  ;; a special operator, so we must check for that
                  ;; possibility.
                  (if (and (null (macro-function name))
                           (not (special-operator-p name)))
                      ;; Again, it must be a function.
                      (fdefinition name)
                      ;; It is fbound, but it is either a macro or a
                      ;; special operator.  The standard says that we
                      ;; must signal a type error then.
                      (error 'type-error
                             :expected-type 'generic-function
                             :datum (fdefinition name))))
              ;; It is not fbound, so we return NIL so that we can
              ;; pass it to ENSURE-GENERIC-FUNCTION-USING-CLASS.
              nil)))
    (apply #'ensure-generic-function-using-class
           generic-function name keys)))
