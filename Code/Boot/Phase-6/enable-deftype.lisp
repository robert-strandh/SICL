(cl:in-package #:sicl-boot-phase-6)

(defun enable-deftype (e5)
  (import-functions-from-host
   '(cleavir-code-utilities:parse-deftype)
   e5)
  (setf (env:fdefinition (env:client e5) e5 '(setf sicl-type::type-expander))
        (lambda (expander name)
          (setf (env:type-expander (env:client e5) e5 name)
                expander)))
  (load-source-file "Types/deftype-defmacro.lisp" e5))
