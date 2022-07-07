(cl:in-package #:sicl-boot-phase-1)

(defun import-number-functions (environment)
  (import-functions-from-host
   '(=
     ;; ODDP is used at compile time in some macro expanders
     oddp)
   environment))

(defun import-cons-functions (environment)
  (import-functions-from-host
   '(;; SECOND is used in many places
     second
     ;; THIRD is used by the expansion of DEFMETHOD
     third
     ;; FOURTH is used in some macro expanders
     fourth
     ;; CAR, CDR, FIRST, and REST are used in many places.
     car cdr first rest
     ;; CDDR is used in many macro expanders.
     cddr
     ;; CONSP is used in many macro expanders.
     consp
     ;; ATOM is used in many macro expanders.
     atom
     ;; NULL is used in many macro expanders.
     null
     ;; ENDP is used in the expansion of LOOP.
     endp
     ;; CONS is used in many macro expanders.
     cons
     ;; LIST is used in many macro expanders.
     list
     ;; APPEND is used at compile time in some macro expanders
     append
     member
     mapcar
     getf
     assoc
     last
     butlast)
   environment))

(defun import-conditionals-support (environment)
  (import-functions-from-host
   '(sicl-conditionals:or-expander
     sicl-conditionals:and-expander
     sicl-conditionals:cond-expander
     sicl-conditionals:case-expander
     sicl-conditionals:ecase-expander
     sicl-conditionals:ccase-expander
     sicl-conditionals:typecase-expander
     sicl-conditionals:etypecase-expander
     sicl-conditionals:ctypecase-expander)
   environment))

(defun import-code-utilities (environment)
  (import-functions-from-host
   '(cleavir-code-utilities:parse-macro
     cleavir-code-utilities:separate-function-body
     cleavir-code-utilities:list-structure)
   environment))

(defun import-trucler-functions (environment)
  (import-functions-from-host
   '(trucler:symbol-macro-expansion
     trucler:macro-function)
   environment))

(defun import-misc (environment)
  (import-functions-from-host
   '(error typep coerce)
   environment))

(defun import-sequence-functions (environment)
  (import-functions-from-host
   '(;; REVERSE is used in macroexpanders such as ROTATEF.
     reverse
     ;; LENGTH is used in macroexpanders such as SETF.
     length)
   environment))

(defun import-from-host (environment)
  (import-number-functions environment)
  (import-cons-functions environment)
  (import-conditionals-support environment)
  (import-code-utilities environment)
  (import-trucler-functions environment)
  (import-misc environment)
  (import-sequence-functions environment))

(defun define-defgeneric-expander (client environment)
  (setf (env:fdefinition client environment 'sicl-clos:defgeneric-expander)
        (lambda (name lambda-list options-and-methods environment)
          (declare (ignore environment))
          (assert (or (null options-and-methods)
                      (and (null (cdr options-and-methods))
                           (eq (caar options-and-methods)
                               :argument-precedence-order))))
          `(ensure-generic-function
            ',name
            :lambda-list ',lambda-list
            ,@(if (null options-and-methods)
                  '()
                  `(:argument-precedence-order ',(cdar options-and-methods)))
            :environment (env:global-environment)))))
