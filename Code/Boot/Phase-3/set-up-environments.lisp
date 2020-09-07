(cl:in-package #:sicl-boot-phase-3)

(defun set-up-environments (boot)
  (with-accessors ((e2 sicl-boot:e2)
                   (e3 sicl-boot:e3)
                   (e4 sicl-boot:e4))
      boot
    (sicl-hir-evaluator:fill-environment e4)
    (import-functions-from-host
     '(cleavir-code-utilities:separate-function-body
       cleavir-code-utilities:required
       cleavir-code-utilities:parse-specialized-lambda-list
       cleavir-code-utilities:proper-list-p
       sicl-genv:fboundp
       sicl-genv:fdefinition
       (setf sicl-genv:fdefinition)
       (setf sicl-genv:function-lambda-list)
       (setf sicl-genv:function-type)
       car cdr rplacd endp null consp atom eq not (setf cdr) apply
       append
       mapcar
       subseq
       elt
       position-if
       sicl-genv:find-class
       1+ add-method copy-list
       +)
     e2)
    (import-functions-from-host
     '(cleavir-code-utilities:parse-generic-function-lambda-list
       cleavir-code-utilities:required
       cleavir-code-utilities:proper-list-p
       (setf sicl-genv:special-variable)
       sicl-genv:find-class
       (setf sicl-genv:fdefinition)
       (setf sicl-genv:macro-function)
       sicl-genv:macro-function
       sicl-genv:get-setf-expansion
       sicl-genv:find-method-combination-template
       (setf sicl-genv:find-method-combination-template)
       sicl-loop::list-car sicl-loop::list-cdr
       cons car cdr cadr cddr caddr cdddr (setf cdr) rplacd first
       reverse
       rplacd + member not symbolp functionp stringp
       consp first
       shared-initialize
       make-list set-difference set-exclusive-or
       remove find-if adjoin
       sicl-clos::make-automaton
       sicl-clos::add-path
       sicl-clos::extract-transition-information
       floor = /= zerop
       find subseq list*
       position sort mapcar eql equal
       nth assoc
       apply endp cons eq coerce
       list null append length cdr
       reduce remove-duplicates copy-list last)
     e3)
    (import-functions-from-host
     '(funcall
       (setf sicl-genv:function-lambda-list)
       (setf sicl-genv:function-type))
     e4)))
