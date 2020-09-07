(cl:in-package #:sicl-boot-phase-4)

(defun import-from-host (boot)
  (with-accessors ((e3 sicl-boot:e3)
                   (e4 sicl-boot:e4)
                   (e5 sicl-boot:e5))
      boot
    (sicl-hir-evaluator:fill-environment e5)
    (sicl-boot:define-cleavir-primops e3)
    (import-functions-from-host
     '((setf sicl-genv:constant-variable)
       sicl-conditionals:cond-expander
       sicl-conditionals:and-expander
       listp sort every
       mapc 1+ 1- subseq butlast position identity nthcdr equal
       find-if-not union reduce count
       gensym values rest second error
       get-properties
       atom cddr (setf cdr) < <=
       not
       remove-if-not reverse find)
     e3)
    (import-functions-from-host
     '(cleavir-code-utilities:parse-generic-function-lambda-list
       cleavir-code-utilities:required
       cleavir-code-utilities:parse-specialized-lambda-list
       cleavir-code-utilities:separate-function-body
       cleavir-code-utilities:proper-list-p
       (setf sicl-genv:type-expander)
       (setf sicl-genv:find-class)
       (setf sicl-genv:special-variable)
       sicl-genv:find-class
       (setf sicl-genv:fdefinition)
       sicl-genv:macro-function
       (setf sicl-genv:macro-function)
       sicl-genv:get-setf-expansion
       sicl-genv:fboundp
       sicl-genv:find-method-combination-template
       (setf sicl-genv:find-method-combination-template)
       sicl-method-combination::define-method-combination-expander
       sicl-clos::add-path
       sicl-clos::extract-transition-information
       sicl-clos::make-automaton
       sicl-loop::list-car sicl-loop::list-cdr
       cons car cdr cadr cddr caddr cdddr (setf cdr) rplacd first
       nth assoc reverse
       copy-list list list* append length
       null atom eq not member symbolp functionp stringp consp
       make-list set-difference remove find find-if adjoin
       eql equal set-exclusive-or
       sort mapcar subseq elt position position-if
       apply endp coerce
       + floor = /= zerop
       reduce last remove-duplicates)
     e4)
    (import-functions-from-host
     '(funcall
       (setf sicl-genv:function-lambda-list)
       (setf sicl-genv:function-type))
     e5)))
