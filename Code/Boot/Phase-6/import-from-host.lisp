(cl:in-package #:sicl-boot-phase-6)

(defun import-from-host (boot)
  (with-accessors ((e5 sicl-boot:e5)
                   (e6 sicl-boot:e6)
                   (e7 sicl-boot:e7))
      boot
    (import-functions-from-host
     '(listp sort every
       mapc 1+ 1- subseq butlast position identity nthcdr equal
       find-if-not mapcar remove-duplicates union reduce count last
       copy-list
       remove-if-not reverse find (setf sicl-genv:constant-variable))
     e5)
    (import-functions-from-host
     '(cleavir-code-utilities:parse-generic-function-lambda-list
       cleavir-code-utilities:required
       cons car cdr cadr cddr caddr cdddr (setf cdr) rplacd
       atom eq not member symbolp functionp stringp
       make-list set-difference
       +)
     e6)
    (import-functions-from-host
     '(funcall
       (setf sicl-genv:function-lambda-list)
       (setf sicl-genv:function-type))
     e7)))
