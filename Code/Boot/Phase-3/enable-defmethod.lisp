(cl:in-package #:sicl-boot-phase-3)

;;; We already know how to execute a DEFGENERIC form in E3.  Now we
;;; need to know how to use DEFMETHOD to define methods on the generic
;;; functions we create with DEFGENERIC.  That is the purpose of this
;;; function.

(defun define-find-specializer-class-t-in-e3 (e2 e3)
  (setf (sicl-genv:fdefinition 'sicl-clos::find-specializer-class-t e3)
        (lambda () (sicl-genv:find-class 't e2))))

(defun define-make-specializer-in-e2 (e2)
  (setf (sicl-genv:fdefinition 'sicl-clos::make-specializer e2)
        (lambda (specializer)
          (cond ((eq specializer 't)
                 (find-class 't))
                ((symbolp specializer)
                 (sicl-genv:find-class specializer e2))
                ((sicl-genv:typep specializer 'specializer e2)
                 specializer)
                (t
                 (error "Specializer must be symbol or specializer metaobject: ~s"
                        specializer))))))

(defun enable-defmethod (boot)
  (with-accessors ((e2 sicl-boot:e2)
                   (e3 sicl-boot:e3)) boot
    (define-find-specializer-class-t-in-e3 e2 e3)
    (setf (sicl-genv:fdefinition 'ensure-generic-function e2)
          (sicl-genv:fdefinition 'ensure-generic-function e3))
    (import-functions-from-host
     '(mapcar
       subseq
       elt
       position-if
       sicl-genv:find-class
       ;; TYPEP is used by ENSURE-METHOD to check that, if a symbol
       ;; was not given, then an instance of SPECIALIZER was.
       sicl-genv:typep
       sicl-genv:fboundp
       sicl-genv:fdefinition
       cleavir-code-utilities:separate-function-body
       cleavir-code-utilities:required
       cleavir-code-utilities:parse-specialized-lambda-list)
     e2)
    (setf (sicl-genv:fdefinition 'sicl-clos:class-prototype e2)
          #'closer-mop:class-prototype)
    (setf (sicl-genv:fdefinition 'sicl-clos:generic-function-method-class e2)
          #'closer-mop:generic-function-method-class)
    ;; 1+ is called by PARSE-METHOD to obtain an interval designator
    ;; for SUBSEQ in order to parse the method body.
    (import-functions-from-host '(1+ add-method copy-list) e2)
    (flet ((ld (name environment)
             (load-fasl name environment)))
      (import-functions-from-host
       '((setf sicl-genv:fdefinition)
         (setf sicl-genv:function-type)
         (setf sicl-genv:function-lambda-list))
       e2)
      (import-function-from-host 'append e2)
      (ld "CLOS/make-method-for-generic-function.fasl" e2)
      (setf (sicl-genv:fdefinition 'sicl-clos::function-of-method e3)
            (sicl-genv:fdefinition 'sicl-clos::method-function e3))
      (setf (sicl-genv:fdefinition 'sicl-clos::add-method-to-generic-function e2)
            (sicl-genv:fdefinition 'sicl-clos::add-method e2))
      (define-make-specializer-in-e2 e2)
      (sicl-boot:with-straddled-function-definition
          (sicl-clos::ensure-method e2 e3)
        (ld "CLOS/ensure-method.fasl" e2)))))
