(cl:in-package #:sicl-hir-evaluator-test)

(defparameter *client*
  (make-instance 'trucler-reference:client))

(defparameter *environment*
  (make-environment))

(defparameter *primop-translations*
  '((cleavir-primop:car car)
    (cleavir-primop:cdr cdr)
    (cleavir-primop:rplaca rplaca)
    (cleavir-primop:rplacd rplacd)
    (cleavir-primop:eq eq)
    (cleavir-primop:multiple-value-call multiple-value-call)))

(defmacro with-primops (&body body)
  `(macrolet ,(loop for (primop translation) in *primop-translations*
                    collect
                    `(,primop (&rest args) `(,',translation ,@args)))
     ,@body))

(defun test (form)
  (let ((v1 (multiple-value-list
             (eval `(with-primops ,form))))
        (v2 (multiple-value-list
             (sicl-hir-evaluator:cst-eval
              *client*
              (cst:cst-from-expression form)
              *environment*))))
    (assert (= (length v1) (length v2)))
    (loop for obj1 in v1
          for obj2 in v2
          do (assert (equal obj1 obj2)))))
