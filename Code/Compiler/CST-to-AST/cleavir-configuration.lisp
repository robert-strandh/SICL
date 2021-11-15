(cl:in-package #:sicl-cst-to-ast)

(defmethod cleavir-cst-to-ast:trivial-constant-p
    ((client sicl-client:sicl) object)
  (or (typep object '(integer #.(- (expt 2 62)) #.(1- (expt 2 62))))
      (characterp object)
      (stringp object)
      (and (symbolp object)
           (let ((package (symbol-package object)))
             (or (eq package (find-package '#:common-lisp))
                 (eq package (find-package '#:keyword)))))))
