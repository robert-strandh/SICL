(cl:in-package #:sicl-boot)

(defmethod cleavir-cst-to-ast:trivial-constant-p
    ((client client) object)
  (typep object '(integer #.(- (expt 2 62)) #.(1- (expt 2 62)))))
