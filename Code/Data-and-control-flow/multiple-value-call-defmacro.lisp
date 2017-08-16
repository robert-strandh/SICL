(cl:in-package #:sicl-data-and-control-flow)

(defmacro multiple-value-call (function &rest arguments)
  `(cleavir-primop:multiple-value-call (coerce ,function 'function)
     ,@arguments))
