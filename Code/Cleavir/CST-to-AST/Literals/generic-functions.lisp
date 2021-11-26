(cl:in-package #:cleavir-literals)

(defgeneric make-load-form-using-client (client object environment))

(defgeneric load-time-literal (client object environment))
