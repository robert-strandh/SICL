(cl:in-package #:sicl-data-and-control-flow)

(defmacro throw (tag result-form)
  `(sicl-run-time:throw ,tag (multiple-value-list ,result-form)))
