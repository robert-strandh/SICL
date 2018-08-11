(cl:in-package #:sicl-iteration)

(defmacro do (variable-clauses end-test &body body)
  (do-dostar-expander 'do 'let 'psetq variable-clauses end-test body))

(defmacro do* (variable-clauses end-test &body body)
  (do-dostar-expander 'do* 'let* 'setq variable-clauses end-test body))
