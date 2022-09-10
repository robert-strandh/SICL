(cl:in-package #:sicl-iteration)

(defmacro do (variable-clauses end-test &body body)
  (do-dostar-expander 'let 'psetq variable-clauses end-test body))

(defmacro do* (variable-clauses end-test &body body)
  (do-dostar-expander 'let* 'setq variable-clauses end-test body))
