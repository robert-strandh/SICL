(cl:in-package #:sicl-extrinsic-hir-compiler)

(loop for symbol being each present-symbol in '#:cleavir-primop
      do (setf (sicl-genv:special-operator symbol *environment*) t))
