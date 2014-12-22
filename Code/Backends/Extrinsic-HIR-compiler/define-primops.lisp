(cl:in-package #:sicl-extrinsic-hir-compiler)

(loop for symbol being each present-symbol in '#:cleavir-primop
      do (setf (sicl-env:special-operator symbol *environment*) t))
