(cl:in-package #:sicl-extrinsic-hir-compiler)

(setf (sicl-env:fdefinition 'sicl-loop::expand-body *environment*)
      (fdefinition 'sicl-loop::expand-body))
