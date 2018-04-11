(cl:in-package #:sicl-extrinsic-hir-compiler)

(setf (sicl-genv:fdefinition 'sicl-loop::expand-body *environment*)
      (fdefinition 'sicl-loop::expand-body))
