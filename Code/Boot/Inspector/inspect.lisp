(cl:in-package #:sicl-boot-inspector)

(defvar *boot*)

(defun inspect (boot)
  (let ((*boot* boot))
    (clouseau:inspect boot)))
