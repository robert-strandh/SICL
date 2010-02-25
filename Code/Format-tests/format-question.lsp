;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sun Oct 17 20:08:18 2004
;;;; Contains: Tests of the ~? and ~@? format directives

(in-package :cl-test)

(compile-and-load "printer-aux.lsp")

(def-format-test format.?.1
  "~?" ("" nil) "")

(def-format-test format.?.2
  "~?" ("~A" '(1)) "1")

(def-format-test format.?.3
  "~?" ("" '(1)) "")

(def-format-test format.?.4
  "~? ~A" ("" '(1) 2) " 2")

(def-format-test format.?.5
  "a~?z" ("b~?y" '("c~?x" ("~A" (1)))) "abc1xyz")

;;; Tests of ~@?

(def-format-test format.@?.1
  "~@?" ("") "")

(def-format-test format.@?.2
  "~@?" ("~A" 1) "1")

(def-format-test format.@?.3
  "~@? ~A" ("<~A>" 1 2) "<1> 2")

(def-format-test format.@?.4
  "a~@?z" ("b~@?y" "c~@?x" "~A" 1) "abc1xyz")

(def-format-test format.@?.5
  "~{~A~@?~A~}" ('(1 "~4*" 2 3 4 5 6)) "16")
