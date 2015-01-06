;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Mon Nov 11 21:40:05 2002
;;;; Contains: Tests for FOR-AS-PACKAGE clause for LOOP

(in-package :sicl-loop-test)

(defpackage "LOOP.CL-TEST.1"
  (:use)
  (:intern "FOO" "BAR" "BAZ")
  (:export "A" "B" "C"))

(defpackage "LOOP.CL-TEST.2"
  (:use "LOOP.CL-TEST.1")
  (:intern "X" "Y" "Z"))

(deftest loop.7.1
  (sort (mapcar #'symbol-name
                (loop for x being the symbols of "LOOP.CL-TEST.1" collect x))
        #'string<)
  ("A" "B" "BAR" "BAZ" "C" "FOO"))

(deftest loop.7.2
  (sort (mapcar #'symbol-name
                (loop for x being each symbol of "LOOP.CL-TEST.1" collect x))
        #'string<)
  ("A" "B" "BAR" "BAZ" "C" "FOO"))

(deftest loop.7.3
  (sort (mapcar #'symbol-name
                (loop for x being the symbol of "LOOP.CL-TEST.1" collect x))
        #'string<)
  ("A" "B" "BAR" "BAZ" "C" "FOO"))

(deftest loop.7.4
  (sort (mapcar #'symbol-name
                (loop for x being each symbols of "LOOP.CL-TEST.1" collect x))
        #'string<)
  ("A" "B" "BAR" "BAZ" "C" "FOO"))

(deftest loop.7.5
  (sort (mapcar #'symbol-name
                (loop for x being the symbols in "LOOP.CL-TEST.1" collect x))
        #'string<)
  ("A" "B" "BAR" "BAZ" "C" "FOO"))

(deftest loop.7.6
  (sort (mapcar #'symbol-name
                (loop for x being each symbol in "LOOP.CL-TEST.1" collect x))
        #'string<)
  ("A" "B" "BAR" "BAZ" "C" "FOO"))

(deftest loop.7.7
  (sort (mapcar #'symbol-name
                (loop for x being the symbol in "LOOP.CL-TEST.1" collect x))
        #'string<)
  ("A" "B" "BAR" "BAZ" "C" "FOO"))

(deftest loop.7.8
  (sort (mapcar #'symbol-name
                (loop for x being each symbols in "LOOP.CL-TEST.1" collect x))
        #'string<)
  ("A" "B" "BAR" "BAZ" "C" "FOO"))

(deftest loop.7.9
  (sort (mapcar #'symbol-name
                (loop for x being the external-symbols of "LOOP.CL-TEST.1" collect x))
        #'string<)
  ("A" "B" "C"))

(deftest loop.7.10
  (sort (mapcar #'symbol-name
                (loop for x being each external-symbol in "LOOP.CL-TEST.1" collect x))
        #'string<)
  ("A" "B" "C"))

(deftest loop.7.11
  (sort (mapcar #'symbol-name
                (loop for x being each external-symbol in (find-package "LOOP.CL-TEST.1") collect x))
        #'string<)
  ("A" "B" "C"))

(deftest loop.7.12
  (sort (mapcar #'symbol-name
                (loop for x being each external-symbol in :LOOP.CL-TEST.1 collect x))
        #'string<)
  ("A" "B" "C"))

;; (deftest loop.7.13
;;   (sort (mapcar #'symbol-name
;;                 (loop for x being the symbols of "LOOP.CL-TEST.2" collect x))
;;         #'string<)
;;   ("A" "B" "C" "X" "Y" "Z"))

;; (deftest loop.7.14
;;   (sort (mapcar #'symbol-name
;;                 (loop for x being the present-symbols of "LOOP.CL-TEST.2" collect x))
;;         #'string<)
;;   ("X" "Y" "Z"))

;; ;;; According to the ANSI CL spec, "If the package for the iteration is not supplied,
;; ;;; the current package is used."  Thse next tests are of the cases that the package
;; ;;; is not supplied in the loop form.

;; (deftest loop.7.15
;;   (let ((*package* (find-package "LOOP.CL-TEST.1")))
;;     (sort (mapcar #'symbol-name (loop for x being each symbol collect x))
;;           #'string<))
;;   ("A" "B" "BAR" "BAZ" "C" "FOO"))

;; (deftest loop.7.16
;;   (let ((*package* (find-package "LOOP.CL-TEST.1")))
;;     (sort (mapcar #'symbol-name (loop for x being each external-symbol collect x))
;;           #'string<))
;;   ("A" "B" "C"))

;; (deftest loop.7.17
;;   (let ((*package* (find-package "LOOP.CL-TEST.2")))
;;     (sort (mapcar #'symbol-name (loop for x being each present-symbol collect x))
;;           #'string<))
;;   ("X" "Y" "Z"))

;; ;;; Cases where the package doesn't exist.  According to the standard,
;; ;;; (section 6.1.2.1.7), this should cause a pacakge-error.

;; (deftest loop.7.18
;;   (let ()
;;     (ignore-errors (delete-package "LOOP.MISSING.PACKAGE"))
;;     (signals-error
;;      (loop for x being each symbol of "LOOP.MISSING.PACKAGE" collect x)
;;      package-error))
;;   t)

;; (deftest loop.7.19
;;   (let ()
;;     (ignore-errors (delete-package "LOOP.MISSING.PACKAGE"))
;;     (signals-error
;;      (loop for x being each present-symbol of "LOOP.MISSING.PACKAGE"
;;            collect x)
;;      package-error))
;;   t)

;; (deftest loop.7.20
;;   (let ()
;;     (ignore-errors (delete-package "LOOP.MISSING.PACKAGE"))
;;     (signals-error
;;      (loop for x being each external-symbol of "LOOP.MISSING.PACKAGE"
;;            collect x)
;;      package-error))
;;   t)

;; ;;; NIL d-var-specs

;; (deftest loop.7.21
;;   (loop for nil being the symbols of "LOOP.CL-TEST.1" count t)
;;   6)

;; (deftest loop.7.22
;;   (loop for nil being the external-symbols of "LOOP.CL-TEST.1" count t)
;;   3)

;; (deftest loop.7.23
;;   (loop for nil being the present-symbols of "LOOP.CL-TEST.2" count t)
;;   3)

;; ;;; Type specs

;; (deftest loop.7.24
;;   (loop for x t being the symbols of "LOOP.CL-TEST.1" count x)
;;   6)

;; (deftest loop.7.25
;;   (loop for x t being the external-symbols of "LOOP.CL-TEST.1" count x)
;;   3)

;; (deftest loop.7.26
;;   (loop for x t being the present-symbols of "LOOP.CL-TEST.2" count x)
;;   3)

;; (deftest loop.7.27
;;   (loop for x of-type symbol being the symbols of "LOOP.CL-TEST.1" count x)
;;   6)

;; (deftest loop.7.28
;;   (loop for x of-type symbol being the external-symbols of "LOOP.CL-TEST.1" count x)
;;   3)

;; (deftest loop.7.29
;;   (loop for x of-type symbol being the present-symbols of "LOOP.CL-TEST.2" count x)
;;   3)

;; ;;; Tests of the 'as' form

;; (deftest loop.7.30
;;   (sort (mapcar #'symbol-name
;;                 (loop as x being the symbols of "LOOP.CL-TEST.1" collect x))
;;         #'string<)
;;   ("A" "B" "BAR" "BAZ" "C" "FOO"))

;; (deftest loop.7.31
;;   (sort (mapcar #'symbol-name
;;                 (loop as x being each symbol of "LOOP.CL-TEST.1" collect x))
;;         #'string<)
;;   ("A" "B" "BAR" "BAZ" "C" "FOO"))

;; (deftest loop.7.32
;;   (sort (mapcar #'symbol-name
;;                 (loop as x being the symbol of "LOOP.CL-TEST.1" collect x))
;;         #'string<)
;;   ("A" "B" "BAR" "BAZ" "C" "FOO"))

;; ;;; Test that explicit calls to macroexpand in subforms
;; ;;; are done in the correct environment

;; (deftest loop.7.33
;;   (macrolet
;;    ((%m (z) z))
;;    (sort (mapcar #'symbol-name
;;                  (loop for x being the symbols of
;;                        (expand-in-current-env (%m "LOOP.CL-TEST.1"))
;;                        collect x))
;;          #'string<))
;;   ("A" "B" "BAR" "BAZ" "C" "FOO"))

;; (deftest loop.7.34
;;   (macrolet
;;    ((%m (z) z))
;;    (sort (mapcar #'symbol-name
;;                  (loop for x being the external-symbols of
;;                        (expand-in-current-env (%m "LOOP.CL-TEST.1"))
;;                        collect x))
;;          #'string<))
;;   ("A" "B" "C"))

;; (deftest loop.7.35
;;   (macrolet
;;    ((%m (z) z))
;;    (sort (mapcar #'symbol-name
;;                  (loop for x being the present-symbols of
;;                        (expand-in-current-env (%m "LOOP.CL-TEST.2"))
;;                        collect x))
;;          #'string<))
;;   ("X" "Y" "Z"))
