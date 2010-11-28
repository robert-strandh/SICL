(in-package #:cl-user)

;;;; Copyright (c) 2008, 2009, 2010
;;;;
;;;;     Robert Strandh (strandh@labri.fr)
;;;;
;;;; all rights reserved. 
;;;;
;;;; Permission is hereby granted to use this software for any 
;;;; purpose, including using, modifying, and redistributing it.
;;;;
;;;; The software is provided "as-is" with no warranty.  The user of
;;;; this software assumes any responsibility of the consequences. 

;;;; This file is part of the cons-high module of the SICL project.
;;;; See the file SICL.text for a description of the project. 
;;;; See the file cons-high.text for a description of the module.

(asdf:defsystem :sicl-cons-high
    :components
  ((:file "packages" :depends-on ())
   (:file "cons-high" :depends-on ("packages"))
   (:file "condition-reporters-en" :depends-on ("packages" "cons-high"))
   (:file "docstrings-en" :depends-on ("packages"))))

(asdf:defsystem :sicl-cons-high-test
  :depends-on (:sicl-cons-high)
  :components
  ((:file "test" :depends-on ())))
