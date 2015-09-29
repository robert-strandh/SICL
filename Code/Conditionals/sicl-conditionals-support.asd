;;;; Copyright (c) 2008 - 2015
;;;;
;;;;     Robert Strandh (robert.strandh@gmail.com)
;;;;
;;;; all rights reserved. 
;;;;
;;;; Permission is hereby granted to use this software for any 
;;;; purpose, including using, modifying, and redistributing it.
;;;;
;;;; The software is provided "as-is" with no warranty.  The user of
;;;; this software assumes any responsibility of the consequences. 

;;;; This file is part of the conditionals module of the SICL project.
;;;; See the file SICL.text for a description of the project. 
;;;; See the file conditionals.text for a description of the module.

(cl:in-package #:asdf-user)

(defsystem :sicl-conditionals-support
  :depends-on (:cleavir-internationalization
	       :cleavir-code-utilities)
  :serial t
  :components
  ((:file "packages")
   (:file "support")
   (:file "conditions")
   (:file "condition-reporters-en")
   (:file "docstrings-en")))
