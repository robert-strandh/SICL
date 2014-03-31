;;;; Copyright (c) 2008, 2009, 2010, 2011, 2012, 2013, 2014
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

(in-package #:cl-user)

(asdf:defsystem :sicl-loop
  :depends-on ("sicl-additional-conditions")
  :serial t
  :components
  ((:file "packages")
   (:file "conditions")
   (:file "condition-reporters-en")
   (:file "utilities")
   (:file "clause")
   (:file "combinatory-parsing")
   (:file "parse-common")
   (:file "type-spec")
   (:file "initial-clause")
   (:file "final-clause")
   (:file "with-clause")
   (:file "return-clause")
   (:file "do-clause")
   (:file "collect-clause")
   (:file "append-clause")
   (:file "nconc-clause")
   (:file "count-clause")
   (:file "maximize-clause")
   (:file "minimize-clause")
   (:file "conditional-clause")
   (:file "while-until-clauses")
   (:file "repeat-clause")
   (:file "analysis")))
