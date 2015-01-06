;;;; Copyright (c) 2014
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

(cl:in-package #:sicl-loop)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Clause FOR-AS-PACKAGE

(defclass for-as-package (for-as-subclause)
  ((%package-form :initarg :package-form :reader package-form)))

(defclass for-as-package-symbols (for-as-package) ())

(defclass for-as-package-present-symbols (for-as-package) ())

(defclass for-as-package-external-symbols (for-as-package) ())

