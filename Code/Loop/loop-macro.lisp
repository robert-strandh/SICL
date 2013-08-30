;;;; Copyright (c) 2008, 2009, 2010, 2011, 2012, 2013
;;;;
;;;;     Robert Strandh (robert.strandh@gmail.com)
;;;; 
;;;; Copyright (c) 2010, 2011
;;;;
;;;;     Matthieu Villeneuve (matthieu.villeneuve@gmail.com)
;;;;
;;;; all rights reserved. 
;;;;
;;;; Permission is hereby granted to use this software for any 
;;;; purpose, including using, modifying, and redistributing it.
;;;;
;;;; The software is provided "as-is" with no warranty.  The user of
;;;; this software assumes any responsibility of the consequences. 

;;;; This file is part of the loop module of the SICL project.
;;;; See the file SICL.text for a description of the project. 

;;;; This file can not be naively compiled with the native file
;;;; compiler, whether in SICL or any other Common Lisp
;;;; implementation.  The reason for that is that you would then be
;;;; redefining a macro (LOOP) that is defined by the system, which is
;;;; in violation of the HyperSpec.
;;;;
;;;; In SICL, we compile it at two different moments of the build
;;;; process.  The first time, it is compiled with the cross compiler.
;;;; Then a macro named LOOP is created in the compilation environment
;;;; managed by the cross compiler, and a SICL FASL file is created.
;;;; The second time, it is compiled with the native file compiler,
;;;; but then we make sure that it is OK to redefine the existing LOOP
;;;; macro.

(cl:in-package #:sicl-loop)

(defmacro loop (&rest forms)
  (let* ((body (parse-loop-body forms))
         (block-name (if (typep (car (clauses body)) 'name-clause)
			 (name (car (clauses body)))
			 nil)))
    (verify-clause-order body)
    (initialize-accumulation (clauses body) body)
    `(block ,block-name
       ,(generate-bindings-and-body (clauses body) body))))

