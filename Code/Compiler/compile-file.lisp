(in-package #:sicl-compiler)

(defun compile-file (input-file &key
				(output-file nil output-file-p)
				(verbose *compile-verbose*)
				(print *compile-print*)
				(external-format :default))
  
  (with-open-file (stream input-file
			  :direction :input
			  :external-format external-format)
    (let* ((*compile-file-pathname* (merge-pathnames input-file))
	   (*compile-file-truename* (truename *compile-file-pathname*))
	   (*package* *package*)
	   (*readtable* *readtable*)
	   (sicl-compiler-phase-1:*compile-file* t))
      (sicl-ast:make-progn-ast 
       (loop with eof-value = (list nil)
	     for form = (read stream nil eof-value)
	     until (eq form eof-value)
	     for result = (sicl-compiler-phase-1:convert-initial form)
	     unless (null result)
	       collect result)))))

