(cl:in-package asdf-user)

(defsystem #:sicl-stream-test
  :depends-on (:cleavir-code-utilities)
  :serial t
  :components
  ((:file "packages") . 
   #.(let* ((system-name '#:sicl-stream)
	    (system (asdf:find-system system-name))
	    (source-files (asdf:component-children system))
	    (names (mapcar #'asdf:component-name source-files))
	    (but-package (remove "packages" names :test #'equal)))
       (mapcar (lambda (name)
		 `(:file ,(concatenate 'string "../" name)))
	       but-package))))
