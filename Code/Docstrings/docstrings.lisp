(setf (structured-documentation '* 'variable)
      '[sicl-documentation::section
	 :title 
	 [sicl-documentation::paragraph
           :contents
	   ("Variable *")]
	 :contents 
	 ([sicl-documentation::section
	    :title
	    [sicl-documentation::paragraph
	      :contents
	      ("Value type")]
	    :contents
	    ([sicl-documentation::paragraph
	       :contents
	       ("An object")])]
	  [sicl-documentation::section
	    :title
	    [sicl-documentation::paragraph
	      :contents
	      ("Initial value")]
	    :contents
	    ([sicl-documentation::paragraph
	       :contents
	       ("Implementation dependent")])]
	  [sicl-documentation::section
	    :title
	    [sicl-documentation::paragraph
	      :contents
	      ("Description")]
	    :contents
	    ([sicl-documentation::paragraph
	       :contents
	       ("The most recent"
		[sicl-documentation::glossary-entry
		  :entry-key "primary key"
		  :contents ("primary value")]
                "printed")])])])

(setf (structured-documentation '** 'variable)
      '[sicl-documentation::section
	 :title
	 [sicl-documentation::paragraph
           :contents
	   ("Variable **")]
	 :contents 
	 ([sicl-documentation::section
	    :title
	    [sicl-documentation::paragraph
	      :contents
	      ("Value type")]
	    :contents
	    ([sicl-documentation::paragraph
	       :contents
	       ("An object")])]
	  [sicl-documentation::section
	    :title
	    [sicl-documentation::paragraph
	      :contents
	      ("Initial value")]
	    :contents
	    ([sicl-documentation::paragraph
	       :contents
	       ("Implementation dependent")])]
	  [sicl-documentation::section
	    :title
	    [sicl-documentation::paragraph
	      :contents
	      ("Description")]
	    :contents
	    ([sicl-documentation::paragraph
	       :contents
	       ("The previous value of"
		[sicl-documentation::documentation
		  :documentation-arguments (* variable)
		  :contents ("*")])])])])

(setf (documentation '*** 'variable)
      '[sicl-documentation::section
	 :title
	 [sicl-documentation::paragraph
           :contents
	   ("Variable ***")]
	 :contents 
	 ([sicl-documentation::section
	    :title
	    [sicl-documentation::paragraph
	      :contents
	      ("Value type")]
	    :contents
	    ([sicl-documentation::paragraph
	       :contents
	       ("An object")])]
	  [sicl-documentation::section
	    :title
	    [sicl-documentation::paragraph
	      :contents
	      ("Initial value")]
	    :contents
	    ([sicl-documentation::paragraph
	       :contents
	       ("Implementation dependent")])]
	  [sicl-documentation::section
	    :title
	    [sicl-documentation::paragraph
	      :contents
	      ("Description")]
	    :contents
	    ([sicl-documentation::paragraph
	       :contents
	       ("The previous value of"
		[sicl-documentation::documentation
		  :documentation-arguments (** variable)
		  :contents ("**")])])])])

(setf (documentation '*break-on-signals* 'variable)
      '[sicl-documentation::section
	 :title
	 [sicl-documentation::paragraph
           :contents
	   ("Variable *break-on-signals*")]
	 :contents 
	 ([sicl-documentation::section
	    :title
	    [sicl-documentation::paragraph
	      :contents
	      ("Value type")]
	    :contents
	    ([sicl-documentation::paragraph
	       :contents
	       ("A type specifier")])]
	  [sicl-documentation::section
	    :title
	    [sicl-documentation::paragraph
	      :contents
	      ("Initial value")]
	    :contents
	    ([sicl-documentation::paragraph
	       :contents
	       ("nil")])]
	  [sicl-documentation::section
	    :title
	    [sicl-documentation::paragraph
	      :contents
	      ("Description")]
	    :contents
	    ([sicl-documentation::paragraph
	       :contents
	       ("When a condition is signaled and that condition"
		"is of the type that is the value of this variable"
		"then the debugger is entered before the condition"
		"is signaled")])])])

(setf (documentation '*compile-file-pathname* 'variable)
      '[sicl-documentation::section
	 :title
	 [sicl-documentation::paragraph
           :contents
	   ("Variable *compile-file-pathname*")]
	 :contents 
	 ([sicl-documentation::section
	    :title
	    [sicl-documentation::paragraph
	      :contents
	      ("Value type")]
	    :contents
	    ([sicl-documentation::paragraph
	       :contents
	       ("A pathname or nil")])]
	  [sicl-documentation::section
	    :title
	    [sicl-documentation::paragraph
	      :contents
	      ("Initial value")]
	    :contents
	    ([sicl-documentation::paragraph
	       :contents
	       ("nil")])]
	  [sicl-documentation::section
	    :title
	    [sicl-documentation::paragraph
	      :contents
	      ("Description")]
	    :contents
	    ([sicl-documentation::paragraph
	       :contents
	       ("This variable is bound by"
		[sicl-documentation::documentation
		  :documentation-arguments (compile-file function)
		  :contents ("compile-file")]
		"to the pathname of the file being compiled"
		"merged with the defaults")])])])

(setf (documentation '*compile-file-truename* 'variable)
      '[sicl-documentation::section
	 :title
	 [sicl-documentation::paragraph
           :contents
	   ("Variable *compile-file-truename*")]
	 :contents 
	 ([sicl-documentation::section
	    :title
	    [sicl-documentation::paragraph
	      :contents
	      ("Value type")]
	    :contents
	    ([sicl-documentation::paragraph
	       :contents
	       ("A physical pathname or nil")])]
	  [sicl-documentation::section
	    :title
	    [sicl-documentation::paragraph
	      :contents
	      ("Initial value")]
	    :contents
	    ([sicl-documentation::paragraph
	       :contents
	       ("nil")])]
	  [sicl-documentation::section
	    :title
	    [sicl-documentation::paragraph
	      :contents
	      ("Description")]
	    :contents
	    ([sicl-documentation::paragraph
	       :contents
	       ("This variable is bound by"
		[sicl-documentation::documentation
		  :documentation-arguments (compile-file function)
		  :contents ("compile-file")]
		"to the truename of the file being compiled")])])])

(setf (documentation '*compile-print* 'variable)
      '[sicl-documentation::section
	 :title
	 [sicl-documentation::paragraph
           :contents
	   ("Variable *compile-print*")]
	 :contents 
	 ([sicl-documentation::section
	    :title
	    [sicl-documentation::paragraph
	      :contents
	      ("Value type")]
	    :contents
	    ([sicl-documentation::paragraph
	       :contents
	       ("A generalized boolean")])]
	  [sicl-documentation::section
	    :title
	    [sicl-documentation::paragraph
	      :contents
	      ("Initial value")]
	    :contents
	    ([sicl-documentation::paragraph
	       :contents
	       ("Implementation dependent")])]
	  [sicl-documentation::section
	    :title
	    [sicl-documentation::paragraph
	      :contents
	      ("Description")]
	    :contents
	    ([sicl-documentation::paragraph
	       :contents
	       ("This variable supplies the default value of"
		"the :print keyword argument to"
		[sicl-documentation::documentation
		  :documentation-arguments (compile-file function)
		  :contents ("compile-file")])])])])

(setf (documentation '*compile-verbose* 'variable)
      '[sicl-documentation::section
	 :title
	 [sicl-documentation::paragraph
           :contents
	   ("Variable *compile-verbose*")]
	 :contents 
	 ([sicl-documentation::section
	    :title
	    [sicl-documentation::paragraph
	      :contents
	      ("Value type")]
	    :contents
	    ([sicl-documentation::paragraph
	       :contents
	       ("A generalized boolean")])]
	  [sicl-documentation::section
	    :title
	    [sicl-documentation::paragraph
	      :contents
	      ("Initial value")]
	    :contents
	    ([sicl-documentation::paragraph
	       :contents
	       ("Implementation dependent")])]
	  [sicl-documentation::section
	    :title
	    [sicl-documentation::paragraph
	      :contents
	      ("Description")]
	    :contents
	    ([sicl-documentation::paragraph
	       :contents
	       ("This variable supplies the default value of"
		"the :verbose keyword argument to"
		[sicl-documentation::documentation
		  :documentation-arguments (compile-file function)
		  :contents ("compile-file")])])])])

(setf (documentation '*debug-io* 'variable)
      '[sicl-documentation::section
	 :title
	 [sicl-documentation::paragraph
           :contents
	   ("Variable *debug-io*")]
	 :contents 
	 ([sicl-documentation::section
	    :title
	    [sicl-documentation::paragraph
	      :contents
	      ("Value type")]
	    :contents
	    ([sicl-documentation::paragraph
	       :contents
	       ("A bidirectional stream")])]
	  [sicl-documentation::section
	    :title
	    [sicl-documentation::paragraph
	      :contents
	      ("Initial value")]
	    :contents
	    ([sicl-documentation::paragraph
	       :contents
	       ("The initial value is an open stream.  The stream can be a"
		"generalized synonym stream"
		"but in that case, it is not a synonym stream to"
		"an I/O customization variable")])]
	  [sicl-documentation::section
	    :title
	    [sicl-documentation::paragraph
	      :contents
	      ("Description")]
	    :contents
	    ([sicl-documentation::paragraph
	       :contents
	       ("A stream used for interactive debugging")])])])

(setf (documentation '*error-output* 'variable)
      '[sicl-documentation::section
	 :title
	 [sicl-documentation::paragraph
           :contents
	   ("Variable *error-output*")]
	 :contents 
	 ([sicl-documentation::section
	    :title
	    [sicl-documentation::paragraph
	      :contents
	      ("Value type")]
	    :contents
	    ([sicl-documentation::paragraph
	       :contents
	       ("A output stream")])]
	  [sicl-documentation::section
	    :title
	    [sicl-documentation::paragraph
	      :contents
	      ("Initial value")]
	    :contents
	    ([sicl-documentation::paragraph
	       :contents
	       ("The initial value is an open stream.  The stream can be a"
		"generalized synonym stream"
		"but in that case, it is not a synonym stream to"
		"an I/O customization variable")])]
	  [sicl-documentation::section
	    :title
	    [sicl-documentation::paragraph
	      :contents
	      ("Description")]
	    :contents
	    ([sicl-documentation::paragraph
	       :contents
	       ("A stream used for reporting error messages and warnings")])])])

(setf (documentation '*query-io* 'variable)
      '[sicl-documentation::section
	 :title
	 [sicl-documentation::paragraph
           :contents
	   ("Variable *query-io*")]
	 :contents 
	 ([sicl-documentation::section
	    :title
	    [sicl-documentation::paragraph
	      :contents
	      ("Value type")]
	    :contents
	    ([sicl-documentation::paragraph
	       :contents
	       ("A bidirectional stream")])]
	  [sicl-documentation::section
	    :title
	    [sicl-documentation::paragraph
	      :contents
	      ("Initial value")]
	    :contents
	    ([sicl-documentation::paragraph
	       :contents
	       ("The initial value is an open stream.  The stream can be a"
		"generalized synonym stream"
		"but in that case, it is not a synonym stream to"
		"an I/O customization variable")])]
	  [sicl-documentation::section
	    :title
	    [sicl-documentation::paragraph
	      :contents
	      ("Description")]
	    :contents
	    ([sicl-documentation::paragraph
	       :contents
	       ("A stream used for interacting with the user")])])])

(setf (documentation '*standard-input* 'variable)
      '[sicl-documentation::section
	 :title
	 [sicl-documentation::paragraph
           :contents
	   ("Variable *standard-input*")]
	 :contents 
	 ([sicl-documentation::section
	    :title
	    [sicl-documentation::paragraph
	      :contents
	      ("Value type")]
	    :contents
	    ([sicl-documentation::paragraph
	       :contents
	       ("An input stream")])]
	  [sicl-documentation::section
	    :title
	    [sicl-documentation::paragraph
	      :contents
	      ("Initial value")]
	    :contents
	    ([sicl-documentation::paragraph
	       :contents
	       ("The initial value is an open stream.  The stream can be a"
		"generalized synonym stream"
		"but in that case, it is not a synonym stream to"
		"an I/O customization variable")])]
	  [sicl-documentation::section
	    :title
	    [sicl-documentation::paragraph
	      :contents
	      ("Description")]
	    :contents
	    ([sicl-documentation::paragraph
	       :contents
	       ("This stream is used by many standard operators"
		"that read input as a default value for an"
		"optional stream argument")])])])

(setf (documentation '*standard output* 'variable)
      '[sicl-documentation::section
	 :title
	 [sicl-documentation::paragraph
           :contents
	   ("Variable *standard-output*")]
	 :contents 
	 ([sicl-documentation::section
	    :title
	    [sicl-documentation::paragraph
	      :contents
	      ("Value type")]
	    :contents
	    ([sicl-documentation::paragraph
	       :contents
	       ("An output stream")])]
	  [sicl-documentation::section
	    :title
	    [sicl-documentation::paragraph
	      :contents
	      ("Initial value")]
	    :contents
	    ([sicl-documentation::paragraph
	       :contents
	       ("The initial value is an open stream.  The stream can be a"
		"generalized synonym stream"
		"but in that case, it is not a synonym stream to"
		"an I/O customization variable")])]
	  [sicl-documentation::section
	    :title
	    [sicl-documentation::paragraph
	      :contents
	      ("Description")]
	    :contents
	    ([sicl-documentation::paragraph
	       :contents
	       ("This stream is used by many standard operators"
		"that generate output as a default value for an"
		"optional stream argument")])])])

(setf (documentation '*trace-output* 'variable)
      '[sicl-documentation::section
	 :title
	 [sicl-documentation::paragraph
           :contents
	   ("Variable *trace-output*")]
	 :contents 
	 ([sicl-documentation::section
	    :title
	    [sicl-documentation::paragraph
	      :contents
	      ("Value type")]
	    :contents
	    ([sicl-documentation::paragraph
	       :contents
	       ("An output stream")])]
	  [sicl-documentation::section
	    :title
	    [sicl-documentation::paragraph
	      :contents
	      ("Initial value")]
	    :contents
	    ([sicl-documentation::paragraph
	       :contents
	       ("The initial value is an open stream.  The stream can be a"
		"generalized synonym stream"
		"but in that case, it is not a synonym stream to"
		"an I/O customization variable")])]
	  [sicl-documentation::section
	    :title
	    [sicl-documentation::paragraph
	      :contents
	      ("Description")]
	    :contents
	    ([sicl-documentation::paragraph
	       :contents
	       ("A stream used for printing trace messages"
		"and for reporting information about execution time")])])])

(setf (documentation '*debugger-hook* 'variable)
      '[sicl-documentation::section
	 :title
	 [sicl-documentation::paragraph
           :contents
	   ("Variable *debugger-hook*")]
	 :contents 
	 ([sicl-documentation::section
	    :title
	    [sicl-documentation::paragraph
	      :contents
	      ("Value type")]
	    :contents
	    ([sicl-documentation::paragraph
	       :contents
	       ("Either nil or a function designator")])]
	  [sicl-documentation::section
	    :title
	    [sicl-documentation::paragraph
	      :contents
	      ("Initial value")]
	    :contents
	    ([sicl-documentation::paragraph
	       :contents
	       ("nil")])]
	  [sicl-documentation::section
	    :title
	    [sicl-documentation::paragraph
	      :contents
	      ("Description")]
	    :contents
	    ([sicl-documentation::paragraph
	       :contents
	       ("If the value is not nil, then before the debugger"
		"is entered for whatever reason"
		"the designated function is called with two"
		"arguments, the condition that provoked the"
		"entry into the debugger, and the value of"
		"this variable."
		"Before the designated function is called,"
		"this variable is bound to nil, thus allowing"
		"the designated function to signal a condition"
		"without being called recursively")])])])

(setf (documentation '*default-pathname-defaults* 'variable)
      '[sicl-documentation::section
	 :title
	 [sicl-documentation::paragraph
           :contents
	   ("Variable *default-pathname-defaults*")]
	 :contents 
	 ([sicl-documentation::section
	    :title
	    [sicl-documentation::paragraph
	      :contents
	      ("Value type")]
	    :contents
	    ([sicl-documentation::paragraph
	       :contents
	       ("A pathname")])]
	  [sicl-documentation::section
	    :title
	    [sicl-documentation::paragraph
	      :contents
	      ("Initial value")]
	    :contents
	    ([sicl-documentation::paragraph
	       :contents
	       ("Implementation dependent")])]
	  [sicl-documentation::section
	    :title
	    [sicl-documentation::paragraph
	      :contents
	      ("Description")]
	    :contents
	    ([sicl-documentation::paragraph
	       :contents
	       ("The value of this variable is a pathname that"
		"is used by certain operators as a default"
		"pathname argument.  The value might for"
		"instance be a pathname for the current directory"
		"when the Lisp process was started")])])])

(setf (documentation '*gensym-counter* 'variable)
      '[sicl-documentation::section
	 :title
	 [sicl-documentation::paragraph
           :contents
	   ("Variable *gensym-counter*")]
	 :contents 
	 ([sicl-documentation::section
	    :title
	    [sicl-documentation::paragraph
	      :contents
	      ("Value type")]
	    :contents
	    ([sicl-documentation::paragraph
	       :contents
	       ("A nonnegative integer")])]
	  [sicl-documentation::section
	    :title
	    [sicl-documentation::paragraph
	      :contents
	      ("Initial value")]
	    :contents
	    ([sicl-documentation::paragraph
	       :contents
	       ("Implementation dependent")])]
	  [sicl-documentation::section
	    :title
	    [sicl-documentation::paragraph
	      :contents
	      ("Description")]
	    :contents
	    ([sicl-documentation::paragraph
	       :contents
	       ("The value of this variable is used by the"
		"gensym"
		"function to construct symbol names"
		"by concatenating some prefix and the"
		"decimal representation of the value of"
		"this variable")])])])

(setf (documentation '*load-pathname* 'variable)
      '[sicl-documentation::section
	 :title
	 [sicl-documentation::paragraph
           :contents
	   ("Variable *load-pathname*")]
	 :contents 
	 ([sicl-documentation::section
	    :title
	    [sicl-documentation::paragraph
	      :contents
	      ("Value type")]
	    :contents
	    ([sicl-documentation::paragraph
	       :contents
	       ("A pathname or nil")])]
	  [sicl-documentation::section
	    :title
	    [sicl-documentation::paragraph
	      :contents
	      ("Initial value")]
	    :contents
	    ([sicl-documentation::paragraph
	       :contents
	       ("nil")])]
	  [sicl-documentation::section
	    :title
	    [sicl-documentation::paragraph
	      :contents
	      ("Description")]
	    :contents
	    ([sicl-documentation::paragraph
	       :contents
	       ("This variable is bound by"
		[sicl-documentation::documentation
		  :documentation-arguments (compile-file function)
		  :contents ("load")]
		"to the pathname of the file being loaded"
		"merged with the defaults")])])])

(setf (documentation '*load-truename* 'variable)
      '[sicl-documentation::section
	 :title
	 [sicl-documentation::paragraph
           :contents
	   ("Variable *load-truename*")]
	 :contents 
	 ([sicl-documentation::section
	    :title
	    [sicl-documentation::paragraph
	      :contents
	      ("Value type")]
	    :contents
	    ([sicl-documentation::paragraph
	       :contents
	       ("A physical pathname or nil")])]
	  [sicl-documentation::section
	    :title
	    [sicl-documentation::paragraph
	      :contents
	      ("Initial value")]
	    :contents
	    ([sicl-documentation::paragraph
	       :contents
	       ("nil")])]
	  [sicl-documentation::section
	    :title
	    [sicl-documentation::paragraph
	      :contents
	      ("Description")]
	    :contents
	    ([sicl-documentation::paragraph
	       :contents
	       ("This variable is bound by"
		[sicl-documentation::documentation
		  :documentation-arguments (compile-file function)
		  :contents ("load")]
		"to the truename of the file being loaded")])])])

(setf (documentation '*load-print* 'variable)
      '[sicl-documentation::section
	 :title
	 [sicl-documentation::paragraph
           :contents
	   ("Variable *load-print*")]
	 :contents 
	 ([sicl-documentation::section
	    :title
	    [sicl-documentation::paragraph
	      :contents
	      ("Value type")]
	    :contents
	    ([sicl-documentation::paragraph
	       :contents
	       ("A generalized boolean")])]
	  [sicl-documentation::section
	    :title
	    [sicl-documentation::paragraph
	      :contents
	      ("Initial value")]
	    :contents
	    ([sicl-documentation::paragraph
	       :contents
	       ("nil")])]
	  [sicl-documentation::section
	    :title
	    [sicl-documentation::paragraph
	      :contents
	      ("Description")]
	    :contents
	    ([sicl-documentation::paragraph
	       :contents
	       ("The value of this variable is used by"
		[sicl-documentation::documentation
		  :documentation-arguments (compile-file function)
		  :contents ("load")]
		"as the default value of the"
		":print"
		"keyword argument")])])])

(setf (documentation '*load-verbose* 'variable)
      '[sicl-documentation::section
	 :title
	 [sicl-documentation::paragraph
           :contents
	   ("Variable *load-verbose*")]
	 :contents 
	 ([sicl-documentation::section
	    :title
	    [sicl-documentation::paragraph
	      :contents
	      ("Value type")]
	    :contents
	    ([sicl-documentation::paragraph
	       :contents
	       ("A generlized boolean")])]
	  [sicl-documentation::section
	    :title
	    [sicl-documentation::paragraph
	      :contents
	      ("Initial value")]
	    :contents
	    ([sicl-documentation::paragraph
	       :contents
	       ("nil")])]
	  [sicl-documentation::section
	    :title
	    [sicl-documentation::paragraph
	      :contents
	      ("Description")]
	    :contents
	    ([sicl-documentation::paragraph
	       :contents
	       ("The value of this variable is used by"
		[sicl-documentation::documentation
		  :documentation-arguments (compile-file function)
		  :contents ("load")]
		"as the default value of the"
		":verbose"
		"keyword argument")])])])
