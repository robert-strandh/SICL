(cl:in-package #:portable-condition-system)

(setf (documentation 'run-debugger-command 'function)
      #.(format
         nil
         "Executes the provided debugger command, reading input from~@
          and printing output to the provided stream. The condition~@
          object the debugger was entered with and optional command~@
          arguments are available for use within the command itself."))

(setf (documentation 'define-command 'function)
      #.(format
         nil
         "Accepts a command name (which should be a keyword) and~@
          generates a DEFMETHOD form in which the stream, condition,~@
          and argument variables are available for use inside the method body."))
  
(setf (documentation 'restart-max-name-length 'function)
      #.(format
         nil
         "Returns the length of the longest name from the provided restarts."))

(setf (documentation 'debugger-invoke-restart 'function)
      #.(format
         nil
         "Finds and invokes a restart with the given name; if no such restart~@
          is available, informs the user about that fact."))

(setf (documentation 'read-eval-print-command 'function)
      #.(format
         nil
         "Implements a single read-eval-print pass of the debugger REPL.~@
          Keywords are treated as debugger commands and integers are~@
          treated as arguments to :RESTART."))  

(setf (documentation '*debug-level* 'variable)
      #.(format
         nil
         "A variable holding the current debugger level, rebound~@
          dynamically on each debugger entry."))

(setf (documentation '*help-hooks* 'variable)
      #.(format
         nil
         "A list of hooks that are called when the :HELP debugger command~@
          is invoked. Each hook must be a function that accepts a condition~@
          object that the debugger was entered with and a stream that the~@
          hook should print to."))

(setf (documentation '*debugger-hook* 'variable)
      #.(format
         nil
         "If set, it is called as a function before entry into the debugger~@
          with two arguments: the condition object that the debugger is~@
          invoked with, and itself."))

(setf (documentation
       (find-method #'run-debugger-command '()
                    (list (find-class 't)
                          (find-class 't)
                          (find-class 't)))
       t)
      #.(format
         nil
         "Informs the user that the provided debugger command was not recognized."))

(setf (documentation
       (find-method #'run-debugger-command '()
                    (list (eql :eval)
                          (find-class 't)
                          (find-class 't)))
       t)
      #.(format
         nil
         "Evaluates a form. The form may be provided as an optional argument;~@
          otherwise, it read from the provided stream."))

(setf (documentation
       (find-method #'run-debugger-command '()
                    (list (eql :report)
                          (find-class 't)
                          (find-class 't)))
       t)
      #.(format
         nil
         "Informs the user that the debugger has been entered and reports the~@
          condition object the debugger was entered with."))

(setf (documentation
       (find-method #'run-debugger-command '()
                    (list (eql :condition)
                          (find-class 't)
                          (find-class 't)))
       t)
      #.(format
         nil
         "Returns the condition object that the debugger was entered with."))

(setf (documentation
       (find-method #'run-debugger-command '()
                    (list (eql :restarts)
                          (find-class 't)
                          (find-class 't)))
       t)
      #.(format
         nil
         "Prints a list of available restarts."))

(setf (documentation
       (find-method #'run-debugger-command '()
                    (list (eql :restart)
                          (find-class 't)
                          (find-class 't)))
       t)
      #.(format
         nil
         "Invokes a particular restart."))

(setf (documentation
       (find-method #'run-debugger-command '()
                    (list (eql :abort)
                          (find-class 't)
                          (find-class 't)))
       t)
      #.(format
         nil
         "Finds and invokes the ABORT restart; if no such restart is available,~@
          informs the user about that fact."))

(setf (documentation
       (find-method #'run-debugger-command '()
                    (list (eql :q)
                          (find-class 't)
                          (find-class 't)))
       t)
      #.(format
         nil
         "Shorthand for :ABORT."))

(setf (documentation
       (find-method #'run-debugger-command '()
                    (list (eql :continue)
                          (find-class 't)
                          (find-class 't)))
       t)
      #.(format
         nil
         "Finds and invokes the CONTINUE restart; if no such restart is available,~@
          informs the user about that fact."))

(setf (documentation
       (find-method #'run-debugger-command '()
                    (list (eql :c)
                          (find-class 't)
                          (find-class 't)))
       t)
      #.(format
         nil
         "Shorthand for :CONTINUE."))

(setf (documentation
       (find-method #'run-debugger-command '()
                    (list (eql :help)
                          (find-class 't)
                          (find-class 't)))
       t)
      #.(format
         nil
         "Prints the debugger help."))

(setf (documentation 'invoke-debugger 'function)
      #.(format
         nil
         "Invokes the debugger with the provided condition object."))

(setf (documentation
       (find-method #'invoke-debugger '() (list (find-class 'condition)))
       t)
      #.(format
         nil
         "If the debugger hook is set, calls it with the condition object~@
          and itself. Then, enters the standard debugger."))
  
(setf (documentation 'break 'function)
      #.(format
         nil
         "Binds *debugger-hook* to NIL, establishes a CONTINUE restart,~@
          and invokes the debugger with a condition object whose report~@
          is constructed from the optional format control and format arguments."))
