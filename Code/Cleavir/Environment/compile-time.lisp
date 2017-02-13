(cl:in-package #:cleavir-environment)

;;;; This file defines a function COMPILE-TIME that reduces an ENTRY
;;;; to only those components that are "fully available" during
;;;; compilation. This is just as for MACROLET:
;;;; "The macro-expansion functions defined by macrolet are defined
;;;;  in the lexical environment in which the macrolet form appears.
;;;;  Declarations and macrolet and symbol-macrolet definitions affect
;;;;  the local macro definitions in a macrolet, but the consequences
;;;;  are undefined if the local macro definitions reference any local
;;;;  variable or function bindings that are visible in that lexical
;;;;  environment."
;;;; In other words, this function strips all lexical variables,
;;;;  functions, etc, while leaving macros and declarations.
;;;; This makes CLEAVIR-ENV:EVAL easier.
;;;; In practice we have to copy everything...

(defgeneric compile-time (environment))

;;; Default, for the global environment: just return it.
(defmethod compile-time (environment) environment)

(defmacro defpunt (class-name)
  `(defmethod compile-time ((environment ,class-name))
     (compile-time (next environment))))

(defmacro defcopy (class-name &rest initkeys-and-readers)
  `(defmethod compile-time ((environment ,class-name))
     (make-instance
      ',class-name
      ,@(loop for (initkey reader) on initkeys-and-readers
              by #'cddr
              appending (list initkey `(,reader environment)))
      :next (compile-time (next environment)))))

(defpunt lexical-variable)
(defcopy special-variable :name name)
(defcopy symbol-macro :name name :expansion expansion)
(defpunt function)
(defpunt macro)
(defpunt block)
(defpunt tag)
(defcopy variable-type :name name :type type)
(defcopy function-type :name name :type type)
(defcopy variable-ignore :name name :ignore ignore)
(defcopy function-ignore :name name :ignore ignore)
(defcopy variable-dynamic-extent :name name)
(defcopy function-dynamic-extent :name name)
(defcopy optimize :optimize optimize :policy policy)
(defcopy inline :name name :inline inline)
(defcopy inline-expansion :name name :ast ast)
