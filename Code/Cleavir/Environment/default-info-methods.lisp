(cl:in-package #:cleavir-environment)

;;;; This file contains methods on the generic functions defined in
;;;; the file query.lisp that are specialized to the classed defined
;;;; in the file default-augmentation-classes.lisp.  
;;;;
;;;; The implementation here is a bit twisted in that we pretty much
;;;; call a generic function for each elementary step.  The reason for
;;;; this way of doing it is so as to allow for an implementation to
;;;; specialize or override every such elementary step by defining
;;;; methods on those generic functions.
;;;;
;;;; In addition, even though the augmentation environments are chains
;;;; of small class instances resembling a list, we do not use
;;;; iteration in order to traverse them.  Instead we use recursion
;;;; where the default action is to make a recursive call, passing the
;;;; next instance in the chain.

(defgeneric make-info (environment defining-info))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; VARIABLE-INFO
;;;
;;; Finding info about a variable is a bit tricky, because there can
;;; be local entries modifying the properties of the variable, in
;;; particular the type of the variable. 
;;;
;;; We proceed by first finding the DEFINING INFO for the variable.
;;; This info might come from the global environment, or it can come
;;; from an ENTRY that defines the variable.
;;;
;;; Once we have found the defining info, we traverse the environment
;;; again in order to find modifying entries.  We do this traversal
;;; for each type of modifying entry possible.  Depending on the type
;;; of modifying entry we are looking for, either the traversal stops
;;; when we found the first relevant modifying entry, or it continues
;;; to find all relevant modifying entries of that type.  Either way
;;; the traversal stops when we reach the place where the variable was
;;; defined.

(defgeneric defining-variable-info (environment symbol))

;;; For entries with the right type to introduce a variable, we check
;;; whether the name in the entry is EQ to the symbol that we are
;;; passed as an argument.  If that is the case, we create and return
;;; a very basic corresponding INFO instance.  If the name in the
;;; entry is not EQ to the symbol we are passed as an argument, we
;;; invoke DEFINING-VARIABLE-INFO recursively with the remaining
;;; environment.
;;;
;;; The relevant entries for variable info are LEXICAL-VARIABLE,
;;; SPECIAL-VARIABLE, and SYMBOL-MACRO.  Since constant variables can
;;; only be global, there is no entry type for constant variables.

(defmethod defining-variable-info ((environment lexical-variable) symbol)
  (if (eq symbol (name environment))
      (make-instance 'lexical-variable-info
	:name symbol
	:identity (identity environment))
      (defining-variable-info (next environment) symbol)))

(defmethod defining-variable-info ((environment special-variable) symbol)
  (if (eq symbol (name environment))
      (make-instance 'special-variable-info
	:name symbol
	:global-p nil)
      (defining-variable-info (next environment) symbol)))

(defmethod defining-variable-info ((environment symbol-macro) symbol)
  (if (eq symbol (name environment))
      (make-instance 'symbol-macro-info
	:name symbol
	:expansion (expansion environment))
      (defining-variable-info (next environment) symbol)))

;;; This method implements the action to take when the argument is an
;;; ENTRY, but it is not an entry defining a variable.  We handle this
;;; situation by just making a recursive call, passing the next entry
;;; in the environment.
(defmethod defining-variable-info ((environment entry) symbol)
  (defining-variable-info (next environment) symbol))

;;; This method implements the action to take when the argument is the
;;; global environment.  We detect this situation by the fact that the
;;; argument is not an ENTRY.  Since we have run out of local
;;; environment entries, we must now consult the implementation by
;;; calling VARIABLE-INFO on the global environment.
(defmethod defining-variable-info (environment symbol)
  (variable-info environment symbol))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function VARIABLE-TYPE.
;;;
;;; This function takes an environment and a defining info instance
;;; and returns a list of type specifiers, one for every entry in the
;;; environment that contains type information for the defining info
;;; instance.

(defgeneric variable-type (environment defining-info))

;;; This method is called when the environment is the global
;;; environment.
(defmethod variable-type (environment defining-info)
  (declare (cl:ignorable environment))
  (list (type defining-info)))

;;; This method is called when the entry is not related to the
;;; defining info instance. 
(defmethod variable-type ((environment entry) defining-info)
  (declare (cl:ignorable environment defining-info))
  (variable-type (next environment) defining-info))

;;; The following three methods are called when the environment entry
;;; is of the same type as the one that resulted in the creation of
;;; the defining info instance.  If the name of the environment entry
;;; is the same as the name of the info instance, then this entry was
;;; the one that resulted in the creation of the defining info
;;; instance.  In other words, we have found no variable type entries
;;; before entry that resulted in the creation of the defining info.
;;; If the names are not the same, we continue the search. 

(defmethod variable-type ((environment lexical-variable)
			  (defining-info lexical-variable-info))
  (if (eq (name environment) (name defining-info))
      (list (type defining-info))
      (variable-type (next environment) defining-info)))

(defmethod variable-type ((environment special-variable)
			  (defining-info special-variable-info))
  (if (eq (name environment) (name defining-info))
      (list (type defining-info))
      (variable-type (next environment) defining-info)))

(defmethod variable-type ((environment symbol-macro)
			  (defining-info symbol-macro-info))
  (if (eq (name environment) (name defining-info))
      (list (type defining-info))
      (variable-type (next environment) defining-info)))

;;; The following three methods are called when the current entry is a
;;; candidate for being the entry containing type information for a
;;; variable info.  We found the right one if the names are the same.
;;; If not, then we continue the search.

(defmethod variable-type ((environment variable-type)
			  (defining-info lexical-variable-info))
  (if (eq (name environment) (name defining-info))
      (cons (type environment)
	    (variable-type (next environment) defining-info))
      (variable-type (next environment) defining-info)))

(defmethod variable-type ((environment variable-type)
			  (defining-info special-variable-info))
  (if (eq (name environment) (name defining-info))
      (cons (type environment)
	    (variable-type (next environment) defining-info))
      (variable-type (next environment) defining-info)))

(defmethod variable-type ((environment variable-type)
			  (defining-info symbol-macro-info))
  (if (eq (name environment) (name defining-info))
      (cons (type environment)
	    (variable-type (next environment) defining-info))
      (variable-type (next environment) defining-info)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function VARIABLE-IGNORE.
;;;
;;; This function takes an environment and a defining info instance
;;; and returns the first entry in the environment that contains ignore
;;; information for the defining info instance, or NIL if there is not
;;; such entry.

(defgeneric variable-ignore (environment defining-info))

;;; This method is called when the environment is the global
;;; environment.
(defmethod variable-ignore (environment defining-info)
  (declare (cl:ignorable environment defining-info))
  nil)

;;; This method is called when the entry is not related to the
;;; defining info instance. 
(defmethod variable-ignore ((environment entry) defining-info)
  (declare (cl:ignorable environment defining-info))
  (variable-ignore (next environment) defining-info))

;;; The following two methods are called when the environment entry
;;; is of the same type as the one that resulted in the creation of
;;; the defining info instance.  If the name of the environment entry
;;; is the same as the name of the info instance, then this entry was
;;; the one that resulted in the creation of the defining info
;;; instance.  In other words, we have found no variable type entries
;;; before entry that resulted in the creation of the defining info.
;;; If the names are not the same, we continue the search. 

(defmethod variable-ignore ((environment lexical-variable)
			    (defining-info lexical-variable-info))
  (if (eq (name environment) (name defining-info))
      nil
      (variable-ignore (next environment) defining-info)))

(defmethod variable-ignore ((environment special-variable)
			    (defining-info special-variable-info))
  (if (eq (name environment) (name defining-info))
      nil
      (variable-ignore (next environment) defining-info)))

;;; The following two methods are called when the current entry is a
;;; candidate for being the entry containing ignore information for a
;;; variable info.  We found the right one if the names are the same.
;;; If not, then we continue the search.

(defmethod variable-ignore ((environment variable-ignore)
			    (defining-info lexical-variable-info))
  (if (eq (name environment) (name defining-info))
      environment
      (variable-ignore (next environment) defining-info)))

(defmethod variable-ignore ((environment variable-ignore)
			    (defining-info special-variable-info))
  (if (eq (name environment) (name defining-info))
      environment
      (variable-ignore (next environment) defining-info)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function VARIABLE-DYNAMIC-EXTENT.
;;;
;;; This function takes an environment and a defining info instance
;;; and returns the first entry in the environment that contains
;;; dynamic-extent information for the defining info instance, or NIL
;;; if there is not such entry.

(defgeneric variable-dynamic-extent (environment defining-info))

;;; This method is called when the environment is the global
;;; environment.
(defmethod variable-dynamic-extent (environment defining-info)
  (declare (cl:ignorable environment defining-info))
  nil)

;;; This method is called when the entry is not related to the
;;; defining info instance. 
(defmethod variable-dynamic-extent ((environment entry) defining-info)
  (declare (cl:ignorable environment defining-info))
  (variable-dynamic-extent (next environment) defining-info))

;;; The following method is called when the environment entry is of
;;; the same type as the one that resulted in the creation of the
;;; defining info instance.  If the name of the environment entry is
;;; the same as the name of the info instance, then this entry was the
;;; one that resulted in the creation of the defining info instance.
;;; In other words, we have found no variable type entries before
;;; entry that resulted in the creation of the defining info.  If the
;;; names are not the same, we continue the search.

(defmethod variable-dynamic-extent ((environment lexical-variable)
			  (defining-info lexical-variable-info))
  (if (eq (name environment) (name defining-info))
      nil
      (variable-dynamic-extent (next environment) defining-info)))

;;; The following method is called when the current entry is a
;;; candidate for being the entry containing dynamic extent
;;; information for a variable info.  We found the right one if the
;;; names are the same.  If not, then we continue the search.

(defmethod variable-dynamic-extent ((environment variable-dynamic-extent)
			  (defining-info lexical-variable-info))
  (if (eq (name environment) (name defining-info))
      environment
      (variable-dynamic-extent (next environment) defining-info)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Methods on MAKE-INFO specialized to INFO classes returned by
;;; VARIABLE-INFO.

(defmethod make-info
    (environment (defining-info lexical-variable-info))
  (make-instance 'lexical-variable-info
    :name (name defining-info)
    :identity (identity defining-info)
    :type (apply #'cleavir-ctype:conjoin
                 nil ; FIXME
                 (variable-type environment defining-info))
    :ignore
    (let ((entry (variable-ignore environment defining-info)))
      (if (null entry) nil (ignore entry)))
    :dynamic-extent
    (let ((entry (variable-dynamic-extent environment defining-info)))
      (if (null entry) (dynamic-extent defining-info) t))))

(defmethod make-info
    (environment (defining-info special-variable-info))
  (make-instance 'special-variable-info
    :name (name defining-info)
    :type (apply #'cleavir-ctype:conjoin
                 nil
		 (variable-type environment defining-info))
    :global-p (global-p defining-info)
    :ignore
    (let ((entry (variable-ignore environment defining-info)))
      (if (null entry) nil (ignore entry)))))

(defmethod make-info
    (environment (defining-info constant-variable-info))
  (declare (cl:ignorable environment))
  defining-info)

(defmethod make-info
    (environment (defining-info symbol-macro-info))
  (make-instance 'symbol-macro-info
    :name (name defining-info)
    :expansion (expansion defining-info)
    :type (apply #'cleavir-ctype:conjoin
                 nil
		 (variable-type environment defining-info))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; The main method on VARIABLE-INFO specialized to ENTRY. 

(defmethod variable-info ((environment entry) symbol)
  (let ((defining-info (defining-variable-info environment symbol)))
    (if (null defining-info)
	;; If DEFINING-INFO is NIL, this means that VARIABLE-INFO
	;; returned NIL when called with the global environment, which
	;; means that there was no information for this symbol.  We
	;; must then also respect the protocol and return nil to our
	;; caller.
	nil
	(make-info environment defining-info))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; FUNCTION-INFO
;;;
;;; Finding info about a function almost as tricky as finding info
;;; about a variable, again because there can be local entries
;;; modifying the properties of the function.
;;;
;;; We proceed by first finding the DEFINING INFO for the function.
;;; This info might come from the global environment, or it can come
;;; from an ENTRY that defines the function.
;;;
;;; Once we have found the defining info, we traverse the environment
;;; again in order to find modifying entries.  We do this traversal
;;; for each type of modifying entry possible.  The traversal stops
;;; either when we found the first relevant modifying entry, or we
;;; reach the place where the function was defined, in which case no
;;; relevant modifying entry was found.

(defgeneric defining-function-info (environment symbol))

;;; For entries with the right type to introduce a function, we check
;;; whether the name in the entry is EQUAL to the function-name (or EQ
;;; to the macro name) that we are passed as an argument.  If that is
;;; the case, we create and return a very basic corresponding INFO
;;; instance.  If the name in the entry is not EQUAL to the
;;; function-name (or EQ to the macro name) we are passed as an
;;; argument, we invoke DEFINING-FUNCTION-INFO recursively with the
;;; remaining environment.
;;;
;;; The relevant entries for function info are FUNCTION, and
;;; MACRO.

(defmethod defining-function-info ((environment function) function-name)
  (if (equal function-name (name environment))
      (make-instance 'local-function-info
	:name function-name
	:identity (identity environment))
      (defining-function-info (next environment) function-name)))

(defmethod defining-function-info ((environment macro) symbol)
  (if (eq symbol (name environment))
      (make-instance 'local-macro-info
	:name symbol
	:expander (expander environment))
      (defining-function-info (next environment) symbol)))

;;; This method implements the action to take when the argument is an
;;; ENTRY, but it is not an entry defining a function.  We handle this
;;; situation by just making a recursive call, passing the next entry
;;; in the environment.
(defmethod defining-function-info ((environment entry) function-name)
  (defining-function-info (next environment) function-name))

;;; This method implements the action to take when the argument is the
;;; global environment.  We detect this situation by the fact that the
;;; argument is not an ENTRY.  Since we have run out of local
;;; environment entries, we must now consult the implementation by
;;; calling FUNCTION-INFO on the global environment.
(defmethod defining-function-info (environment function-name)
  (function-info environment function-name))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function FUNCTION-TYPE.
;;;
;;; This function takes an environment and a defining info instance
;;; and returns a list of type specifiers, on for each entry in the
;;; environment that contains type information for the defining info
;;; instance.

(defgeneric function-type (environment defining-info))

;;; This method is called when the environment is the global
;;; environment.
(defmethod function-type (environment defining-info)
  (declare (cl:ignorable environment))
  (list (type defining-info)))

;;; This method is called when the entry is not related to the
;;; defining info instance. 
(defmethod function-type ((environment entry) defining-info)
  (declare (cl:ignorable environment defining-info))
  (function-type (next environment) defining-info))

;;; The following two methods are called when the environment entry
;;; is of the same type as the one that resulted in the creation of
;;; the defining info instance.  If the name of the environment entry
;;; is the same as the name of the info instance, then this entry was
;;; the one that resulted in the creation of the defining info
;;; instance.  In other words, we have found no function type entries
;;; before entry that resulted in the creation of the defining info.
;;; If the names are not the same, we continue the search. 

(defmethod function-type ((environment function)
			  (defining-info local-function-info))
  (if (equal (name environment) (name defining-info))
      (list (type defining-info))
      (function-type (next environment) defining-info)))

(defmethod function-type ((environment macro)
			  (defining-info local-macro-info))
  (if (eq (name environment) (name defining-info))
      (list (type defining-info))
      (function-type (next environment) defining-info)))

;;; The following four methods are called when the current entry is a
;;; candidate for being the entry containing type information for a
;;; function info.  We found the right one if the names are the same.
;;; If not, then we continue the search.

(defmethod function-type ((environment function-type)
			  (defining-info local-function-info))
  (if (equal (name environment) (name defining-info))
      (cons (type environment)
	    (function-type (next environment) defining-info))
      (function-type (next environment) defining-info)))

(defmethod function-type ((environment function-type)
			  (defining-info global-function-info))
  (if (equal (name environment) (name defining-info))
      (cons (type environment)
	    (function-type (next environment) defining-info))
      (function-type (next environment) defining-info)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function FUNCTION-IGNORE.
;;;
;;; This function takes an environment and a defining info instance
;;; and returns the first entry in the environment that contains
;;; ignore information for the defining info instance, or NIL if there
;;; is not such entry.

(defgeneric function-ignore (environment defining-info))

;;; This method is called when the environment is the global
;;; environment.
(defmethod function-ignore (environment defining-info)
  (declare (cl:ignorable environment defining-info))
  nil)

;;; This method is called when the entry is not related to the
;;; defining info instance. 
(defmethod function-ignore ((environment entry) defining-info)
  (declare (cl:ignorable environment defining-info))
  (function-ignore (next environment) defining-info))

;;; The following two methods are called when the environment entry
;;; is of the same type as the one that resulted in the creation of
;;; the defining info instance.  If the name of the environment entry
;;; is the same as the name of the info instance, then this entry was
;;; the one that resulted in the creation of the defining info
;;; instance.  In other words, we have found no function type entries
;;; before entry that resulted in the creation of the defining info.
;;; If the names are not the same, we continue the search. 

(defmethod function-ignore ((environment function)
			  (defining-info local-function-info))
  (if (equal (name environment) (name defining-info))
      nil
      (function-ignore (next environment) defining-info)))

(defmethod function-ignore ((environment macro)
			  (defining-info local-macro-info))
  (if (eq (name environment) (name defining-info))
      nil
      (function-ignore (next environment) defining-info)))

;;; The following four methods are called when the current entry is a
;;; candidate for being the entry containing ignore information for a
;;; function info.  We found the right one if the names are the same.
;;; If not, then we continue the search.

(defmethod function-ignore ((environment function-ignore)
			  (defining-info local-function-info))
  (if (equal (name environment) (name defining-info))
      environment
      (function-ignore (next environment) defining-info)))

(defmethod function-ignore ((environment function-ignore)
			  (defining-info global-function-info))
  (if (equal (name environment) (name defining-info))
      environment
      (function-ignore (next environment) defining-info)))

(defmethod function-ignore ((environment function-ignore)
			  (defining-info local-macro-info))
  (if (eq (name environment) (name defining-info))
      environment
      (function-ignore (next environment) defining-info)))

(defmethod function-ignore ((environment function-ignore)
			  (defining-info global-macro-info))
  (if (eq (name environment) (name defining-info))
      environment
      (function-ignore (next environment) defining-info)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function FUNCTION-DYNAMIC-EXTENT.
;;;
;;; This function takes an environment and a defining info instance
;;; and returns the first entry in the environment that contains
;;; dynamic-extent information for the defining info instance, or NIL
;;; if there is not such entry.

(defgeneric function-dynamic-extent (environment defining-info))

;;; This method is called when the environment is the global
;;; environment.
(defmethod function-dynamic-extent (environment defining-info)
  (declare (cl:ignorable environment defining-info))
  nil)

;;; This method is called when the entry is not related to the
;;; defining info instance. 
(defmethod function-dynamic-extent ((environment entry) defining-info)
  (declare (cl:ignorable environment defining-info))
  (function-dynamic-extent (next environment) defining-info))

;;; The following method is called when the environment entry is of
;;; the same type as the one that resulted in the creation of the
;;; defining info instance.  If the name of the environment entry is
;;; the same as the name of the info instance, then this entry was the
;;; one that resulted in the creation of the defining info instance.
;;; In other words, we have found no function type entries before
;;; entry that resulted in the creation of the defining info.  If the
;;; names are not the same, we continue the search.

(defmethod function-dynamic-extent ((environment function)
				    (defining-info local-function-info))
  (if (eq (name environment) (name defining-info))
      nil
      (function-dynamic-extent (next environment) defining-info)))

;;; The following method is called when the current entry is a
;;; candidate for being the entry containing dynamic-extent
;;; information for a function info.  We found the right one if the
;;; names are the same.  If not, then we continue the search.

(defmethod function-dynamic-extent ((environment function-dynamic-extent)
				    (defining-info local-function-info))
  (if (equal (name environment) (name defining-info))
      environment
      (function-dynamic-extent (next environment) defining-info)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function FUNCTION-INLINE.
;;;
;;; This function takes an environment and a defining info instance
;;; and returns the first entry in the environment that contains inline
;;; information for the defining info instance, or NIL if there is no
;;; such entry.

(defgeneric function-inline (environment defining-info))

;;; This method is called when the environment is the global
;;; environment.
(defmethod function-inline (environment defining-info)
  (declare (cl:ignore environment))
  nil)

;;; This method is called when the entry is not related to the
;;; defining info instance. 
(defmethod function-inline ((environment entry) defining-info)
  (declare (cl:ignorable environment defining-info))
  (function-inline (next environment) defining-info))

;;; The following method is called when the environment entry is of
;;; the same type as the one that resulted in the creation of the
;;; defining info instance.  If the name of the environment entry is
;;; the same as the name of the info instance, then this entry was the
;;; one that resulted in the creation of the defining info instance.
;;; In other words, we have found no function type entries before
;;; entry that resulted in the creation of the defining info.  If the
;;; names are not the same, we continue the search.

(defmethod function-inline ((environment function)
			  (defining-info local-function-info))
  (if (equal (name environment) (name defining-info))
      nil
      (function-inline (next environment) defining-info)))

;;; The following two methods are called when the current entry is a
;;; candidate for being the entry containing inline information for a
;;; function info.  We found the right one if the names are the same.
;;; If not, then we continue the search.

(defmethod function-inline ((environment inline)
			    (defining-info local-function-info))
  (if (equal (name environment) (name defining-info))
      environment
      (function-inline (next environment) defining-info)))

(defmethod function-inline ((environment inline)
			    (defining-info global-function-info))
  (if (equal (name environment) (name defining-info))
      environment
      (function-inline (next environment) defining-info)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function FUNCTION-INLINE-EXPANSION.
;;;
;;; This function takes an environment and a defining info instance
;;; and returns the first entry in the environment that contains
;;; an expansion for the defining info instance, or NIL if there is
;;; no such entry.

(defgeneric function-inline-expansion (environment defining-info))

;;; This method is called when the environment is the global
;;; environment.
(defmethod function-inline-expansion (environment defining-info)
  (declare (cl:ignore environment))
  nil)

;;; This method is called when the entry is not related to the
;;; defining info instance. 
(defmethod function-inline-expansion
    ((environment entry) defining-info)
  (declare (cl:ignorable environment defining-info))
  (function-inline-expansion (next environment) defining-info))

;;; The following method is called when the environment entry is of
;;; the same type as the one that resulted in the creation of the
;;; defining info instance.  If the name of the environment entry is
;;; the same as the name of the info instance, then this entry was the
;;; one that resulted in the creation of the defining info instance.
;;; In other words, we have found no function type entries before
;;; entry that resulted in the creation of the defining info.  If the
;;; names are not the same, we continue the search.

(defmethod function-inline-expansion
    ((environment function) (defining-info local-function-info))
  (if (equal (name environment) (name defining-info))
      nil
      (function-inline-expansion (next environment) defining-info)))

;;; The following method is called when the current entry is a
;;; candidate for being the entry containing inline information for a
;;; function info.  We found the right one if the names are the same.
;;; If not, then we continue the search.

(defmethod function-inline-expansion
    ((environment inline-expansion)
     (defining-info local-function-info))
  (if (equal (name environment) (name defining-info))
      environment
      (function-inline-expansion (next environment) defining-info)))

;;; No inline-expansions for global function infos.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Methods on MAKE-INFO specialized to INFO classes returned by
;;; FUNCTION-INFO.

(defmethod make-info
    (environment (defining-info local-function-info))
  (make-instance 'local-function-info
    :name (name defining-info)
    :identity (identity defining-info)
    :type (apply #'cleavir-ctype:conjoin
                 nil
		 (function-type environment defining-info))
    :ignore
    (let ((entry (function-ignore environment defining-info)))
      (ignore (or entry defining-info)))
    :inline
    (let ((entry (function-inline environment defining-info)))
      (inline (or entry defining-info)))
    :ast
    (let ((entry (function-inline-expansion environment
					    defining-info)))
      (ast (or entry defining-info)))
    :dynamic-extent
    (let ((entry (function-dynamic-extent environment defining-info)))
      (if (null entry) (dynamic-extent defining-info) t))
    :attributes (attributes defining-info)))

(defmethod make-info
    (environment (defining-info global-function-info))
  (make-instance 'global-function-info
    :name (name defining-info)
    :type (apply #'cleavir-ctype:conjoin
                 nil
		 (function-type environment defining-info))
    :ignore
    (let ((entry (function-ignore environment defining-info)))
      (ignore (or entry defining-info)))
    :inline
    (let ((entry (function-inline environment defining-info)))
      (inline (or entry defining-info)))
    ;; don't bother with function-inline-expansion, since there
    ;;  shouldn't be local expansions for global functions.
    :ast (ast defining-info)
    :compiler-macro (compiler-macro defining-info)
    :attributes (attributes defining-info)
    :dynamic-extent
    (let ((entry (function-dynamic-extent environment defining-info)))
      (if (null entry) (dynamic-extent defining-info) t))))

(defmethod make-info
    (environment (defining-info local-macro-info))
  (declare (cl:ignore environment))
  defining-info)

(defmethod make-info
    (environment (defining-info global-macro-info))
  (make-instance 'global-macro-info
    :name (name defining-info)
    :compiler-macro (compiler-macro defining-info)
    :expander (expander defining-info)))

(defmethod make-info
    (environment (defining-info special-operator-info))
  (declare (cl:ignore environment))
  defining-info)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; The main method on FUNCTION-INFO specialized to ENTRY. 

(defmethod function-info ((environment entry) symbol)
  (let ((defining-info (defining-function-info environment symbol)))
    (if (null defining-info)
	;; If DEFINING-INFO is NIL, this means that FUNCTION-INFO
	;; returned NIL when called with the global environment, which
	;; means that there was no information for this symbol.  We
	;; must then also respect the protocol and return nil to our
	;; caller.
	nil
	(make-info environment defining-info))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; BLOCK-INFO
;;;
;;; Finding info about a BLOCK is particularly easy because there can
;;; be no entries modifying the properties of the block. 

;;; This method implements the action to take when the argument is a
;;; BLOCK entry.
(defmethod block-info ((environment block) symbol)
  (if (eq symbol (name environment))
      ;; We found a BLOCK entry with the same name, so we are done.
      ;; Create and return a valid BLOCK-INFO instance for this
      ;; environment.
      (make-instance 'block-info
	:name symbol
	:identity (identity environment))
      ;; We had a BLOCK entry, but it does not have the name that we
      ;; are looking for.  Try the next entry in the environment.
      (block-info (next environment) symbol)))

;;; This method implements the action to take when the argument is an
;;; ENTRY, but it is not a BLOCK entry.  We handle this situation by
;;; just making a recursive call, passing the next entry in the
;;; environment.
(defmethod block-info ((environment entry) symbol)
  (block-info (next environment) symbol))

;;; This method implements the action to take when the argument is the
;;; global environment.  We detect this situation by the fact that the
;;; argument is not an ENTRY.  Since the global environment can not
;;; have any blocks, it is safe to return NIL as the specification
;;; stipulates.
(defmethod block-info (environment symbol)
  (declare (cl:ignorable environment symbol))
  nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; TAG-INFO
;;;
;;; Finding info about a TAG is particularly easy because there can
;;; be no entries modifying the properties of the tag. 

;;; This method implements the action to take when the argument is a
;;; TAG entry.
(defmethod tag-info ((environment tag) symbol)
  (if (eql symbol (name environment))
      ;; We found a TAG entry with the same name, so we are done.
      ;; Create and return a valid TAG-INFO instance for this
      ;; environment.
      (make-instance 'tag-info
	:name symbol
	:identity (identity environment))
      ;; We had a TAG entry, but it does not have the name that we
      ;; are looking for.  Try the next entry in the environment.
      (tag-info (next environment) symbol)))

;;; This method implements the action to take when the argument is an
;;; ENTRY, but it is not a TAG entry.  We handle this situation by
;;; just making a recursive call, passing the next entry in the
;;; environment.
(defmethod tag-info ((environment entry) symbol)
  (tag-info (next environment) symbol))

;;; This method implements the action to take when the argument is the
;;; global environment.  We detect this situation by the fact that the
;;; argument is not an ENTRY.  Since the global environment can not
;;; have any tags, it is safe to return NIL as the specification
;;; stipulates.
(defmethod tag-info (environment symbol)
  (declare (cl:ignorable environment symbol))
  nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; OPTIMIZE-INFO
;;;
;;;
;;; In order to avoid recomputing policies, full optimize info and
;;; policies are stored in each optimize entry.
;;;
;;; This makes the existence of a dedicated optimize-info class
;;; redundant, but it's not a big deal.

(defmethod optimize-info ((environment optimize))
  (make-instance 'optimize-info
    :optimize (optimize environment)
    :policy (policy environment)))

(defmethod optimize-info ((environment entry))
  (optimize-info (next environment)))

;;; OK it's a big enough deal that here is a helper.
(defun environment-policy (environment)
  (policy (optimize-info environment)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; GLOBAL-ENVIRONMENT.

;;; This method is called on the global environment.
(defmethod global-environment (environment)
  environment)

;;; This method is called when the environment is an instance of
;;; ENTRY, i.e., anything except the global environment.
(defmethod global-environment ((environment entry))
  (global-environment (next environment)))
