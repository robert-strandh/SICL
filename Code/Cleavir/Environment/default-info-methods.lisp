(cl:in-package #:cleavir-environment)

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
;;; for each type of modifying entry possible.  The traversal stops
;;; either when we found the first relevant modifying entry, or we
;;; reach the place where the variable was defined, in which case no
;;; relevant modifying entry was found.

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
	:identity (identity environment))
      (defining-variable-info (next environment) symbol)))

(defmethod defining-variable-info ((environment symbol-macro) symbol)
  (if (eq symbol (name environment))
      (make-instance 'symbol-macro-info
	:name symbol
	:identity (identity environment))
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
;;; and returns the first entry in the environment that contains type
;;; information for the defining info instance, or NIL if there is not
;;; such entry.

(defgeneric variable-type (environment defining-info))

;;; This method is called when the environment is the global
;;; environment.
(defmethod variable-type (environment defining-info)
  (declare (cl:ignorable environment defining-info))
  nil)

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
      nil
      (variable-type (next environment) defining-info)))

(defmethod variable-type ((environment special-variable)
			  (defining-info special-variable-info))
  (if (eq (name environment) (name defining-info))
      nil
      (variable-type (next environment) defining-info)))

(defmethod variable-type ((environment symbol-macro)
			  (defining-info symbol-macro-info))
  (if (eq (name environment) (name defining-info))
      nil
      (variable-type (next environment) defining-info)))

;;; The following three methods are called when the current entry is a
;;; candidate for being the entry containing type information for a
;;; variable info.  We found the right one if the names are the same.
;;; If not, then we continue the search.

(defmethod variable-type ((environment variable-type)
			  (defining-info lexical-variable-info))
  (if (eq (name environment) (name defining-info))
      environment
      (variable-type (next environment) defining-info)))

(defmethod variable-type ((environment variable-type)
			  (defining-info special-variable-info))
  (if (eq (name environment) (name defining-info))
      environment
      (variable-type (next environment) defining-info)))

(defmethod variable-type ((environment variable-type)
			  (defining-info symbol-macro-info))
  (if (eq (name environment) (name defining-info))
      environment
      (variable-type (next environment) defining-info)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function VARIABLE-IGNORE.
;;;
;;; This function takes an environment and a defining info instance
;;; and returns the first entry in the environment that contains type
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
;;; candidate for being the entry containing type information for a
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
;;; and returns the first entry in the environment that contains type
;;; information for the defining info instance, or NIL if there is not
;;; such entry.

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
;;; candidate for being the entry containing type information for a
;;; variable info.  We found the right one if the names are the same.
;;; If not, then we continue the search.

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
    :type
    (let ((entry (variable-type environment defining-info)))
      (type (if (null entry) defining-info entry)))
    :ignore
    (let ((entry (variable-ignore environment defining-info)))
      (ignore (if (null entry) defining-info entry)))
    :dynamic-extent
    (let ((entry (variable-dynamic-extent environment defining-info)))
      (dynamic-extent (if (null entry) (dynamic-extent defining-info) t)))))

(defmethod make-info
    (environment (defining-info special-variable-info))
  (make-instance 'special-variable-info
    :name (name defining-info)
    :type
    (let ((entry (variable-type environment defining-info)))
      (type (if (null entry) defining-info entry)))
    :ignore
    (let ((entry (variable-ignore environment defining-info)))
      (ignore (if (null entry) defining-info entry)))))

(defmethod make-info
    (environment (defining-info constant-variable-info))
  (declare (cl:ignorable environment))
  defining-info)

(defmethod make-info
    (environment (defining-info symbol-macro-info))
  (make-instance 'symbol-macro-info
    :name (name defining-info)
    :expansion (expansion defining-info)
    :type
    (let ((entry (variable-type environment defining-info)))
      (type (if (null entry) defining-info entry)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; The main method on VARIABLE-INFO specialized to ENTRY. 

(defmethod variable-info ((environment entry) symbol)
  (let ((defining-info (defining-variable-info environment symbol)))
    (make-info environment defining-info)))

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
  (block-info (next environment)))

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
  (if (eq symbol (name environment))
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
  (tag-info (next environment)))

;;; This method implements the action to take when the argument is the
;;; global environment.  We detect this situation by the fact that the
;;; argument is not an ENTRY.  Since the global environment can not
;;; have any tags, it is safe to return NIL as the specification
;;; stipulates.
(defmethod tag-info (environment symbol)
  (declare (cl:ignorable environment symbol))
  nil)
