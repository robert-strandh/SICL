(cl:in-package #:cleavir-environment)

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
  (declare (ignorable environment symbol))
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
  (declare (ignorable environment symbol))
  nil)
