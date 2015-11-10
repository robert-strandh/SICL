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
      nil))

(defmethod defining-variable-info ((environment special-variable) symbol)
  (if (eq symbol (name environment))
      (make-instance 'special-variable-info
	:name symbol
	:global-p nil)
      nil))

(defmethod defining-variable-info ((environment symbol-macro) symbol)
  (if (eq symbol (name environment))
      (make-instance 'symbol-macro-info
	:name symbol
	:expansion (expansion environment))
      nil))

(defmethod defining-variable-info ((environment environment) symbol)
  (or (loop for entry in (augmentations environment)
	    when (defining-variable-info entry symbol)
	      return it)
      (definining-variable-info (global-environment environment))))

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

(defgeneric variable-type (environment symbol))

(defmethod variable-type (environment symbol)
  (declare (cl:ignore environment symbol))
  (values nil nil))

(defmethod variable-type ((environment variable-type) symbol)
  (if (eq (name environment) symbol)
      (values (type environment) t)
      (values nil nil)))

(defun find-variable-types (augmentations symbol sentinel)
  (loop with result = '()
	for augmentation in augmentations
	until (eq augmentation sentinel)
	do (multiple-value-bind (type valid-p)
	       (variable-type augmentation symbol)
	     (when valid-p
	       (push type result)))
	finally (return result)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function VARIABLE-IGNORE.

(defgeneric variable-ignore (environment symbol))

(defmethod variable-ignore (environment symbol)
  (declare (ignore environment symbol))
  (values nil nil))

(defmethod variable-ignore ((environment variable-ignore) symbol)
  (if (eq (name environment) symbol)
      (ignore environment)
      nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function VARIABLE-DYNAMIC-EXTENT.

(defgeneric variable-dynamic-extent (environment symbols))

(defmethod variable-dynamic-extent (environment symbol)
  (declare (cl:ignorable environment symbol))
  nil)

;;; This method is called when the entry is not related to the
;;; defining info instance. 
(defmethod variable-dynamic-extent ((environment entry) symbol)
  (declare (cl:ignorable environment symbol))
  nil)

(defmethod variable-dynamic-extent ((environment variable-dynamic-extent) symbol)
  (eq (name environment) symbol))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Methods on MAKE-INFO specialized to INFO classes returned by
;;; VARIABLE-INFO.

(defmethod make-info
    (environment (defining-info lexical-variable-info))
  (make-instance 'lexical-variable-info
    :name (name defining-info)
    :identity (identity defining-info)
    :type (cons 'and (variable-type environment defining-info))
    :ignore
    (let ((entry (variable-ignore environment defining-info)))
      (if (null entry) nil (ignore defining-info)))
    :dynamic-extent
    (let ((entry (variable-dynamic-extent environment defining-info)))
      (if (null entry) (dynamic-extent defining-info) t))))

(defmethod make-info
    (environment (defining-info special-variable-info))
  (make-instance 'special-variable-info
    :name (name defining-info)
    :type (cons 'and (variable-type environment defining-info))
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
    :type (cons 'and (variable-type environment defining-info))))

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
      nil))

(defmethod defining-function-info ((environment macro) symbol)
  (if (eq symbol (name environment))
      (make-instance 'local-macro-info
	:name symbol
	:expander (expander environment))
      nil))

;;; This method implements the action to take when the argument is an
;;; ENTRY, but it is not an entry defining a function.  We handle this
;;; situation by just making a recursive call, passing the next entry
;;; in the environment.
(defmethod defining-function-info ((environment environment) function-name)
  (or (loop for entry in (augmentations environment)
	    when (defining-function-info entry symbol)
	      return it)
      (definining-function-info (global-environment environment))))

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

(defgeneric function-type (environment symbol))

(defmethod function-type (environment symbol)
  (declare (ignore environment symbol))
  (values nil nil))

(defmethod function-type ((environment function-type) symbol)
  (if (eq (name environment) symbol)
      (values (type environment) t)
      (values nil nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function FUNCTION-IGNORE.

(defgeneric function-ignore (environment function-name))

(defmethod function-ignore (environment function-name)
  (declare (ignore environment function-name))
  (values nil nil))

(defmethod function-ignore ((environment function-ignore) function-name)
  (if (equal (name environment) function-name)
      (ignore environment)
      nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function FUNCTION-DYNAMIC-EXTENT.

(defgeneric function-dynamic-extent (environment function-name))

(defmethod function-dynamic-extent (environment function-name)
  (declare (ignore environment function-name))
  (values nil nil))

(defmethod function-dynamic-extent ((environment function-dynamic-extent) function-name)
  (if (equal (name environment) function-name)
      (dynamic-extent environment)
      nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function FUNCTION-INLINE.
;;;
;;; This function takes an environment and a defining info instance
;;; and returns the first entry in the environment that contains inline
;;; information for the defining info instance, or NIL if there is not
;;; such entry.

(defgeneric function-inline (environment function-name))

(defmethod function-inline ((environment entry) function-name)
  (declare (cl:ignorable environment defining-info))
  nil)

(defmethod function-inline ((environment inline) function-name)
  (if (equal (name environment) (name defining-info))
      (inline environment)
      nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Methods on MAKE-INFO specialized to INFO classes returned by
;;; FUNCTION-INFO.

(defmethod make-info
    (environment (defining-info local-function-info))
  (make-instance 'local-function-info
    :name (name defining-info)
    :identity (identity defining-info)
    :type (cons 'and (function-type environment defining-info))
    :ignore
    (let ((entry (function-ignore environment defining-info)))
      (if (null entry) nil (ignore entry)))
    :dynamic-extent
    (let ((entry (function-dynamic-extent environment defining-info)))
      (if (null entry) (dynamic-extent defining-info) t))))

(defmethod make-info
    (environment (defining-info global-function-info))
  (make-instance 'global-function-info
    :name (name defining-info)
    :type (cons 'and (function-type environment defining-info))
    :ignore
    (let ((entry (function-ignore environment defining-info)))
      (if (null entry) nil (ignore entry)))
    :inline (function-inline environment defining-info)
    :ast (ast defining-info)
    :compiler-macro (compiler-macro defining-info)
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
    ;; FIXME: add compiler-macro
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
      nil))

(defmethod block-info ((environment environment) symbol)
  (loop for entry in (augmentations environment)
	when (block-info entry symbol)
	  return it))

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
      nil))

(defmethod tag-info ((environment environment) symbol)
  (loop for entry in (augmentations environment)
	when (tag-info entry symbol)
	  return it))

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
;;; To determine the optimize info, we determine each component
;;; individually, and then create the final INFO instance.
;;;
;;; Each component is found by obtaining the first entry in the
;;; environment that corresponds, or of there are no entries, we get
;;; the value from the info instance returned for the global environment. 

(defgeneric quality-value (environment name))

;;; This method is called on the global environment
(defmethod quality-value (environment name)
  (funcall name (optimize-info environment)))

;;; This method is called when the entry is unrelated.
(defmethod quality-value ((environment entry) name)
  (quality-value (next environment) name))

;;; This method is called when we have an OPTIMIZE entry.
(defmethod quality-value ((environment optimize) name)
  (if (eq name (quality environment))
      ;; We found an entry with the right quality.  We are done.
      ;; Return its value
      (value environment)
      ;; It was an optimize entry, but it does not mention the
      ;; quality we are interested in.  Search further.  
      (quality-value (next environment) name)))

(defmethod optimize-info ((environment entry))
  (make-instance 'optimize-info
    :speed (quality-value environment 'speed)
    :debug (quality-value environment 'debug)
    :space (quality-value environment 'space)
    :compilation-speed (quality-value environment 'compilation-speed)
    :safety (quality-value environment 'safety)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; GLOBAL-ENVIRONMENT.

;;; This method is called on the global environment.
(defmethod global-environment (environment)
  environment)
