(cl:in-package #:cleavir-environment)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; VARIABLE-INFO


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
;;; just making a recursive call, passing the next entry in the list.
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
;;; just making a recursive call, passing the next entry in the list.
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
