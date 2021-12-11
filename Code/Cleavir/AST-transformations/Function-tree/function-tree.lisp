(cl:in-package #:cleavir-ast-function-tree)

(defclass lexical-location-occurrence ()
  ((%parent :initarg :parent :reader parent)
   (%child-number :initarg :child-number :reader child-number)
   (%node :initarg :node :reader node)))

(defun make-lexical-location-occurrence (parent child-number node)
  (make-instance 'lexical-location-occurrence
    :parent parent
    :child-number child-number
    :node node))

(defclass lexical-location-information ()
  (;; When processing is done, this slot will contain a single
   ;; instance of LEXICAL-LOCATION-OCCURRENCE corresponding to the
   ;; definition of the LEXICAL-AST.
   (%definition :initform nil :accessor definition)
   ;; When processing is done, this slot will contain a list of
   ;; instances of LEXICAL-LOCATION-OCCURRENCE corresponding to the
   ;; occurrences of the LEXICAL-AST in the left-hand side of a
   ;; SETQ-AST.
   (%assignments :initform '() :accessor assignments)
   ;; When processing is done, this slot will contain a list of
   ;; instances of LEXICAL-LOCATION-OCCURRENCE corresponding to the
   ;; occurrences of the LEXICAL-AST in any value position.
   (%uses :initform '() :accessor uses)))

(defclass node ()
  ((%function-ast :initarg :function-ast :reader function-ast)
   (%parent :initarg :parent :reader parent)
   (%children :initform '() :accessor children)))

;;; This variable contains an EQ hash table with lexical locations as
;;; keys, and instances of LEXICAL-LOCATION-INFORMATION as values.
(defvar *lexical-locations*)

(defun ensure-lexical-location-information (lexical-location)
  (let ((location-information
          (gethash lexical-location *lexical-locations*)))
    (if (null location-information)
        (setf (gethash lexical-location *lexical-locations*)
              (make-instance 'lexical-location-information))
        location-information)))

(defun add-definition (lexical-location parent child-number node)
  (let ((location-information
          (ensure-lexical-location-information lexical-location)))
    (setf (definition location-information)
          (make-lexical-location-occurrence parent child-number node))))

(defun add-assignment (lexical-location parent child-number node)
  (let ((location-information
          (ensure-lexical-location-information lexical-location)))
    (push (make-lexical-location-occurrence parent child-number node)
          (assignments location-information))))

(defun add-use (lexical-location parent child-number node)
  (let ((location-information
          (ensure-lexical-location-information lexical-location)))
    (push (make-lexical-location-occurrence parent child-number node)
          (uses location-information))))

(defvar *visited*)

(defgeneric traverse (client parent child child-number root-node))

(defun traverse-children (client parent root-node)
  (loop for child in (cleavir-ast:children parent)
        for child-number from 0
        do (traverse client parent child child-number root-node)))

(defmethod traverse :around (client parent child child-number root-node)
  (let ((key (cons parent child)))
    (unless (gethash key *visited*)
      (setf (gethash key *visited*) t)
      (call-next-method))))

(defmethod traverse (client parent child child-number root-node)
  (traverse-children client child root-node))

(defmethod traverse
    (client parent (child cleavir-ast:lexical-ast) child-number root-node)
  (add-use child parent child-number root-node))

;;; This method is called when the BODY-AST is not a LEXICAL-AST.
(defmethod traverse
    (client
     (parent cleavir-ast:function-ast)
     child
     (child-number (eql 0))
     root-node)
  (traverse-children client child root-node))

;;; This method is called when the BODY-AST is a LEXICAL-AST.
(defmethod traverse
    (client
     (parent cleavir-ast:function-ast)
     (child cleavir-ast:lexical-ast)
     (child-number (eql 0))
     root-node)
  (add-use child parent 0 root-node))

;;; This method is called when CHILD-NUMBER is never not 0.  Then
;;; CHILD is a parameter.
(defmethod traverse
    (client
     (parent cleavir-ast:function-ast)
     (child cleavir-ast:lexical-ast)
     child-number
     root-node)
  (add-definition child parent child-number root-node))

(defmethod traverse
    (client
     (parent cleavir-ast:lexical-bind-ast)
     child
     (child-number (eql 1))
     root-node)
  (traverse-children client child root-node))

(defmethod traverse
    (client
     (parent cleavir-ast:lexical-bind-ast)
     (child cleavir-ast:lexical-ast)
     (child-number (eql 0))
     root-node)
  (add-definition child parent child-number root-node))

(defmethod traverse
    (client
     (parent cleavir-ast:lexical-bind-ast)
     (child cleavir-ast:lexical-ast)
     (child-number (eql 1))
     root-node)
  (add-use child parent child-number root-node))

(defmethod traverse
    (client
     (parent cleavir-ast:setq-ast)
     child
     (child-number (eql 1))
     root-node)
  (traverse-children client child root-node))

(defmethod traverse
    (client
     (parent cleavir-ast:setq-ast)
     (child cleavir-ast:lexical-ast)
     (child-number (eql 0))
     root-node)
  (add-assignment child parent child-number root-node))

(defmethod traverse
    (client
     (parent cleavir-ast:setq-ast)
     (child cleavir-ast:lexical-ast)
     (child-number (eql 1))
     root-node)
  (add-use child parent child-number root-node))

(defun function-tree (client ast)
  (let ((root-node (make-instance 'node
                     :function-ast nil
                     :parent nil))
        (*lexical-locations* (make-hash-table :test #'eq))
        (*visited* (make-hash-table :test #'equal)))
    (traverse client nil ast -1 root-node)
    (values root-node *lexical-locations*)))
