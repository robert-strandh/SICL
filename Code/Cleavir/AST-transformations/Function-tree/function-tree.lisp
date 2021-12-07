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
  ((%definition :initarg :definition :reader definition)
   (%assignments :initform '() :accessor assignments)
   (%uses :initform '() :accessor uses)))

(defclass node ()
  ((%function-ast :initarg :function-ast :reader function-ast)
   (%parent :initarg :parent :reader parent)
   (%children :initform '() :accessor children)
   (%defined-lexicals :initform '() :accessor defined-lexicals)
   (%used-lexicals :initform '() :accessor used-lexicals)
   (%assigned-lexicals :initform '() :accessor assigned-lexicals)))

(defvar *lexical-locations*)

(defun add-definition (lexical-location parent child-number node)
  (setf (gethash lexical-location *lexical-locations*)
        (make-instance 'lexical-location-information
          :definition
          (make-lexical-location-occurrence parent child-number node))))

(defun add-assignment (lexical-location parent child-number node)
  (push (make-lexical-location-occurrence parent child-number node)
        (assignments (gethash lexical-location *lexical-locations*))))

(defun add-use (lexical-location parent child-number node)
  (push (make-lexical-location-occurrence parent child-number node)
        (uses (gethash lexical-location *lexical-locations*))))

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

(defmethod traverse
    (client (parent cleavir-ast:function-ast) child child-number root-node)
  (let ((node (make-instance 'node
                :function-ast parent
                :parent root-node))
        (lambda-list (cleavir-ast:lambda-list parent)))
    (tagbody
     required
       ;; It is possible that LAMBDA-LIST is empty, or that it starts
       ;; with &OPTIONAL, &REST, &KEY, or a LEXICAL-AST representing a
       ;; required parameter.
       (if (null lambda-list)
           (go out)
           (let ((next (pop lambda-list)))
             (case next
               (&optional (pop lambda-list) (go optional))
               (&rest (pop lambda-list) (go rest))
               (&key (pop lambda-list) (go key))
               (t
                ;; We have a required parameter.  Add it as a definition.
                (add-definition next parent -1 node)
                ;; Process remaining required parameters if any.
                (go required)))))
     optional
       ;; We have seen &OPTIONAL which has been popped off
       ;; LAMBDA-LIST.  At this point, it is possible that LAMBDA-LIST
       ;; is empty, or that it starts with &REST, with &KEY, or with a
       ;; list of two LEXICAL-ASTs that represent an optional
       ;; parameter and its associated SUPPLIED-P parameter.
       (if (null lambda-list)
           (go out)
           (let ((next (pop lambda-list)))
             (case next
               (&rest (pop lambda-list) (go rest))
               (&key (pop lambda-list) (go key))
               (t
                ;; We have an optional parameter represented as a list
                ;; of two LEXICAL-ASTs.  Add them as definitions.
                (add-definition (first next) parent -1 node)
                (add-definition (second next) parent -1 node)
                ;; Process remaining optional parameters if any.
                (go optional)))))
     rest
       ;; We have seen &REST which has been popped off LAMBDA-LIST.
       ;; At this point, there must be a rest parameter first on
       ;; LAMBDA-LIST.  So we pop it off and push it onto the list of
       ;; defined lexicals for this node.
       (push (pop lambda-list) (defined-lexicals node))
       (if (null lambda-list)
           (go out)
           ;; If LAMBDA-LIST is not empty, then it must start with &KEY.
           (progn (pop lambda-list)
                  (go key)))
     key
       ;; We have seen &KEY which has been popped off LAMBDA-LIST.  At
       ;; this point, it is possible that LAMBDA-LIST is empty, or
       ;; that it starts with &ALLOW-OTHER-KEYS or a keyword parameter
       ;; in the form of a list of three elements.
       (if (null lambda-list)
           (go out)
           (let ((next (pop lambda-list)))
             (case next
               (&allow-other-keys (go out))
               (t
                ;; We have a keyword parameter in the form of a list
                ;; containing a symbol and two LEXICAL-ASTs.  Add the
                ;; LEXICAL-ASTs as definitions.
                (add-definition (second next) parent -1 node)
                (add-definition (third next) parent -1 node)
                ;; Process remaining keyword parameters if any.
                (go key)))))
     out)
    (push node (children root-node))
    (if (typep child 'cleavir-ast:lexical-ast)
        (add-use child parent 0 node)
        (traverse-children client child node))))

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
