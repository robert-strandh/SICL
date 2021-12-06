(cl:in-package #:cleavir-ast-function-tree)

(defclass parent-information ()
  ((%parent :initarg :parent :reader parent)
   (%child-number :initarg :child-number :reader child-number)))

(defun compute-parents (ast)
  (let ((result (make-hash-table :test #'eq)))
    (cleavir-ast:map-ast-depth-first-preorder
     (lambda (ast)
       (loop for child in (cleavir-ast:children ast)
             for i from 0
             do (push (make-instance 'parent-information
                        :parent ast
                        :child-number i)
                      (gethash child result))))
     ast)
    result))

(defclass node ()
  ((%function-ast :initarg :function-ast :reader function-ast)
   (%parent :initarg :parent :reader parent)
   (%children :initform '() :accessor children)
   (%defined-lexicals :initform '() :accessor defined-lexicals)
   (%used-lexicals :initform '() :accessor used-lexicals)
   (%assigned-lexicals :initform '() :accessor assigned-lexicals)))

(defvar *visited*)

(defgeneric traverse (client ast root-node))

(defmethod traverse :around (client ast root-node)
  (unless (gethash ast *visited*)
    (setf (gethash ast *visited*) t)
    (call-next-method)))

(defmethod traverse (client ast root-node)
  (loop for child in (cleavir-ast:children ast)
        do (traverse client child root-node)))

(defmethod traverse (client (ast cleavir-ast:function-ast) root-node)
  (let ((node (make-instance 'node
                :function-ast ast
                :parent root-node))
        (lambda-list (cleavir-ast:lambda-list ast)))
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
                ;; We have a required parameter.  Push it onto the
                ;; list of defined lexicals for this node.
                (push next (defined-lexicals node))
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
                ;; of two LEXICAL-ASTs.  Push them onto the list of
                ;; defined lexicals for this node.
                (push (first next) (defined-lexicals node))
                (push (second next) (defined-lexicals node))
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
                ;; containing a symbol and two LEXICAL-ASTs.  Push the
                ;; LEXICAL-ASTs onto the list of defined lexicals for
                ;; this node.
                (push (second next) (defined-lexicals node))
                (push (third next) (defined-lexicals node))
                ;; Process remaining keyword parameters if any.
                (go key)))))
     out)
    (push node (children root-node))
    (traverse client (cleavir-ast:body-ast ast) node)))

(defmethod traverse (client (ast cleavir-ast:lexical-bind-ast) root-node)
  (push (cleavir-ast:lexical-variable-ast ast)
        (defined-lexicals root-node))
  (traverse client (cleavir-ast:value-ast ast) root-node))

(defmethod traverse (client (ast cleavir-ast:setq-ast) root-node)
  (push (cleavir-ast:lexical-variable-ast ast)
        (assigned-lexicals root-node))
  (traverse client (cleavir-ast:value-ast ast) root-node))

(defmethod traverse (client (ast cleavir-ast:lexical-ast) root-node)
  (push ast (used-lexicals root-node)))

(defun function-tree (client ast)
  (let ((root-node (make-instance 'node
                     :function-ast nil
                     :parent nil))
        (*visited* (make-hash-table :test #'eq)))
    (traverse client ast root-node)
    root-node))
