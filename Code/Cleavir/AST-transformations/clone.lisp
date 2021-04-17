(cl:in-package #:cleavir-ast-transformations)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Cloning an AST.

;;; In step one of cloning an AST, we create a dictionary mapping
;;; every node in the original AST to a cloned node.  The cloned node
;;; is created by calling MAKE-INSTANCE on the class of the original
;;; node.  No initialization arguments are passed to MAKE-INSTANCE, so
;;; the cloned node is uninitialized except for slots that are
;;; initialized by :INITFORM.
(defun clone-create-dictionary (ast)
  (let ((dictionary (make-hash-table :test #'eq)))
    (cleavir-ast:map-ast-depth-first-preorder
     (lambda (node)
       (setf (gethash node dictionary)
             (make-instance (class-of node))))
     ast)
    dictionary))

;;; This generic function is used to obtain some substructure
;;; (typically a slot) for the new AST, given the corresponding
;;; substructure of the model AST.
(defgeneric finalize-substructure (object dictionary))

;;; For most objects, such as numbers, symbols, strings, etc, the
;;; object to be used in the new AST is the same as the one in the
;;; original AST.
(defmethod finalize-substructure (object dictionary)
  (declare (ignore dictionary))
  object)

;;; If the substructure of an AST is a CONS (typically a proper list),
;;; then we obtain the corresponding substructure of the new AST by
;;; copying the CONS and calling FINALIZE-SUBSTRUCTURE on the CAR and
;;; the CDR.
(defmethod finalize-substructure ((object cons) dictionary)
  (cons (finalize-substructure (car object) dictionary)
        (finalize-substructure (cdr object) dictionary)))

;;; If the substructure of an AST is another AST, then the
;;; corresponding substructure of the new AST is obtained from the
;;; dictionary.
;;; If it's not in the dictionary, it is not part of the AST being
;;; cloned, e.g. because it's an outer block. So use the original.
(defmethod finalize-substructure ((object cleavir-ast:ast) dictionary)
  (multiple-value-bind (ast present-p)
      (gethash object dictionary)
    (if present-p
        ast
        object)))

;;; Given an AST to finalize and the MODEL AST of which the AST is a
;;; clone,  reinitialize the AST with new substructure.
(defun finalize (ast model dictionary)
  (apply #'reinitialize-instance
         ast
         (loop for (keyword reader) in (cleavir-io:save-info model)
               for value = (funcall reader model)
               collect keyword
               collect (finalize-substructure value dictionary))))

(defun clone-ast (ast)
  (let ((dictionary (clone-create-dictionary ast)))
    (maphash (lambda (key value)
               (finalize value key dictionary))
             dictionary)
    (gethash ast dictionary)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generating code for cloning an AST.

;;; In step one, we create a dictionary mapping every node in the
;;; original AST to a symbol that will be used as a local variable in
;;; the generated code.  That variable will hold the cloned AST.
(defun codegen-clone-create-dictionary (ast)
  (let ((dictionary (make-hash-table :test #'eq)))
    (cleavir-ast:map-ast-depth-first-preorder
     (lambda (node)
       (setf (gethash node dictionary) (gensym)))
     ast)
    dictionary))

(defgeneric codegen-finalize-substructure (object dictionary))

(defmethod codegen-finalize-substructure (object dictionary)
  (declare (ignore dictionary))
  `',object)

(defmethod codegen-finalize-substructure ((object cons) dictionary)
  `(cons ,(codegen-finalize-substructure (car object) dictionary)
         ,(codegen-finalize-substructure (cdr object) dictionary)))

(defmethod codegen-finalize-substructure ((object cleavir-ast:ast) dictionary)
  (multiple-value-bind (ast present-p)
      (gethash object dictionary)
    (if present-p
        ast
        object)))

(defun codegen-finalize (model dictionary)
  `(reinitialize-instance
    ,(gethash  model dictionary)
    ,@(loop for (keyword reader) in (cleavir-io:save-info model)
            for value = (funcall reader model)
            collect keyword
            collect (codegen-finalize-substructure value dictionary))))

(defun codegen-clone-ast (ast)
  (let ((dictionary (codegen-clone-create-dictionary ast)))
    `(let ,(loop for key being each hash-key of dictionary
                   using (hash-value variable)
                 for class = (class-of key)
                 for class-name = (class-name class)
                 collect `(,variable (make-instance ',class-name)))
       ,@(loop for key being each hash-key of dictionary
                 using (hash-value variable)
               collect (codegen-finalize key dictionary))
       ,(gethash ast dictionary))))
