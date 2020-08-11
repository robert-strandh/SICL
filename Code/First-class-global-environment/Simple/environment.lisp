(cl:in-package #:sicl-simple-environment)

;;;; This file contains definitions of classes, functions, and
;;;; variables for creating and manipulating global environments.  It
;;;; contains no definitions involving symbols in the COMMON-LISP
;;;; package.  Such definitions are located in separate files. 

;;;; An environment contains ENTRIES of various kinds.  An entry can
;;;; be a BASE ENTRY.  This is the kind of entry used for variables,
;;;; functions, and macros, but also for autonomous declarations such
;;;; as OPTIMIZE.  Other entries are AUXILIARY entries, in that they
;;;; provide additional information about other entries.  This type of
;;;; entry is used for declarations of type, inline, and dynamic
;;;; extent, but a compiler-macro entry is also an auxiliary entry.
;;;;
;;;; When the information about some entity is wanted at some point in
;;;; the compilation process, the current environment is first
;;;; searched for a BASE ENTRY.  Then, it is searched again for
;;;; auxiliary entries that provide additional information about the
;;;; particular base entry that was found.  All this information is
;;;; then combined into an object called an INFO object which is
;;;; finally returned.

(deftype function-name ()
  `(or symbol (cons (eql setf) (cons symbol null))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Global environment.
;;;
;;; The global environment is divided into namespaces, but a namespace
;;; may be represented in several different slots.  So for instance,
;;; what is usually considered the FUNCTION namespace, we have divided
;;; into four separate lists, consisting of global function entries,
;;; global macro entries, special operator entries and compiler-macro
;;; entries.  Likewise, what is usually considered the VARIABLE
;;; namespace, we have divided into three separate lists, consisting
;;; of special variable entries, constant variable entries and global
;;; symbol macro entries.

(defclass simple-environment (sicl-global-environment:environment)
  (;; A value used in this environment for unbound variables. 
   (%unbound :initform (list nil) :reader unbound)
   ;; The package namespace.  A hash table mapping names of packages
   ;; (i.e., strings) to package objects.
   (%packages :initform (make-hash-table :test #'equal)
              :accessor packages)
   ;; The class namespace.  An #'EQ hash table mapping names to
   ;; classes.
   (%classes :initform (make-hash-table :test #'eq) :accessor classes)
   ;; This slot holds an association list, mapping symbols to
   ;; type-expander functions.
   (%type-expanders :initform '() :accessor type-expanders)
   ;; The typed structure namespace, which is separate from
   ;; both the class and type namespaces, and only used by defstruct.
   (%structure-entries :initform (make-hash-table :test #'eq)
                       :accessor structure-entries)
   ;; This slot holds a list of variable entries.
   (%variable-entries :initform '() :accessor variable-entries)
   ;; This slot holds an #'EQUAL hash table of function entries.
   (%function-entries :initform (make-hash-table :test #'equal)
                      :accessor function-entries)
   ;; This slot holds an #'EQ hash table mapping a function object to
   ;; a list of names of that function in this environment
   (%function-names :initform (make-hash-table :test #'eq)
                    :accessor function-names)
   ;; This slot holds an association list, mapping function names to
   ;; compiler-macro functions.
   (%compiler-macro-expanders :initform '() :accessor compiler-macro-expanders)
   ;; Some entries in this list are base entries, such as OPTIMIZE and
   ;; DECLARATION.  Others are auxiliary entries such as TYPE, INLINE,
   ;; DYNAMIC-EXTENT.
   (%proclamations :initform '() :accessor proclamations)
   ;; This slot holds an EQ hash table, mapping symbols to
   ;; method-combination templates.
   (%method-combination-templates :initform (make-hash-table :test #'eq)
                                  :accessor method-combination-templates)
   ;; The value of this slot is either a SETF expander function, or
   ;; NIL if the function name of this entry does not have a SETF
   ;; expander associated with it.
   (%setf-expanders :initform nil :accessor setf-expanders)
   ;; The default SETF expander, to be used when no other SETF
   ;; expander exists.
   (%default-setf-expander :initform nil :accessor default-setf-expander)
   ;; An EQ hash table that acts as a dictionary, recording
   ;; proclamations of DECLARATION.
   (%declarations :initform (make-hash-table :test #'eq)
                  :reader declarations)
   ;; An EQ hash table containing property lists for symbols.
   (%property-lists :initform (make-hash-table :test #'eq)
                    :reader property-lists)
   ;; An association list where each element is of the form
   ;; (QUALITY-NAME VALUE) for each OPTIMIZE quality allowed in this
   ;; environment.
   (%optimize-quality-values
    :initform '((speed 0) (compilation-speed 0)
                (space 0) (debug 3) (safety 3))
    :accessor sicl-global-environment:optimize-quality-values)
   ;; A compilation policy.
   (%policy
    :reader sicl-global-environment:policy)
   (%client :initarg :client :reader sicl-global-environment:client)))

;;; We do not use backquotes in this function, because we sometimes
;;; need to read and compile this code in the host during
;;; bootstrapping, but have the result be used by the extrinsic
;;; compiler.  However, the extrinsic compiler uses the Eclector
;;; version of the backquote macro, and that is not necessarily
;;; compatible with that of the host.
(defmethod initialize-instance :after ((instance simple-environment) &key)
  (setf (default-setf-expander instance)
        (lambda (form environment)
          (declare (ignore environment))
          (if (symbolp form)
              (let ((new (gensym)))
                (values '()
                        '()
                        (list new)
                        (list 'setq form new)
                        form))
              (let ((temps (loop for arg in (rest form) collect (gensym)))
                    (new (gensym)))
                (values temps
                        (rest form)
                        (list new)
                        (list* 'funcall
                               (list 'function (list 'setf (first form)))
                               new
                               temps)
                        (cons (first form) temps)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Methods to keep the policy consistent with the optimize values.

;;; Not sure this is totally correct.
(defmethod shared-initialize :after
    ((instance simple-environment) slot-names &key)
  (declare (ignore slot-names))
  (setf (slot-value instance '%policy)
        (cleavir-policy:compute-policy
         (sicl-genv:optimize-quality-values instance)
         instance)))

(defmethod (setf sicl-genv:optimize-quality-values) :after
    (values (environment simple-environment))
  (setf (slot-value environment '%policy)
        (cleavir-policy:compute-policy
         (sicl-genv:optimize-quality-values environment)
         environment)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Environment entries.

(defclass entry ()
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; NAMED-ENTRY.
;;; 
;;; A mixin class for entries having a name associated with them. 
(defclass named-entry ()
  ((%name :initarg :name :reader name)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; The nature of the entry.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; DEFINITION-ENTRY.
;;;
;;; For entries that have a complete definition in the environment.
;;; This is the case for macros, symbol macros, and constant
;;; variables.

(defclass definition-entry ()
  ((%definition :initarg :definition :accessor definition)))

(defgeneric definition-entry-p (object))

(defmethod definition-entry-p (object)
  (declare (ignore object))
  nil)

(defmethod definition-entry-p ((object definition-entry))
  (declare (ignorable object))
  t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class VARIABLE-ENTRY.

(defclass variable-entry (named-entry)
  (;; This slot contains true if and only if this entry represents a
   ;; constant variable.
   (%constantp :initform nil :accessor constantp)
   ;; This slot contains true if and only if this entry represents a
   ;; special variable.
   (%specialp :initform nil :accessor specialp)
   ;; This slot contains a CONS cell which is the value cell
   ;; associated with the variable.  The CAR of the CONS cell contains
   ;; the global value of the variable.  If there is no value
   ;; associated with the variable, then the CAR of the value cell
   ;; contains the contents of the UNBOUND slot in the environment.
   (%value-cell :initform (list nil) :reader value-cell)
   ;; The value of this slot is either a macro function, or NIL if
   ;; this entry does not currently represent a symbol macro.
   (%macro-function :initform nil :accessor macro-function)
   ;; When this entry represents a symbol macro, then this slot
   ;; contains the expansion for that symbol macro, i.e., a form that
   ;; the symbol macro expands to.  When this entry does not represent
   ;; a symbol macro, this slot contains NIL.
   (%expansion :initform nil :accessor expansion)
   ;; The value of this slot is the proclaimed type of the variable
   ;; whenever this entry is associated with a variable.
   (%type :initform t :accessor type)))

(defun find-variable-entry (environment name)
  (let ((entry (find name (variable-entries environment)
                     :test #'eq :key #'name)))
    (cond (entry entry)
          ((keywordp name)
           (setf entry (make-instance 'variable-entry :name name)
                 (constantp entry) t
                 (car (value-cell entry)) name)
           (push entry (variable-entries environment))
           entry)
          (t nil))))

(defun ensure-variable-entry (environment name)
  (let ((entry (find-variable-entry environment name)))
    (when (null entry)
      (setf entry
            (make-instance 'variable-entry :name name))
      (setf (car (value-cell entry))
            (unbound environment))
      (push entry (variable-entries environment)))
    entry))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class FUNCTION-ENTRY.
;;;
;;; A function entry represents information of a name used as a
;;; function, whether it is an ordinary function, a generic function,
;;; a macro, or a special operator.
;;;
;;; Whenever some non-trivial information about some function name is
;;; required, an instance of this class is created, and then never
;;; removed.

(defclass function-entry (named-entry)
  (;; The value of this slot is a CONS cell in which the CAR is always
   ;; a function.  When the function is unbound, it contains the
   ;; function in the UNBOUND slot, which signals an error.
   (%function-cell :initform (list nil) :reader function-cell)
   ;; The value of this slot is a function that signals an error
   ;; indicating that the function is unbound.  The function
   ;; represented by this entry is unbound if and only if the function
   ;; in this slot is the contents of the CAR of the function cell in
   ;; the FUNCTION-CELL slot.
   (%unbound :initarg :unbound :reader unbound)
   ;; The value of this slot is either a macro function, or NIL if
   ;; this entry does not currently represent a macro. 
   (%macro-function :initform nil :accessor macro-function)
   ;; The value of this slot is a generalized boolean indicating
   ;; whether this entry represents a special operator.
   (%special-operator :initform nil :accessor special-operator)
   ;; The value of this slot is the proclaimed type of the function
   ;; whenever this entry has a function associated with it.
   (%type :initform t :accessor type)
   ;; The value of this slot is either the abstract syntax tree
   ;; corresponding to the function, or NIL if no abstract syntax tree
   ;; has been associated with the function.
   (%ast :initform nil :initarg :ast :accessor ast)
   ;; The value of this slot is the proclaimed inline information
   ;; whenever this entry has a function associated with it.
   (%inline :initform nil :accessor inline)
   ;; The value of this slot is the lambda list of the function, or
   ;; NIL if no lambda list is known for the function.
   (%lambda-list :initform nil :accessor lambda-list)
   ;; The value of this slot is true if the lambda list for the
   ;; function is know and false otherwise.
   (%lambda-list-valid-p :initform nil :accessor lambda-list-valid-p)))

;;; When an instance of FUNCTION-ENTRY is created, we need to set the
;;; slot UNBOUND to a function that calls ERROR.  We can not make this
;;; function an :INITFORM because it must contain the name of the
;;; entry which is not known until after the instance has been
;;; created.  We also need to set the CAR of the FUNCTION-CELL to the
;;; newly created function.
(defmethod initialize-instance :after ((entry function-entry)
                                       &key environment
                                       &allow-other-keys)
  (let* ((name (name entry))
         (fun (lambda (&rest args)
                (declare (ignore args))
                (error 'undefined-function :name name :environment environment))))
    (reinitialize-instance entry :unbound fun)
    (setf (car (function-cell entry)) fun)))

(defgeneric function-entry-p (object)
  (:method (object)
    (declare (ignore object))
    nil)
  (:method ((object function-entry))
    (declare (ignorable object))
    t))

(defun find-function-entry (environment name)
  (assert (or (symbolp name)
              (and (consp name)
                   (consp (cdr name))
                   (null (cddr name))
                   (eq (car name) 'setf)
                   (symbolp (cadr name)))))
  (gethash name (function-entries environment)))

(defun ensure-function-entry (environment name)
  (let ((entry (find-function-entry environment name)))
    (when (null entry)
      (setf entry
            (make-instance 'function-entry
              :name name
              :environment environment))
      (setf (gethash name (function-entries environment)) entry))
    entry))
