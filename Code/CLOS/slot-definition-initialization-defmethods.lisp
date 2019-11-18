(cl:in-package #:sicl-clos)

;;;; This file contains methods for initializing slot definition
;;;; metaobjects according to the description on the following page:
;;;; http://metamodular.com/CLOS-MOP/initialization-of-slot-definition-metaobjects.html

(defmethod initialize-instance :before
    ((slot-definition slot-definition)
     &key
       (name nil name-p)
     &allow-other-keys)
  (declare (ignore name))
  ;; During initialization the NAME keyword argument must be supplied.
  (unless name-p
    (error 'slot-definition-name-must-be-supplied
           :slot-definition slot-definition)))

(defmethod shared-initialize :before
    ((slot-definition slot-definition)
     slot-names
     &key
       ;; If the :NAME initargs is not supplied, then it must be the
       ;; case that we are re-initializing, and in that case we can
       ;; get the name from the slot-definition metaobject itself.
       (name (slot-definition-name slot-definition))
       (initform nil initform-p)
       (initfunction nil initfunction-p)
       type
       allocation
       initargs
       readers
       writers
       documentation
     &allow-other-keys)
  (declare (ignore initform initfunction))
  ;; FIXME: Remove this IGNORE declaration of TYPE once we have code
  ;; to check its validity.
  (declare (ignore type))
  (when (and initform-p (not initfunction-p))
    (error 'when-initform-supplied-initfunction-must-be-supplied
           :slot-definition slot-definition
           :name name))
  (when (and (not initform-p) initfunction-p)
    (error 'when-initfunction-supplied-initform-must-be-supplied
           :slot-definition slot-definition
           :name name))
  ;; FIXME: Check that if the TYPE keywords argument is supplied it is
  ;; a valid type specifier.
  (unless (symbolp allocation)
    (error 'allocation-must-be-symbol
           :slot-definition slot-definition
           :allocation allocation))
  (unless (cleavir-code-utilities:proper-list-p initargs)
    (error 'initargs-must-be-proper-list
           :slot-definition slot-definition
           :initargs initargs))
  (unless (every #'symbolp initargs)
    (error 'initarg-must-be-symbol
           :slot-definition slot-definition
           :initarg (find-if-not #'symbolp initargs)))
  (unless (cleavir-code-utilities:proper-list-p readers)
    (error 'readers-must-be-proper-list
           :slot-definition slot-definition
           :readers readers))
  ;; The AMOP says that the readers must be function names, but it
  ;; would be silly to have a reader named (SETF <mumble>) so we
  ;; require that the readers be symbols.
  (unless (every #'symbolp readers)
    (error 'initarg-must-be-symbol
           :slot-definition slot-definition
           :reader (find-if-not #'symbolp readers)))
  (unless (cleavir-code-utilities:proper-list-p writers)
    (error 'writers-must-be-proper-list
           :slot-definition slot-definition
           :writers writers))
  (loop for writer in writers
        unless (or (symbolp writer)
                   (and (consp writer)
                        (eq (car writer) 'setf)
                        (consp (cdr writer))
                        (symbolp (cadr writer))
                        (null (cddr writer))))
          do (error 'writer-must-be-function-name
                    :slot-definition slot-definition
                    :writer writer))
  (unless (or (null documentation) (stringp documentation))
    (error 'slot-definition-documentation-must-be-nil-or-string
           :slot-definition slot-definition
           :documentation documentation)))
