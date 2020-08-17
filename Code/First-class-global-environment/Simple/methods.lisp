(cl:in-package #:sicl-simple-environment)

;;; Utility function for associating a name with a function.
(defun add-function-name (function function-name env)
  (pushnew function-name
           (gethash function (function-names env))
           :test #'equal))

;;; Utility function for disassociating a name from a function.
(defun remove-function-name (function function-name env)
  (setf (gethash function (function-names env))
        (remove function-name (gethash function (function-names env))
                :test #'equal))
  (when (null (gethash function (function-names env)))
    (remhash function (function-names env))))

;;; Recall that this function should return true if FUNCTION-NAME has
;;; a definition in ENVIRONMENT as an ordinary function, a generic
;;; function, a macro, or a special operator.
(defmethod sicl-genv:fboundp (function-name (env simple-environment))
  (let ((entry (find-function-entry env function-name)))
    (and (not (null entry))
         (or (not (eq (car (function-cell entry))
                      (unbound entry)))
             (not (null (macro-function entry)))
             (not (null (special-operator entry)))))))

(defmethod sicl-genv:fmakunbound (function-name (env simple-environment))
  (let ((entry (find-function-entry env function-name)))
    (unless (null entry)
      (remove-function-name (car (function-cell entry)) function-name env)
      (setf (car (function-cell entry))
            (unbound entry))
      (setf (macro-function entry) nil)
      (setf (special-operator entry) nil)
      (setf (type entry) t)
      (setf (inline entry) nil))))

(defmethod sicl-genv:special-operator (function-name (env simple-environment))
  (let ((entry (find-function-entry env function-name)))
    (if (null entry)
        nil
        (special-operator entry))))

(defmethod (setf sicl-genv:special-operator)
    (new-definition function-name (env simple-environment))
  (let ((entry (find-function-entry env function-name)))
    (cond ((and (null entry) (null new-definition))
           nil)
          ((null entry)
           (setf entry (ensure-function-entry env function-name))
           (setf (special-operator entry) new-definition))
          ((not (eq (car (function-cell entry))
                    (unbound entry)))
           (error 'redefinition :name function-name
                                :oldtype :function :newtype :special-operator))
          ((not (null (macro-function entry)))
           (error 'redefinition :name function-name
                                :oldtype :macro :newtype :special-operator))
          (t
           (setf (special-operator entry) new-definition)))))

(defmethod sicl-genv:fdefinition (function-name (env simple-environment))
  (let ((entry (find-function-entry env function-name)))
    (cond ((null entry)
           (error 'undefined-function :name function-name))
          ((not (eq (car (function-cell entry))
                    (unbound entry)))
           (car (function-cell entry)))
          ((not (null (macro-function entry)))
           `(cl:macro-function ,(macro-function entry)))
          ((not (null (special-operator entry)))
           `(cl:special (special-operator entry)))
          (t
           (error 'undefined-function :name function-name)))))

(defmethod (setf sicl-genv:fdefinition)
    (new-definition function-name (env simple-environment))
  (assert (functionp new-definition))
  (let ((entry (ensure-function-entry env function-name)))
    (cond ((not (null (special-operator entry)))
           (error 'redefinition :name function-name
                                :oldtype :special-operator :newtype :function))
          (t
           (remove-function-name (car (function-cell entry)) function-name env)
           (setf (car (function-cell entry)) new-definition)
           (add-function-name new-definition function-name env)
           (setf (macro-function entry) nil)
           (setf (type entry) t)
           (setf (inline entry) nil)
           new-definition))))

(defmethod sicl-genv:macro-function (symbol (env simple-environment))
  (let ((entry (find-function-entry env symbol)))
    (if (null entry)
        nil
        (macro-function entry))))

(defmethod (setf sicl-genv:macro-function)
    (new-definition function-name (env simple-environment))
  (assert (functionp new-definition))
  (let ((entry (ensure-function-entry env function-name)))
    (remove-function-name (car (function-cell entry)) function-name env)
    (setf (car (function-cell entry)) (unbound entry))
    (setf (macro-function entry) new-definition)
    (setf (type entry) t)
    (setf (inline entry) nil)
    new-definition))

(defmethod sicl-genv:compiler-macro-function
    (function-name (env simple-environment))
  (let ((entry (assoc function-name (compiler-macro-expanders env)
                      :test #'equal)))
    (if (null entry)
        nil
        (cdr entry))))

(defmethod (setf sicl-genv:compiler-macro-function)
    (new-definition function-name (env simple-environment))
  (assert (or (functionp new-definition) (null new-definition)))
  (if (null new-definition)
      (setf (compiler-macro-expanders env)
            (remove function-name (compiler-macro-expanders env)
                    :key #'car :test #'equal))
      (let ((entry (assoc function-name (compiler-macro-expanders env)
                          :test #'equal)))
        (if (null entry)
            (push (cons function-name new-definition)
                  (compiler-macro-expanders env))
            (setf (cdr entry) new-definition))))
  new-definition)

(defmethod sicl-genv:function-type (function-name (env simple-environment))
  (let ((entry (find-function-entry env function-name)))
    (if (or (null entry)
            (not (null (special-operator entry)))
            (not (null (macro-function entry))))
        nil
        (type entry))))

(defmethod (setf sicl-genv:function-type)
    (new-type function-name (env simple-environment))
  (let ((entry (ensure-function-entry env function-name)))
    (cond ((not (null (special-operator entry)))
           (error 'attempt-to-proclaim-type-of-special-operator
                  :name  function-name))
          ((not (null (macro-function entry)))
           (error 'attempt-to-proclaim-type-of-macro
                  :name  function-name))
          (t
           (setf (type entry) new-type)))))

(defmethod sicl-genv:function-inline (function-name (env simple-environment))
  (let ((entry (ensure-function-entry env function-name)))
    (if (null entry)
        (error 'undefined-function :name function-name)
        (inline entry))))

(defmethod (setf sicl-genv:function-inline)
    (new-inline function-name (env simple-environment))
  (assert (member new-inline '(nil cl:inline cl:notinline)))
  (let ((entry (find-function-entry env function-name)))
    (if (or (null entry)
            (eq (car (function-cell entry)) (unbound entry)))
        (error 'undefined-function :name function-name)
        (setf (inline entry) new-inline))))

(defmethod sicl-genv:function-cell (function-name (env simple-environment))
  (let ((entry (ensure-function-entry env function-name)))
    (function-cell entry)))

(defmethod sicl-genv:function-unbound (function-name (env simple-environment))
  (let ((entry (ensure-function-entry env function-name)))
    (unbound entry)))

(defmethod sicl-genv:function-lambda-list
    (function-name (env simple-environment))
  (let ((entry (find-function-entry env function-name)))
    (cond ((null entry)
           (values nil nil))
          ((lambda-list-valid-p entry)
           (values (lambda-list entry) t))
          (t
           (values nil nil)))))

(defmethod (setf sicl-genv:function-lambda-list)
    (new-lambda-list function-name (env simple-environment))
  (let ((entry (ensure-function-entry env function-name)))
    (setf (lambda-list entry) new-lambda-list)
    (setf (lambda-list-valid-p entry) t)))

(defmethod sicl-genv:function-ast (function-name (env simple-environment))
  (let ((entry (find-function-entry env function-name)))
    (if (null entry)
        nil
        (ast entry))))

(defmethod (setf sicl-genv:function-ast)
    (new-ast function-name (env simple-environment))
  (let ((entry (ensure-function-entry env function-name)))
    (setf (ast entry) new-ast)))

;;; According to the protocol definition, the consequences are
;;; undefined if FUNCTION is not a function object.  We define the
;;; consequences here to be that an error is signaled.
(defmethod sicl-genv:function-names (function (env simple-environment))
  (assert (functionp function))
  (gethash function (function-names env)))

(defmethod sicl-genv:boundp (symbol (env simple-environment))
  (let ((entry (find-variable-entry env symbol)))
    (and (not (null entry))
         (or (not (null (macro-function entry)))
             (not (eq (car (value-cell entry)) (unbound env)))))))

(defmethod sicl-genv:makunbound (symbol (env simple-environment))
  (let ((entry (find-variable-entry env symbol)))
    (unless (null entry)
      (setf (macro-function entry) nil)
      ;; Free up the expansion so that the garbage collector can
      ;; recycle it.
      (setf (expansion entry) nil)
      (setf (constantp entry) nil)
      (setf (car (value-cell entry)) (unbound env)))))

(defmethod sicl-genv:constant-variable (symbol (env simple-environment))
  (let ((entry (find-variable-entry env symbol)))
    (if (or (null entry) (not (constantp entry)))
        (values nil nil)
        (values (car (value-cell entry)) t))))

(defmethod (setf sicl-genv:constant-variable)
    (value symbol (env simple-environment))
  (let ((entry (ensure-variable-entry env symbol)))
    (cond ((not (null (macro-function entry)))
           (error 'redefinition
                  :name symbol
                  :oldtype :symbol-macro :newtype :constant))
          ((specialp entry)
           (error 'redefinition
                  :name symbol
                  :oldtype :special-variable :newtype :constant))
          ((and (constantp entry)
                (not (eql (car (value-cell entry)) value)))
           (error 'constant-redefinition
                  :name symbol
                  :old (car (value-cell entry)) :new value))
          (t
           (setf (constantp entry) t)
           (setf (car (value-cell entry)) value)))))

(defmethod sicl-genv:special-variable (symbol (env simple-environment))
  (let ((entry (find-variable-entry env symbol)))
    (values (if (or (null entry)
                    (not (specialp entry))
                    (eq (car (value-cell entry)) (unbound env)))
                nil
                (car (value-cell entry)))
            (and (not (null entry)) (specialp entry)))))

(defmethod (setf sicl-genv:special-variable)
    (value symbol (env simple-environment) initialize-p)
  (let ((entry (ensure-variable-entry env symbol)))
    (cond ((constantp entry)
           (error 'redefinition :name symbol
                  :oldtype :constant :newtype :special-variable))
          ((not (null (macro-function entry)))
           (error 'redefinition
                  :name symbol
                  :oldtype :symbol-macro :newtype :special-variable))
          (t
           (setf (specialp entry) t)
           (when initialize-p
             (setf (car (value-cell entry)) value))))))

(defmethod sicl-genv:symbol-macro (symbol (env simple-environment))
  (let ((entry (find-variable-entry env symbol)))
    (if (or (null entry) (null (macro-function entry)))
        (values nil nil)
        (values (macro-function entry) (expansion entry)))))

(defmethod (setf sicl-genv:symbol-macro)
    (expansion symbol (env simple-environment))
  (let ((entry (ensure-variable-entry env symbol)))
    (cond ((specialp entry)
           (error 'redefinition
                  :name symbol
                  :oldtype :special-variable :newtype :symbol-macro))
          ((constantp entry)
           (error 'redefinition
                  :name symbol
                  :oldtype :constant :newtype :symbol-macro))
          (t
           (setf (expansion entry) expansion)
           (setf (macro-function entry)
                 (lambda (form environment)
                   (declare (ignore form environment))
                   expansion))))))

(defmethod sicl-genv:variable-type (symbol (env simple-environment))
  (let ((entry (find-variable-entry env symbol)))
    (if (null entry)
        t
        (type entry))))

(defmethod (setf sicl-genv:variable-type)
    (new-type symbol (env simple-environment))
  (let ((entry (ensure-variable-entry env symbol)))
    (if (constantp entry)
        (error 'attempt-to-proclaim-type-of-constant :name symbol)
        (setf (type entry) new-type))))

(defmethod sicl-genv:variable-cell (symbol (env simple-environment))
  (let ((entry (ensure-variable-entry env symbol)))
    (value-cell entry)))

(defmethod sicl-genv:find-class (symbol (env simple-environment))
  (gethash symbol (classes env)))

(defmethod sicl-genv:variable-unbound (symbol (env simple-environment))
  (declare (ignore symbol))
  (unbound env))

(defmethod (setf sicl-genv:find-class)
    (new-class symbol (env simple-environment))
  (setf (gethash symbol (classes env)) new-class)
  new-class)

(defmethod sicl-genv:setf-expander (symbol (env simple-environment))
  (cdr (assoc symbol (setf-expanders env) :test #'eq)))

(defmethod (setf sicl-genv:setf-expander)
    (new-expander symbol (env simple-environment))
  (let ((association (assoc symbol (setf-expanders env) :test #'eq)))
    (if (null association)
        (push (cons symbol new-expander) (setf-expanders env))
        (setf (cdr association) new-expander))))

(defmethod sicl-genv:default-setf-expander ((env simple-environment))
  (default-setf-expander env))

(defmethod (setf sicl-genv:default-setf-expander)
    (new-expander (env simple-environment))
  (setf (default-setf-expander env) new-expander))

(defmethod sicl-genv:type-expander (symbol (env simple-environment))
  (cdr (assoc symbol (type-expanders env) :test #'eq)))

(defmethod (setf sicl-genv:type-expander)
    (new-expander symbol (env simple-environment))
  (let ((association (assoc symbol (type-expanders env) :test #'eq)))
    (if (null association)
        (push (cons symbol new-expander) (type-expanders env))
        (setf (cdr association) new-expander))))

(defmethod sicl-genv:find-package (name (env simple-environment))
  (values (gethash name (packages env))))

(defmethod (setf sicl-genv:find-package)
    (new-package name (env simple-environment))
  (unless (null (gethash name (packages env)))
    (error 'a-package-with-the-given-name-already-exists
           :name name))
  (setf (gethash name (packages env)) new-package))

(defmethod sicl-genv:find-method-combination-template
    (symbol (env simple-environment))
  (gethash symbol (method-combination-templates env)))

(defmethod (setf sicl-genv:find-method-combination-template)
    (new-template symbol (env simple-environment))
  (setf (gethash symbol (method-combination-templates env)) new-template)
  new-template)

(defmethod sicl-genv:declaration (name (env simple-environment))
  (gethash name (declarations env)))

(defmethod (setf sicl-genv:declaration) (value name (env simple-environment))
  (if (null value)
      (remhash name (declarations env))
      (setf (gethash name (declarations env)) t)))

(defmethod sicl-genv:declarations ((env simple-environment))
  (loop for key being each hash-key of (declarations env)
        collect key))

(defmethod sicl-genv:symbol-plist (symbol (env simple-environment))
  (gethash symbol (property-lists env)))

(defmethod (setf sicl-genv:symbol-plist)
    (new-plist symbol (env simple-environment))
  (setf (gethash symbol (property-lists env)) new-plist))

(defmethod sicl-genv:structure-description (name (env simple-environment))
  (gethash name (structure-entries env)))

(defmethod (setf sicl-genv:structure-description) (type name (env simple-environment))
  (setf (gethash name (structure-entries env)) type))
