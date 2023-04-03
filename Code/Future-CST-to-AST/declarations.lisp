(cl:in-package #:sicl-expression-to-ast)

;;; For a particular variable identified by VARIABLE-NAME-AST,
;;; traverse the declarations, and for each declaration that mentions
;;; the variable, add it to the environment.  Finally return the
;;; modified environment.

(defgeneric add-declaration-for-variable
    (client
     environment
     variable-name-ast
     declaration-ast))

;;; An FTYPE declaration can not be about a variable.
(defmethod add-declaration-for-variable
    (client
     environment
     variable-name-ast
     (declaration-ast ico:ftype-ast))
  environment)

;;; An OPTIMIZE declaration can not not be about a variable.
(defmethod add-declaration-for-variable
    (client
     environment
     variable-name-ast
     (declaration-ast ico:optimize-ast))
  environment)

;;; An INLINE or a NOTINLINE declaration can not not be about a
;;; variable.
(defmethod add-declaration-for-variable
    (client
     environment
     variable-name-ast
     (declaration-ast ico:inline-or-notinline-ast))
  environment)

(defmethod add-declaration-for-variable
    (client
     environment
     variable-name-ast
     (declaration-ast ico:dynamic-extent-ast))
  (loop with result = environment
        with name = (ico:name variable-name-ast)
        for name-ast in (ico:name-asts declaration-ast)
        when (and (typep name-ast 'ico:variable-name-ast)
                  (eq name (ico:name name-ast)))
          do (setf result
                   (trucler:add-variable-dynamic-extent 
                    client result name))
        finally (return result)))

(defmethod add-declaration-for-variable
    (client
     environment
     variable-name-ast
     (declaration-ast ico:ignore-or-ignorable-ast))
  (loop with result = environment
        with name = (ico:name variable-name-ast)
        for name-ast in (ico:name-asts declaration-ast)
        when (and (typep name-ast 'ico:variable-name-ast)
                  (eq name (ico:name name-ast)))
          do (setf result
                   (trucler:add-variable-ignore
                    client result name
                    (if (typep declaration-ast 'ico:ignore-ast)
                        'ignore
                        'ignorable)))
        finally (return result)))

(defmethod add-declaration-for-variable
    (client
     environment
     variable-name-ast
     (declaration-ast ico:special-ast))
  (loop with result = environment
        with name = (ico:name variable-name-ast)
        for name-ast in (ico:name-asts declaration-ast)
        when (eq name (ico:name name-ast))
          do (setf result
                   (trucler:add-special-variable
                    client result name))
        finally (return result)))

;;; For now, ignore type declarations.
(defmethod add-declaration-for-variable
    (client
     environment
     variable-name-ast
     (declaration-ast ico:type-ast))
  environment)

(defun add-declarations-for-variable
    (client environment variable-name-ast declaration-asts)
  (loop with result = environment
        for declaration-ast in declaration-asts
        do (setf result
                 (add-declaration-for-variable
                  client result variable-name-ast declaration-ast))))
        
  
