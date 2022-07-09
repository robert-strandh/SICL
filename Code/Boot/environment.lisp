(cl:in-package #:sicl-boot)

(defgeneric loaded-files (environment))

(defgeneric (setf loaded-files) (loaded-files environment))

(defclass environment (env:run-time-environment)
  ((%name :initarg :name :reader name)
   ;; This slot holds a list of names of functions that are called
   ;; during bootstrapping, but that were not explicitly defined in
   ;; the environment, so instead the host version of the function was
   ;; used.
   (%host-functions :initform '() :accessor host-functions)
   ;; This slot holds an association list.  The key of an element is
   ;; the pathname naming a file that has been loaded into this
   ;; environment.  The value of an element is a universal time when
   ;; the file was loaded.
   (%loaded-files :initform '() :accessor loaded-files)
   ;; This slot holds a list of ASDF systems that have been loaded
   ;; into the environment.
   (%loaded-asdf-systems :initform '() :accessor loaded-asdf-systems)
   ;; This slot holds a list of override entries.  Such an entry is a
   ;; CONS cell, where the CAR of the CONS cell is a function name,
   ;; and the CDR of the CONS cell is a FUNCTION CELL (which is
   ;; another CONS cell)
   ;;
   ;; The list of override entries affects two things.  The main
   ;; purpose is that when a FASL is loaded into the environment, the
   ;; function FUNCTION CELL first looks in this list before
   ;; consulting the normal environment entries.  
   (%overridden-function-cells
    :initform '()
    :accessor overridden-function-cells))
  (:default-initargs :client (make-instance 'client)))

(defmethod overridden-function-cells
    ((environment env:evaluation-environment))
  (overridden-function-cells (env:parent environment)))

(defmethod overridden-function-cells
    ((environment env:compilation-environment))
  (overridden-function-cells (env:parent environment)))

;;; This variable holds a list of names of host functions that can be
;;; called during bootstrapping as a replacement for target functions
;;; with the same name.
(defparameter *allowed-host-functions*
  '(;; CONS
    car cdr caar cadr cdar cddr
    caaar caadr cadar caddr cdaar cdadr cddar cdddr
    first second third fourth rest
    nth last butlast
    cons consp listp atom null endp list list*
    make-list copy-list
    append
    member
    mapcar mapc
    getf
    assoc rassoc
    union set-difference set-exclusive-or adjoin
    ;; Sequence
    mismatch
    reduce
    reverse nreverse
    position position-if position-if-not
    find find-if find-if-not
    count count-if count-if-not
    remove remove-duplicates
    sort subseq length
    elt
    ;; Number
    + - * < <= = > >= /= floor 1+ 1-
    zerop oddp evenp plusp minusp numberp integerp
    expt random
))

(defmethod env:function-cell :around (client (environment environment) name)
  (let ((cell (call-next-method)))
    (when (eq (car cell) (cdr cell))
      ;; The function is undefined.
      (let ((undefined-function
              (if (member name *allowed-host-functions* :test #'equal)
                  (lambda (&rest arguments)
                    (pushnew name (host-functions environment)
                             :test #'equal)
                    (apply name arguments))
                  (lambda (&rest arguments)
                    (declare (ignore arguments))
                    (error "Attempt to call function ~s in environment ~s"
                           name environment)))))
        (setf (car cell) undefined-function)
        (setf (cdr cell) undefined-function)))
    cell))

(defmethod trucler:restrict-for-macrolet-expander
    (client (environment environment))
  environment)

(defun define-macroexpand (environment)
  (setf (env:fdefinition (env:client environment) environment 'macroexpand-1)
        (lambda (form &optional (env environment))
          (etypecase form
            ((cons symbol)
             (let ((expander (trucler:macro-function (car form) env)))
               (if (null expander)
                   (values form nil)
                   (values (funcall expander form env) t))))
            (symbol
             (let ((expansion (trucler:symbol-macro-expansion form env)))
               (if (eq expansion form)
                   (values form nil)
                   (values expansion t)))))))
    (setf (env:fdefinition (env:client environment) environment 'macroexpand)
        (lambda (form &optional (env environment))
          (let ((result form)
                (e-p nil))
            (loop do (multiple-value-bind (expansion expanded-p)
                         (funcall (env:fdefinition (env:client environment) environment 'macroexpand-1)
                                  result env)
                       (if expanded-p
                           (setf result expansion
                                 e-p t)
                           (return (values result e-p)))))))))

(defun define-environment-functions (client environment)
  (flet ((def (name function)
           (setf (env:fdefinition client environment name)
                 function)))
    (symbol-macrolet ((c sicl-client:*client*))
      (setf (env:special-variable client environment 'env:*environment* t)
            environment)
      (def 'proclaim
          (lambda (declaration-specifier)
            (case (first declaration-specifier)
              ;; We handle only FTYPE for now.
              (ftype (loop for name in (rest (rest declaration-specifier))
                           do (setf (env:function-type c environment name)
                                    (second declaration-specifier)))))))
      (def 'fboundp
            (lambda (name)
              (or (not (null (env:fdefinition c environment name)))
                  (or (not (null (env:special-operator c environment name)))
                      (not (null (env:macro-function c environment name)))))))
      (def 'special-operator-p
            (lambda (name)
              (env:special-operator c environment name)))
      (def '(setf macro-function)
            (lambda (macro-function name &optional env)
              (assert (null env))
              (setf (env:macro-function c environment name)
                    macro-function)))
      (def 'macro-function
            (lambda (name &optional env)
              (assert (null env))
              (env:macro-function c environment name)))
      (def '(setf sicl-data-and-control-flow:setf-expander)
            (lambda (setf-expander name)
              (setf (env:setf-expander c environment name)
                    setf-expander)))
      (def 'sicl-data-and-control-flow:setf-expander
            (lambda (name)
              (env:setf-expander c environment name)))
      (def '(setf fdefinition)
            (lambda (function name)
              (setf (env:fdefinition c environment name)
                    function)))
      (def 'fdefinition
            (lambda (name)
              (cond ((env:special-operator c environment name)
                     :special-operator)
                    ((not (null (env:macro-function c environment name)))
                     :macro)
                    (t (let ((function (env:fdefinition c environment name)))
                         (if (null function)
                             (error 'undefined-function :name name)
                             function))))))
      (def '(setf symbol-function)
            (lambda (function name)
              (setf (env:fdefinition c environment name)
                    function)))
      (def 'symbol-function
            (lambda (name)
              (env:fdefinition c environment name)))
      (def 'get-setf-expansion
            (lambda (place &optional env)
              (declare (ignore env))
              (env:get-setf-expansion c environment place)))
      (def '(setf sicl-data-and-control-flow:special-variable)
            (lambda (value name set-if-assigned-p)
              (setf (env:special-variable c environment name set-if-assigned-p)
                    value)))
      (def '(setf sicl-data-and-control-flow:constant-variable)
            (lambda (value name)
              (setf (env:constant-variable c environment name)
                    value)))
      (def 'sicl-method-combination:find-method-combination-template
            (lambda (name)
              (env:find-method-combination-template name environment)))
      (def '(setf sicl-method-combination:find-method-combination-template)
            (lambda (template name)
              (setf (env:find-method-combination-template name environment)
                    template)))
      (def 'find-class
            (lambda (symbol &optional (errorp t) env)
              (declare (ignore env))
              (let ((class (env:find-class c environment symbol)))
                (if (and errorp (null class))
                    (error "no class named ~s in ~s" symbol environment)
                    class))))
      (def '(setf find-class)
            (lambda (new-class symbol &optional errorp env)
              (declare (ignore errorp env))
              (setf (env:find-class c environment symbol)
                    new-class)))
      (def 'find-package
            (lambda (name)
              (env:find-package c environment (string name))))
      (def 'sicl-data-and-control-flow:function-cell
            (lambda (name)
              (let* ((override-entry
                       (find name (overridden-function-cells environment)
                             :key #'car :test #'equal))
                     (result (if (null override-entry)
                                 (env:function-cell c environment name)
                                 (cdr override-entry))))
                result)))
      (def 'sicl-symbol:variable-cell
            (lambda (name)
              (env:variable-cell c environment name)))
      (def 'compiler-macro-function
            (lambda (symbol)
              (env:compiler-macro-function c environment symbol)))
      (def '(setf compiler-macro-function)
            (lambda (function symbol)
              (setf (env:compiler-macro-function c environment symbol)
                    function)))
      (def 'sicl-type:type-expander
            (lambda (name)
              (env:type-expander c environment name)))
      (def '(setf sicl-type:type-expander)
          (lambda (expander name)
            (setf (env:type-expander c environment name)
                  expander)))
      (def 'fmakunbound
            (lambda (name)
              (setf (env:fdefinition c environment name) nil)
              (setf (env:macro-function c environment name) nil)))))
  (define-macroexpand environment))

(defun import-standard-functions (environment)
  (import-functions-from-host
   '(;; Data and control flow
     identity every
     ;; Evaluation and compilation
     constantp
     ;; Symbols
     gensym symbolp
     ;; Characters
     characterp char-code code-char char= char-name standard-char-p
     alphanumericp digit-char-p char-upcase
     ;; Strings
     stringp
     ;; For debugging
     format
     ;; Conditions
     error break)
   environment))

(defun define-setf-functions (environment)
  (let ((client (env:client environment)))
    (setf (env:fdefinition client environment '(setf car))
          (lambda (new-value cons)
            (setf (car cons) new-value)))
    (setf (env:fdefinition client environment '(setf cdr))
          (lambda (new-value cons)
            (setf (cdr cons) new-value)))
    (setf (env:fdefinition client environment '(setf cadr))
          (lambda (new-value cons)
            (setf (cadr cons) new-value)))
    (setf (env:fdefinition client environment '(setf second))
          (lambda (new-value cons)
            (setf (second cons) new-value)))
    (setf (env:fdefinition client environment '(setf cddr))
          (lambda (new-value cons)
            (setf (cddr cons) new-value)))
    (setf (env:fdefinition client environment '(setf nth))
          (lambda (new-value n list)
            (setf (nth n list) new-value)))
    (setf (env:fdefinition client environment '(setf char))
          (lambda (new-value string index)
            (setf (char string index) new-value)))
    (setf (env:fdefinition client environment '(setf schar))
          (lambda (new-value string index)
            (setf (schar string index) new-value)))))

(defun define-special-operators (environment)
  (let ((client (env:client environment)))
    (do-external-symbols (symbol '#:common-lisp)
      (when (special-operator-p symbol)
        (setf (env:special-operator client environment symbol)
              t)))))

(defun define-primops (environment)
  (let ((client (env:client environment)))
    (do-external-symbols (symbol '#:cleavir-primop)
      (setf (env:special-operator client environment symbol)
            t))
    (do-external-symbols (symbol '#:sicl-primop)
      (setf (env:special-operator client environment symbol)
            t))))

(defun import-run-time-functions (environment)
  (import-functions-from-host
   '(sicl-run-time:symbol-value
     (setf sicl-run-time:symbol-value)
     sicl-run-time:boundp
     sicl-run-time:makunbound)
   environment))

(defmethod initialize-instance :after ((environment environment) &key)
  (let ((client (env:client environment)))
    (flet ((def (name function)
             (setf (env:fdefinition client environment name)
                   function)))
      (def 'env:global-environment
          (lambda (&optional lexical-environment)
            (if (null lexical-environment)
                environment
                (trucler:global-environment client lexical-environment))))
      (import-standard-functions environment)
      (import-run-time-functions environment)
      (import-functions-from-host
       '(alexandria:make-gensym-list)
       environment)
      (def 'funcall
          (lambda (function-designator &rest arguments)
            (let ((function (if (symbolp function-designator)
                                (env:fdefinition client environment function-designator)
                                function-designator)))
              (apply function arguments))))
      (def 'apply
          (lambda (function-designator &rest arguments)
            (let ((function (if (symbolp function-designator)
                                (env:fdefinition client environment function-designator)
                                function-designator)))
              (apply #'apply function arguments))))
      (def 'host-symbol-name #'symbol-name)
      (def 'host-symbol-package #'symbol-package)
      (def 'host-array-dimensions #'array-dimensions)
      (def 'host-package-name #'package-name)
      (define-special-operators environment)
      (define-primops environment)
      (define-setf-functions environment)
      (define-environment-functions client environment)
      (setf (env:special-variable client environment '*trace-output* t)
            *trace-output*)
      ;; Make it possible to inspect from the REPL
      (import-functions-from-host '(clouseau:inspect) environment))))

(defmethod print-object ((object environment) stream)
  (print-unreadable-object (object stream)
    (format stream "Environment ~a" (name object))))
