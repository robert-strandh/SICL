(cl:in-package #:cleavir-code-utilities)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function DESTRUCTURE-LAMBDA-LIST.
;;;
;;; Destructuring a tree according to a lambda list.
;;;
;;; The destructuring itself is typically done when a macro function
;;; is run, and the purpose is to take the macro form apart and assign
;;; parts of it to the parameter of the lambda list of the macro.
;;;
;;; The function DESTRUCTURE-LAMBDA-LIST generates the code for doing
;;; the destrucuring.  It is typically run by the expansion of
;;; DEFMACRO.  Recall that DEFMACRO must take the definition of a
;;; macro, in particular its lambda list, and generate a macro
;;; function.  The macro function takes the macro form as input and
;;; generates the expanded form.  Destructuring is done by a LET*
;;; form, and this code generates the bindings of that LET* form.
;;;
;;; The bindings generated will contain generated variables that are
;;; not used in the body of the macro definition, so we want them to
;;; be declared IGNORE.  For that reason, DESTRUCTURE-LAMBDA-LIST
;;; returns two values: the bindings mentioned above, and a list of
;;; variables to declare IGNORE in the beginning of the body of the
;;; macro function.
;;;
;;; We assume that the lambda-list/pattern are syntactically correct.
;;; They should be, because this function takes as input not the raw
;;; lambda list, but a PARSED lambda list in the form of a
;;; standard-object.

;;; The function DESTRUCTURE-REQUIRED and DESTRUCTURE-OPTIONALS return
;;; two values:
;;;
;;;  * A list of binding forms to be used in a LET*.
;;;
;;;  * A variable to be used to destructure the remaining pattern.

;;; Destructure the required parameters of a lambda list.
;;;
;;; Recall that the required parameters of a parsed lambda list is
;;; either the keyword :NONE, or a list of patterns.
(defun destructure-required (required var)
  (if (or (eq required :none) (null required))
      (values '() var)
      (let ((temp1 (gensym))
            (temp2 (gensym)))
        (multiple-value-bind (bindings rest-var)
            (destructure-required (cdr required) temp1)
          (values (append `((,temp1 (if (consp ,var)
                                        (cdr ,var)
                                        (error "required end early")))
                            (,temp2 (car ,var)))
                          (destructure-pattern (car required) temp2)
                          bindings)
                  rest-var)))))

;;; Destructure the optional parameters of a lambda list.
;;;
;;; Recall that the optional parameters of a parsed lambda list is
;;; either the keyword :none, or it is a list of &optional entries.
;;; Each optional entry has one of the two forms:
;;;
;;;  * (pattern init-arg)
;;;
;;;  * (pattern init-arg supplied-p-parameter)
;;;
;;; If the original lambda-list did not have an init-arg, the parsing
;;; process supplied NIL so that the parsed lambda list always has an
;;; init-arg.
;;;
;;; The HyperSpec says that if there is an &optional parameter and the
;;; object to be destructured "ends early", then the init-arg is
;;; evaluated and destructured instead.  We interpret this phrase to
;;; mean that either:
;;;
;;;   * the object to be destructured is NIL, in which case it "ends
;;;     early", and we destructure the init-arg instead.
;;;
;;;   * the object is a CONS, in which case the CAR of the object is
;;;     matched against the pattern.  If it doesn't match, then an
;;;     error is signaled and no attemp is made to match agains the
;;;     init-arg.
;;;
;;;   * the object is an atom other than NIL.  Then an error is
;;;     signaled.
(defun destructure-optionals (optionals var)
  (if (or (eq optionals :none) (null optionals))
      (values '() var)
      (let ((optional (car optionals))
            (temp1 (gensym))
            (temp2 (gensym)))
        (multiple-value-bind (bindings rest-var)
            (destructure-optionals (cdr optionals) temp1)
          (values (append `((,temp1 (if (consp ,var)
                                        (cdr ,var)
                                        (if (null ,var)
                                            '()
                                            (error "optional expected"))))
                            ;; If the object is not a CONS, then it is
                            ;; either NIL in which case we destructure
                            ;; the init-arg instead, or else it is an
                            ;; atom other than NIL and we have already
                            ;; signaled an error before, so we don't
                            ;; need to handle that case again.
                            (,temp2 (if (consp ,var)
                                        (car ,var)
                                        ,(cadr optional)))
                            ;; If a supplied-p-parameter exists, then
                            ;; we give it the value TRUE whenever the
                            ;; object is a CONS, even though later
                            ;; an error might be signaled because there
                            ;; is no match.
                            ,@(if (consp (cddr optional))
                                  `((,(caddr optional) (consp ,var)))))
                          (destructure-pattern (car optional) temp2)
                          bindings)
                  rest-var)))))

;;; Destructure the keyword parameters of a lambda list.
;;;
;;; Recall that the keys part of a compiled lambda list is either
;;; :none, which means that no &key was given at all, or a list if
;;; &key entries.  If the list is empty, it means that &key was given,
;;; but no &key parameters followed.
;;;
;;; A &key entry is either:
;;;
;;;  * ((keyword pattern) init-form)
;;;
;;;  * ((keyword pattern) init-form supplied-p-parameter)
;;;
;;; The HyperSpec is pretty skimpy about what happens with keyword
;;; arguments ("the rest of the list ... is taken apart
;;; appropriately").  What we do is the following:
;;;
;;;  * If there is a keyword argument with the right keyword, then
;;;    its value is matched against the pattern.
;;;
;;;  * Otherwise, the value of the init-form is matched agains the
;;;    pattern.
(defun destructure-keys (keys var)
  (if (or (eq keys :none) (null keys))
      '()
      (let ((key (car keys))
            (temp (gensym)))
        (append `(;; What we do in step 1 depends on whether there is
                  ;; a supplied-p-parameter or not.  If there is, then
                  ;; in step 1, we return a list of two things:
                  ;;
                  ;;  * a boolean indicating whether we found the
                  ;;    keyword.
                  ;;
                  ;;  * either the argument found, or the value of the
                  ;;    init-form if no argument was found.
                  ;;
                  ;; If there is no supplied-p-parameter, then we just
                  ;; return the argument found or the value of the
                  ;; init-form if no argument was found.
                  (,temp
                   ,(if (consp (cddr key))
                        `(loop for rem = ,var then (cddr rem)
                               while (consp rem)
                               when (eq (car rem) ',(caar key))
                                 return (list t (cadr rem))
                               finally (return (list nil ,(cadr key))))
                        `(loop for rem = ,var then (cddr rem)
                               while (consp rem)
                               when (eq (car rem) ',(caar key))
                                 return (cadr rem)
                               finally (return ,(cadr key)))))
                  ;; If there is no supplied-p-parameter, then we are
                  ;; done.  If there is, we must get it from the first
                  ;; element of the list computed in step 1, and we must
                  ;; replace that list with its second element.
                  ,@(if (consp (cddr key))
                        `((,(caddr key)
                           (prog1 (car ,temp)
                             (setf ,temp (cadr ,temp)))))
                        '()))
                (destructure-pattern (cadar key) temp)
                (destructure-keys (cdr keys) var)))))

;;; We return two values.  The first value is a list of bindings to be
;;; used with a LET* and the purpose of which is to destructure the
;;; arguments in VAR according to the LAMBDA-LIST.  The second value
;;; is a list of variables that ar bound in the bindings, but that
;;; should be declared IGNORE because they are introduced for
;;; technical reasons and not used anywhere.
;;;
;;; This code is used in macro functions, so we want to avoid using
;;; macros in the expansion for the simple case of no keyword
;;; arguments.
(defun destructure-lambda-list (lambda-list var)
  (multiple-value-bind (required-bindings var1)
      (destructure-required (required lambda-list) var)
    (multiple-value-bind (optional-bindings var2)
        (destructure-optionals (optionals lambda-list) var1)
      (let ((error-check-bindings '())
            (variables-to-ignore '()))
        ;; Generate bindings that check some conditions.
        (cond ((and (eq (rest-body lambda-list) :none)
                    (eq (keys lambda-list) :none))
               ;; If there is neither a &rest/&body nor any keyword
               ;; parameters, then the remaining list must be NIL, or
               ;; else we signal an error.
               (let ((temp (gensym)))
                 (push temp variables-to-ignore)
                 (push `(,temp (if (not (null ,var2))
                                   (error "too many arguments supplied")))
                       error-check-bindings)))
              ((not (eq (keys lambda-list) :none))
               ;; If there are keyword parameters, then we must check
               ;; several things.  First, we must check that the
               ;; remaining list is a proper list and that it has an
               ;; even number of elements.
               (let ((temp (gensym)))
                 (push temp variables-to-ignore)
                 (push `(,temp (multiple-value-bind (length structure)
                                   (list-structure ,var2)
                                 ;; First, the remaining list must be
                                 ;; a proper list.
                                 (unless (eq structure :proper)
                                   (error "with keyword parameters, ~
                                           the arguments must be a ~
                                           proper list."))
                                 ;; Second, it must have an even
                                 ;; number of elements.
                                 (unless (evenp length)
                                   (error "with keyword parameters, ~
                                           the keyword part of the ~
                                            arguments must have an ~
                                            even number of elements."))))
                       error-check-bindings))
               ;; If &allow-other keys was not given, more checks have
               ;; to be made.
               (unless (allow-other-keys lambda-list)
                 (let ((temp (gensym))
                       (allowed-keywords (mapcar #'caar (keys lambda-list))))
                   (push temp variables-to-ignore)
                   ;; Perhaps there was a :allow-other-keys <true> in
                   ;; the argument list.  As usual, if there are
                   ;; several pairs :allow-other-keys <mumble> then it
                   ;; is the first one that counts.  This happens to
                   ;; be exactly what GETF checks for so use it.
                   (push `(,temp (unless (getf ,var2 :allow-other-keys)
                                   ;; Either no :allow-other-keys was
                                   ;; found, or the first one found
                                   ;; had a value of NIL.  Then every
                                   ;; keyword in the argument list
                                   ;; must be one of the ones supplied
                                   ;; in the parameters.
                                   (let ()
                                     (loop for keyword in ,var2 by #'cddr
                                           unless (member keyword
                                                          ',allowed-keywords)
                                             do (error "unknown keyword ~s"
                                                       keyword)))))
                         error-check-bindings))))
              (t
               ;; If there are no keyword parameters, but there is a
               ;; &rest/&body, then we do no checks, which means that
               ;; the argument list can have any structure, including
               ;; circular.  the remaining list is simply matched with
               ;; the &rest/&body pattern.
               nil))
        (let ((rest-bindings
                (if (eq (rest-body lambda-list) :none)
                    '()
                    (destructure-pattern (rest-body lambda-list) var2)))
              (key-bindings
                (destructure-keys (keys lambda-list) var2)))
          (values (append required-bindings
                          optional-bindings
                          rest-bindings
                          (reverse error-check-bindings)
                          key-bindings
                          (if (eq (aux lambda-list) :none)
                              '()
                              (aux lambda-list)))
                  variables-to-ignore))))))

;;; Destructure a pattern.
;;; FIXME: say more.
(defun destructure-pattern (pattern var)
  (cond ((null pattern)
         `((,(gensym) (unless (null ,var)
                        (error "tree should be NIL")))))
        ((symbolp pattern)
         `((,pattern ,var)))
        ((consp pattern)
         (let ((temp1 (gensym))
               (temp2 (gensym)))
           (append `((,temp1 (if (consp ,var)
                                 (car ,var)
                                 (error "no match"))))
                   (destructure-pattern (car pattern) temp1)
                   `((,temp2 (cdr ,var)))
                   (destructure-pattern (cdr pattern) temp2))))
        (t
         (destructure-lambda-list pattern var))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Parse arguments according to a lambda list.
;;;
;;; This process is similar to DESTRUCTURING above, except that
;;; destructuring is done at compile-time and argument parsing is done
;;; at runtime as part of the prologue of a function.
;;;
;;; The function MATCH-LAMBDA-LIST is similar to
;;; DESTRUCTURE-LAMBDA-LIST in that it generates two values: the
;;; bindings of a LET* form and a list of variables to ignore.  The
;;; difference is that MATCH-LAMBDA-LIST takes its input as two
;;; special forms, supplied as parameters, but typically (ARGCOUNT)
;;; and (ARG i) where i is a fixnum.  The form (ARGCOUNT) returns the
;;; number of arguments to the function and the form (ARG i) returns
;;; the i'th argument, where i is between 0 and and N-1 and N is the
;;; value returned by (ARGCOUNT).  Another difference is that
;;; MATCH-LAMBDA-LIST only handles ordinary lambda lists, so no
;;; destructuring is necessary, and the &ENVIRONMENT and &WHOLE
;;; keywords are not present.

;;; Recall that the required parameters of a parsed lambda list is
;;; either the keyword :NONE, or a list of variables.
(defun match-required (required arg-op)
  (if (eq required :none)
      '()
      (loop for parameter in required
            for i from 0
            collect `(,parameter (,arg-op ,i)))))

;;; Recall that the optional parameters of a parsed lambda list is
;;; either the keyword :NONE or a list of optional entries, and that
;;; an optional-entry is a list of one of two forms:
;;;
;;;  * (parameter init-arg)
;;;  * (parameter init-arg supplied-p-parameter)
;;;
;;; We assume that a binding has been generated that assigns the
;;; argument count to some generated variable in the LET* form.  That
;;; generated variable is passed as an argument to this function.
(defun match-optionals (optional-entries first-count arg-count-var arg-op)
  (if (eq optional-entries :none)
      '()
      (loop for (parameter init-arg . rest) in optional-entries
            for i from first-count
            collect `(,parameter (if (> ,arg-count-var ,i)
                                     (,arg-op ,i)
                                     ,init-arg))
            unless (null rest)
              collect `(,(car rest) (> ,arg-count-var ,i)))))

;;; We assume that a binding has been generated that assigns the
;;; argument count to some generated variable in the LET* form.  That
;;; generated variable is passed as an argument to this function.
(defun match-rest/body (rest/body first-count arg-count-var arg-op)
  (if (eq rest/body :none)
      '()
      `((,rest/body (let ((temp '())
                          (n ,arg-count-var))
                      (tagbody
                       again
                         (when (= n ,first-count)
                           (go out))
                         (setq n (1- n))
                         (setq temp (cons (,arg-op n) temp))
                         (go again)
                       out)
                      temp)))))

;;; Recall that a key-entry is a list of one of two forms:
;;;
;;;  * ((keyword parameter) init-form)
;;;  * ((keyword parameter) init-form supplied-p-parameter)
;;;
(defun match-keys (key-entries first-count arg-count-var arg-op)
  (if (eq key-entries :none)
      '()
      (loop for ((keyword parameter) init-form . rest) in key-entries
            for counter = (gensym)
            for supplied-p-temp = (gensym)
            unless (null rest)
              collect `(,supplied-p-temp nil)
            collect `(,parameter (let ((,counter ,first-count))
                                   (block nil
                                     (tagbody
                                      again
                                        (when (= ,counter ,arg-count-var)
                                          (go out))
                                        (when (eq (,arg-op ,counter) ,keyword)
                                          ,@(if (null rest)
                                                '()
                                                `((setq ,supplied-p-temp t)))
                                          (return (,arg-op (1+ ,counter))))
                                        (setq ,counter (+ ,counter 2))
                                        (go again)
                                      out)
                                     ,init-form)))
            unless (null rest)
              collect `(,(car rest) ,supplied-p-temp))))

;;; Generate code to check that there is an even number of keyword
;;; arguments.
(defun check-even-number-of-keyword-arguments (first-count arg-count-op)
  `(unless (,(if (evenp first-count) 'evenp 'oddp) (,arg-count-op))
     (error "odd number of keyword arguments")))

;;; Generate code to check that either :ALLOW-OTHER KEYS <true> is a
;;; keyword argument or that all the keyword arguments are valid.  The
;;; HyperSpec says that :ALLOW-OTHER-KEYS <something> is always valid,
;;; so even if we have :ALLOW-OTHER-KEYS <false>, it is valid.
;;; Furthermore, since there can be multiple instances of keyword
;;; arguments, and the first one is used to determine the ultimate
;;; value of the corresponding variable, we must determine whether
;;; :ALLOW-OTHER-KEYS is true or not from the first occurrence.  This
;;; function is only called when &allow-other-keys is not given in the
;;; lambda list.
(defun check-keyword-validity
    (variable keywords first-count arg-count-var arg-op)
  (let ((counter (gensym)))
    `((,variable
       (let ((,counter ,first-count))
         (block nil
           (tagbody
              ;; In phase 1 we search for the first
              ;; occurrence of :allow-other-keys.
            phase1
              (when (>= ,counter ,arg-count-var)
                ;; We ran out of arguments without finding any
                ;; :allow-other-keys.  We must now go check that
                ;; each keyword argument is a valid one.
                (go phase2))
              (if (eq (,arg-op ,counter) :allow-other-keys)
                  ;; We found the first :allow-other-keys.
                  ;; We look no further.
                  (if (,arg-op (1+ ,counter))
                      ;; The argument following :allow-other keys is
                      ;; true.  Then any keyword is allowed, so we are
                      ;; done checking.
                      (return nil)
                      ;; The argument following :allow-other keys is
                      ;; false, which means that only explicitly
                      ;; mentioned keywords are allowed, so we do the
                      ;; next phase.
                      (go phase2))
                  ;; The keyword we found is something other than
                  ;; :allow-other-keys.  Try the next one.
                  (progn (setq ,counter (+ ,counter 2))
                         (go phase1)))
            phase2
              ;; Start over from the first keyword argument.
              (setq ,counter ,first-count)
            again
              (when (>= ,counter ,arg-count-var)
                ;; We ran out of arguments without finding any
                ;; invalid keywords.  We are done.
                (return nil))
              ;; Check that the current keyword is valid.
              (unless (member (,arg-op ,counter) ',keywords)
                ;; Found an invalid keyword.
                (error "invalid keyword ~s" (,arg-op ,counter)))
              ;; Come here if the current keyword is not invalid.
              ;; Try the next one.
              (setq ,counter (+ ,counter 2))
              (go again))))))))

(defun check-arg-count
    (variable arg-count-var required-count optional-count rest-p key-p)
  `((,variable
     (progn
       ,@(if (plusp required-count)
             `((if (< ,arg-count-var ,required-count)
                   (error 'too-few-arguments)))
             '())
       ,@(if (and (not rest-p) (not key-p))
             `((if (> ,arg-count-var
                      ,(+ required-count optional-count))
                   (error 'too-many-arguments)))
             '())
       ,@(if key-p
             (if (evenp (+ required-count optional-count))
                 `((when (and (> ,arg-count-var
                                 ,(+ required-count
                                     optional-count))
                              (oddp ,arg-count-var))
                     (error 'odd-number-of-keyword-arguments)))
                 `((when (and (> ,arg-count-var
                                 ,(+ required-count
                                     optional-count))
                              (evenp ,arg-count-var))
                     (error 'odd-number-of-keyword-arguments))))
             '())))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Preprocess an ordinary lambda list.
;;;
;;; Preprocessing a lambda list extracts the code for initializing
;;; unsupplied arguments.  The idea here is that there is some
;;; low-level argument-parsing mechanism that runs through the
;;; arguments supplied to a function and sets main parameters and
;;; SUPPLIED-P parameters according to what it finds.  The code
;;; generated here then takes over and checks the result of the
;;; argument-parsing mechanism and sets the value of main parameters
;;; that have not been supplied, as indicated by the associated
;;; SUPPLIED-P parameter.

;;; Make sure that every &OPTIONAL and &KEY parameter of
;;; PARSED-LAMBDA-LIST has not only an INIT-FORM but also a SUPPLIED-P
;;; parameter.
(defun ensure-supplied-p-parameters (parsed-lambda-list)
  (let ((required (required parsed-lambda-list))
        (optionals (optionals parsed-lambda-list))
        (keys (keys parsed-lambda-list))
        (allow-other-keys (allow-other-keys parsed-lambda-list)))
    (make-instance 'lambda-list
      :required required
      :optionals (if (eq optionals :none)
                     :none
                     (loop for optional in optionals
                           collect (if (= (length optional) 3)
                                       optional
                                       (append optional (list (gensym))))))
      :keys (if (eq keys :none)
                :none
                (loop for key in keys
                      collect (if (= (length key) 3)
                                  key
                                  (append key (list (gensym))))))
      :allow-other-keys allow-other-keys)))

;;; Given a parsed lambda list where all the &OPTIONAL and &KEY
;;; parameters are known to have not only an INIT-FORM but also a
;;; SUPPLIED-P parameter, create an unparsed lambda list of the
;;; following form:
;;;
;;; ([r1 .. rl [&optional o1 ..om] [&key k1 .. kn &allow-other-keys]]])
;;;
;;; where:
;;;
;;;   - Each ri is a symbol
;;;
;;;   - Each oi is a list of two symbols.  The second of the
;;;     two conceptually contains a Boolean value indicating whether
;;;     the first one contains a value supplied by the caller.
;;;
;;;   - Each ki is a list of three symbols.  The first symbol is the
;;;     keyword-name that a caller must supply in order to pass the
;;;     corresponding argument.  The third symbol conceptually
;;;     contains a Boolean value indicating whether the first
;;;     LEXICAL-AST contains a value supplied by the caller.
(defun extract-entry-lambda-list (parsed-lambda-list)
  (let ((required (required parsed-lambda-list))
        (optionals (optionals parsed-lambda-list))
        (keys (keys parsed-lambda-list))
        (allow-other-keys (allow-other-keys parsed-lambda-list)))
    (append
     required
     (if (eq optionals :none)
         '()
         (cons '&optional
               (loop for (name nil supplied-p) in optionals
                     collect (list name supplied-p))))
     (if (eq keys :none)
         '()
         (cons '&key
               (loop for ((keyword name) nil supplied-p) in keys
                     collect (list keyword name supplied-p))))
     (if allow-other-keys '(&allow-other-keys) '()))))

;;; Given a parsed lambda list where all the &OPTIONAL and &KEY
;;; parameters are known to have not only an INIT-FORM but also a
;;; SUPPLIED-P parameter, generate code that tests each SUPPLIED-P
;;; parameter and sets the parameter to the value of the INIT-FORM if
;;; it turns out the SUPPLIED-P parameter is false.
(defun extract-initforms (parsed-lambda-list)
  (let ((optionals (optionals parsed-lambda-list))
        (keys (keys parsed-lambda-list)))
    `(,@(if (eq optionals :none)
            '()
            (loop for (name init-form supplied-p) in optionals
                  collect `(unless ,supplied-p
                             (setq ,name ,init-form))))
      ,@(if (eq keys :none)
            '()
            (loop for ((nil name) init-form supplied-p) in keys
                  collect `(unless ,supplied-p
                             (setq ,name ,init-form)))))))

(defun preprocess-lambda-list (parsed-lambda-list)
  (let ((ll (ensure-supplied-p-parameters parsed-lambda-list)))
    (values (extract-entry-lambda-list ll)
            (extract-initforms ll))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; PARSE-MACRO
;;;
;;; According to CLtL2.

(defun parse-macro (name lambda-list body &optional environment)
  (declare (ignore name environment)) ; For now.
  (let* ((parsed-lambda-list (parse-macro-lambda-list lambda-list))
         (env-var (environment parsed-lambda-list))
         (final-env-var (if (eq env-var :none) (gensym) env-var))
         (form-var (whole parsed-lambda-list))
         (final-form-var (if (eq form-var :none) (gensym) form-var))
         (args-var (gensym)))
    (multiple-value-bind (declarations documentation forms)
        (separate-function-body body)
      (multiple-value-bind (bindings ignored-variables)
          (destructure-lambda-list parsed-lambda-list args-var)
        `(lambda (,final-form-var ,final-env-var)
           ,@(if (null documentation) '() (list documentation))
           ;; If the lambda list does not contain &environment, then
           ;; we IGNORE the GENSYMed parameter to avoid warnings.
           ;; If the lambda list does contain &environment, we do
           ;; not want to make it IGNORABLE because we would want a
           ;; warning if it is not used then.
           ,@(if (eq env-var :none)
                 `((declare (ignore ,final-env-var)))
                 `())
           (let ((,args-var (cdr ,final-form-var)))
             (let* ,bindings
               (declare (ignore ,@ignored-variables))
               ,@declarations
               ,@forms)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; PARSE-COMPILER-MACRO
;;;
;;; This function differs from parse-macro only in the code that
;;; destructures the lambda list from the arguments.

(defun parse-compiler-macro (name lambda-list body &optional environment)
  (declare (ignore name environment)) ; For now.
  (let* ((parsed-lambda-list (parse-macro-lambda-list lambda-list))
         (env-var (environment parsed-lambda-list))
         (final-env-var (if (eq env-var :none) (gensym) env-var))
         (form-var (whole parsed-lambda-list))
         (final-form-var (if (eq form-var :none) (gensym) form-var))
         (args-var (gensym)))
    (multiple-value-bind (bindings ignored-variables)
        (destructure-lambda-list parsed-lambda-list args-var)
      `(lambda (,final-form-var ,final-env-var)
         ;; If the lambda list does not contain &environment, then
         ;; we IGNORE the GENSYMed parameter to avoid warnings.
         ;; If the lambda list does contain &environment, we do
         ;; not want to make it IGNORABLE because we would want a
         ;; warning if it is not used then.
         ,@(if (eq env-var :none)
               `((declare (ignore ,final-env-var)))
               `())
         (let ((,args-var (if (and (eq (car ,final-form-var) 'funcall)
                                   (consp (cdr ,final-form-var))
                                   (consp (cadr ,final-form-var))
                                   (eq (car (cadr ,final-form-var)) 'function))
                              (cddr ,final-form-var)
                              (cdr ,final-form-var))))
           (let* ,bindings
             (declare (ignore ,@ignored-variables))
             ,@body))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; PARSE-DEFTYPE

(defun parse-deftype (name lambda-list body)
  (declare (ignore name))
  (let* ((parsed-lambda-list (parse-deftype-lambda-list lambda-list))
         (env-var (environment parsed-lambda-list))
         (final-env-var (if (eq env-var :none) (gensym) env-var))
         (form-var (whole parsed-lambda-list))
         (final-form-var (if (eq form-var :none) (gensym) form-var))
         (args-var (gensym)))
    (multiple-value-bind (bindings ignored-variables)
        (destructure-lambda-list parsed-lambda-list args-var)
      `(lambda (,final-form-var ,final-env-var)
         ;; If the lambda list does not contain &environment, then
         ;; we IGNORE the GENSYMed parameter to avoid warnings.
         ;; If the lambda list does contain &envionrment, we do
         ;; not want to make it IGNORABLE because we would want a
         ;; warning if it is not used then.
         ,@(if (eq env-var :none)
               `((declare (ignore ,final-env-var)))
               `())
         (let ((,args-var (cdr ,final-form-var)))
           (let* ,bindings
             (declare (ignore ,@ignored-variables))
             ,@body))))))
