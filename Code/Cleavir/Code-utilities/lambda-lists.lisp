(cl:in-package #:cleavir-code-utilities)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Lambda list utilities.

;;; There is much to say about lambda lists.
;;;
;;; There are 10 different types of lambda lists and they vary both
;;; with respect to syntax and semantics.  It gets pretty messy in
;;; fact.
;;;
;;; Lambda lists admit something known as "lambda list keywords".
;;; They are not keywords in the normal sense of the word (i.e.,
;;; symbols in the :KEYWORD package), but just ordinary symbols in
;;; the COMMON-LISP package that happen to have names that start with
;;; the `&' character (ampersand).
;;;
;;; The lambda list keywords that are allowed in each different type
;;; of lambda list are clearly indicated in the CLHS for that type of
;;; lambda list.  However, the CLHS also allows for
;;; implementation-specific lambda list keywords.  The complete list
;;; of lambda list keywords that a particular implementation
;;; recognizes is available as the value of the variable
;;; LAMBDA-LIST-KEYWORDS.  However, there is no way to determine what
;;; an implementation-specific lambda list keyword means, nor how it
;;; is used or even in which type of lambda list it is allowed.  There
;;; is also no indication as to whether implementation-specific lambda
;;; list keywords must begin with `&'.
;;;
;;; The lambda list keywords that are recognized by the CLHS are:
;;; &allow-other-keys, &aux, &body, &environment, &key, &optional,
;;; &rest, and &whole.  The lambda list keywords &body and &rest are
;;; synonymous, but good style gives preference to one rather than the
;;; other according to the type of lambda list it occurs in.
;;;
;;; To make things more complicated, the CLHS does not tell us how to
;;; handle occurrences of a particular lambda list keyword in a lambda
;;; list of a type that does not recognize it.  One interpretation
;;; could be that such a lambda list keyword should be treated as just
;;; any symbol, so that it becomes the name of a parameter.  But with
;;; this interpretation, a program can have some subtle bugs just
;;; because a programmer incorrectly believes that a particular
;;; lambda-list keyword is acceptable in a type of lambda list where
;;; in fact it is not.  According to another interpretation, a
;;; program-error should be signaled in this case.  At the very least,
;;; it seems reasonable to give a style-warning in that case.
;;;
;;; Similarly, the CLHS does not indicate how to handle occurrences of
;;; symbols that do not occur in LAMBDA-LIST-KEYWORDS, but that happen
;;; to start with `&'.  Again, some subtle bugs could result if such a
;;; situation were not to be flagged to the programmer.  Again, at the
;;; very least, a style warning seems to be appropriate.
;;;
;;; Lambda list keywords have different arities, i.e., the number of
;;; items that can follow it in a lambda list.  The good news is that
;;; each lambda list keyword has the same arity no matter what type of
;;; lambda list it occurs in.  Thus &allow-other-keys always has arity
;;; 0 (zero), &rest, &body, &whole, and &environment always have arity
;;; 1 (one), and the remaining ones (&aux, &key, and &optional) can
;;; take any number of items, so have arbitrary arity.
;;;
;;; Another piece of relatively good news is that the order in which
;;; lambda list keywords can occur in a lambda list is independent of
;;; the type of lambda list in which they occur, and that the relative
;;; order between two lambda list keywords is fixed, with &environment
;;; being the only exception, because it can occur anywhere (except
;;; before &whole) in the lambda lists in which it is allowed.
;;;
;;; A piece of not-so-good news is that &whole, whenever it is
;;; allowed, must appear first in the lambda list.  That is, not only
;;; first as in the first lambda list keyword, but as the first item
;;; in the lambda list, before the list of required variables.  This
;;; rule messes up syntax checking a bit.

;;; A list of lambda list keywords in the order that they can occur in
;;; a lambda list (except &environment, which can occur anywhere, and
;;; except &rest and &body which do not have any relative order
;;; because they cannot both occur).  For each keyword, we indicate
;;; its min and max arity, where NIL means unbounded.
(defparameter *lambda-list-keywords*
  `((&whole 1 1)
    (&environment 1 nil)
    (&optional 0 nil)
    (&rest 1 1)
    (&body 1 1)
    (&key 0 nil)
    (&allow-other-keys 0 0)
    (&aux 0 nil)))

(defun potential-lambda-list-keyword-p (object)
  (and (symbolp object)
       (plusp (length (symbol-name object)))
       (eql (char (symbol-name object) 0) #\&)))

;;; Use this function for lambda lists that can be proper or dotted.
(defun check-lambda-list-not-circular (lambda-list)
  (when (circular-list-p lambda-list)
    (error 'lambda-list-must-not-be-circular
           :code lambda-list)))

;;; Use this function for lambda lists that must be proper lists.
(defun check-lambda-list-proper (lambda-list)
  (unless (proper-list-p lambda-list)
    (error 'lambda-list-must-be-proper-list
           :code lambda-list)))

;;; Check for restrictions common to all lambda lists.
;;;
;;; Before calling this function, individual parsers must check the
;;; structure of the lambda list, in particular that it is a list, and
;;; that it is not circular.  This function can deal with proper and
;;; dotted lists.
;;;
;;; We do the following checks:
;;;
;;;  * check for lambda list keywords not allowed
;;;
;;;  * check and warn if a symbol starting with & apperas, but
;;;    it is not a recognized lambda list keyword.
;;;
;;;  * check that each keyword appears with the correct arity.
;;;
;;;  * check that each keyword appears at most once.
;;;
;;;  * check that the keywords appear in the right order.
;;;
;;;  * check that if &whole appears, it appears first.  This is safe,
;;;    because we have first checked whether &whole is allowed at all,
;;;    so there is no risk that we will give an error message about
;;;    &whole for a lambda list that does not allow it.
;;;
;;; We do NOT do any of the following checks:
;;;
;;;  * We do not check the restrictions on keywords that must be
;;;    respected for dotted lists, because it would look funny if such
;;;    an error were reported for a lambda list that is not allowed to
;;;    be dotted in the first place.
;;;
;;;  * We do not check the nature of the arguments to the lambda list
;;;    keywords.  The parser for each type of lambda list must do that.
(defun check-lambda-list-keywords (lambda-list keywords)
  ;; We assume that KEYWORDS is a subset of LAMBDA-LIST-KEYWORDS, in
  ;; other words that we are given only valid lambda list keywords as
  ;; defined by the system.
  (let* (;; All symbols in the lambda list that look like they might
         ;; be lambda-list keywords, in the order that the occur in
         ;; the lambda list.  Multiple occurrences are preserved.
         (potential (loop for remaining = lambda-list then (cdr remaining)
                          while (consp remaining)
                          when (potential-lambda-list-keyword-p (car remaining))
                            collect (car remaining)))

         ;; All symbols in the lambda list that are also lambda-list
         ;; keywords as defined by the system, in the order that they
         ;; occur in the lambda list.
         (real (remove-if-not (lambda (x) (member x lambda-list-keywords))
                              potential))
         ;; All symbols in the lambda list that look like they might
         ;; be lambda-list keywords, but that are not lambda list
         ;; keywords defined by the system, in any old order.
         (suspect (set-difference potential lambda-list-keywords))
         ;; All symbols in the lambda list that are also lambda-list
         ;; keywords as defined by the system, but that are not in the
         ;; list of lambda list keywords allowed for this type of
         ;; lambda list, in any old order.
         (forbidden (set-difference real keywords))
         ;; All symbols in the lambda list that are also in the list
         ;; of valid keywords for this lambda list, in the order that
         ;; they appear in the lambda list.  Multiple occurrences are
         ;; preserved.
         (to-process (remove-if-not (lambda (x) (member x keywords))
                                    potential)))
    ;; Check for forbidden keywords.
    (unless (null forbidden)
        (error 'lambda-list-keyword-not-allowed
               :code lambda-list
               :keyword (car forbidden)))
    ;; Check for suspect keywords.
    (unless (null suspect)
      (warn 'suspect-lambda-list-keyword
            :code lambda-list
            :keyword (car suspect)))
    ;; Check for multiple occurrences.
    (loop for keyword in to-process
          do (when (> (count keyword to-process) 1)
               (error 'multiple-occurrences-of-lambda-list-keyword
                      :code lambda-list
                      :keyword keyword)))
    (when (> (+ (count '&body to-process) (count '&rest to-process)) 1)
      (error 'both-rest-and-body-occur-in-lambda-list
             :code lambda-list))
    ;; Check the order of keywords.
    (loop for rem = to-process then (cdr rem)
          until (null (cdr rem))
          do (when (and (not (eq (car rem) '&environment))
                        (not (eq (cadr rem) '&environment))
                        (> (position (car rem) *lambda-list-keywords* :key #'car)
                           (position (cadr rem) *lambda-list-keywords* :key #'car)))
               (error 'incorrect-keyword-order
                      :code lambda-list
                      :keyword1 (car rem)
                      :keyword2 (cadr rem))))
    ;; Check arities.
    (flet ((check-arity (keyword number-of-args)
             (if (eq keyword '&whole)
                 (when (zerop number-of-args)
                   (error 'whole-must-be-followed-by-variable
                          :code lambda-list))
                 (let ((arities (cdr (assoc keyword *lambda-list-keywords*))))
                   (when (or (< number-of-args (car arities))
                             (and (not (null (cadr arities)))
                                  (> number-of-args (cadr arities))))
                     (error "wrong arity for ~s" keyword))))))
      (loop with positions = (mapcar (lambda (x) (position x lambda-list))
                                     to-process)
            for keyword in to-process
            for (pos next-pos) on (append positions
                                          (list (list-structure lambda-list)))
            do (check-arity keyword (- next-pos pos 1))))
    ;; Check that if &whole is present, it appears first.
    (when (and (member '&whole to-process)
               (not (eq (car lambda-list) '&whole)))
      (error 'whole-must-appear-first
             :code lambda-list))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; A pattern is either:
;;;
;;;  * a tree (a symbol or a CONS cell), or
;;;  * an instance of the class LAMBDA-LIST.
;;;
;;; An &optional entry (after canonicalization) is one of:
;;;
;;;  * (pattern init-form)
;;;  * (pattern init-form supplied-p-parameter)
;;;
;;; A &key entry (after canonicalization) is one of:
;;;
;;;  * ((keyword pattern) init-form)
;;;  * ((keyword pattern) init-form supplied-p-parameter)
;;;
;;; An &aux entry (after canonicalization) is of the form:
;;;
;;;  * (var init-form)


(defgeneric required (lambda-list))
(defgeneric (setf required) (required lambda-list))
(defgeneric environment (lambda-list))
(defgeneric (setf environment) (environment lambda-list))
(defgeneric whole (lambda-list))
(defgeneric (setf whole) (whole lambda-list))
(defgeneric optionals (lambda-list))
(defgeneric (setf optionals) (optionals lambda-list))
(defgeneric rest-body (lambda-list))
(defgeneric (setf rest-body) (rest-body lambda-list))
(defgeneric keys (lambda-list))
(defgeneric (setf keys) (keys lambda-list))
(defgeneric allow-other-keys (lambda-list))
(defgeneric (setf allow-other-keys) (allow-other-keys lambda-list))
(defgeneric aux (lambda-list))
(defgeneric (setf aux) (aux lambda-list))

(defclass lambda-list ()
  (;; A possibly empty list of patterns.
   (%required :initform '() :initarg :required :accessor required)
   ;; Either:
   ;;  * :none, meaning &environment was not given, or
   ;;  * a single variable, represented as a symbol.
   (%environment :initform :none :initarg :environment :accessor environment)
   ;; Either:
   ;;  * :none, meaning &whole was not given, or
   ;;  * a single variable, represented as a symbol.
   (%whole :initform :none :initarg :whole :accessor whole)
   ;; Either:
   ;;  * :none, meaning &optional was not given at all,
   ;;  * a possibly empty list of &optional entries.
   (%optionals :initform :none :initarg :optionals :accessor optionals)
   ;; Either:
   ;;  * :none, meaning &rest or &body was not given at all, or
   ;;  * a single pattern.
   (%rest-body :initform :none :initarg :rest-body :accessor rest-body)
   ;; Either:
   ;;  * :none, meaning &key was not given at all,
   ;;  * a possibly empty list of &key entries.
   (%keys :initform :none :initarg :keys :accessor keys)
   ;; Either:
   ;;  * nil, meaning &allow-other-keys was not given at all,
   ;;  * t, meaning &allow-other-keys was given.
   (%allow-other-keys :initform nil
                      :initarg :allow-other-keys
                      :accessor allow-other-keys)
   ;; Either:
   ;;  * :none, meaning &aux was not given at all,
   ;;  * a possibly empty list of &aux entries.
   (%aux :initform '() :initarg :aux :accessor aux)))

(defun list-has-keyword-p (list)
  (loop for rest = list then (cdr rest)
        while (consp rest)
        when (member (car rest) *lambda-list-keywords* :key #'car)
          return t))

;;; We only check that the tree doesn't have any illegal atoms in it.
;;; At this point, we do not check for multiple occurrences of
;;; variables.
(defun check-tree (tree)
  (labels ((check-aux (subtree)
             (cond ((or (null subtree)
                        (and (symbolp subtree)
                             (not (constantp subtree))))
                    nil)
                   ((consp subtree)
                    (check-aux (car subtree))
                    (check-aux (cdr subtree)))
                   (t
                    (error 'malformed-destructuring-tree
                           :code tree)))))
    (check-aux tree)))

(defun parse-pattern (tree-or-lambda-list)
  (cond ((and (symbolp tree-or-lambda-list)
              (not (constantp tree-or-lambda-list)))
         tree-or-lambda-list)
        ((consp tree-or-lambda-list)
         (cond ((list-has-keyword-p tree-or-lambda-list)
                (parse-destructuring-lambda-list tree-or-lambda-list))
               (t
                (check-tree tree-or-lambda-list)
                tree-or-lambda-list)))
        (t
         (error 'malformed-lambda-list-pattern :code nil))))

(defun parse-ordinary-required (required)
  (unless (and (symbolp required)
               (not (constantp required)))
    (error 'required-must-be-variable
           :code required))
  required)

(defun parse-destructuring-required (required)
  ;; A required argument can be any pattern.
  (parse-pattern required))

;;; Parse a specialized required parameter.
;;; We canonicalize it, so that instead of having the original
;;; 3 different possible forms:
;;;
;;;   * var
;;;   * (var)
;;;   * (var specializer)
;;;
;;; we boil it down to just 1:
;;;
;;;   * (var specializer)
;;;
;;; by replacing var or (var) by (var t)
(defun parse-specialized-required (required)
  (if (consp required)
      (progn
        (unless (and (symbolp (car required))
                     (not (constantp (car required)))
                     (or (null (cdr required))
                         (and (null (cddr required))
                              (or (symbolp (cadr required))
                                  (and (consp (cadr required))
                                       (consp (cdadr required))
                                       (null (cddadr required))
                                       (eq (caadr required) 'eql))))))
          (error 'malformed-specialized-required
                 :code required))
        `(,(car required) ,(if (null (cdr required)) t (cadr required))))
      (progn
        (unless (and (symbolp required)
                     (not (constantp required)))
          (error 'malformed-specialized-required
                 :code required))
        `(,required t))))

(defun parse-all-required (lambda-list start end item-parser)
  (loop for i from start below end
        for rest = (nthcdr start lambda-list) then (cdr rest)
        for required = (car rest)
        collect (funcall item-parser required)))

;;; Parse an ordinary &optional item.
;;; We canonicalize it a bit, so that instead of having the original
;;; 4 different possible forms:
;;;
;;;   * var
;;;   * (var)
;;;   * (var init-form)
;;;   * (var init-form supplied-p-parameter)
;;;
;;; we boil it down to 2:
;;;
;;;   * (var init-form)
;;;   * (var init-form supplied-p-parameter)
;;;
;;; by replacing var or (var) by (var nil)
(defun parse-ordinary-optional (optional)
  (if (consp optional)
      (multiple-value-bind (length structure)
          (list-structure optional)
        (unless (and (eq structure :proper)
                     (<= 1 length 3)
                     (symbolp (car optional))
                     (not (constantp (car optional)))
                     (or (< length 3)
                         (symbolp (caddr optional))
                         (not (constantp (caddr optional)))))
          (error 'malformed-ordinary-optional
                 :code optional))
        `(,(car optional)
          ,(if (> length 1) (cadr optional) nil)
          . ,(cddr optional)))
      (progn
        (unless (and (symbolp optional)
                     (not (constantp optional)))
          (error 'malformed-ordinary-optional
                 :code optional))
        `(,optional nil))))

;;; Parse a defgeneric &optional item.
;;; We canonicalize it, so that instead of having the original
;;; 2 different possible forms:
;;;
;;;   * var
;;;   * (var)
;;;
;;; we boil it down to just 1:
;;;
;;;   * var
;;;
;;; by replacing (var) by var.
(defun parse-defgeneric-optional (optional)
  (if (consp optional)
      (progn
        (unless (and (null (cdr optional))
                     (symbolp (car optional))
                     (not (constantp (car optional))))
          (error 'malformed-defgeneric-optional
                 :code optional))
        (car optional))
      (progn
        (unless (and (symbolp optional)
                     (not (constantp optional)))
          (error 'malformed-defgeneric-optional
                 :code optional))
        optional)))

;;; Parse a destructuring &optional item.
;;; We canonicalize it a bit, so that instead of having the original
;;; 4 different possible forms:
;;;
;;;   * var
;;;   * (pattern)
;;;   * (pattern init-form)
;;;   * (pattern init-form supplied-p-parameter)
;;;
;;; we boil it down to 2:
;;;
;;;   * (pattern init-form)
;;;   * (pattern init-form supplied-p-parameter)
;;;
;;; by replacing var by (var <default>) and (pattern) by (pattern nil).
(defun parse-destructuring/deftype-optional (optional default)
  (if (consp optional)
      (multiple-value-bind (length structure)
          (list-structure optional)
        (unless (and (eq structure :proper)
                     (<= 1 length 3)
                     (or (< length 3)
                         (symbolp (caddr optional))
                         (not (constantp (caddr optional)))))
          (error 'malformed-destructuring-optional
                 :code optional))
        `(,(car optional)
          ,(if (> length 1) (cadr optional) `',default)
          . ,(cddr optional)))
      (progn
        (unless (and (symbolp optional)
                     (not (constantp optional)))
          (error 'malformed-destructuring-optional
                 :code optional))
        `(,optional ',default))))

(defun parse-destructuring-optional (optional)
  (parse-destructuring/deftype-optional optional nil))

(defun parse-deftype-optional (optional)
  (parse-destructuring/deftype-optional optional '*))

(defun parse-all-optionals
    (lambda-list positions item-parser)
  (cond ((and
          ;; there is a keyword yet to be processed.
          (not (null (cdr positions)))
          ;; that keyword is &optional.
          (eq (elt lambda-list (car positions)) '&optional))
         (values (loop for i from (1+ (car positions)) below (cadr positions)
                       for optional in (nthcdr (1+ (car positions)) lambda-list)
                       collect (funcall item-parser optional))
                 (cdr positions)))
        (t
         (values :none positions))))

;;; Parse an ordinary &key item.
;;; We canonicalize it a bit, so that instead of having the original
;;; 7 different possible forms:
;;;
;;;   * var
;;;   * (var)
;;;   * (var init-form)
;;;   * (var init-form supplied-p-parameter)
;;;   * ((keyword var))
;;;   * ((keyword var) init-form)
;;;   * ((keyword var) init-form supplied-p-parameter)
;;;
;;; we boil it down to 2:
;;;
;;;   * ((keyword var) init-form)
;;;   * ((keyword var) init-form supplied-p-parameter)
;;;
;;; by replacing var or (var) by ((:var var) nil),
;;; by replacing (var init-form) by ((:var var) init-form), and
;;; by replacing (var init-form supplied-p-parameter) by
;;; ((:var var) init-form supplied-p-parameter).
(defun parse-ordinary-key (key)
  (if (consp key)
      (multiple-value-bind (length structure)
          (list-structure key)
        (unless (and (eq structure :proper)
                     (<= 1 length 3)
                     (or (and (symbolp (car key))
                              (not (constantp (car key))))
                         (and (consp (car key))
                              (symbolp (caar key))
                              (consp (cdar key))
                              (symbolp (cadar key))
                              (not (constantp (cadar key)))
                              (null (cddar key))))
                     (or (< length 3)
                         (symbolp (caddr key))
                         (not (constantp (caddr key)))))
          (error 'malformed-ordinary-key
                 :code key))
        `(,(if (symbolp (car key))
               `(,(intern (symbol-name (car key)) :keyword) ,(car key))
               (car key))
          ,(if (> length 1) (cadr key) nil)
          . ,(cddr key)))
      (progn
        (unless (and (symbolp key)
                     (not (constantp key)))
          (error 'malformed-ordinary-key
                 :code key))
        `((,(intern (symbol-name key) :keyword) ,key) nil))))

;;; Parse a defgeneric &key item.
;;; We canonicalize it, so that instead of having the original
;;; 3 different possible forms:
;;;
;;;   * var
;;;   * (var)
;;;   * ((keyword var))
;;;
;;; we boil it down to just 1:
;;;
;;;   * ((keyword var))
;;;
;;; by replacing var and (var) by ((:var var))
(defun parse-defgeneric-key (key)
  (if (consp key)
      (progn
        (unless (and (null (cdr key))
                     (or (and (symbolp (car key))
                              (not (constantp (car key))))
                         (and (consp (car key))
                              (symbolp (caar key))
                              (consp (cdar key))
                              (symbolp (cadar key))
                              (not (constantp (cadar key)))
                              (null (cddar key)))))
          (error 'malformed-defgeneric-key
                 :code key))
        `(,(if (symbolp (car key))
               `(,(intern (symbol-name (car key)) :keyword) ,(car key))
               (car key))))
      (progn
        (unless (and (symbolp key)
                     (not (constantp key)))
          (error 'malformed-defgeneric-key
                 :code key))
        `(,(intern (symbol-name key) :keyword) ,key))))

;;; Parse a destructuring &key item.
;;; We canonicalize it a bit, so that instead of having the original
;;; 7 different possible forms:
;;;
;;;   * var
;;;   * (var)
;;;   * (var init-form)
;;;   * (var init-form supplied-p-parameter)
;;;   * ((keyword pattern))
;;;   * ((keyword pattern) init-form)
;;;   * ((keyword pattern) init-form supplied-p-parameter)
;;;
;;; we boil it down to 2:
;;;
;;;   * ((keyword pattern) init-form)
;;;   * ((keyword pattern) init-form supplied-p-parameter)
;;;
;;; by replacing var or (var) by ((:var var) <default>),
;;; by replacing (var init-form) by ((:var var) init-form), and
;;; by replacing (var init-form supplied-p-parameter) by
;;; ((:var var) init-form supplied-p-parameter).
(defun parse-destructuring/deftype-key (key default)
  (if (consp key)
      (multiple-value-bind (length structure)
          (list-structure key)
        (unless (and (eq structure :proper)
                     (<= 1 length 3)
                     (or (and (symbolp (car key))
                              (not (constantp (car key))))
                         (and (consp (car key))
                              (symbolp (caar key))
                              (consp (cdar key))
                              (null (cddar key))))
                     (or (< length 3)
                         (symbolp (caddr key))
                         (not (constantp (caddr key)))))
          (error 'malformed-ordinary-key
                 :code key))
        `(,(if (symbolp (car key))
               `(,(intern (symbol-name (car key)) :keyword) ,(car key))
               `(,(caar key) ,(parse-pattern (cadar key))))
          ,(if (> length 1) (cadr key) default)
          . ,(cddr key)))
      (progn
        (unless (and (symbolp key)
                     (not (constantp key)))
          (error 'malformed-ordinary-key
                 :code key))
        `((,(intern (symbol-name key) :keyword) ,key) ,default))))

(defun parse-destructuring-key (key)
  (parse-destructuring/deftype-key key nil))

(defun parse-deftype-key (key)
  (parse-destructuring/deftype-key key '*))

(defun parse-all-keys
    (lambda-list positions item-parser)
  (cond ((and
          ;; there is a keyword yet to be processed.
          (not (null (cdr positions)))
          ;; that keyword is &key.
          (eq (elt lambda-list (car positions)) '&key))
         (values (loop for i from (1+ (car positions)) below (cadr positions)
                       for key in (nthcdr (1+ (car positions)) lambda-list)
                       collect (funcall item-parser key))
                 (cdr positions)))
        (t
         (values :none positions))))

;;; Parse an &aux item.
;;; We canonicalize it, so that instead of having the original
;;; 3 different possible forms:
;;;
;;;   * var
;;;   * (var)
;;;   * (var intitform)
;;;
;;; we boil it down to just 1:
;;;
;;;   * (var initform)
;;;
;;; by replacing var and (var) by (var nil)
(defun parse-aux (aux)
  (if (consp aux)
      (progn
        (unless (and (symbolp (car aux))
                     (not (constantp (car aux)))
                     (or (null (cdr aux))
                         (null (cddr aux))))
          (error 'malformed-aux
                 :code aux))
        `(,(car aux) ,(if (null (cdr aux)) nil (cadr aux))))
      (progn
        (unless (and (symbolp aux)
                     (not (constantp aux)))
          (error 'malformed-aux
                 :code aux))
        `(,aux nil))))

(defun parse-all-aux (lambda-list positions)
  (cond ((and
          ;; there is a keyword yet to be processed.
          (not (null (cdr positions)))
          ;; that keyword is &aux.
          (eq (elt lambda-list (car positions)) '&aux))
         (values (loop for i from (1+ (car positions)) below (cadr positions)
                       for aux in (nthcdr (1+ (car positions)) lambda-list)
                       collect (parse-aux aux))
                 (cdr positions)))
        (t
         (values :none positions))))

(defun parse-allow-other-keys (lambda-list positions)
  (cond ((and
          ;; there is a keyword yet to be processed.
          (not (null (cdr positions)))
          ;; that keyword is &allow-other-keys.
          (eq (elt lambda-list (car positions)) '&allow-other-keys))
         (values t (cdr positions)))
        (t
         (values nil positions))))

(defun parse-environment (lambda-list positions)
  (cond ((and
          ;; there is a keyword yet to be processed.
          (not (null (cdr positions)))
          ;; that keyword is &environment.
          (eq (elt lambda-list (car positions)) '&environment))
         ;; The arity has already been checked so we know there is
         ;; something after it, but we don't know what.
         (let ((arg (elt lambda-list (1+ (car positions)))))
           (unless (and (symbolp arg)
                        (not (constantp arg)))
             (error 'environment-must-be-followed-by-variable
                    :code lambda-list))
           (values arg (cdr positions))))
        (t
         (values :none positions))))

(defun parse-rest/body (lambda-list positions)
  (cond ((and
          ;; there is a keyword yet to be processed.
          (not (null (cdr positions)))
          ;; that keyword is &rest or &body
          (or (eq (elt lambda-list (car positions)) '&rest)
              (eq (elt lambda-list (car positions)) '&body)))
         ;; The arity has already been checked so we know there is
         ;; something after it, but we don't know what.
         (let ((arg (elt lambda-list (1+ (car positions)))))
           (unless (and (symbolp arg)
                        (not (constantp arg)))
             (error 'rest/body-must-be-followed-by-variable
                    :code lambda-list))
           (values arg (cdr positions))))
        (t
         (values :none positions))))

(defun parse-destructuring-rest/body (lambda-list positions)
  (cond ((and
          ;; there is a keyword yet to be processed.
          (not (null (cdr positions)))
          ;; that keyword is &rest or &body
          (or (eq (elt lambda-list (car positions)) '&rest)
              (eq (elt lambda-list (car positions)) '&body)))
         ;; The arity has already been checked so we know there is
         ;; something after it, but we don't know what.
         (let ((arg (elt lambda-list (1+ (car positions)))))
           (values (parse-pattern arg) (cdr positions))))
        (t
         (values :none positions))))

(defun parse-whole (lambda-list positions)
  (cond ((and
          ;; there is a keyword yet to be processed.
          (not (null (cdr positions)))
          ;; that keyword is &whole
          (eq (elt lambda-list (car positions)) '&whole))
         ;; The arity has already been checked so we know there is
         ;; something after it, but we don't know what.
         (let ((arg (elt lambda-list (1+ (car positions)))))
           (unless (and (symbolp arg)
                        (not (constantp arg)))
             (error 'whole-must-be-followed-by-variable
                    :code lambda-list))
           (values arg (cdr positions))))
        (t
         (values :none positions))))

;;; Compute the position of each of the allowed keywords
;;; that appears in the lambda list, and add the length
;;; of the lambda list (i.e., the number of CONS cells it has)
;;; at the end of the computed list.
(defun compute-keyword-positions (lambda-list allowed)
  (loop for rest = lambda-list then (cdr rest)
        for i from 0
        unless (consp rest) collect i
        while (consp rest)
        when (member (car rest) allowed)
          collect i))

(defun finalize-optionals (optionals)
  (if (eq optionals :none) '() (list (cons '&optional optionals))))

(defun finalize-rest-body (rest-body)
  (if (eq rest-body :none) '() (list (list '&rest rest-body))))

(defun finalize-keys (keys allow-other-keys)
  (if (eq keys :none)
      '()
      (list (append (cons '&key keys)
                    (if (null allow-other-keys)
                        '()
                        (list '&allow-other-keys))))))

(defun finalize-aux (aux)
  (if (eq aux :none) '() (list (cons '&aux aux))))

(defun parse-generic-function-lambda-list (lambda-list)
  (let ((allowed '(&optional &rest &key &allow-other-keys)))
    (check-lambda-list-proper lambda-list)
    (check-lambda-list-keywords lambda-list allowed)
    (let ((positions (compute-keyword-positions lambda-list allowed))
          required optionals rest-body keys allow-other-keys)
      (setf required
            (parse-all-required
             lambda-list 0 (car positions) #'parse-ordinary-required))
      (setf (values optionals positions)
            (parse-all-optionals
             lambda-list positions #'parse-defgeneric-optional))
      (setf (values rest-body positions)
            (parse-rest/body lambda-list positions))
      (setf (values keys positions)
            (parse-all-keys
             lambda-list positions #'parse-defgeneric-key))
      (setf (values allow-other-keys positions)
            (parse-allow-other-keys lambda-list positions))
      ;; We should have run out of parameters now.
      (unless (null (cdr positions))
        (error 'lambda-list-too-many-parameters :parameters (cdr positions)))
      (append (list required)
              (finalize-optionals optionals)
              (finalize-rest-body rest-body)
              (finalize-keys keys allow-other-keys)))))

(defun parse-destructuring-lambda-list (lambda-list)
  (multiple-value-bind (length structure) (list-structure lambda-list)
    (when (eq structure :circular)
      (error 'lambda-list-must-not-be-circular
             :code lambda-list))
    (if (eq structure :dotted)
        (progn
          (when (zerop length)
            (error 'lambda-list-must-be-list
                   :code lambda-list))
          (let ((allowed '(&whole &optional)))
            (check-lambda-list-keywords lambda-list allowed)
            (let ((positions (compute-keyword-positions lambda-list allowed))
                  (result (make-instance 'lambda-list)))
              (if (eq (car lambda-list) '&whole)
                  (progn
                    (setf (values (whole result) positions)
                          (parse-whole lambda-list positions))
                    (setf (required result)
                          (parse-all-required lambda-list
                                              2 (car positions)
                                              #'parse-pattern)))
                  (setf (required result)
                        (parse-all-required lambda-list
                                            0 (car positions)
                                            #'parse-pattern)))
              (setf (values (optionals result) positions)
                    (parse-all-optionals
                     lambda-list positions #'parse-destructuring-optional))
              ;; We should have run out of parameters now.
              (unless (null (cdr positions))
                (error 'lambda-list-too-many-parameters :parameters (cdr positions)))
              ;; All that remains is to deal with the dotted end
              ;; of the list.
              (let ((rest (cdr (last lambda-list))))
                (unless (and (symbolp rest)
                             (not (constantp rest)))
                  (error 'atomic-lambda-list-tail-must-be-variable
                         :code lambda-list))
                (setf (rest-body result) rest))
              result)))
        (progn
          (let ((allowed '(&whole &optional &rest &body
                           &key &allow-other-keys &aux)))
            (check-lambda-list-keywords lambda-list allowed)
            (let ((positions (compute-keyword-positions lambda-list allowed))
                  (result (make-instance 'lambda-list)))
              (if (eq (car lambda-list) '&whole)
                  (progn
                    (setf (values (whole result) positions)
                          (parse-whole lambda-list positions))
                    (setf (required result)
                          (parse-all-required lambda-list
                                              2 (car positions)
                                              #'parse-pattern)))
                  (setf (required result)
                        (parse-all-required lambda-list
                                            0 (car positions)
                                            #'parse-pattern)))
              (setf (values (optionals result) positions)
                    (parse-all-optionals
                     lambda-list positions #'parse-destructuring-optional))
              (setf (values (rest-body result) positions)
                    (parse-destructuring-rest/body lambda-list positions))
              (setf (values (keys result) positions)
                    (parse-all-keys
                     lambda-list positions #'parse-destructuring-key))
              (setf (values (allow-other-keys result) positions)
                    (parse-allow-other-keys lambda-list positions))
              (setf (values (aux result) positions)
                    (parse-all-aux lambda-list positions))
              ;; We should have run out of parameters now.
              (unless (null (cdr positions))
                (error 'lambda-list-too-many-parameters :parameters (cdr positions)))
              result))))))

(defun parse-defsetf-lambda-list (lambda-list)
  (let ((allowed '(&optional &rest &key &allow-other-keys &environment)))
    (check-lambda-list-proper lambda-list)
    (check-lambda-list-keywords lambda-list allowed)
    (let ((positions (compute-keyword-positions lambda-list allowed))
          (result (make-instance 'lambda-list)))
      ;; FIXME: check that if &environment occurs, then it is last.
      (setf (required result)
            (parse-all-required
             lambda-list 0 (car positions) #'parse-ordinary-required))
      (setf (values (optionals result) positions)
            (parse-all-optionals
             lambda-list positions #'parse-ordinary-optional))
      (setf (values (rest-body result) positions)
            (parse-rest/body lambda-list positions))
      (setf (values (keys result) positions)
            (parse-all-keys
             lambda-list positions #'parse-ordinary-key))
      (setf (values (allow-other-keys result) positions)
            (parse-allow-other-keys lambda-list positions))
      (setf (values (environment result) positions)
            (parse-environment lambda-list positions))
      ;; We should have run out of parameters now.
      (unless (null (cdr positions))
        (error 'lambda-list-too-many-parameters :parameters (cdr positions)))
      result)))

(defun parse-define-modify-macro-lambda-list (lambda-list)
  (let ((allowed '(&optional &rest)))
    (check-lambda-list-proper lambda-list)
    (check-lambda-list-keywords lambda-list allowed)
    (let ((positions (compute-keyword-positions lambda-list allowed))
          (result (make-instance 'lambda-list)))
      (setf (required result)
            (parse-all-required
             lambda-list 0 (car positions) #'parse-ordinary-required))
      (setf (values (optionals result) positions)
            (parse-all-optionals
             lambda-list positions #'parse-ordinary-optional))
      (setf (values (rest-body result) positions)
            (parse-rest/body lambda-list positions))
      ;; We should have run out of parameters now.
      (unless (null (cdr positions))
        (error 'lambda-list-too-many-parameters :parameters (cdr positions)))
      result)))

(defun parse-define-method-combination-arguments-lambda-list
    (lambda-list)
  (let ((allowed '(&whole &optional &rest &key &allow-other-keys &aux)))
    (check-lambda-list-proper lambda-list)
    (check-lambda-list-keywords lambda-list allowed)
    (let ((positions (compute-keyword-positions lambda-list allowed))
          (result (make-instance 'lambda-list)))
      (setf (required result)
            (parse-all-required
             lambda-list 0 (car positions) #'parse-ordinary-required))
      (setf (values (optionals result) positions)
            (parse-all-optionals
             lambda-list positions #'parse-ordinary-optional))
      (setf (values (rest-body result) positions)
            (parse-rest/body lambda-list positions))
      (setf (values (keys result) positions)
            (parse-all-keys
             lambda-list positions #'parse-ordinary-key))
      (setf (values (allow-other-keys result) positions)
            (parse-allow-other-keys lambda-list positions))
      (setf (values (aux result) positions)
            (parse-all-aux lambda-list positions))
      ;; We should have run out of parameters now.
      (unless (null (cdr positions))
        (error 'lambda-list-too-many-parameters :parameters (cdr positions)))
      result)))
