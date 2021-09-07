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

;;; It may be possible to use generic functions here if we are clever
;;; during bootstrapping and make sure the generic functions are
;;; satiated before being used.  But for now, we use lists instead of
;;; standard objects.

(defun lambda-list-keyword (intrinsic-feature)
  (first intrinsic-feature))

(defun arity (intrinsic-feature)
  (second intrinsic-feature))

(defun occurrence-count (intrinsic-feature)
  (third intrinsic-feature))

(defun order (intrinsic-feature)
  (fourth intrinsic-feature))

;;; ARITY can be:
;;;
;;; - A non-negative fixnum, which is then the number of items that
;;;   must follow.
;;;
;;; - The symbol *, which means that any number of items can follow.
;;;
;;; OCCURRENCE-COUNT is The number of times this lambda-list keyword
;;; can occur in any lambda list.
(defun make-intrinsic-feature
    (lambda-list-keyword arity occurrence-count order)
  (list lambda-list-keyword arity occurrence-count order))

(defparameter *intrinsic-features*
  (list (make-intrinsic-feature '&whole            1  1 100)
        (make-intrinsic-feature '&environment      1  1 nil)
        (make-intrinsic-feature '&optional         '* 1 200)
        (make-intrinsic-feature '&rest             1  1 300)
        (make-intrinsic-feature '&body             1  1 300)
        (make-intrinsic-feature '&key              '* 1 400)
        (make-intrinsic-feature '&allow-other-keys 0  1 500)
        (make-intrinsic-feature '&aux              '* 1 600)))

(defun intrinsic-keywords ()
  (mapcar #'lambda-list-keyword *intrinsic-features*))

(defun find-feature (keyword)
  (find keyword *intrinsic-features*
        :test #'eq :key #'lambda-list-keyword))

(defun compute-positions (lambda-list keywords)
  (append (loop with end-position = -1
                with result = '()
                for i from 0
                for element in lambda-list
                do (when (member element keywords)
                     (when (<= i end-position)
                       ;; This means that the previous keyword had
                       ;; fewer or equal number of arguments compared
                       ;; to its declared arity.  Then we don't want
                       ;; to pick up the position corresponding to
                       ;; that arity, and we instead just pick up the
                       ;; current one.
                       (setf end-position -1))
                     (let ((arity (arity (find-feature element))))
                       (when (integerp arity)
                         (setf end-position (+ i arity 1)))
                       (push i result)))
                   (when (= i end-position)
                     (push end-position result))
                finally (return (reverse result)))
          (list (length lambda-list))))

(defun check-allowed (keyword canonicalizers)
  (unless (member keyword canonicalizers :test #'eq :key #'car)
    (error "Illegal lambda list keyword: ~s" keyword)))

(defun check-all-allowed (keywords canonicalizers)
  (loop for keyword in keywords
        do (check-allowed keyword canonicalizers)))

(defun check-occurrence-counts (keywords)
  (let ((occurrences '()))
    (loop for keyword in keywords
          for entry = (assoc keyword occurrences)
          do (if (null entry)
                 (push (cons keyword 1) occurrences)
                 (incf (cdr entry))))
    (loop for (keyword . count) in occurrences
          for feature = (find-feature keyword)
          unless (= count (occurrence-count feature))
            do (error "Lambda-list keyword ~s occurs ~s times"
                      keyword count))))

(defun check-order (keywords)
  (let ((orders (loop for keyword in keywords
                      for feature = (find-feature keyword)
                      for order = (order feature)
                      unless (null order)
                        collect order)))
    ;; FIXME: give more details in errors.
    (unless (apply #'< -2 -1 orders)
      (error "Lambda-list keywords occur in the wrong order"))))

(defun check-arities (canonicalized-lambda-list)
  (loop with keywords = (intrinsic-keywords)
        for group in canonicalized-lambda-list
        for first = (first group)
        do (when (member first keywords :test #'eq)
             (let* ((feature (find-feature first))
                    (arity (arity feature)))
               (unless (or (eq arity '*)
                           (= arity (length (rest group))))
                 (error "Incorrect arity for lambda-list keyword ~s"
                        first))))))

(defun canonicalize-ordinary-required (parameter)
  (unless (and (symbolp parameter)
               (not (constantp parameter)))
    (error 'required-must-be-variable
           :code parameter))
  parameter)

;;; Canonicalize an ordinary &optional item.
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
(defun canonicalize-nontrivial-optional (optional default)
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
          ,(if (> length 1) (cadr optional) default)
          . ,(cddr optional)))
      (progn
        (unless (and (symbolp optional)
                     (not (constantp optional)))
          (error 'malformed-ordinary-optional
                 :code optional))
        `(,optional ,default))))

(defun canonicalize-ordinary-optional (optional)
  (canonicalize-nontrivial-optional optional 'nil))

(defun canonicalize-deftype-optional (optional)
  (canonicalize-nontrivial-optional optional ''*))

(defun canonicalize-ordinary-rest (parameter)
  (unless (and (symbolp parameter)
               (not (constantp parameter)))
    (error 'rest/body-must-be-followed-by-variable
           :code parameter))
  parameter)

;;; Canonicalize an ordinary &key item.
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
(defun canonicalize-nontrivial-key (key default)
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
          ,(if (> length 1) (cadr key) default)
          . ,(cddr key)))
      (progn
        (unless (and (symbolp key)
                     (not (constantp key)))
          (error 'malformed-ordinary-key
                 :code key))
        `((,(intern (symbol-name key) :keyword) ,key) ,default))))

(defun canonicalize-ordinary-key (key)
  (canonicalize-nontrivial-key key 'nil))

(defun canonicalize-deftype-key (key)
  (canonicalize-nontrivial-key key '*))

;;; Canonicalize a defgeneric &optional item.
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
(defun canonicalize-defgeneric-optional (optional)
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

;;; Canonicalize a defgeneric &key item.
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
(defun canonicalize-defgeneric-key (key)
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

;;; Canonicalize a specialized required parameter.
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
(defun canonicalize-specialized-required (required)
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

(defun canonicalize-environment (parameter)
  (unless (and (symbolp parameter)
               (not (constantp parameter)))
    (error 'environment-must-be-followed-by-variable
           :code parameter))
  parameter)

(defun canonicalize-whole (parameter)
  (unless (and (symbolp parameter)
               (not (constantp parameter)))
    (error 'whole-must-be-followed-by-variable
           :code parameter))
  parameter)

(defun canonicalize-destructuring-required (parameter)
  (cond ((and (symbolp parameter) (not (constantp parameter)))
         parameter)
        ((consp parameter)
         (canonicalize-destructuring-lambda-list parameter))
        (t
         (error "Required must be a variable or a CONS."))))

(defun canonicalize-destructuring-rest (parameter)
  (cond ((and (symbolp parameter) (not (constantp parameter)))
         parameter)
        ((consp parameter)
         (canonicalize-destructuring-lambda-list parameter))
        (t
         (error "&REST or &BODY parameter must be a variable or a CONS."))))

;;; Canonicalize an &aux item.
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
(defun canonicalize-aux (aux)
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

(defparameter *ordinary-canonicalizers*
  `((nil . ,#'canonicalize-ordinary-required)
    (&optional . ,#'canonicalize-ordinary-optional)
    (&rest . ,#'canonicalize-ordinary-rest)
    (&key . ,#'canonicalize-ordinary-key)
    (&allow-other-keys . ,#'identity)
    (&aux . ,#'canonicalize-aux)))

(defparameter *generic-function-canonicalizers*
  `((nil . ,#'canonicalize-ordinary-required)
    (&optional . ,#'canonicalize-defgeneric-optional)
    (&rest . ,#'canonicalize-ordinary-rest)
    (&key . ,#'canonicalize-defgeneric-key)
    (&allow-other-keys . ,#'identity)))

(defparameter *specialized-canonicalizers*
  `((nil . ,#'canonicalize-specialized-required)
    (&optional . ,#'canonicalize-ordinary-optional)
    (&rest . ,#'canonicalize-ordinary-rest)
    (&key . ,#'canonicalize-ordinary-key)
    (&allow-other-keys . ,#'identity)
    (&aux . ,#'canonicalize-aux)))

(defparameter *macro-canonicalizers*
  `((nil . ,#'canonicalize-destructuring-required)
    (&whole . ,#'canonicalize-whole)
    (&environment . ,#'canonicalize-environment)
    (&optional . ,#'canonicalize-ordinary-optional)
    (&rest . ,#'canonicalize-destructuring-rest)
    (&body . ,#'canonicalize-destructuring-rest)
    (&key . ,#'canonicalize-ordinary-key)
    (&allow-other-keys . ,#'identity)
    (&aux . ,#'canonicalize-aux)))

(defparameter *destructuring-canonicalizers*
  `((nil . ,#'canonicalize-destructuring-required)
    (&whole . ,#'canonicalize-whole)
    (&optional . ,#'canonicalize-ordinary-optional)
    (&rest . ,#'canonicalize-destructuring-rest)
    (&body . ,#'canonicalize-destructuring-rest)
    (&key . ,#'canonicalize-ordinary-key)
    (&allow-other-keys . ,#'identity)
    (&aux . ,#'canonicalize-aux)))

(defparameter *defsetf-canonicalizers*
  `((nil . ,#'canonicalize-ordinary-required)
    (&environment . ,#'canonicalize-environment)
    (&optional . ,#'canonicalize-ordinary-optional)
    (&rest . ,#'canonicalize-ordinary-rest)
    (&key . ,#'canonicalize-ordinary-key)
    (&allow-other-keys . ,#'identity)))

(defparameter *deftype-canonicalizers*
  `((nil . ,#'canonicalize-destructuring-required)
    (&whole . ,#'canonicalize-whole)
    (&environment . ,#'canonicalize-environment)
    (&optional . ,#'canonicalize-deftype-optional)
    (&rest . ,#'canonicalize-destructuring-rest)
    (&body . ,#'canonicalize-destructuring-rest)
    (&key . ,#'canonicalize-deftype-key)
    (&allow-other-keys . ,#'identity)
    (&aux . ,#'canonicalize-aux)))

(defparameter *define-modify-macro-canonicalizers*
  `((nil . ,#'canonicalize-ordinary-required)
    (&optional . ,#'canonicalize-ordinary-optional)
    (&rest . ,#'canonicalize-ordinary-rest)))

(defparameter *define-method-combination-canonicalizers*
  `((nil . ,#'canonicalize-ordinary-required)
    (&whole . ,#'canonicalize-whole)
    (&optional . ,#'canonicalize-ordinary-optional)
    (&rest . ,#'canonicalize-ordinary-rest)
    (&key . ,#'canonicalize-ordinary-key)
    (&allow-other-keys . ,#'identity)
    (&aux . ,#'canonicalize-aux)))

(defun canonicalize-groups (groups canonicalizers)
  (loop with keywords = (intrinsic-keywords)
        for group in groups
        collect
        (if (or (null group)
                (not (member (first group) keywords :test #'eq)))
            (mapcar (cdr (assoc nil canonicalizers :test #'eq))
                    group)
            (cons (first group)
                  (mapcar (cdr (assoc (first group) canonicalizers :test #'eq))
                          (rest group))))))

(defun canonicalize-lambda-list (lambda-list canonicalizers)
  (let* ((keywords (intrinsic-keywords))
         (positions (compute-positions lambda-list keywords))
         (effective-positions
           ;; If the first position is 0, then that could mean two
           ;; things.  Either the lamda list is entirely empty, or the
           ;; first element of the lambda list is a lambda-list
           ;; keyword.  In the first case, we want the result to be
           ;; the empty list, so by making the EFFECTIVE-POSITIONS the
           ;; empty list, we will accomplish this effect.  In the
           ;; second case, we want the first group to start with 0 and
           ;; end with a value greater than 0, so we eliminate the
           ;; first element of POSITIONS to accomlish this effect.
           (if (zerop (first positions)) (rest positions) positions))
         (result 
           (loop for start = 0 then end
                 for end in effective-positions
                 collect (subseq lambda-list start end)))
         (present-keywords
           (loop for position in (butlast positions)
                 for element = (nth position lambda-list)
                 when (member element keywords :test #'eq)
                   collect element)))
    (check-all-allowed present-keywords canonicalizers)
    (check-occurrence-counts present-keywords)
    (check-order present-keywords)
    (check-arities result)
    (canonicalize-groups result canonicalizers)))

(defun canonicalize-ordinary-lambda-list (lambda-list)
  (canonicalize-lambda-list lambda-list *ordinary-canonicalizers*))

(defun canonicalize-generic-function-lambda-list (lambda-list)
  (canonicalize-lambda-list lambda-list *generic-function-canonicalizers*))

(defun canonicalize-specialized-lambda-list (lambda-list)
  (canonicalize-lambda-list lambda-list *specialized-canonicalizers*))

(defun canonicalize-macro-lambda-list (lambda-list)
  (let ((no-dotted (if (dotted-list-p lambda-list)
                       (append (butlast lambda-list)
                               (list (car (last lambda-list))
                                     '&rest
                                     (cdr (last lambda-list))))
                       lambda-list)))
    (canonicalize-lambda-list no-dotted *macro-canonicalizers*)))

(defun canonicalize-destructuring-lambda-list (lambda-list)
  (let ((no-dotted (if (dotted-list-p lambda-list)
                       (append (butlast lambda-list)
                               (list (car (last lambda-list))
                                     '&rest
                                     (cdr (last lambda-list))))
                       lambda-list)))
    (canonicalize-lambda-list no-dotted *destructuring-canonicalizers*)))

(defun canonicalize-defsetf-lambda-list (lambda-list)
  (canonicalize-lambda-list lambda-list *defsetf-canonicalizers*))

(defun canonicalize-deftype-lambda-list (lambda-list)
  (canonicalize-lambda-list lambda-list *deftype-canonicalizers*))

(defun canonicalize-define-modify-macro-lambda-list (lambda-list)
  (canonicalize-lambda-list lambda-list *define-modify-macro-canonicalizers*))

(defun canonicalize-define-method-combination-arguments-lambda-list (lambda-list)
  (canonicalize-lambda-list lambda-list *define-method-combination-canonicalizers*))

(defun extract-required (canonicalized-lambda-list)
  (loop with keywords = (intrinsic-keywords)
        for group in canonicalized-lambda-list
        unless (member (first group) keywords :test #'eq)
          return group))

(defun extract-named-group (canonicalized-lambda-list lambda-list-keyword)
  (loop with keywords = (intrinsic-keywords)
        for group in canonicalized-lambda-list
        when (and (not (null group))
                  (eq (first group) lambda-list-keyword))
          return group))
