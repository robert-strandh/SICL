(cl:in-package #:cleavir-code-utilities)

(defgeneric lambda-list-keyword (intrinsic-feature))
(defgeneric arity (intrinsic-feature))
(defgeneric ocurrence-count (intrinsic-feature))
(defgeneric order (intrinsic-feature))

(defclass intrinsic-feature ()
  ((%lambda-list-keyword
    :initarg :lambda-list-keyword
    :reader lambda-list-keyword)
   ;; The arity can be:
   ;;
   ;; - A non-negative fixnum, which is then the number of items that
   ;;   must follow.
   ;;
   ;; - The symbol *, which means that any number of items can follow.
   (%arity :initarg :arity :reader arity)
   ;; The number of times this lambda-list keyword can occur in any
   ;; lambda list.
   (%occurence-count
    :initform 0
    :initarg :occurrence-count
    :reader occurrence-count)
   (%order :initarg :order :reader order)))

(defparameter *intrinsic-features*
  (list (make-instance 'intrinsic-feature
          :lambda-list-keyword '&whole
          :arity 1
          :occurrence-count 1
          :order 100)
        (make-instance 'intrinsic-feature
          :lambda-list-keyword '&environment
          :arity 1
          :occurrence-count 1
          :order nil)
        (make-instance 'intrinsic-feature
          :lambda-list-keyword '&optional
          :arity '*
          :occurrence-count 1
          :order 200)
        (make-instance 'intrinsic-feature
          :lambda-list-keyword '&rest
          :arity 1
          :occurrence-count 1
          :order 300)
        (make-instance 'intrinsic-feature
          :lambda-list-keyword '&body
          :arity 1
          :occurrence-count 1
          :order 300)
        (make-instance 'intrinsic-feature
          :lambda-list-keyword '&key
          :arity '*
          :occurrence-count 1
          :order 400)
        (make-instance 'intrinsic-feature
          :lambda-list-keyword '&allow-other-keys
          :arity 0
          :occurrence-count 1
          :order 500)
        (make-instance 'intrinsic-feature
          :lambda-list-keyword '&aux
          :arity '*
          :occurrence-count 1
          :order 600)))

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
  (canonicalize-nontrivial-optional optional '*))

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
    (&key . ,#'parse-deftype-key)
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

(defun intrinsic-keywords ()
  (mapcar #'lambda-list-keyword *intrinsic-features*))

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

(defun canonicalize-define-method-combination-lambda-list (lambda-list)
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
