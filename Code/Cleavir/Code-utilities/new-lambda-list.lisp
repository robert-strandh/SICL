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
          :arity *
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
          :arity *
          :occurrence-count 1
          :order 400)
        (make-instance 'intrinsic-feature
          :lambda-list-keyword '&allow-other-keys
          :arity 0
          :occurrence-count 1
          :order 500)
        (make-instance 'intrinsic-feature
          :lambda-list-keyword '&aux
          :arity *
          :occurrence-count 1
          :order 600)))

(defun compute-positions (lambda-list keywords)
  (append (loop for i from 0
                for element in lambda-list
                when (member element keywords)
                  collect i)
          (list (length lambda-list))))

(defun check-allowed (keyword canonicalizers)
  (unless (member keyword canonicalizers :test #'eq :key #'car)
    (error "Illegal lambda list keyword: ~s" keyword)))

(defun check-all-allowed (keywords canonicalizers)
  (loop for keyword in keywords
        do (check-allowed keyword canonicalizers)))

(defun find-feature (keyword)
  (find keyword *intrinsic-features*
        :test #'eq :key #'lambda-list-keyword))

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

(defun canonicalize-ordinary-required (parameter)
  (unless (and (symbolp parameter)
               (not (constantp parameter)))
    (error 'required-must-be-variable
           :code parameter))
  parameter)

(defun canonicalize-ordinary-rest (parameter)
  (unless (and (symbolp parameter)
               (not (constantp parameter)))
    (error 'rest/body-must-be-followed-by-variable
           :code parameter))
  parameter)

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

(defparameter *ordinary-canonicalizers*
  `((nil . ,#'canonicalize-ordinary-required)
    (&optional . ,#'parse-ordinary-optional)
    (&rest . ,#'canonicalize-ordinary-rest)
    (&key . ,#'parse-ordinary-key)
    (&allow-other-keys . ,#'identity)
    (&aux . ,#'parse-aux)))

(defparameter *generic-function-canonicalizers*
  `((nil . ,#'canonicalize-ordinary-required)
    (&optional . ,#'parse-defgeneric-optional)
    (&rest . ,#'canonicalize-ordinary-rest)
    (&key . ,#'parse-defgeneric-key)
    (&allow-other-keys . ,#'identity)))

(defparameter *specialized-canonicalizers*
  `((nil . ,#'parse-specialized-required)
    (&optional . ,#'parse-ordinary-optional)
    (&rest . ,#'canonicalize-ordinary-rest)
    (&key . ,#'parse-ordinary-key)
    (&allow-other-keys . ,#'identity)
    (&aux . ,#'parse-aux)))

(defparameter *macro-canonicalizers*
  `((nil . ,#'canonicalize-destructuring-required)
    (&whole . ,#'canonicalize-whole)
    (&environment . ,#'canonicalize-environment)
    (&optional . ,#'parse-ordinary-optional)
    (&rest . ,#'canonicalize-destructuring-rest)
    (&key . ,#'parse-ordinary-key)
    (&allow-other-keys . ,#'identity)))

(defparameter *destructuring-canonicalizers*
  `((nil . ,#'canonicalize-destructuring-required)
    (&whole . ,#'canonicalize-whole)
    (&optional . ,#'parse-ordinary-optional)
    (&rest . ,#'canonicalize-destructuring-rest)
    (&key . ,#'parse-ordinary-key)
    (&allow-other-keys . ,#'identity)))

(defparameter *defsetf-canonicalizers*
  `((nil . ,#'canonicalize-ordinary-required)
    (&environment . ,#'canonicalize-environment)
    (&optional . ,#'parse-ordinary-optional)
    (&rest . ,#'canonicalize-ordinary-rest)
    (&key . ,#'parse-ordinary-key)
    (&allow-other-keys . ,#'identity)))

(defparameter *define-modify-macro-canonicalizers*
  `((nil . ,#'canonicalize-ordinary-required)
    (&optional . ,#'parse-ordinary-optional)
    (&rest . ,#'canonicalize-ordinary-rest)))

(defparameter *define-method-combination-canonicalizers*
  `((nil . ,#'canonicalize-ordinary-required)
    (&whole . ,#'canonicalize-whole)
    (&optional . ,#'parse-ordinary-optional)
    (&rest . ,#'canonicalize-ordinary-rest)
    (&key . ,#'parse-ordinary-key)
    (&allow-other-keys . ,#'identity)
    (&aux . ,#'parse-aux)))

(defun parse-lambda-list-no-whole (lambda-list positions)
  (loop for start = 0 then end
        for end in positions
        collect (subseq lambda-list start end)))

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
         (result 
           (if (and (not (null lambda-list))
                    (eq (first lambda-list) '&whole))
               (progn (unless (>= (second positions) 2)
                        (error "Incorrect arity for lambda-list keyword &WHOLE: ~s"
                               (1- (second positions))))
                      (let* ((no-whole (subseq lambda-list 2))
                             (new-positions
                               (compute-positions no-whole keywords)))
                        (cons (subseq lambda-list 0 2)
                              (parse-lambda-list-no-whole
                               no-whole new-positions))))
               (parse-lambda-list-no-whole lambda-list positions)))
         (present-keywords
           (loop for position in (butlast positions)
                 collect (nth position lambda-list))))
    (check-all-allowed present-keywords canonicalizers)
    (check-occurrence-counts present-keywords)
    (check-order present-keywords)
    (canonicalize-groups result canonicalizers)))

(defun canonicalize-ordinary-lambda-list (lambda-list)
  (canonicalize-lambda-list lambda-list *ordinary-canonicalizers*))

(defun canonicalize-generic-function-lambda-list (lambda-list)
  (canonicalize-lambda-list lambda-list *generic-function-canonicalizers*))

(defun canonicalize-specialized-lambda-list (lambda-list)
  (canonicalize-lambda-list lambda-list *specialized-canonicalizers*))

(defun canonicalize-macro-lambda-list (lambda-list)
  (canonicalize-lambda-list lambda-list *macro-canonicalizers*))

(defun canonicalize-destructuring-lambda-list (lambda-list)
  (canonicalize-lambda-list lambda-list *destructuring-canonicalizers*))

(defun canonicalize-defsetf-lambda-list (lambda-list)
  (canonicalize-lambda-list lambda-list *defsetf-canonicalizers*))

(defun canonicalize-define-modify-macro-lambda-list (lambda-list)
  (canonicalize-lambda-list lambda-list *define-modify-macro-canonicalizers*))

(defun canonicalize-define-method-combination-lambda-list (lambda-list)
  (canonicalize-lambda-list lambda-list *define-method-combination-canonicalizers*))

(defun extract-required (canonicalized-lambda-list)
  (loop with keywords = (intrinsic-keywords)
        for group in canonicalized-lambda-list
        when (or (null group)
                 (not (member (first group) keywords :test #'eq)))
          return group))

(defun extract-named-group (canonicalized-lambda-list lambda-list-keyword)
  (loop with keywords = (intrinsic-keywords)
        for group in canonicalized-lambda-list
        when (and (not (null group))
                  (eq (first group) lambda-list-keyword))
          return group))
