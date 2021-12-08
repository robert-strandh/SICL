(cl:in-package #:cleavir-translate-lambda-list)

(defun allocate-lexical-variables (canonicalized-lambda-list)
  (let ((lexical-variables (make-hash-table :test #'eq)))
    (loop for group in (reverse canonicalized-lambda-list)
          do (case (first group)
               ((&optional &key)
                (loop for parameter in (rest group)
                      do (setf (gethash parameter lexical-variables)
                               (list (gensym) (gensym)))))
               (&rest
                (setf (gethash group lexical-variables) (gensym)))
               ((&allow-other-keys &aux)
                nil)
               (otherwise
                (loop for parameter in group
                      do (setf (gethash parameter lexical-variables) (gensym))))))
    lexical-variables))

(defun create-inner-bindings (canonicalized-lambda-list lexical-variables)
  (loop with inner-bindings = '()
        for group in (reverse canonicalized-lambda-list)
        do (case (first group)
             (&optional
              (loop for parameter in (reverse (rest group))
                    for (name form . rest) = parameter
                    for (lex-name lex-supplied-p)
                      = (gethash parameter lexical-variables)
                    do (unless (null rest)
                         (push `(,(first rest) ,lex-supplied-p)
                               inner-bindings))
                       (push `(,name (if ,lex-supplied-p ,lex-name ,form))
                             inner-bindings)))
             (&rest
              (push `(,(second group)
                      ,(gethash group lexical-variables))
                    inner-bindings))
             (&key
              (loop for parameter in (reverse (rest group))
                    for ((keyword name) form . rest) = parameter
                    for (lex-name lex-supplied-p)
                      = (gethash parameter lexical-variables)
                    do (unless (null rest)
                         (push `(,(first rest) ,lex-supplied-p)
                               inner-bindings))
                       (push `(,name (if ,lex-supplied-p ,lex-name ,form))
                             inner-bindings)))
             (&allow-other-keys
              nil)
             (&aux
              (setf inner-bindings
                    (append (rest group) inner-bindings)))
             (otherwise
              (loop for parameter in (reverse group)
                    do (push `(,parameter ,(gethash parameter lexical-variables))
                             inner-bindings))))
        finally (return inner-bindings)))

(defun create-outer-bindings (canonicalized-lambda-list lexical-variables)
  (let ((required-count
          (if (or (null canonicalized-lambda-list)
                  (member (first (first canonicalized-lambda-list))
                          lambda-list-keywords))
              0
              (length (first canonicalized-lambda-list))))
        (optional-count
          (let ((group (find '&optional canonicalized-lambda-list
                             :test #'eq :key #'first)))
            (if (null group)
                0
                (length (rest group))))))
    (loop with outer-bindings = '()
          for group in (reverse canonicalized-lambda-list)
          do (case (first group)
               (&optional
                (loop for parameter in (reverse (rest group))
                      for (name form . rest) = parameter
                      for (lex-name lex-supplied-p)
                        = (gethash parameter lexical-variables)
                      for index downfrom (+ required-count optional-count -1)
                      do (push `(,lex-name
                                 (if ,lex-supplied-p (primop-arg ,index) nil))
                               outer-bindings)
                         (push `(,lex-supplied-p
                                 (> (primop-argcount ,index)))
                               outer-bindings)))
               (&rest
                (push `(,(gethash group lexical-variables)
                        (loop with result = '()
                              for i downfrom (1- (primop-argcount)) to
                              ,(+ required-count optional-count)
                              do (push (primop-arg i) result)
                              finally (return result)))
                      outer-bindings))
               (&key
                (loop for parameter in (reverse (rest group))
                      for keyword = (first (first parameter))
                      for (lex-name lex-supplied-p)
                        = (gethash parameter lexical-variables)
                      do (push `(,lex-name
                                 (if (<= (primop-argcount)
                                         ,(+ required-count optional-count))
                                     nil
                                     (loop for keyword-index from
                                           ,(+ required-count optional-count)
                                             below (primop-argcount)
                                           by 2
                                           for value-index from
                                           ,(+ required-count optional-count 1)
                                           by 2
                                           when (eq (primop-arg keyword-index)
                                                    ,keyword)
                                             return (progn (setq ,lex-name t)
                                                           (primop-arg value-index)))))
                               outer-bindings)
                         (push `(,lex-supplied-p nil)
                               outer-bindings)))
               ((&allow-other-keys &aux)
                nil)
               (otherwise
                (loop for parameter in (reverse group)
                      for lex-name = (gethash parameter lexical-variables)
                      for index downfrom (1- required-count) to 0
                      do (push `(,lex-name (primop-arg ,index))
                               outer-bindings))))
          finally (return outer-bindings))))

(defun wrap-in-checks (canonicalized-lambda-list body)
  (let ((required-count
          (if (or (null canonicalized-lambda-list)
                  (member (first (first canonicalized-lambda-list))
                          lambda-list-keywords))
              0
              (length (first canonicalized-lambda-list))))
        (optional-count
          (let ((group (find '&optional canonicalized-lambda-list
                             :test #'eq :key #'first)))
            (if (null group)
                0
                (length (rest group)))))
        (result body))
    ;; If requred, check that keywords are valid.
    (when (and (member '&key canonicalized-lambda-list
                       :test #'eq :key #'first)
               (not (member '&allow-other-keys canonicalized-lambda-list
                            :test #'eq :key #'first)))
      (setf result
            `(progn
               (let ((allow-other-keys
                       (if (< (primop-argcount)
                              ,(<= (+ required-count optional-count)))
                           nil
                           (loop for keyword-index from ,(+ required-count optional-count)
                                   below (primop-argcount)
                                 by 2
                                 for value-index from ,(+ required-count optional-count 1)
                                 by 2
                                 when (eq (primop-arg keyword-index) :allow-other-keys)
                                   return (primop-arg value-index)))))
                 (unless allow-other-keys
                   (loop for keyword-index from 3 below (primop-argcount) by 2
                         unless (member (primop-arg keyword-index)
                                        '(:e :allow-other-keys))
                                (error "invalid keyword"))))
               ,result)))
    ;; If &KEY is given, check that there is an even number of keyword
    ;; arguments.
    (when (member '&key canonicalized-lambda-list
                  :test #'eq :key #'first)
      (setf result
            `(if (,(if (oddp (+ required-count optional-count)) 'evenp 'oddp)
                  (primop-argcount))
                 (error "Odd number of keyword arguments")
                 ,result)))
    ;; Unless &REST or &KEY is given, check for maximum number of
    ;; arguments.
    (unless (or (member '&rest canonicalized-lambda-list
                        :test #'eq :key #'first)
                (member '&key canonicalized-lambda-list
                        :test #'eq :key #'first))
      (setf result
            `(if (> (primop-argcount) ,(+ required-count optional-count))
                 (error "Too many arguments")
                 ,result)))
    ;; Unless there are no required parameters, check for the minimum
    ;; number of arguments.
    (unless (zerop required-count)
      (setf result
            `(if (< (primop-argcount) ,required-count)
                 (error "Too few arguments")
                 ,result)))
    result))

(defun translate-lambda-list (lambda-list body)
  (let* ((canonicalized-lambda-list
           (cleavir-code-utilities:canonicalize-ordinary-lambda-list lambda-list))
         (lexical-variables
           (allocate-lexical-variables canonicalized-lambda-list))
         (inner-bindings
           (create-inner-bindings canonicalized-lambda-list lexical-variables))
         (outer-bindings
           (create-outer-bindings canonicalized-lambda-list lexical-variables)))
    (wrap-in-checks
     canonicalized-lambda-list
     `(let* ,outer-bindings
        (let* ,inner-bindings
          ,body)))))
