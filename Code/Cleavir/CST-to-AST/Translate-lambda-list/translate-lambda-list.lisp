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

(defun translate-lambda-list (lambda-list body)
  (let* ((canonicalized-lambda-list
           (cleavir-code-utilities:canonicalize-ordinary-lambda-list lambda-list))
         (lexical-variables
           (allocate-lexical-variables canonicalized-lambda-list))
         (inner-bindings
           (create-inner-bindings canonicalized-lambda-list lexical-variables)))
    `(let* ,inner-bindings
       ,body)))
