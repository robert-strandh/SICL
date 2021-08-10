(cl:in-package #:cleavir-code-utilities)

(defun first-group-is (remaining lambda-list-keyword)
  (and (not (null remaining))
       (not (null (first remaining)))
       (eq (first (first remaining)) lambda-list-keyword)))

(defun handle-aux (remaining body)
  (if (first-group-is remaining '&aux)
      (values (rest remaining)
              `(let* ,(rest (first remaining))
                 ,body))
      (values remaining body)))

(defun new-destructure-lambda-list (canonicalized-lambda-list variable body)
  (let ((remaining (reverse canonicalized-lambda-list))
        (result body)
        (allow-other-keys nil))
    (when (first-group-is remaining '&aux)
      (setf result
            `(let* ,(rest (pop remaining))
               ,result)))
    (when (first-group-is remaining '&allow-other-keys)
      (setf allow-other-keys t)
      (pop remaining))
    (when (first-group-is remaining '&key)
      (let* ((group (pop remaining))
             (keywords (mapcar #'caar (rest group))))
        (loop for ((keyword var) default supplied-p)
                in (reverse (rest group))
              for temp = (gensym)
              do (setf result
                       `(let ((,var (if (null ,temp)
                                        ,default
                                        (second ,temp))))
                          ,result))
                 (unless (null supplied-p)
                   (setf result
                         `(let ((,supplied-p (not (null ,temp))))
                            ,result)))
                 (setf result
                       `(let ((,temp (member ,keyword ,variable :test #'eq)))
                          ,result)))
        (unless allow-other-keys
          (let ((temp (gensym)))
            (setf result
                  `(progn (unless (getf ,variable :allow-other-keys)
                            (loop for ,temp in ,variable (by #'cddr)
                                  unless (member ,temp ',keywords)
                                    do (error "Invalid keyword: ~s" ,temp)))
                          ,result))))
        (setf result
              `(if (oddp (length ,variable))
                   (error "Odd number of keywords/arguments: ~s"
                          ,variable)
                   ,result))))
    (when (or (first-group-is remaining '&rest)
              (first-group-is remaining '&body))
      (let ((pattern (second (pop remaining))))
        (setf result
              (if (symbolp pattern)
                  `(let (,pattern ,variable)
                     ,result)
                  (let ((temp (gensym)))
                    `(let ((,temp ,variable))
                       ,(new-destructure-lambda-list
                         pattern temp result)))))))
    (when (first-group-is remaining '&optional)
      (loop for (var default supplied-p) in (reverse (rest (pop remaining)))
            do (setf result
                     `(let ((,var (if (null ,variable)
                                      ,default
                                      (pop ,variable))))
                        ,result))
               (unless (null supplied-p)
                 (setf result
                       `(let ((,supplied-p (not (null ,variable))))
                          ,result)))))
    (loop for pattern in (reverse (pop remaining))
          do (setf result
                   `(if (null ,variable)
                        (error "Not enough arguments.")
                        ,(if (symbolp pattern)
                             `(let ((,pattern (pop ,variable)))
                                ,result)
                             (let ((temp (gensym)))
                               `(let ((,temp (pop ,variable)))
                                  ,(new-destructure-lambda-list
                                    pattern temp result)))))))
    (when (first-group-is remaining '&whole)
      (setf result
            `(let ((,(second (first remaining)) ,variable))
               ,result)))
    result))
