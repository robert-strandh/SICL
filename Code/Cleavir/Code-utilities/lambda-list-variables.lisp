(cl:in-package #:cleavir-code-utilities)

;;; Given an ordinary lambda list, return the
;;; variables introduced by that lambda list.
(defun lambda-list-variables (lambda-list)
  (let* ((canonicalized-lambda-list
           (canonicalize-ordinary-lambda-list lambda-list))
         (required
           (extract-required canonicalized-lambda-list))
         (optional
           (extract-named-group canonicalized-lambda-list '&optional))
         (rest
           (extract-named-group canonicalized-lambda-list '&rest))
         (key
           (extract-named-group canonicalized-lambda-list '&key))
         (aux
           (extract-named-group canonicalized-lambda-list '&aux)))
    `(,@required
      ,@(if (null optional)
            '()
            (mapcar #'first (rest optional)))
      ,@(if (null rest)
            '()
            `(,(second rest)))
      ,@(if (null key)
            '()
            (mapcar (lambda (x) (second (first x))) (rest key)))
      ,@(if (null aux)
            '()
            (mapcar #'first (rest aux))))))
    
