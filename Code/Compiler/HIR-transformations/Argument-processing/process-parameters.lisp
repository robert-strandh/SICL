(cl:in-package #:sicl-argument-processing)

(defun extract-required-parameters (lambda-list)
  (let ((first-lambda-list-keyword-position
          (position-if (lambda (x) (member x lambda-list-keywords))
                       lambda-list)))
    (if (null first-lambda-list-keyword-position)
        lambda-list
        (subseq lambda-list 0 first-lambda-list-keyword-position))))

(defun extract-optional-parameters (lambda-list)
  (let ((optional-position (position '&optional lambda-list :test #'eq)))
    (if (null optional-position)
        '()
         (let ((next-lambda-list-keyword-position
                 (position-if (lambda (x) (member x lambda-list-keywords))
                              lambda-list
                              :start (1+ optional-position))))
           (if (null next-lambda-list-keyword-position)
               (subseq lambda-list (1+ optional-position))
               (subseq lambda-list
                       (1+ optional-position)
                       next-lambda-list-keyword-position))))))

(defun extract-rest-parameter (lambda-list)
  (let ((rest-position (position '&rest lambda-list :test #'eq)))
    (if (null rest-position)
        nil
        (nth (1+ rest-position) lambda-list))))

(defun extract-key-parameters (lambda-list)
  (let ((key-position (position '&key lambda-list :test #'eq)))
    (if (null key-position)
        (values '() nil nil)
        (let ((allow-other-keys-p (eq (first (last lambda-list))
                                      '&allow-other-keys)))
          (if allow-other-keys-p
              (values (subseq lambda-list
                              (1+ key-position)
                              (1- (length lambda-list)))
                      t
                      t)
              (values (subseq lambda-list (1+ key-position))
                      t
                      nil))))))
                
