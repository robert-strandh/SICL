(cl:in-package #:cleavir-cst-to-ast)

(defmethod items-from-parameter-group
    ((parameter-group cst:ordinary-required-parameter-group))
  (loop for parameter in (cst:parameters parameter-group)
        collect (list (cst:name parameter))))

(defmethod items-from-parameter-group
    ((parameter-group cst:ordinary-optional-parameter-group))
  (loop for parameter in (cst:parameters parameter-group)
        for name = (cst:name parameter)
        for supplied-p = (cst:supplied-p parameter)
        collect (if (null supplied-p)
                    (list name)
                    (list name supplied-p))))

(defmethod items-from-parameter-group
    ((parameter-group cst:ordinary-rest-parameter-group))
  (list (list (cst:name (cst:parameter parameter-group)))))

(defmethod items-from-parameter-group
    ((parameter-group cst:ordinary-key-parameter-group))
  (loop for parameter in (cst:parameters parameter-group)
        for name = (cst:name parameter)
        for supplied-p = (cst:supplied-p parameter)
        collect (if (null supplied-p)
                    (list name)
                    (list name supplied-p))))

(defmethod items-from-parameter-group
    ((parameter-group cst:aux-parameter-group))
  '())

;;; Given a parsed lambda list, return a list of items.  There are as
;;; many items in the list as there are bindings in the lambda list.
;;; In this case, an occurrence of a parameter together with a
;;; supplied-p parameter is considered to be a single item.  Each item
;;; is a list of one or two CSTs.  For a parameter with an associated
;;; supplied-p parameter, the item contains the CSTs for both the
;;; parameter and the associated supplied-p parameter.  Otherwise, the
;;; item contains just the CST for the parameter.
(defun itemize-lambda-list (parsed-lambda-list)
  (loop for parameter-group in (cst:children parsed-lambda-list)
        collect (items-from-parameter-group parameter-group)))
