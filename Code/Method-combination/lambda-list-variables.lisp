(cl:in-package #:sicl-method-combination)

;;; This file contains code to determine what variables are introduced
;;; by an ordinary lambda list.

;;; Given a parameter group of a parsed lambda list, return a list of
;;; variables (i.e. symbols) that are introduced by that parameter
;;; group.
(defgeneric group-variables (parameter-group))

;;; The variables that are introduced by an ordinary required
;;; parameter group are simply the names of the parameters in the
;;; group.
(defmethod group-variables
    ((parameter-group cst:ordinary-required-parameter-group))
  (loop for parameter in (cst:parameters parameter-group)
        collect (cst:name parameter)))

;;; In an ordinary optional parameter group, each parameter may
;;; introduce a SUPPLIED-P variable in addition to the main variable
;;; of the parameter.
(defmethod group-variables
    ((parameter-group cst:ordinary-optional-parameter-group))
  (loop for parameter in (cst:parameters parameter-group)
        collect (cst:name parameter)
        when (cst:supplied-p parameter)
          collect it))

;;; An ordinary rest parameter group contains a single variable.
(defmethod group-variables
    ((parameter-group cst:ordinary-rest-parameter-group))
  (list (cst:name (cst:parameter parameter-group))))

;;; As with the ordinary optional parameter group, in an ordinary key
;;; parameter group, each parameter may introduce a SUPPLIED-P
;;; variable in addition to the main variable of the parameter.
(defmethod group-variables
    ((parameter-group cst:ordinary-key-parameter-group))
  (loop for parameter in (cst:parameters parameter-group)
        collect (cst:name parameter)
        when (cst:supplied-p parameter)
          collect it))

;;; The variables that are introduced by an aux parameter group are
;;; simply the names of the parameters in the group.
(defmethod group-variables ((parameter-group cst:aux-parameter-group))
  (loop for parameter in (cst:parameters parameter-group)
        collect (cst:name parameter)))

(defun lambda-list-variables (lambda-list)
  (let* ((cst (cst:cst-from-expression lambda-list))
         (parsed-lambda-list (cst:parse-ordinary-lambda-list nil cst))
         (parameter-groups (cst:children parsed-lambda-list)))
    (mapcar #'cst:raw
            (loop for parameter-group in parameter-groups
                  append (group-variables parameter-group)))))
