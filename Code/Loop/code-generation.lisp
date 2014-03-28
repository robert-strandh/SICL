;;;; Copyright (c) 2010, 2011
;;;;
;;;;     Matthieu Villeneuve (matthieu.villeneuve@gmail.com)
;;;;
;;;; all rights reserved. 
;;;;
;;;; Permission is hereby granted to use this software for any 
;;;; purpose, including using, modifying, and redistributing it.
;;;;
;;;; The software is provided "as-is" with no warranty.  The user of
;;;; this software assumes any responsibility of the consequences. 

;;;; This file is part of the LOOP module of the SICL project.
;;;; See the file SICL.text for a description of the project. 

(cl:in-package #:sicl-loop)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Code generation

(defun progn-or-single-form (forms)
  (if (> (length forms) 1)
      `(progn ,@forms)
      (first forms)))

(defgeneric generate-bindings (clause))

(defmethod generate-bindings (clause)
  nil)

(defmethod generate-bindings ((clause repeat-clause))
  `((,(var-spec clause) ,(form clause))))

(defmethod generate-bindings ((clause with-clause))
  (mapcar (lambda (subclause)
            `(,(var-spec subclause) ,(if (form-present-p subclause)
                                         (form subclause)
                                         nil)))
          (subclauses clause)))

(defmethod generate-bindings ((clause list-accumulation-clause))
  (if (null (into-var clause))
      nil
      `((,(into-var clause) nil)
        (,(into-tail-var clause) nil))))

(defmethod generate-bindings ((clause numeric-accumulation-clause))
  (if (null (into-var clause))
      nil
      `((,(into-var clause) 0))))

(defgeneric generate-prologue (clause))

(defmethod generate-prologue (clause)
  nil)

(defmethod generate-prologue ((clause initially-clause))
  (progn-or-single-form (forms clause)))

(defgeneric generate-epilogue (clause))

(defmethod generate-epilogue (clause)
  nil)

(defmethod generate-epilogue ((clause finally-clause))
  (progn-or-single-form (forms clause)))

(defgeneric generate-termination-check (clause))

(defmethod generate-termination-check (clause)
  nil)

(defmethod generate-termination-check ((clause while-clause))
  `(unless ,(form clause)
     (go end)))

(defmethod generate-termination-check ((clause until-clause))
  `(when ,(form clause)
     (go end)))

(defmethod generate-termination-check ((clause repeat-clause))
  (let ((repeat-counter (var-spec clause)))
    `(if (plusp ,repeat-counter)
         (decf ,repeat-counter)
         (go end))))

(defvar *body*)

(defgeneric generate-main-code (clause))

(defmethod generate-main-code (clause)
  nil)

(defmethod generate-main-code ((clause do-clause))
  `(progn ,@(forms clause)))

(defmethod generate-main-code ((clause collect-clause))
  (let ((var (or (into-var clause)
                 (accumulation-variable *body*)))
        (tail (if (into-var clause)
                  (into-tail-var clause)
                  (accumulation-list-tail *body*)))
        (cons (gensym)))
    `(let ((,cons (cons ,(form clause) nil)))
       (if (null ,var)
           (setf ,var ,cons
                 ,tail ,cons)
           (setf (cdr ,tail) ,cons
                 ,tail ,cons)))))

(defmethod generate-main-code ((clause sum-clause))
  (let ((var (or (into-var clause)
                 (accumulation-variable *body*))))
    `(incf ,var ,(form clause))))

(defun generate-body (body)
  (let ((clauses (clauses body))
        (*body* body))
    `(tagbody
        ,@(remove nil (mapcar #'generate-prologue clauses))
      again
        ,@(remove nil (mapcar #'generate-termination-check clauses))
        ,@(remove nil (mapcar #'generate-main-code clauses))
        (go again)
      end
        ,@(remove nil (mapcar #'generate-epilogue clauses)))))

(defun generate-accumulation-bindings-and-body (body)
  (let ((body-form (generate-body body)))
    (case (accumulation-type body)
      (:list
       `(let ((,(accumulation-variable body) nil)
              (,(accumulation-list-tail body) nil))
              ,body-form))
      (:numeric
       `(let ((,(accumulation-variable body) 0))
          ,body-form))
      ((nil)
       body-form))))

(defun generate-bindings-and-body (binding-clauses body)
  (if (endp binding-clauses)
      (generate-accumulation-bindings-and-body body)
      (let ((bindings-first (generate-bindings (first binding-clauses)))
            (bindings-rest-and-body (generate-bindings-and-body
                                     (rest binding-clauses)
                                     body)))
        (if (null bindings-first)
            bindings-rest-and-body
            `(let (,@bindings-first)
               ,bindings-rest-and-body)))))

(defun initialize-accumulation (clauses body)
  (unless (endp clauses)
    (let ((clause (first clauses)))
      (when (and (typep clause 'accumulation-clause)
                 (null (into-var clause)))
        (let ((clause-accumulation-type
               (cond ((typep clause 'list-accumulation-clause)
                      :list)
                     ((typep clause 'numeric-accumulation-clause)
                      :numeric))))
          (cond ((null (accumulation-type body))
                 (setf (accumulation-variable body) (gensym)
                       (accumulation-list-tail body) (gensym)
                       (accumulation-type body) clause-accumulation-type))
                ((not (eq (accumulation-type body) clause-accumulation-type))
                 (error "Conflicting accumulation types")))))
      (initialize-accumulation (rest clauses) body))))
