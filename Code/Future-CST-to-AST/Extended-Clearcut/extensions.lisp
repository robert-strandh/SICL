(cl:in-package #:sicl-extended-clearcut)

(defgeneric origin* (client expression))

(defmethod origin*
  ((client clearcut-implementation-s-expression:client)
   expression)
  expression)

(defmethod origin*
  ((client clearcut-implementation-concrete-syntax-tree:client)
   expression)
  (cst:source expression))

(defun origin (expression)
  (origin* clearcut:*client* expression))

(defgeneric (setf origin*) (new-origin client expression))

(defmethod (setf origin*)
    (new-origin
     (client clearcut-implementation-s-expression:client)
     expression)
  nil)

(defmethod (setf origin*)
    (new-origin
     (client clearcut-implementation-concrete-syntax-tree:client)
     expression)
  (setf (cst:source expression) new-origin))

(defun (setf origin) (new-origin expression)
  (setf (origin* clearcut:*client* expression) new-origin))

(defgeneric reconstruct* (client new-form old-cooked-form))

(defmethod reconstruct*
  ((client clearcut-implementation-s-expression:client)
   new-form
   old-cooked-form)
  new-form)

(defmethod reconstruct*
  ((client clearcut-implementation-concrete-syntax-tree:client)
   new-form
   old-cooked-form)
  (cst:reconstruct client new-form old-cooked-form))

(defun reconstruct (new-form old-cooked-form)
  (reconstruct* clearcut:*client* new-form old-cooked-form))
