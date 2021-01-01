(cl:in-package #:cleavir-ast-visualizer)

(defclass layout ()
  ((%ast :initarg :ast :reader ast)
   (%indirect-p :initarg :indirect-p :reader indirect-p)
   (%position :initarg :position :accessor position)
   (%children :initarg :children :reader children)
   (%profile :initarg :profile :reader profile)))

(defun move-layout (layout dx dy)
  (with-accessors ((position position)) layout
    (setf position (make-point (+ (x position) dx) (+ (y position) dy))))
  (loop for child in (children layout)
        do (move-layout child dx dy)))

(defun combine-layouts (layouts)
  (destructuring-bind (first . rest) layouts
    (loop with accumulated-profile = (profile first)
          for layout in rest
          for profile = (profile layout)
          do (multiple-value-bind (new-profile dy)
                 (combine-vertical-profiles accumulated-profile profile)
               (setf accumulated-profile new-profile)
               (move-layout layout 0 dy))
          finally (return accumulated-profile))))
             
(defgeneric layout-from-ast (table pane ast))

(defun layout-from-ast-with-children (table pane ast children)
  (let ((width (ast-width pane ast))
        (height (ast-height pane ast)))
    (cond ((gethash ast table)
           (make-instance 'layout
             :ast ast
             :position (make-point 0 2)
             :profile (list (make-point 1 4))
             :children '()
             :indirect-p t))
          ((null children)
           (setf (gethash ast table) t)
           (make-instance 'layout
             :ast ast
             :position (make-point 0 0)
             :profile (list (make-point (+ width 20) (+ height 20)))
             :children '()
             :indirect-p nil))
          (t (setf (gethash ast table) t)
             (let* ((children (loop for child in children
                                    collect (layout-from-ast table pane child)))
                    (child-profiles (combine-layouts children))
                    (profile (combine-horizontal-profiles
                              (make-point (+ width 20) (+ height 20))
                              child-profiles)))
               (loop for child in children
                     do (move-layout child (+ width 20) 0))
               (make-instance 'layout
                 :ast ast
                 :position (make-point 0 0)
                 :profile profile
                 :children children
                 :indirect-p nil))))))
               
(defmethod layout-from-ast (table pane ast)
  (layout-from-ast-with-children table pane ast (cleavir-ast:children ast)))

(defmethod layout-from-ast (table pane (ast cleavir-ast:function-ast))
  (let* ((children (cleavir-ast:children ast))
         (reorganized (if (null children) '()
                          (append (rest children) (list (first children))))))
    (layout-from-ast-with-children table pane ast reorganized)))

(defun make-layout (pane ast)
  (let ((table (make-hash-table :test #'eq)))
    (layout-from-ast table pane ast)))
