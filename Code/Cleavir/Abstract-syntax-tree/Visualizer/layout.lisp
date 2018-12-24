(cl:in-package #:cleavir-ast-visualizer)

(defun make-point (x y) (cons x y))

(defun x (point) (car point))

(defun y (point) (cdr point))

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

(defun combine-vertical-profiles (upper lower)
  (let ((lower-width (x (first (last lower)))))
    ;; Find the first element (X . Y) in UPPER such that X is greater
    ;; than or equal to the width of LOWER.
    (let* ((point (find-if (lambda (point) (>= (x point) lower-width))
                           upper))
           (new-lower-y (if (null point)
                            (y (first (last upper)))
                            (y point)))
           (new-lower (loop for (x . y) in lower
                            collect (cons x (+ y new-lower-y)))))
      (values (if (>= (y (first (last new-lower)))
                      (y (first (last upper))))
                  (append (butlast new-lower)
                          (list (make-point (max (x (first (last upper)))
                                                 (x (first (last new-lower))))
                                            (y (first (last new-lower))))))
                  (let ((remaining
                          (loop for (a b . rest) on upper
                                when (> (y b) (y (first (last new-lower))))
                                  return (list* a b rest))))
                    
                    (append (butlast new-lower)
                            (cons (make-point (x (first remaining))
                                              (y (first (last new-lower))))
                                  (rest remaining)))))
              new-lower-y))))

(defun combine-layouts (layouts)
  (destructuring-bind (first . rest) layouts
    (loop with accumulated-profile = (profile first)
          for layout in rest
          do (multiple-value-bind (profile dy)
                 (combine-vertical-profiles accumulated-profile (profile layout))
               (setf accumulated-profile profile)
               (move-layout layout 0 dy))
          finally (return accumulated-profile))))
             
(defgeneric layout-from-ast (table pane ast))

;;; LEFT is just a single point for the lower-right corner of the
;;; parent.  RIGHT is the combined profile of the children.
(defun combine-horizontal-profiles (left right)
  ;; Start by moving all the points in the right profile to make room
  ;; for the left one
  (let ((new-right (loop for point in right
                         collect (make-point (+ (x point) (x left) 20)
                                             (y point)))))
    (cond ((>= (y left)
               (y (first (last new-right))))
           (list (make-point (x (first (last new-right))) (y left))))
          ((null (rest right))
           ;; RIGHT is a rectangle
           (if (<= (y (first new-right)) (y left))
               ;; The left one is at least as tall as the right one.
               (list (make-point (x (first new-right))
                                 (y left)))
               ;; The right one is taller
               (list left (first new-right))))
          ((> (y (first new-right)) (y left))
           (cons left new-right))
          (t (let ((remaining
                     (loop for (a b . rest) on new-right
                           when (> (y b) (y left))
                             return (list* a b rest))))
               (cons (make-point (- (x (first remaining)) 20)
                                 (y left))
                     (rest remaining)))))))

(defun layout-from-ast-with-children (table pane ast children)
  (let ((width (ast-width pane ast))
        (height (ast-height pane ast)))
    (cond ((gethash ast table)
           (make-instance 'layout
             :ast ast
             :position (make-point 0 2)
             :profile (list (make-point 4 1))
             :children '()
             :indirect-p t))
          ((null children)
           (setf (gethash ast table) t)
           (make-instance 'layout
             :ast ast
             :position (make-point 10 10)
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
                 :position (make-point 10 10)
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
