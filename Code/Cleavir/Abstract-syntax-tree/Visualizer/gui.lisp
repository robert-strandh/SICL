(cl:in-package #:cleavir-ast-visualizer)

(clim:define-application-frame visualizer ()
    ((%ast :initarg :ast :reader ast))
  (:panes (application
           :application
           :scroll-bars nil
           :display-function 'display-ast
           :text-style (clim:make-text-style :sans-serif :roman 12))
          (interactor :interactor :scroll-bars nil))
  (:layouts (default (clim:vertically (:width 1200 :height 900)
                       (4/5 (clim:scrolling () application))
                       (1/5 (clim:scrolling () interactor))))))

(defgeneric label (ast))

(defmethod label (ast)
  (cleavir-ast-graphviz::label ast))

(defun draw (ast pane x y)
  (clim:draw-rectangle* pane x y (+ x 60) (+ y 30) :filled nil)
  (clim:draw-text* pane (label ast)
                   (+ x 30) (+ y 15) :align-x :center :align-y :center))

(defun display-ast (frame pane)
  (let* ((ast (ast frame))
         (table (make-hash-table :test #'eq)))
    (labels ((draw-ast (ast x y)
               (if (gethash ast table)
                   y
                   (let ((children (cleavir-ast:children ast)))
                     (draw ast pane x y)
                     (setf (gethash ast table) t)
                     (if (null children)
                         (+ y 40)
                         (loop for child in children
                               do (setf y (draw-ast child (+ x 70) y))
                               finally (return y)))))))
      (draw-ast ast 10 10))))
                 
(defun visualize (ast)
  (clim:run-frame-top-level
   (clim:make-application-frame 'visualizer :ast ast)))

(define-visualizer-command (com-quit :name t) ()
  (clim:frame-exit clim:*application-frame*))
