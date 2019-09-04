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

(defgeneric ast-width (pane ast))

(defmethod ast-width :around (pane ast)
  (ceiling (call-next-method)))

(defmethod ast-width (pane ast)
  (+ 5 (nth-value 0 (clim:text-size pane (label ast)))))

(defgeneric ast-height (pane ast))

(defmethod ast-height :around (pane ast)
  (ceiling (call-next-method)))

(defmethod ast-height (pane ast)
  (+ 5 (nth-value 1 (clim:text-size pane (label ast)))))

(defgeneric line-thickness (ast))

(defmethod line-thickness (ast)
  (declare (ignore ast))
  1)

(defgeneric draw (ast pane x y))

(defmethod draw (ast pane x y)
  (let ((width (ast-width pane ast))
        (height (ast-height pane ast))
        (pen-color clim:+black+)
        (line-thickness (line-thickness ast)))
    (clim:with-output-as-presentation (pane ast 'cleavir-ast:ast)
      (clim:draw-rectangle* pane
                            x y (+ x width) (+ y height)
                            :ink (background-color ast)
                            :filled t)
      (clim:draw-rectangle* pane
                            x y (+ x width) (+ y height)
                            :line-thickness line-thickness
                            :ink pen-color
                            :filled nil)
      (clim:draw-text* pane (label ast)
                       (+ x (/ width 2)) (+ y (/ height 2))
                       :align-x :center :align-y :center))))

(defun display-layout (pane layout)
  (unless (indirect-p layout)
    (let* ((position (position layout))
           (x (+ (x position) 10))
           (y (+ (y position) 10)))
      (draw (ast layout) pane x y)
      (loop for child in (children layout)
            do (display-layout pane child)))))

(defun compute-positions (layout)
  (let ((table (make-hash-table :test #'eq)))
    (labels ((aux (layout)
               (unless (indirect-p layout)
                 (setf (gethash (ast layout) table)
                       (make-point (+ (x (position layout)) 10)
                                   (+ (y (position layout)) 10)))
                 (loop for child in (children layout)
                       do (aux child)))))
      (aux layout))
    table))

(defun draw-edge-to-children (pane right middle-right)
  (clim:draw-line* pane
                   right middle-right
                   (+ right 10) middle-right))

(defun draw-edge-to-parent (pane left left-middle parent-x parent-y dashes)
  (clim:draw-lines* pane
                    (list left left-middle
                          parent-x left-middle
                          parent-x left-middle
                          parent-x parent-y)
                    :line-dashes dashes))

(defun draw-edges (table pane layout)
  (labels ((aux (layout)
             (unless (or (indirect-p layout) (null (children layout)))
               (let* ((ast (ast layout))
                      (position (gethash ast table))
                      (width (ast-width pane ast))
                      (right (+ (x position) width))
                      (height (ast-height pane ast))
                      (middle-right (+ (y position) (/ height 2))))
                 (draw-edge-to-children pane right middle-right)
                 (loop for child in (children layout)
                       do (aux child)
                          (if (indirect-p child)
                              (let* ((position (position child))
                                     (left (x position))
                                     (middle-left (+ (y position) 2))
                                     (ast (ast child))
                                     (ast-position (gethash ast table))
                                     (height (ast-height pane ast))
                                     (width (ast-width pane ast))
                                     (bottom (+ (y ast-position) height))
                                     (middle-bottom (+ (x ast-position) (/ width 2))))
                                (draw-edge-to-parent
                                 pane left middle-left middle-bottom bottom t)
                                (draw-edge-to-parent
                                 pane left middle-left (+ right 10) middle-right nil))
                              (let* ((ast (ast child))
                                     (position (gethash ast table))
                                     (left (x position))
                                     (height (ast-height pane ast))
                                     (middle-left (+ (y position) (/ height 2))))
                                (draw-edge-to-parent
                                 pane left middle-left (+ right 10) middle-right nil))))))))
    (aux layout)))

(defun display-ast (frame pane)
  (let* ((ast (ast frame))
         (layout (make-layout pane ast))
         (table (compute-positions layout)))
    (display-layout pane layout)
    (draw-edges table pane layout)
    ;; (clim:with-output-as-presentation (pane layout 'layout)
    ;;   (clim:draw-rectangle* pane 500 500 600 600 :ink clim:+red+))
    layout))
                 
(defun visualize (ast &key new-process-p)
  (let ((frame (clim:make-application-frame 'visualizer :ast ast)))
    (flet ((run ()
             (clim:run-frame-top-level frame)))
      (if new-process-p
          (clim-sys:make-process #'run)
          (run)))))

(define-visualizer-command (com-quit :name t) ()
  (clim:frame-exit clim:*application-frame*))

(define-visualizer-command (com-inspect-ast :name t)
    ((ast 'cleavir-ast:ast))
  (clouseau:inspect ast))

(define-visualizer-command (com-inspect-layout :name t)
    ((layout 'layout))
  (clouseau:inspect layout))
