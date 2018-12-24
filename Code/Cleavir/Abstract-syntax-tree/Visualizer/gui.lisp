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

(defmethod label ((ast cleavir-ast:constant-fdefinition-ast))
  "c-fdef")

(defmethod label ((ast cleavir-ast:fdefinition-ast))
  "fdef")

(defmethod label ((ast cleavir-ast:constant-ast))
  "const")

(defmethod label ((ast cleavir-ast:load-time-value-ast))
  "l-t-v")

(defgeneric ast-width (pane ast))

(defmethod ast-width (pane ast)
  (+ 5 (nth-value 0 (clim:text-size pane (label ast)))))

(defmethod ast-width (pane (ast cleavir-ast:constant-fdefinition-ast))
  (let* ((width1 (nth-value 0 (clim:text-size pane (label ast))))
         (function-name (format nil "~S" (cleavir-ast:name ast)))
         (width2 (nth-value 0 (clim:text-size pane function-name))))
    (+ 5 (max width1 width2))))

(defgeneric ast-height (pane ast))

(defmethod ast-height (pane ast)
  (+ 5 (nth-value 1 (clim:text-size pane (label ast)))))

(defmethod ast-height (pane (ast cleavir-ast:constant-fdefinition-ast))
  (let* ((height1 (nth-value 1 (clim:text-size pane "c-fdef")))
         (function-name (format nil "~S" (cleavir-ast:name ast)))
         (height2 (nth-value 1 (clim:text-size pane function-name))))
    (+ 10 height1 height2)))

(defgeneric draw (ast pane x y))

(defmethod draw (ast pane x y)
  (let ((width (ast-width pane ast))
        (height (ast-height pane ast)))
    (clim:with-output-as-presentation (pane ast 'cleavir-ast:ast)
      (clim:draw-rectangle* pane x y (+ x width) (+ y height) :filled nil)
      (clim:draw-text* pane (label ast)
                       (+ x (/ width 2)) (+ y (/ height 2))
                       :align-x :center :align-y :center))))

(defmethod draw ((ast cleavir-ast:constant-fdefinition-ast) pane x y)
  (let* ((width (ast-width pane ast))
         (height (ast-height pane ast))
         (function-name (format nil "~S" (cleavir-ast:name ast))))
    (clim:with-output-as-presentation (pane ast 'cleavir-ast:ast)
      (clim:draw-rectangle* pane x y (+ x width) (+ y height) :filled nil)
      (clim:draw-text* pane "c-fdef"
                       (+ x (/ width 2)) (+ y (/ height 3))
                       :align-x :center :align-y :center)
      (clim:draw-text* pane function-name
                       (+ x (/ width 2)) (+ y (* height 2/3))
                       :align-x :center :align-y :center))))

(defgeneric draw-children (table pane ast children x y))

(defgeneric draw-ast (table pane ast x y))

(defmethod draw-ast (table pane ast x y)
  (if (gethash ast table)
      (+ y 10)
      (let ((children (cleavir-ast:children ast)))
        (draw ast pane x y)
        (setf (gethash ast table) (cons x y))
        (if (null children)
            (+ y (ast-height pane ast) 10)
            (draw-children table pane ast children x y)))))

(defmethod draw-ast (table pane (ast cleavir-ast:function-ast) x y)
  (if (gethash ast table)
      (+ y 10)
      (let* ((children (cleavir-ast:children ast))
             (reordered (append (rest children) (list (first children)))))
        (draw ast pane x y)
        (setf (gethash ast table) (cons x y))
        (draw-children table pane ast reordered x y))))

(defmethod draw-children (table pane ast children x y)
  (clim:draw-line* pane
                   (+ x (ast-width pane ast))
                   (+ y (/ (ast-height pane ast) 2))
                   (+ x (ast-width pane ast) 10)
                   (+ y (/ (ast-height pane ast) 2)))
  (loop with yy = y
        with width = (ast-width pane ast)
        for child in children
        for height = (ast-height pane child)
        for child-pos = (gethash child table)
        for yyy = (draw-ast table pane child (+ x width 20) yy)
        do (clim:draw-line* pane
                            (+ x width 10)
                            (+ y (/ (ast-height pane ast) 2))
                            (+ x width 10)
                            (+ yy (/ height 2)))
           (if (null child-pos)
               (clim:draw-line* pane
                                (+ x width 10)
                                (+ yy (/ height 2))
                                (+ x width 20)
                                (+ yy (/ height 2)))
               (clim:draw-lines* pane
                                 (list (+ x width 10)
                                       (+ yy (/ height 2))
                                       (+ (car child-pos)
                                          (/ (ast-width pane child) 2))
                                       (+ yy (/ height 2))
                                       (+ (car child-pos)
                                          (/ (ast-width pane child) 2))
                                       (+ yy (/ height 2))
                                       (+ (car child-pos)
                                          (/ (ast-width pane child) 2))
                                       (+ (cdr child-pos)
                                          (/ (ast-height pane child) 2)))
                                 :line-dashes t))
           (setf yy yyy)
        finally (return (+ yy 10))))

(defun display-layout (table pane layout)
  (unless (indirect-p layout)
    (let ((position (gethash (ast layout) table)))
      (draw (ast layout) pane (x position) (y position))
      (loop for child in (children layout)
            do (display-layout table pane child)))))

(defun compute-positions (layout)
  (let ((table (make-hash-table :test #'eq)))
    (labels ((aux (layout)
               (unless (indirect-p layout)
                 (setf (gethash (ast layout) table)
                       (make-point (+ (x (position layout)) 10)
                                   (y (position layout))))
                 (loop for child in (children layout)
                       do (aux child)))))
      (aux layout))
    table))

(defun draw-edges (table pane layout)
  (labels ((aux (layout)
             (unless (or (indirect-p layout) (null (children layout)))
               (let* ((ast (ast layout))
                      (position (gethash ast table))
                      (width (ast-width pane ast))
                      (right (+ (x position) width))
                      (height (ast-height pane layout))
                      (middle (+ (y position) (/ height 2))))
                 (clim:draw-line* pane
                                  right middle
                                  (+ right 10) middle))
               (loop for child in (children layout)
                     do (aux child)))))
    (aux layout)))

(defun display-ast (frame pane)
  (let* ((ast (ast frame))
         (layout (make-layout pane ast))
         (table (compute-positions layout)))
    ;; (draw-ast table pane ast 10 10)
    (display-layout table pane layout)
    (draw-edges table pane layout)
    layout))
                 
(defun visualize (ast)
  (clim:run-frame-top-level
   (clim:make-application-frame 'visualizer :ast ast)))

(define-visualizer-command (com-quit :name t) ()
  (clim:frame-exit clim:*application-frame*))

(define-visualizer-command (com-inspect-ast :name t)
    ((ast 'cleavir-ast:ast))
  (clouseau:inspector ast))

(define-visualizer-command (com-inspect-layout :name t)
    ((layout 'layout))
  (clouseau:inspector layout))
