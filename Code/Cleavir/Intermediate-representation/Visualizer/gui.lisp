(cl:in-package #:cleavir-ir-visualizer)

(clim:define-application-frame visualizer ()
  ((%initial-instruction :initarg :initial-instruction
                         :accessor initial-instruction))
  (:panes (application
           :application
           :scroll-bars nil
           :display-function 'display-hir
           :text-style (clim:make-text-style :sans-serif :roman 20))
          (interactor :interactor :scroll-bars nil))
  (:layouts (default (clim:vertically (:width 1200 :height 900)
                       (4/5 (clim:scrolling () application))
                       (1/5 (clim:scrolling () interactor))))))

;;; Map instructions to a CONS of their horizontal and vertical
;;; positions.
(defvar *instruction-position-table*)

(defun next-layer (layer table)
  (loop for node in layer
        append (loop for successor in (cleavir-ir:successors node)
                     when (null (gethash successor table))
                       collect successor)
        when (typep node 'cleavir-ir:enclose-instruction)
          collect (cleavir-ir:code node)))

(defun next-layer-no-nesting (layer table)
  (loop for node in layer
        append (loop for successor in (cleavir-ir:successors node)
                     when (null (gethash successor table))
                       do (setf (gethash successor table) t)
                       and collect successor)))

(defun node-label (node)
  (if (typep node 'cleavir-ir:enter-instruction)
      "enter"
      (cleavir-ir-graphviz:label node)))

(defun node-width (node pane)
  (+ (clim:text-size pane (node-label node)) 5))

(defun node-height (pane)
  (+ (nth-value 1 (clim:text-size pane "A")) 5))

(defparameter *horizontal-node-separation* 100)

;;; Compute the width of a layer of nodes.
(defun compute-layer-width (nodes pane)
  (loop for node in nodes
        for width = (node-width node pane)
        for hpos = (+ (floor width 2) 10)
          then (+ hpos width *horizontal-node-separation*)
        finally (return hpos)))

;;; Compute the width and the height of a function
(defun compute-function-dimensions (enter-instruction pane)
  (let ((table (make-hash-table :test #'eq)))
    (loop for layer = (list enter-instruction)
            then (next-layer-no-nesting layer table)
          for vpos from 20 by (* 3 (node-height pane))
          until (null layer)
          maximize (compute-layer-width layer pane) into width
          finally (return (values width vpos)))))

(defun find-enclose-instructions (enter-instruction)
  (let ((table (make-hash-table :test #'eq))
        (result '()))
    (labels ((traverse (instruction)
               (unless (gethash instruction table)
                 (setf (gethash instruction table) t)
                 (when (typep instruction 'cleavir-ir:enclose-instruction)
                   (push instruction result))
                 (loop for successor in (cleavir-ir:successors instruction)
                       do (traverse successor)))))
      (traverse enter-instruction))
    (reverse result)))

(defun next-rack (rack)
  (loop for instruction in rack
        for enclose-instructions = (find-enclose-instructions instruction)
        for enter-instructions = (mapcar #'cleavir-ir:code enclose-instructions)
        append enter-instructions))

(defun assign-instruction-positions (enter-instruction hpos vpos pane)
  (loop for layer = (list enter-instruction)
          then (next-layer-no-nesting layer *instruction-position-table*)
        for dy from 20 by (* 3 (node-height pane))
        until (null layer)
        do (loop for node in layer
                 for width = (node-width node pane)
                 for dx = (+ (floor width 2) 10)
                   then (+ dx width *horizontal-node-separation*)
                 do (setf (gethash node *instruction-position-table*)
                          (cons (+ hpos dx) (+ vpos dy))))))

(defun layout-program (enter-instruction pane)
  (loop for hpos = 10 then (+ hpos rack-width *horizontal-node-separation*)
        for rack = (list enter-instruction) then (next-rack rack)
        for dimensions = (loop for inst in rack
                               collect (multiple-value-bind (width height)
                                           (compute-function-dimensions inst pane)
                                         (cons width height)))
        for rack-width = (loop for dim in dimensions maximize (car dim))
        until (null rack)
        do (loop for enter-instruction in rack
                 for dimension in dimensions
                 for vpos = 10 then (+ vpos (cdr dimension) 50)
                 do (assign-instruction-positions
                     enter-instruction hpos vpos pane))))

(defun compute-dx (instruction instructions)
  (let ((length (length instructions))
        (position (position instruction instructions)))
    (* 6 (let ((middle-position (/ (1- length) 2)))
           (- position middle-position)))))

(defun draw-control-flow-arc (from-node to-node pane)
  (destructuring-bind (hpos1 . vpos1)
      (gethash from-node *instruction-position-table*)
    (destructuring-bind (hpos2 . vpos2)
        (gethash to-node *instruction-position-table*)
      (let ((dx1 (compute-dx to-node (cleavir-ir:successors from-node)))
            (dx2 (compute-dx from-node (cleavir-ir:predecessors to-node)))
            (dy1 (/ (node-height pane) 2))
            (dy2 (- (/ (node-height pane) 2))))
        (clim:draw-arrow* pane
                          (+ hpos1 dx1) (+ vpos1 dy1)
                          (+ hpos2 dx2) (+ vpos2 dy2))))))

(defun draw-arcs (pane table)
  (loop for node being each hash-key of table
          using (hash-value (hpos1 . vpos1))
        for bottom = (+ vpos1 10)
        do (loop for successor in (cleavir-ir:successors node)
                 do (draw-control-flow-arc node successor pane))
           (when (typep node 'cleavir-ir:enclose-instruction)
             (let* ((enter (cleavir-ir:code node))
                    (pos (gethash enter table)))
               (destructuring-bind (hpos2 . vpos2) pos
                 (clim:draw-arrow* pane
                                   hpos2 (- vpos2 10) hpos1 bottom
                                   :line-dashes t))))))

(defun draw-node (node hpos vpos pane)
  (let ((width (node-width node pane))
        (height (node-height pane)))
    (clim:draw-rectangle* pane
                          (- hpos (floor width 2)) (- vpos (/ height 2))
                          (+ hpos (floor width 2)) (+ vpos (/ height 2))
                          :filled nil)
    (clim:draw-text* pane
                     (node-label node)
                     hpos vpos
                     :align-x :center :align-y :center)))

(defun draw-nodes (initial-instruction pane)
  (cleavir-ir:map-instructions-arbitrary-order
   (lambda (node)
     (destructuring-bind (hpos . vpos)
         (gethash node *instruction-position-table*)
       (draw-node node hpos vpos pane)))
   initial-instruction))

(defun display-hir (frame pane)
  (let* ((*instruction-position-table* (make-hash-table :test #'eq))
         (table *instruction-position-table*))
    (layout-program (initial-instruction frame) pane)
    (draw-nodes (initial-instruction frame) pane)
    (draw-arcs pane table)))

(defun visualize (initial-instruction)
  (clim:run-frame-top-level 
   (clim:make-application-frame 'visualizer
                                :initial-instruction initial-instruction)))

(define-visualizer-command (com-quit :name t) ()
  (clim:frame-exit clim:*application-frame*))

(define-visualizer-command (com-zoom-in :name t) ()
  (let* ((current-text-style (clim:medium-text-style *standard-output*))
         (partial-text-style (clim:make-text-style nil nil :larger)))
    (setf (clim:medium-text-style *standard-output*)
          (clim:merge-text-styles partial-text-style current-text-style))))

(define-visualizer-command (com-zoom-out :name t) ()
  (let* ((current-text-style (clim:medium-text-style *standard-output*))
         (partial-text-style (clim:make-text-style nil nil :smaller)))
    (setf (clim:medium-text-style *standard-output*)
          (clim:merge-text-styles partial-text-style current-text-style))))
