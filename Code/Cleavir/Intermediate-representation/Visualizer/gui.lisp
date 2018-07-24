(cl:in-package #:cleavir-ir-visualizer)

(clim:define-application-frame visualizer ()
  ((%initial-instruction :initarg :initial-instruction
                         :accessor initial-instruction))
  (:panes (application :application :scroll-bars nil
                       :display-function 'display-hir)
          (interactor :interactor :scroll-bars nil))
  (:layouts (default (clim:vertically (:width 1200 :height 900)
                       (4/5 (clim:scrolling () application))
                       (1/5 (clim:scrolling () interactor))))))

(defun next-layer (layer table)
  (loop for node in layer
        append (loop for successor in (cleavir-ir:successors node)
                     when (null (gethash successor table))
                       collect successor)
        when (typep node 'cleavir-ir:enclose-instruction)
          collect (cleavir-ir:code node)))

(defun node-label (node)
  (if (typep node 'cleavir-ir:enter-instruction)
      "enter"
      (cleavir-ir-graphviz:label node)))

(defun node-width (node pane)
  (+ (clim:text-size pane (node-label node)) 5))

(defun draw-arcs (pane table)
  (loop for node being each hash-key of table
          using (hash-value (hpos1 . vpos1))
        for bottom = (+ vpos1 10)
        do (loop for successor in (cleavir-ir:successors node)
                 for (hpos2 . vpos2) = (gethash successor table)
                 for top = (- vpos2 10)
                 do (clim:draw-arrow* pane
                                      hpos1 bottom hpos2 top))
           (when (typep node 'cleavir-ir:enclose-instruction)
             (let* ((enter (cleavir-ir:code node))
                    (pos (gethash enter table)))
               (destructuring-bind (hpos2 . vpos2) pos
                 (clim:draw-arrow* pane
                                   hpos2 (- vpos2 10) hpos1 bottom
                                   :line-dashes t))))))

(defun display-hir (frame pane)
  (let ((table (make-hash-table :test #'eq)))
    (loop for layer = (list (initial-instruction frame))
            then (next-layer layer table)
          for vpos from 20 by 40
          until (null layer)
          do (loop for node in layer
                   for width = (node-width node pane)
                   for hpos = (+ (floor width 2) 10) then (+ hpos width 50)
                   do (setf (gethash node table)
                            (cons hpos vpos))
                      (clim:draw-rectangle*
                       pane
                       (- hpos (floor width 2)) (- vpos 10)
                       (+ hpos (floor width 2)) (+ vpos 10)
                       :filled nil)
                      (clim:draw-text* pane
                                       (node-label node)
                                       hpos vpos
                                       :align-x :center :align-y :center)))
    (draw-arcs pane table)))

(defun visualize (initial-instruction)
  (clim:run-frame-top-level 
   (clim:make-application-frame 'visualizer
                                :initial-instruction initial-instruction)))

(define-visualizer-command (com-quit :name t) ()
  (clim:frame-exit clim:*application-frame*))

