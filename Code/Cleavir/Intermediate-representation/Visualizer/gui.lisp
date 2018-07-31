(cl:in-package #:cleavir-ir-visualizer)

(clim:define-application-frame visualizer ()
  ((%initial-instruction :initarg :initial-instruction
                         :accessor initial-instruction)
   (%highlight-successors
    :initform (make-hash-table :test #'eq)
    :accessor highlight-successors))
  (:panes (application
           :application
           :scroll-bars nil
           :display-function 'display-hir
           :text-style (clim:make-text-style :sans-serif :roman 20))
          (interactor :interactor :scroll-bars nil))
  (:layouts (default (clim:vertically (:width 1200 :height 900)
                       (4/5 (clim:scrolling () application))
                       (1/5 (clim:scrolling () interactor))))))

(defun node-label (node)
  (if (typep node 'cleavir-ir:enter-instruction)
      "enter"
      (cleavir-ir-graphviz:label node)))

(defun node-width (node pane)
  (+ (clim:text-size pane (node-label node)) 5))

(defun node-height (pane)
  (+ (nth-value 1 (clim:text-size pane "A")) 5))

;;; Compute the width of a layer of nodes.
(defun compute-layer-width (nodes pane)
  (loop for node in nodes
        for width = (node-width node pane)
        for hpos = (+ (floor width 2) 10)
          then (+ hpos width horizontal-node-separation)
        finally (return hpos)))

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

(defun layout-program (enter-instruction pane)
  (loop for hpos = 10 then (+ hpos (+ rack-width horizontal-node-separation))
        for rack = (list enter-instruction) then (next-rack rack)
        for dimensions = (loop for inst in rack
                               collect (multiple-value-bind (width height)
                                           (compute-function-dimensions inst pane)
                                         (cons width height)))
        for rack-width = (loop for dim in dimensions maximize (car dim))
        until (null rack)
        do (loop for enter-instruction in rack
                 for dimension in dimensions
                 for vpos = 10
                   then (+ vpos (cdr dimension) vertical-rack-separation)
                 do (layout-function enter-instruction hpos vpos))))

(defun draw-node (node hpos vpos pane)
  (let ((width (node-width node pane))
        (height (node-height pane))
        (highlight-p
          (loop for predecessor in (cleavir-ir:predecessors node)
                thereis (gethash predecessor
                                 (highlight-successors clim:*application-frame*)))))
    (clim:with-output-as-presentation
        (pane node 'cleavir-ir:instruction)
      (clim:draw-rectangle* pane
                            (- hpos (floor width 2))
                            (- vpos (floor height 2))
                            (+ hpos (floor width 2))
                            (+ vpos (floor height 2))
                            :ink (if highlight-p clim:+blue+ clim:+black+)
                            :line-thickness (if highlight-p 2 1)
                            :filled nil)
      (clim:draw-text* pane
                       (node-label node)
                       hpos vpos
                       :ink (if highlight-p clim:+blue+ clim:+black+)
                       :align-x :center :align-y :center))))

(defun draw-nodes (initial-instruction pane)
  (cleavir-ir:map-instructions-arbitrary-order
   (lambda (node)
     (multiple-value-bind (hpos vpos) (instruction-position node)
       (draw-node node hpos vpos pane)))
   initial-instruction))

(defun layout-datum (datum)
  (let* ((defining-instructions (cleavir-ir:defining-instructions datum))
         (using-instructions (cleavir-ir:using-instructions datum))
         (instructions (append defining-instructions using-instructions)))
    (let ((max-vpos (loop for instruction in instructions
                          maximize (instruction-vertical-position instruction)))
          (min-hpos (loop for instruction in instructions
                          minimize (instruction-horizontal-position instruction)))
          (min-vpos (loop for instruction in instructions
                          minimize (instruction-vertical-position instruction))))
      (let ((hpos (+ min-hpos 120)))
        (setf (datum-position datum) (cons hpos (/ (+ max-vpos min-vpos) 2)))))))

(defun layout-inputs-and-outputs (instruction)
  (loop for input in (cleavir-ir:inputs instruction)
        do (layout-datum input))
  (loop for output in (cleavir-ir:outputs instruction)
        do (layout-datum output)))

(defun layout-data (initial-instruction pane)
  (declare (ignore pane))
  (cleavir-ir:map-instructions-arbitrary-order
   (lambda (instruction) (layout-inputs-and-outputs instruction))
   initial-instruction))

(defun draw-data-edge (instruction datum pane ink)
  (multiple-value-bind (hpos1 vpos1) (instruction-position instruction)
    (multiple-value-bind (hpos2 vpos2) (datum-position datum)
      (let ((h1 (if (> hpos2 hpos1)
                    (+ hpos1 (/ (node-width instruction pane) 2))
                    (- hpos1 (/ (node-width instruction pane) 2))))
            (h2 (if (> hpos2 hpos1)
                    (- hpos2 30)
                    (+ hpos2 30)))
            (v2 (if (> vpos2 vpos1)
                    (- vpos2 10)
                    (+ vpos2 10))))
        (clim:draw-line* pane
                         h1 vpos1
                         h2 v2
                         :ink ink
                         :line-dashes t)))))
(defgeneric draw-datum (datum pane))

(defmethod draw-datum :around (datum pane)
  (clim:with-output-as-presentation
      (pane datum 'cleavir-ir:datum)
    (call-next-method)))

(defmethod draw-datum (datum pane)
  (multiple-value-bind (hpos vpos) (datum-position datum)
    (clim:draw-oval* pane hpos vpos
                     (floor datum-width 2) (floor datum-height 2)
                     :filled nil)))

(defmethod draw-datum ((datum cleavir-ir:lexical-location) pane)
  (multiple-value-bind (hpos vpos) (datum-position datum)
    (clim:draw-oval* pane hpos vpos
                     (floor datum-width 2) (floor datum-height 2)
                     :filled nil)
    (clim:with-text-size (pane :small)
      (clim:draw-text* pane (string (cleavir-ir:name datum))
                       hpos vpos
                       :align-x :center :align-y :center
                       :ink clim:+dark-green+))))

(defmethod draw-datum ((datum cleavir-ir:values-location) pane)
  (multiple-value-bind (hpos vpos) (datum-position datum)
    (clim:draw-oval* pane hpos vpos
                     (floor datum-width 2) (floor datum-height 2)
                     :filled nil)
    (clim:with-text-size (pane :small)
      (clim:draw-text* pane "values"
                       hpos vpos
                       :align-x :center :align-y :center
                       :ink clim:+dark-blue+))))

(defmethod draw-datum ((datum cleavir-ir:constant-input) pane)
  (multiple-value-bind (hpos vpos) (datum-position datum)
    (clim:draw-oval* pane hpos vpos
                     (floor datum-width 2) (floor datum-height 2)
                     :filled nil)
    (let ((label (princ-to-string (cleavir-ir:value datum))))
      (clim:with-text-size (pane :small)
        (clim:draw-text* pane
                         (subseq label 0 (min 15 (length label)))
                         hpos vpos
                         :align-x :center :align-y :center
                       :ink clim:+dark-blue+)))))

(defmethod draw-datum ((datum cleavir-ir:load-time-value-input) pane)
  (multiple-value-bind (hpos vpos) (datum-position datum)
    (clim:draw-oval* pane hpos vpos
                     (floor datum-width 2) (floor datum-height 2)
                     :filled nil)
    (let ((label (princ-to-string (cleavir-ir:form datum))))
      (clim:with-text-size (pane :small)
        (clim:draw-text* pane
                         (subseq label 0 (min 15 (length label)))
                         hpos vpos
                         :align-x :center :align-y :center
                       :ink clim:+dark-blue+)))))

(defun draw-data (pane)
  (loop for datum being each hash-key of *data-position-table*
        do (draw-datum datum pane)
           (loop for instruction in (cleavir-ir:defining-instructions datum)
                 do (draw-data-edge instruction datum pane clim:+red+))
           (loop for instruction in (cleavir-ir:using-instructions datum)
                 do (draw-data-edge instruction datum pane clim:+dark-green+))))

(defun display-hir (frame pane)
  (let ((*instruction-position-table* (make-hash-table :test #'eq))
        (*data-position-table* (make-hash-table :test #'eq)))
    (multiple-value-bind (*base-width* *base-height*)
        (clim:text-size pane "enclose")
      (layout-program (initial-instruction frame) pane)
      (draw-nodes (initial-instruction frame) pane)
      (layout-data (initial-instruction frame) pane)
      (fix-data-overlaps (initial-instruction frame))
      (draw-data pane)
      (draw-arcs pane (make-arcs pane *instruction-position-table*)))))

(defun visualize (initial-instruction)
  (cleavir-ir:reinitialize-data initial-instruction)
  (clim:run-frame-top-level
   (clim:make-application-frame 'visualizer
                                :initial-instruction initial-instruction)))

(define-visualizer-command (com-quit :name t) ()
  (clim:frame-exit clim:*application-frame*))

(define-visualizer-command (com-inspect :name t)
    ((instruction 'cleavir-ir:instruction))
  (clouseau:inspector instruction))

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

(define-visualizer-command (com-highlight-successors :name t)
    ((instruction 'cleavir-ir:instruction))
  (setf (gethash instruction (highlight-successors clim:*application-frame*))
        t))

(define-visualizer-command (com-unhighlight-successors :name t)
    ((instruction 'cleavir-ir:instruction))
  (setf (gethash instruction (highlight-successors clim:*application-frame*))
        nil))
