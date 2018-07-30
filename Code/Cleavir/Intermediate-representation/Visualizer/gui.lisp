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

(defun compute-dx (instruction instructions)
  (let ((length (length instructions))
        (position (position instruction instructions)))
    (round (* *base-width*
              0.15
              (let ((middle-position (/ (1- length) 2)))
                (- position middle-position))))))

(defclass long-arc ()
  (;; This slot contains the horizontal position where the arc starts.
   (%hpos1 :initarg :hpos1 :accessor hpos1)
   ;; This slot contains the horizontal position where the arc leaps
   ;; from one vertical position to another.  The value of this slot
   ;; may be modified slightly if several arcs overlap in the same
   ;; position.
   (%hpos2 :initarg :hpos2 :accessor hpos2)
   ;; This slot contains the horizontal position where the arc ends.
   (%hpos3 :initarg :hpos3 :accessor hpos3)
   ;; This slot contains the vertical position where the arc starts.
   (%vpos1 :initarg :vpos1 :accessor vpos1)
   ;; This slot contains the vertical position where the arc turns
   ;; from its start node to the leap position.
   (%vpos2 :initarg :vpos2 :accessor vpos2)
   ;; This slot contains the vertical position where the arcs turns
   ;; from its leap to approach its target instruction.  The value of
   ;; this slot might be modified slightly if many arcs share the same
   ;; target instruction.
   (%vpos3 :initarg :vpos3 :accessor vpos3)
   ;; This slot contains the vertical position where the arc ends.
   (%vpos4 :initarg :vpos4 :accessor vpos4)))

(defun same-destination-p (arc1 arc2)
  (and (< (abs (- (vpos4 arc1) (vpos4 arc2))) *base-width*)
       (< (abs (- (hpos3 arc1) (hpos3 arc2))) *base-height*)))

;;; Extract all the arcs from ARCS that have the same destination as
;;; the first one.
(defun extract-same-destination (arcs)
  (loop with result = (list (first arcs))
        for arc in (rest arcs)
        when (same-destination-p (first result) arc)
          do (push arc result)
        finally (return result)))

;;; When two arcs have the same target instruction, we must make sure
;;; that the incoming arcs have different vertical positions.
;;; Otherwise, we can not distinguish which arc represents which
;;; predecessor.  Notice that the separation in the other dimension
;;; has already been taken care of by taking into account the order
;;; between predecessors.
(defun separate-overlapping-arcs-vertically (arcs)
  (loop with remaining = arcs
        until (null remaining)
        do (let ((group (extract-same-destination remaining)))
             (loop for arc in (rest group)
                   for delta from 3
                   do (decf (vpos3 arc) delta))
             (setf remaining (set-difference remaining group)))))

(defun draw-long-arc (pane arc)
  (with-accessors ((hpos1 hpos1) (vpos1 vpos1)
                   (hpos2 hpos2) (vpos2 vpos2)
                   (hpos3 hpos3) (vpos3 vpos3)
                   (vpos4 vpos4))
      arc
    (clim:draw-polygon* pane
                        (list hpos1 vpos1
                              hpos1 vpos2
                              hpos2 vpos2
                              hpos2 vpos3
                              hpos3 vpos3)
                        :closed nil
                        :filled nil)
    (clim:draw-arrow* pane
                      hpos3 vpos3
                      hpos3 vpos4)))

(defun draw-long-arcs (arcs pane)
  (separate-overlapping-arcs-vertically arcs)
  (loop for arc in arcs
        do (draw-long-arc pane arc)))

(defun draw-control-flow-arc (from-node to-node pane)
  (let (;; DX is used only for long arcs.  It is the horizontal
        ;; distance from the center of the FROM-NODE (if the TO-NODE
        ;; is to the left of the FROM-NODE) or from the center of the
        ;; TO-NODE (if the TO-NODE is to the right of FROM-NODE) to
        ;; the horizontal position where the arc leaps from one
        ;; vertical position to another.
        (dx (round (* 0.6 *base-width*)))
        (ddy1 (round (* 0.2 vertical-node-separation)))
        (ddy2 (- (round (* 0.5 vertical-node-separation))))
        (long-arcs '()))
    (multiple-value-bind (hpos1 vpos1) (instruction-position from-node)
      (multiple-value-bind (hpos2 vpos2) (instruction-position to-node)
        (let ((dx1 (compute-dx to-node (cleavir-ir:successors from-node)))
              (dx2 (compute-dx from-node (cleavir-ir:predecessors to-node)))
              (dy1 (/ (node-height pane) 2))
              (dy2 (- (/ (node-height pane) 2))))
          (cond ((and (< (abs (- hpos2 hpos1)) *base-width*)
                      (> vpos2 vpos1)
                      (< (- vpos2 vpos1) (* 1.5 vertical-node-separation)))
                 ;; Draw a short downward control arc
                 (clim:draw-arrow* pane
                                   (+ hpos1 dx1) (+ vpos1 dy1)
                                   (+ hpos2 dx2) (+ vpos2 dy2)))
                ((> hpos2 hpos1)
                 (push (make-instance 'long-arc
                         :hpos1 (+ hpos1 dx1)
                         :hpos2 (- hpos2 dx)
                         :hpos3 (+ hpos2 dx2)
                         :vpos1 (+ vpos1 dy1)
                         :vpos2 (+ vpos1 dy1 ddy1)
                         :vpos3 (+ vpos2 dy2 ddy2)
                         :vpos4 (+ vpos2 dy2))
                       long-arcs))
                (t
                 (push (make-instance 'long-arc
                         :hpos1 (+ hpos1 dx1)
                         :hpos2 (- hpos1 dx)
                         :hpos3 (+ hpos2 dx2)
                         :vpos1 (+ vpos1 dy1)
                         :vpos2 (+ vpos1 dy1 ddy1)
                         :vpos3 (+ vpos2 dy2 ddy2)
                         :vpos4 (+ vpos2 dy2))
                       long-arcs))))))
    (draw-long-arcs long-arcs pane)))

(defun draw-enclosure-arc (from-node to-node pane)
  (multiple-value-bind (hpos1 vpos1) (instruction-position from-node)
    (multiple-value-bind (hpos2 vpos2) (instruction-position to-node)
      (let ((dx1 -20)
            (dx2 20)
            (dy1 (/ (node-height pane) 2))
            (dy2 (- (/ (node-height pane) 2))))
        (clim:draw-arrow* pane
                          (+ hpos1 dx1) (+ vpos1 dy1)
                          (+ hpos2 dx2) (+ vpos2 dy2)
                          :line-dashes t)))))

(defun draw-arcs (pane table)
  (loop for node being each hash-key of table
          using (hash-value (hpos1 . vpos1))
        for bottom = (+ vpos1 10)
        do (loop for successor in (cleavir-ir:successors node)
                 do (draw-control-flow-arc node successor pane))
           (when (typep node 'cleavir-ir:enclose-instruction)
             (draw-enclosure-arc (cleavir-ir:code node) node pane))))

(defun draw-node (node hpos vpos pane)
  (let ((width (node-width node pane))
        (height (node-height pane)))
    (clim:with-output-as-presentation
        (pane node 'cleavir-ir:instruction)
      (clim:draw-rectangle* pane
                            (- hpos (floor width 2))
                            (- vpos (floor height 2))
                            (+ hpos (floor width 2))
                            (+ vpos (floor height 2))
                            :filled nil)
      (clim:draw-text* pane
                       (node-label node)
                       hpos vpos
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
      (draw-arcs pane *instruction-position-table*))))

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
