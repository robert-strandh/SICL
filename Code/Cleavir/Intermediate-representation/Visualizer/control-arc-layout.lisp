(cl:in-package #:cleavir-ir-visualizer)

(defgeneric draw-arc (pane arc))

(defun compute-dx (instruction instructions)
  (let ((length (length instructions))
        (position (position instruction instructions)))
    (round (* *base-width*
              0.15
              (let ((middle-position (/ (1- length) 2)))
                (- position middle-position))))))

(defclass short-arc ()
  (;; This slot contains the horizontal position where the arc starts.
   (%hpos1 :initarg :hpos1 :accessor hpos1)
   ;; This slot contains the horizontal position where the arc ends.
   (%hpos2 :initarg :hpos2 :accessor hpos2)
   ;; This slot contains the vertical position where the arc starts.
   (%vpos1 :initarg :vpos1 :accessor vpos1)
   ;; This slot contains the vertical position where the arc ends.
   (%vpos2 :initarg :vpos2 :accessor vpos2)))

(defun make-short-arc (from-instruction to-instruction pane)
  (multiple-value-bind (hpos1 vpos1)
      (instruction-position from-instruction)
    (multiple-value-bind (hpos2 vpos2)
        (instruction-position to-instruction)
      (let ((dx1 (compute-dx to-instruction
                             (cleavir-ir:successors from-instruction)))
            (dx2 (compute-dx from-instruction
                             (cleavir-ir:predecessors to-instruction)))
            (dy1 (/ (node-height pane) 2))
            (dy2 (- (/ (node-height pane) 2))))
        (make-instance 'short-arc
          :hpos1 (+ hpos1 dx1)
          :vpos1 (+ vpos1 dy1)
          :hpos2 (+ hpos2 dx2)
          :vpos2 (+ vpos2 dy2))))))

(defmethod draw-arc (pane (arc short-arc))
  (clim:draw-arrow* pane (hpos1 arc) (vpos1 arc)  (hpos2 arc) (vpos2 arc)))

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

(defun make-long-arc (from-instruction to-instruction pane)
  (let (;; DX is the horizontal distance from the center of the
        ;; FROM-INSTRUCTION (if the TO-INSTRUCTION is to the left of
        ;; the FROM-INSTRUCTION) or from the center of the
        ;; TO-INSTRUCTION (if the TO-INSTRUCTION is to the right of
        ;; FROM-INSTRUCTION) to the horizontal position where the arc
        ;; leaps from one vertical position to another.
        (dx (round (* 0.6 *base-width*)))
        (ddy1 (round (* 0.2 vertical-node-separation)))
        (ddy2 (- (round (* 0.5 vertical-node-separation)))))
    (multiple-value-bind (hpos1 vpos1)
        (instruction-position from-instruction)
      (multiple-value-bind (hpos2 vpos2)
          (instruction-position to-instruction)
        (let ((dx1 (compute-dx to-instruction
                               (cleavir-ir:successors from-instruction)))
              (dx2 (compute-dx from-instruction
                               (cleavir-ir:predecessors to-instruction)))
              (dy1 (/ (node-height pane) 2))
              (dy2 (- (/ (node-height pane) 2))))
          (make-instance 'long-arc
            :hpos1 (+ hpos1 dx1)
            :hpos2 (- (if (> hpos2 hpos1) hpos2 hpos1) dx)
            :hpos3 (+ hpos2 dx2)
            :vpos1 (+ vpos1 dy1)
            :vpos2 (+ vpos1 dy1 ddy1)
            :vpos3 (+ vpos2 dy2 ddy2)
            :vpos4 (+ vpos2 dy2)))))))

(defmethod draw-arc (pane (arc long-arc))
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

(defclass enclosure-arc ()
  (;; This slot contains the horizontal position where the arc starts.
   (%hpos1 :initarg :hpos1 :accessor hpos1)
   ;; This slot contains the horizontal position where the arc ends.
   (%hpos2 :initarg :hpos2 :accessor hpos2)
   ;; This slot contains the vertical position where the arc starts.
   (%vpos1 :initarg :vpos1 :accessor vpos1)
   ;; This slot contains the vertical position where the arc ends.
   (%vpos2 :initarg :vpos2 :accessor vpos2)))

(defun make-enclosure-arc (from-node to-node pane)
  (multiple-value-bind (hpos1 vpos1) (instruction-position from-node)
    (multiple-value-bind (hpos2 vpos2) (instruction-position to-node)
      (let ((dx1 -20)
            (dx2 20)
            (dy1 (/ (node-height pane) 2))
            (dy2 (- (/ (node-height pane) 2))))
        (make-instance 'enclosure-arc
          :hpos1 (+ hpos1 dx1)
          :hpos2 (+ hpos2 dx2)
          :vpos1 (+ vpos1 dy1)
          :vpos2 (+ vpos2 dy2))))))

(defmethod draw-arc (pane (arc enclosure-arc))
  (clim:draw-arrow* pane
                    (hpos1 arc) (vpos1 arc) (hpos2 arc) (vpos2 arc)
                    :line-dashes t))

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

(defun arc-is-short-p (from-instruction to-instruction)
  (multiple-value-bind (hpos1 vpos1)
      (instruction-position from-instruction)
    (multiple-value-bind (hpos2 vpos2)
        (instruction-position to-instruction)
      (and (< (abs (- hpos2 hpos1)) *base-width*)
           (> vpos2 vpos1)
           (< (- vpos2 vpos1) (* 1.5 vertical-node-separation))))))

(defun draw-arcs (pane arcs)
  (loop for arc in arcs
        do (draw-arc pane arc)))

(defun make-arcs (pane table)
  (loop for node being each hash-key of table
          using (hash-value (hpos1 . vpos1))
        for bottom = (+ vpos1 10)
        append (loop for successor in (cleavir-ir:successors node)
                     collect (if (arc-is-short-p node successor)
                                 (make-short-arc node successor pane)
                                 (make-long-arc node successor pane)))
        when (typep node 'cleavir-ir:enclose-instruction)
          collect (make-enclosure-arc (cleavir-ir:code node) node pane)))
