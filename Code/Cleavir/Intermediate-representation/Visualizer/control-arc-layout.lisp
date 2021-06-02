(cl:in-package #:cleavir-ir-visualizer)

(defgeneric draw-arc (pane arc))

(defun compute-dx (instruction instructions)
  (let ((length (length instructions))
        (position (or (position instruction instructions) 0)))
    (round (* *base-width*
              0.15
              (let ((middle-position (/ (1- length) 2)))
                (- position middle-position))))))

(defun arc-should-be-highlighted-p (from-instruction to-instruction)
  (declare (ignore to-instruction))
  (gethash from-instruction (highlight-successors clim:*application-frame*)))

(defclass arc ()
  ((%highlighted-p :initarg :highlighted-p
                   :initform nil :accessor highlighted-p)))

(defclass short-arc (arc)
  (;; This slot contains the horizontal position where the arc starts.
   (%hpos1 :initarg :hpos1 :accessor hpos1)
   ;; This slot contains the horizontal position where the arc ends.
   (%hpos2 :initarg :hpos2 :accessor hpos2)
   ;; This slot contains the vertical position where the arc starts.
   (%vpos1 :initarg :vpos1 :accessor vpos1)
   ;; This slot contains the vertical position where the arc ends.
   (%vpos2 :initarg :vpos2 :accessor vpos2)))

(defmethod print-object ((object short-arc) stream)
  (print-unreadable-object (object stream :type t)
    (format stream "[~a ~a] [~a ~a]"
            (hpos1 object) (vpos1 object) (hpos2 object) (vpos2 object))))

(defun make-short-arc (from-instruction to-instruction pane)
  (let ((highlight-p
          (arc-should-be-highlighted-p from-instruction to-instruction)))
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
            :hpos1 (round (+ hpos1 dx1))
            :vpos1 (round (+ vpos1 dy1))
            :hpos2 (round (+ hpos2 dx2))
            :vpos2 (round (+ vpos2 dy2))
            :highlighted-p highlight-p))))))

(defmethod draw-arc (pane (arc short-arc))
  (clim:draw-arrow* pane
                    (hpos1 arc) (vpos1 arc)
                    (hpos2 arc) (vpos2 arc)
                    :ink
                    (if (highlighted-p arc) clim:+blue+ clim:+black+)
                    :line-thickness
                    (if (highlighted-p arc) 2 1)))

(defclass long-arc (arc)
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

(defmethod print-object ((object long-arc) stream)
  (with-accessors ((hpos1 hpos1) (vpos1 vpos1)
                   (hpos2 hpos2) (vpos2 vpos2)
                   (hpos3 hpos3) (vpos3 vpos3)
                   (vpos4 vpos4))
      object
    (print-unreadable-object (object stream)
      (format stream "[~a ~a] [~a ~a] [~a ~a] [~a ~a] [~a ~a] [~a ~a]"
              hpos1 vpos1
              hpos1 vpos2
              hpos2 vpos2
              hpos2 vpos3
              hpos3 vpos3
              hpos3 vpos4))))

(defun same-destination-p (arc1 arc2)
  (and (< (abs (- (vpos4 arc1) (vpos4 arc2))) *base-height*)
       (< (abs (- (hpos3 arc1) (hpos3 arc2))) *base-width*)))

(defun make-long-arc (from-instruction to-instruction pane)
  (let ((highlight-p
          (arc-should-be-highlighted-p from-instruction to-instruction))
        ;; DX is the horizontal distance from the center of the
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
            :hpos1 (round (+ hpos1 dx1))
            :hpos2 (round (- (if (> hpos2 hpos1) hpos2 hpos1) dx))
            :hpos3 (round (+ hpos2 dx2))
            :vpos1 (round (+ vpos1 dy1))
            :vpos2 (round (+ vpos1 dy1 ddy1))
            :vpos3 (round (+ vpos2 dy2 ddy2))
            :vpos4 (round (+ vpos2 dy2))
            :highlighted-p highlight-p))))))

(defmethod draw-arc (pane (arc long-arc))
  (with-accessors ((hpos1 hpos1) (vpos1 vpos1)
                   (hpos2 hpos2) (vpos2 vpos2)
                   (hpos3 hpos3) (vpos3 vpos3)
                   (vpos4 vpos4) (highlighted-p highlighted-p))
      arc
    (clim:draw-polygon* pane
                        (list hpos1 vpos1
                              hpos1 vpos2
                              hpos2 vpos2
                              hpos2 vpos3
                              hpos3 vpos3)
                        :ink
                        (if highlighted-p clim:+blue+ clim:+black+)
                        :line-thickness
                        (if highlighted-p 2 1)
                        :closed nil
                        :filled nil)
    (clim:draw-arrow* pane
                      hpos3 vpos3
                      hpos3 vpos4
                      :ink
                      (if highlighted-p clim:+blue+ clim:+black+)
                      :line-thickness
                      (if highlighted-p 2 1))))

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
                   for delta from 5 by 5
                   do (decf (vpos3 arc) delta))
             (setf remaining (set-difference remaining group)))))

(defun same-jump-position-p (arc1 arc2)
  (let ((min1 (min (vpos2 arc1) (vpos3 arc1)))
        (min2 (min (vpos2 arc2) (vpos3 arc2)))
        (max1 (max (vpos2 arc1) (vpos3 arc1)))
        (max2 (max (vpos2 arc2) (vpos3 arc2))))
    (and (< (abs (- (hpos2 arc1) (hpos2 arc2))) 3)
         (or (and (< min2 (+ max1 3)) (> max2 (- min1 3)))
             (and (< min1 (+ max2 3)) (> max1 (- min2 3)))))))

(defun some-same-jump-position-p (arcs arc)
  (loop for a in arcs
        thereis (same-jump-position-p a arc)))

(defun find-same-jump-position (arcs candidates)
  (find-if (lambda (x) (some-same-jump-position-p arcs x)) candidates))

(defun extract-same-jump-position (arcs)
  (loop with result = (list (first arcs))
        with remaining = (rest arcs)
        for candidate = (find-same-jump-position result remaining)
        until (null candidate)
        do (push candidate result)
           (setf remaining (remove candidate remaining))
        finally (return result)))

(defun separate-overlapping-arcs-horizontally (arcs)
  (loop with remaining = arcs
        until (null remaining)
        do (let ((group (extract-same-jump-position remaining)))
             (loop for arc in (rest group)
                   for delta from 5 by 5
                   do (decf (hpos2 arc) delta))
             (setf remaining (set-difference remaining group)))))

(defclass enclosure-arc (arc)
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

(defun arc-is-short-p (from-instruction to-instruction)
  (multiple-value-bind (hpos1 vpos1)
      (instruction-position from-instruction)
    (multiple-value-bind (hpos2 vpos2)
        (instruction-position to-instruction)
      (and (< (abs (- hpos2 hpos1)) *base-width*)
           (> vpos2 vpos1)
           (< (- vpos2 vpos1) (* 1.5 vertical-node-separation))))))

(defun draw-arcs (pane arcs)
  (let* ((long-arcs (remove-if-not (lambda (x) (typep x 'long-arc)) arcs))
         (other-arcs (set-difference arcs long-arcs)))
    (loop for arc in other-arcs
          do (draw-arc pane arc))
    (separate-overlapping-arcs-vertically long-arcs)
        (separate-overlapping-arcs-horizontally long-arcs)
    (loop for arc in long-arcs
          do (draw-arc pane arc))))

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
