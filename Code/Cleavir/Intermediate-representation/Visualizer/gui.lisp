(cl:in-package #:cleavir-ir-visualizer)

(clim:define-application-frame visualizer ()
  ((%initial-instruction :initarg :initial-instruction
                         :accessor initial-instruction)
   (%highlight-successors
    :initform (make-hash-table :test #'eq)
    :accessor highlight-successors)
   (%highlight-clients
    :initform (make-hash-table :test #'eq)
    :accessor highlight-clients)
   (%highlight-data
    :initform (make-hash-table :test #'eq)
    :accessor highlight-data))
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
      (label node)))

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
  (loop for hpos = 50 then (+ hpos (+ rack-width horizontal-node-separation))
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
                       (string (node-label node))
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
      (let ((hpos (+ min-hpos 300)))
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

(defun data-edge-should-be-highlighted-p (instruction datum)
  (or (gethash datum (highlight-clients clim:*application-frame*))
      (gethash instruction (highlight-data clim:*application-frame*))))

(defun draw-data-edge (instruction datum pane ink)
  (let ((line-thickness
          (if (data-edge-should-be-highlighted-p instruction datum) 3 1)))
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
                           :line-thickness line-thickness
                           :ink ink
                           :line-dashes t))))))

(defun datum-should-be-highlighted-p (datum)
  (or (gethash datum (highlight-clients clim:*application-frame*))
      (loop for instruction in (cleavir-ir:using-instructions datum)
              thereis (gethash instruction
                               (highlight-data clim:*application-frame*)))
      (loop for instruction in (cleavir-ir:defining-instructions datum)
              thereis (gethash instruction
                               (highlight-data clim:*application-frame*)))))

(defgeneric draw-datum (datum pane))

(defmethod draw-datum :around (datum pane)
  ;; Avoid drawing data which have coordinates too high for xlib to
  ;; handle.
  (multiple-value-bind (hpos vpos) (datum-position datum)
    (when (or (> hpos 20000)
              (> vpos 20000))
      (return-from draw-datum)))
  (let ((line-thickness
          (if (datum-should-be-highlighted-p datum) 2 1))
        (ink
          (if (datum-should-be-highlighted-p datum) clim:+magenta+ clim:+black+)))
    (clim:with-drawing-options (pane :ink ink :line-thickness line-thickness)
      (clim:with-output-as-presentation
          (pane datum 'cleavir-ir:datum)
        (call-next-method)))))

(defmethod draw-datum (datum pane)
  (multiple-value-bind (hpos vpos) (datum-position datum)
    (clim:draw-oval* pane hpos vpos
                     (floor datum-width 2) (floor datum-height 2)
                     :filled nil)))

(defun ink-for-element-type (type)
  (case type
    ((single-float double-float) clim:+purple+)
    (t clim:+dark-green+)))

(defmethod draw-datum ((datum cleavir-ir:lexical-location) pane)
  (multiple-value-bind (hpos vpos) (datum-position datum)
    (clim:draw-oval* pane hpos vpos
                     (floor datum-width 2) (floor datum-height 2)
                     :filled nil)
    (clim:with-text-size (pane :small)
      (clim:draw-text* pane (string (cleavir-ir:name datum))
                       hpos vpos
                       :align-x :center :align-y :center
                       :ink (ink-for-element-type
                             (cleavir-ir:element-type datum))))))

(defmethod draw-datum ((datum cleavir-ir:constant-input) pane)
  (multiple-value-bind (hpos vpos) (datum-position datum)
    (clim:draw-oval* pane hpos vpos
                     (floor datum-width 2) (floor datum-height 2)
                     :ink clim:+pink+
                     :filled t)
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

(defmethod draw-datum ((datum cleavir-ir:immediate-input) pane)
  (multiple-value-bind (hpos vpos) (datum-position datum)
    (clim:draw-oval* pane hpos vpos
                     (floor datum-width 2) (floor datum-height 2)
                     :ink clim:+yellow+
                     :filled t)
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

(defmethod draw-datum ((datum cleavir-ir:raw-integer) pane)
  (multiple-value-bind (hpos vpos) (datum-position datum)
    (clim:draw-oval* pane hpos vpos
                     (floor datum-width 2) (floor datum-height 2)
                     :ink clim:+light-green+
                     :filled t)
    (clim:draw-oval* pane hpos vpos
                     (floor datum-width 2) (floor datum-height 2)
                     :filled nil)))

(defmethod draw-datum ((datum cleavir-ir:register-location) pane)
  (multiple-value-bind (hpos vpos) (datum-position datum)
    (clim:draw-oval* pane hpos vpos
                     (floor datum-width 2) (floor datum-height 2)
                     :ink clim:+light-blue+
                     :filled t)
    (clim:draw-oval* pane hpos vpos
                     (floor datum-width 2) (floor datum-height 2)
                     :filled nil)
    (clim:with-text-size (pane :small)
      (clim:draw-text* pane
                       (cleavir-ir:name datum)
                       hpos vpos
                       :align-x :center :align-y :center
                       :ink clim:+dark-blue+))))

(defvar *dynamic-environment-locations*)

(defun draw-data (pane)
  (loop for datum being each hash-key of *data-position-table*
        do (unless (member datum *dynamic-environment-locations*)
             (draw-datum datum pane)
             (loop for instruction in (cleavir-ir:defining-instructions datum)
                   do (draw-data-edge instruction datum pane clim:+red+))
             (loop for instruction in (cleavir-ir:using-instructions datum)
                   do (draw-data-edge instruction datum pane clim:+dark-green+)))))

(defun find-all-dynamic-environment-locations (initial-instruction)
  (let ((result '()))
    (cleavir-ir:map-instructions-arbitrary-order
     (lambda (instruction)
       (pushnew (cleavir-ir:dynamic-environment-location instruction)
                result))
     initial-instruction)
    result))

(defun display-hir (frame pane)
  (let ((*instruction-position-table* (make-hash-table :test #'eq))
        (*data-position-table* (make-hash-table :test #'eq))
        (*dynamic-environment-locations*
          (find-all-dynamic-environment-locations (initial-instruction frame))))
    (multiple-value-bind (*base-width* *base-height*)
        (clim:text-size pane "enclose")
      (layout-program (initial-instruction frame) pane)
      (draw-nodes (initial-instruction frame) pane)
      (layout-data (initial-instruction frame) pane)
      (fix-data-overlaps (initial-instruction frame))
      (draw-data pane)
      (draw-arcs pane (make-arcs pane *instruction-position-table*)))))

(defun visualize (initial-instruction &key new-process-p)
  (cleavir-ir:reinitialize-data initial-instruction)
  (let ((frame (clim:make-application-frame
                'visualizer
                :initial-instruction initial-instruction)))
    (flet ((run ()
             (clim:run-frame-top-level frame)))
      (if new-process-p
          (clim-sys:make-process #'run)
          (run)))))
