(cl:in-package #:sicl-hir-visualizer)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Global commands.

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

(define-visualizer-command (com-unhighlight-everything :name t) ()
  (clrhash (highlight-successors clim:*application-frame*))
  (clrhash (highlight-clients clim:*application-frame*))
  (clrhash (highlight-clients clim:*application-frame*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction commands.

(define-visualizer-command (com-inspect-instruction :name t)
    ((instruction 'ir:instruction))
  (clouseau:inspect instruction))

(clim:define-presentation-to-command-translator inspect-instruction
    (ir:instruction
     com-inspect-instruction
     visualizer
     :documentation "Inspect")
    (object)
  (list object))

(define-visualizer-command (com-highlight-successors :name t)
    ((instruction 'ir:instruction))
  (setf (gethash instruction (highlight-successors clim:*application-frame*))
        t))

(clim:define-presentation-to-command-translator highlight-successors
    (ir:instruction
     com-highlight-successors
     visualizer
     :documentation "Highlight successors")
    (object)
  (list object))

(define-visualizer-command (com-unhighlight-successors :name t)
    ((instruction 'ir:instruction))
  (setf (gethash instruction (highlight-successors clim:*application-frame*))
        nil))

(clim:define-presentation-to-command-translator unhighlight-successors
    (ir:instruction
     com-unhighlight-successors
     visualizer
     :documentation "Unhighlight successors")
    (object)
  (list object))

(define-visualizer-command (com-highlight-data :name t)
    ((instruction 'ir:instruction))
  (setf (gethash instruction (highlight-data clim:*application-frame*))
        t))

(clim:define-presentation-to-command-translator highlight-data
    (ir:instruction
     com-highlight-data
     visualizer
     :documentation "Highlight data")
    (object)
  (list object))

(define-visualizer-command (com-unhighlight-data :name t)
    ((instruction 'ir:instruction))
  (setf (gethash instruction (highlight-data clim:*application-frame*))
        nil))

(clim:define-presentation-to-command-translator unhighlight-data
    (ir:instruction
     com-unhighlight-data
     visualizer
     :documentation "Unhighlight data")
    (object)
  (list object))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Datum commands.

(define-visualizer-command (com-inspect-datum :name t)
    ((datum 'ir:datum))
  (clouseau:inspect datum))

(clim:define-presentation-to-command-translator inspect-datum
    (ir:datum
     com-inspect-datum
     visualizer
     :documentation "Inspect")
    (object)
  (list object))

(define-visualizer-command (com-highlight-clients :name t)
    ((datum 'ir:datum))
  (setf (gethash datum (highlight-clients clim:*application-frame*))
        t))

(clim:define-presentation-to-command-translator highlight-clients
    (ir:datum
     com-highlight-clients
     visualizer
     :documentation "Highlight clients")
    (object)
  (list object))

(define-visualizer-command (com-unhighlight-clients :name t)
    ((datum 'ir:datum))
  (setf (gethash datum (highlight-clients clim:*application-frame*))
        nil))

(clim:define-presentation-to-command-translator unhighlight-clients
    (ir:datum
     com-unhighlight-clients
     visualizer
     :documentation "Unhighlight clients")
    (object)
  (list object))
