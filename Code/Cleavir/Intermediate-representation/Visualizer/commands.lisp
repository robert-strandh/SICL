(cl:in-package #:cleavir-ir-visualizer)


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
  (clrhash (highlight-successors clim:*application-frame*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction commands.

(define-visualizer-command (com-inspect-instruction :name t)
    ((instruction 'cleavir-ir:instruction))
  (clouseau:inspector instruction))

(clim:define-presentation-to-command-translator inspect-instruction
    (cleavir-ir:instruction
     com-inspect-instruction
     visualizer
     :documentation "Inspect")
    (object)
  (list object))

(define-visualizer-command (com-highlight-successors :name t)
    ((instruction 'cleavir-ir:instruction))
  (setf (gethash instruction (highlight-successors clim:*application-frame*))
        t))

(clim:define-presentation-to-command-translator highlight-successors
    (cleavir-ir:instruction
     com-highlight-successors
     visualizer
     :documentation "Highlight successors")
    (object)
  (list object))

(define-visualizer-command (com-unhighlight-successors :name t)
    ((instruction 'cleavir-ir:instruction))
  (setf (gethash instruction (highlight-successors clim:*application-frame*))
        nil))

(clim:define-presentation-to-command-translator unhighlight-successors
    (cleavir-ir:instruction
     com-unhighlight-successors
     visualizer
     :documentation "Unhighlight successors")
    (object)
  (list object))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Datum commands.

(define-visualizer-command (com-inspect-datum :name t)
    ((instruction 'cleavir-ir:datum))
  (clouseau:inspector instruction))

(clim:define-presentation-to-command-translator inspect-datum
    (cleavir-ir:datum
     com-inspect-datum
     visualizer
     :documentation "Inspect")
    (object)
  (list object))

(define-visualizer-command (com-highlight-clients :name t)
    ((instruction 'cleavir-ir:instruction))
  (setf (gethash instruction (highlight-clients clim:*application-frame*))
        t))

(clim:define-presentation-to-command-translator highlight-clients
    (cleavir-ir:datum
     com-highlight-clients
     visualizer
     :documentation "Highlight clients")
    (object)
  (list object))
