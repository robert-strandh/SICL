(cl:in-package #:sicl-who-calls-visualizer)

(define-visualizer-command (com-quit :name t) ()
  (clim:frame-exit clim:*application-frame*))

(define-visualizer-command (com-show-source :name t)
    ((position 'sicl-source-tracking:source-position))
  (setf (current-source clim:*application-frame*)
        position))

(clim:define-presentation-to-command-translator show-source
    (sicl-source-tracking:source-position
     com-show-source
     visualizer
     :documentation "Show Source")
    (object)
  (list object))

(define-visualizer-command (com-show-locations :name t)
    ((function-name 'function-name))
  (setf (source-locations clim:*application-frame*)
        (locations function-name)))

(clim:define-presentation-to-command-translator inspect-argument
    (function-name
     com-show-locations
     visualizer
     :documentation "Inspect Argument")
    (object)
  (list object))
