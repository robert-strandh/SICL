(cl:in-package #:sicl-boot-backtrace-inspector)

(define-inspector-command (com-quit :name t) ()
  (clim:frame-exit clim:*application-frame*))

(define-inspector-command (com-show-source :name t)
    ((entry 'sicl-hir-interpreter:call-stack-entry))
  (setf (current-entry clim:*application-frame*)
        entry))

(clim:define-presentation-to-command-translator show-source
    (sicl-hir-interpreter:call-stack-entry
     com-show-source
     inspector
     :documentation "Show Source")
    (object)
  (list object))
