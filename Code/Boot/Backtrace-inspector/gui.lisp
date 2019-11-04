(cl:in-package #:sicl-boot-backtrace-inspector)

(clim:define-application-frame inspector ()
  ((%stack :initarg :stack :reader stack)
   (%current-frame :initform nil))
  (:panes (backtrace :application
                     :scroll-bars nil
                     :display-function 'display-backtrace)
          (source :application
                  :scroll-bars nil
                  :display-function 'display-source)
          (inter :interactor :scroll-bars nil))
  (:layouts (default (clim:horizontally (:width 1200 :height 900)
                       (1/2 (clim:vertically ()
                              (9/10 (clim:scrolling () backtrace))
                              (1/10 (clim:scrolling () inter))))
                       (1/2 (clim:scrolling () source))))))

(defun display-backtrace (frame pane)
  (loop for entry in (stack frame)
        for origin = (sicl-hir-interpreter:origin entry)
        for ink = (if (null origin) clim:+red+ clim:+green+)
        do (clim:with-drawing-options (pane :ink ink)
             (format pane
                     "entry~%"))))

(defun display-source (frame pane)
  (declare (ignore frame pane))
  nil)

(defun inspect (stack &key new-process-p)
  (let ((frame (clim:make-application-frame 'inspector
                 :stack stack)))
    (flet ((run ()
             (clim:run-frame-top-level frame)))
      (if new-process-p
          (clim-sys:make-process #'run)
          (run)))))
  
                         
