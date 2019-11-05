(cl:in-package #:sicl-boot-backtrace-inspector)

(clim:define-application-frame inspector ()
  ((%stack :initarg :stack :reader stack)
   (%current-entry :initform nil :accessor current-entry))
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

(defun display-entry (pane entry)
  (let ((origin (sicl-hir-interpreter:origin entry)))
    (when (eq entry (current-entry clim:*application-frame*))
      (format pane "+"))
    (multiple-value-bind (x y)
        (clim:stream-cursor-position pane)
      (declare (ignore x))
      (setf (clim:stream-cursor-position pane)
            (values 20 y)))
    (clim:with-output-as-presentation
        (pane entry 'sicl-hir-interpreter:call-stack-entry)
      (if (null origin)
          (clim:with-drawing-options (pane :ink clim:+red+)
            (format pane "entry~%"))
          (clim:with-drawing-options (pane :ink clim:+dark-green+)
            (let* ((start (car origin))
                   (line-index (sicl-source-tracking:line-index start))
                   (line (aref (sicl-source-tracking:lines start) line-index))
                   (char-index (sicl-source-tracking:character-index start)))
              (multiple-value-bind (x y)
                  (clim:stream-cursor-position pane)
                (declare (ignore x))
                (setf (clim:stream-cursor-position pane)
                      (values 20 y)))
              (format pane
                      "entry ~a~%"
                      (subseq line char-index))))))))

(defun display-backtrace (frame pane)
  (loop for entry in (stack frame)
        do (display-entry pane entry)))

(defun display-source (frame pane)
  (let ((entry (current-entry frame)))
    (unless (or (null entry)
                (null (sicl-hir-interpreter:origin entry)))
      (loop with entry = (current-entry frame)
            with origin = (sicl-hir-interpreter:origin entry)
            with start = (car origin)
            with end = (cdr origin)
            with lines = (sicl-source-tracking:lines start)
            for line across lines
            do (format pane "~a~%" line)))))

(defun inspect (stack &key new-process-p)
  (let ((frame (clim:make-application-frame 'inspector
                 :stack stack)))
    (flet ((run ()
             (clim:run-frame-top-level frame)))
      (if new-process-p
          (clim-sys:make-process #'run)
          (run)))))
  
                         
