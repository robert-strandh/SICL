(cl:in-package #:sicl-who-calls-visualizer)

(clim:define-application-frame visualizer ()
  ((%function-names :initarg :function-names :reader function-names)
   (%source-locations :initform '() :accessor source-locations)
   (%current-source :initform nil :accessor current-source))
  (:panes (function-names :application
                          :scroll-bars nil
                            :incremental-redisplay t
                          :end-of-line-action :allow
                          :display-function 'display-names)
          (source-locations :application
                            :scroll-bars nil
                            :incremental-redisplay t
                            :display-function 'display-source-locations)
          (source :application
                  :scroll-bars nil
                  :display-function 'display-source))
  (:layouts (default (clim:horizontally (:width 1600 :height 900)
                       (3/10 (clim:scrolling () function-names))
                       (2/10 (clim:scrolling () source-locations))
                       (5/10 (clim:scrolling () source))))))

(defclass function-name ()
  ((%locations :initarg :locations :reader locations)))

(defun function-name-less (fn1 fn2)
  (string< (if (symbolp fn1) fn1 (second fn1))
           (if (symbolp fn2) fn2 (second fn2))))

(defun display-names (frame pane)
  (let ((function-names '()))
    (maphash (lambda (name locations)
               (push (cons name
                           (make-instance 'function-name :locations locations))
                     function-names))
             (function-names frame))
    (setf function-names
          (sort function-names #'function-name-less :key #'car))
    (loop for function-name in function-names
          do (clim:with-output-as-presentation
                 (pane (cdr function-name) 'function-name)
               (let ((name (car function-name)))
                 (if (symbolp name)
                     (progn (format pane "~a " (symbol-name name))
                            (clim:with-drawing-options (pane :ink clim:+blue+)
                              (clim:with-text-size (pane :small)
                                (format pane "(~a)~%"
                                        (package-name (symbol-package name))))))
                     (let ((symbol (second name)))
                       (format pane "~a " (symbol-name symbol))
                       (clim:with-drawing-options (pane :ink clim:+blue+)
                         (clim:with-text-size (pane :small)
                           (format pane "(~a)~%"
                                   (package-name (symbol-package symbol))))))))))))

(defun display-source-locations (frame pane)
  (loop for location in (source-locations frame)
        do (clim:with-output-as-presentation
               (pane location 'sicl-source-tracking:source-position)
             (format pane "location~%"))))

(defun display-source (frame pane)
  (let ((source (current-source frame)))
    (unless (null source)
      (let* ((start (car source))
             (start-line-index (sicl-source-tracking:line-index start))
             (start-character-index (sicl-source-tracking:character-index start))
             (end (cdr source))
             (end-line-index (sicl-source-tracking:line-index end))
             (end-character-index (sicl-source-tracking:character-index end))
             (lines (sicl-source-tracking:lines start)))
        (setf (clim:stream-drawing-p pane) nil)
        ;; Display all the lines preceding the source location
        (loop for i from 0 below start-line-index
              do (format pane "~a~%" (aref lines i)))
        (if (= start-line-index end-line-index)
            (let ((line (aref lines start-line-index)))
              (format pane "~a" (subseq line 0 start-character-index))
              (clim:with-drawing-options (pane :ink clim:+red+)
                (format pane "~a"
                        (subseq line start-character-index end-character-index)))
              (format pane "~a~%" (subseq line end-character-index)))
            (let ((start-line (aref lines start-line-index))
                  (end-line (aref lines end-line-index)))
              (format pane "~a" (subseq start-line 0 start-character-index))
              (clim:with-drawing-options (pane :ink clim:+red+)
                (format pane "~a~%" (subseq start-line start-character-index))
                (loop for i from (1+ start-line-index) below end-line-index
                      do (format pane "~a~%" (aref lines i)))
                (format pane "~a" (subseq end-line 0 end-character-index)))
              (format pane "~a~%" (subseq end-line end-character-index))))
        (loop for i from (1+ end-line-index) below (length lines)
              do (format pane "~a~%" (aref lines i)))
        (setf (clim:stream-drawing-p pane) t)
        (clim:replay (clim:stream-output-history pane) pane)))))

(defun visualize (function-names &key new-process-p)
  (let ((frame (clim:make-application-frame 'visualizer
                 :function-names function-names)))
    (flet ((run ()
             (clim:run-frame-top-level frame)))
      (if new-process-p
          (clim-sys:make-process #'run)
          (run)))))
