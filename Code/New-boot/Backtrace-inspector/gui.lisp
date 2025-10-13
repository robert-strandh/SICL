(cl:in-package #:sicl-new-boot-backtrace-inspector)

(clim:define-application-frame inspector ()
  ((%stack :initarg :stack :reader stack)
   (%current-entry :initform nil :accessor current-entry))
  (:panes (called-function :application
                           :scroll-bars nil
                           :end-of-line-action :allow
                           :display-function 'display-called-function)
          (arguments :application
                     :scroll-bars nil
                     :end-of-line-action :allow
                     :display-function 'display-arguments)
          (backtrace :application
                     :scroll-bars nil
                     :incremental-redisplay t
                     :display-function 'display-backtrace)
          (source :application
                  :scroll-bars nil
                  :display-function 'display-source)
          (inter :interactor :scroll-bars nil))
  (:layouts (default (clim:horizontally (:width 1200 :height 900)
                       (1/2 (clim:vertically ()
                              (1/10 (clim:scrolling () called-function))
                              (3/10 (clim:scrolling () arguments))
                              (5/10 (clim:scrolling () backtrace))
                              (1/10 (clim:scrolling () inter))))
                       (1/2 (clim:scrolling () source))))))

(defclass called-function () ())

(defun display-called-function (frame pane)
  (let ((entry (current-entry frame)))
    (unless (null entry)
      (let ((called-function (cb:called-function entry)))
        (clim:with-output-as-presentation
            (pane called-function 'called-function)
          (format pane "~s~%" called-function))))))

(defclass argument () ())

(defun display-arguments (frame pane)
  (let ((entry (current-entry frame)))
    (unless (null entry)
      (loop for argument in (cb:arguments entry)
            for i from 1
            do (clim:with-drawing-options (pane :ink clim:+red+)
                 (format pane "~a:" i))
               (multiple-value-bind (x y)
                   (clim:stream-cursor-position pane)
                 (declare (ignore x))
                 (setf (clim:stream-cursor-position pane)
                       (values 30 y)))
               (clim:with-output-as-presentation
                   (pane argument 'argument)
                 (format pane "~s~%" argument))))))

(defun display-entry (pane entry)
  (let ((origin (cb:origin entry)))
    (when (eq entry (current-entry clim:*application-frame*))
      (format pane "+"))
    (multiple-value-bind (x y)
        (clim:stream-cursor-position pane)
      (declare (ignore x))
      (setf (clim:stream-cursor-position pane)
            (values 20 y)))
    (clim:with-output-as-presentation
        (pane entry 'cb:stack-entry)
      (if (or (null origin) (null (cst:source origin)))
          (clim:with-drawing-options (pane :ink clim:+red+)
            (format pane "entry with no source information~%"))
          (clim:with-drawing-options (pane :ink clim:+dark-green+)
            (let* ((start (car (cst:source origin)))
                   (line-index (sicl-source-tracking:line-index start))
                   (line (aref (sicl-source-tracking:lines start) line-index))
                   (char-index (sicl-source-tracking:character-index start)))
              (multiple-value-bind (x y)
                  (clim:stream-cursor-position pane)
                (declare (ignore x))
                (setf (clim:stream-cursor-position pane)
                      (values 20 y)))
              (format pane "~a~%" (subseq line char-index))))))))

(defun display-backtrace (frame pane)
  (loop for entry in (stack frame)
        do (display-entry pane entry)))

(defun display-source (frame pane)
  (let ((entry (current-entry frame)))
    (unless (null entry)
      (let ((origin (cb:origin entry)))
        (unless (null origin)
          (let ((source (cst:source origin)))
            (unless (null source)
              (let* ((start (car source))
                     (start-line-index
                       (sicl-source-tracking:line-index start))
                     (start-character-index
                       (sicl-source-tracking:character-index start))
                     (end (cdr source))
                     (end-line-index
                       (sicl-source-tracking:line-index end))
                     (end-character-index
                       (sicl-source-tracking:character-index end))
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
                #+(or)(clim:replay (clim:stream-output-history pane) pane)))))))))

(defun inspect (stack &key new-process-p)
  (let ((frame (clim:make-application-frame 'inspector
                 :stack stack)))
    (flet ((run ()
             (clim:run-frame-top-level frame)))
      (if new-process-p
          (clim-sys:make-process #'run)
          (run)))))
