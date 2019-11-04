(cl:in-package #:sicl-boot-inspector)

(defmethod clouseau:inspect-object-using-state
    ((object t)
     (state  inspected-very-pure-standard-writer-method)
     (style  (eql :collapsed))
     (stream t))
  (clim:with-drawing-options (stream :ink clim:+gold+)
    (format stream
            "Very purestandard writer method")))

(defmethod clouseau:inspect-object-using-state
    ((object t)
     (state  inspected-very-pure-standard-writer-method)
     (style  (eql :expanded-header))
     (stream t))
  (clim:with-drawing-options (stream :ink clim:+gold+)
    (format stream "Very purestandard writer method")))

(defmethod clouseau:inspect-object-using-state
    ((object t)
     (state  inspected-very-pure-standard-writer-method)
     (style  (eql :expanded-body))
     (stream t))
  (let ((e6 (sicl-boot::e6 *boot*)))
    (declare (ignore e6))
    (clouseau:with-preserved-cursor-x (stream)
      (clim:formatting-table (stream)
        (clouseau:format-place-row
         stream
         object
         'clouseau:pseudo-place
         (class-of-object object)
         :label "Class")
        (clouseau:format-place-row
         stream
         object
         'clouseau:pseudo-place
         (aref (rack-of-object object) 0)
         :label "Stamp")
        (present-very-pure-object-slots object stream)))))
