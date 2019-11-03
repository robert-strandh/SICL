(cl:in-package #:sicl-boot-inspector)

(defmethod clouseau:inspect-object-using-state
    ((object t)
     (state  inspected-impure-effective-slot-definition)
     (style  (eql :collapsed))
     (stream t))
  (clim:with-drawing-options (stream :ink clim:+red+)
    (format stream
            "Impure effective slot definition")))

(defmethod clouseau:inspect-object-using-state
    ((object t)
     (state  inspected-impure-effective-slot-definition)
     (style  (eql :expanded-header))
     (stream t))
  (clim:with-drawing-options (stream :ink clim:+red+)
    (format stream "Impure effective slot definition")))

(defmethod clouseau:inspect-object-using-state
    ((object t)
     (state  inspected-impure-effective-slot-definition)
     (style  (eql :expanded-body))
     (stream t))
  (let ((e4 (sicl-boot::e4 *boot*)))
    (declare (ignore e4))
    (clouseau:with-preserved-cursor-x (stream)
      (clim:formatting-table (stream)
        (clouseau:format-place-row
         stream
         object
         'clouseau:pseudo-place
         (aref (rack-of-object object) 0)
         :label "Stamp")
        (present-impure-object-slots object stream)))))
