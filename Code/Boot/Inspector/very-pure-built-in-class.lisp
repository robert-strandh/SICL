(cl:in-package #:sicl-boot-inspector)

(defmethod clouseau:inspect-object-using-state
    ((object t)
     (state  inspected-very-pure-built-in-class)
     (style  (eql :collapsed))
     (stream t))
  (clim:with-drawing-options (stream :ink clim:+gold+)
    (format stream
            "Very purebuilt-in class ~s"
            (funcall (sicl-genv:fdefinition 'class-name
                                            (sicl-boot::e5 *boot*))
                     object))))

(defmethod clouseau:inspect-object-using-state
    ((object t)
     (state  inspected-very-pure-built-in-class)
     (style  (eql :expanded-header))
     (stream t))
  (clim:with-drawing-options (stream :ink clim:+gold+)
    (format stream
            "Very purebuilt-in class ~s"
            (funcall (sicl-genv:fdefinition 'class-name
                                            (sicl-boot::e6 *boot*))
                     object))))

(defmethod clouseau:inspect-object-using-state
    ((object t)
     (state  inspected-very-pure-built-in-class)
     (style  (eql :expanded-body))
     (stream t))
  (let ((e6 (sicl-boot::e6 *boot*)))
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
        (clouseau:format-place-row
         stream
         object
         'clouseau:pseudo-place
         (funcall (sicl-genv:fdefinition 'class-name e6)
                  object)
         :label "Name")
        (present-very-pure-object-slots object stream)))))
