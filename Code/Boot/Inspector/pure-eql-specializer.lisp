(cl:in-package #:sicl-boot-inspector)

(defmethod clouseau:inspect-object-using-state
    ((object t)
     (state  inspected-pure-eql-specializer)
     (style  (eql :collapsed))
     (stream t))
  (clim:with-drawing-options (stream :ink clim:+blue+)
    (format stream
            "Pure eql specializer ~s"
            (funcall (sicl-genv:fdefinition 'class-name
                                            (sicl-boot::e5 *boot*))
                     object))))

(defmethod clouseau:inspect-object-using-state
    ((object t)
     (state  inspected-pure-eql-specializer)
     (style  (eql :expanded-header))
     (stream t))
  (clim:with-drawing-options (stream :ink clim:+blue+)
    (format stream
            "Pure eql specializer ~s"
            (funcall (sicl-genv:fdefinition 'class-name
                                            (sicl-boot::e5 *boot*))
                     object))))

(defmethod clouseau:inspect-object-using-state
    ((object t)
     (state  inspected-pure-eql-specializer)
     (style  (eql :expanded-body))
     (stream t))
  (let ((e5 (sicl-boot::e5 *boot*)))
    (clouseau:with-preserved-cursor-x (stream)
      (clim:formatting-table (stream)
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
         (funcall (sicl-genv:fdefinition 'class-name e5)
                  object)
         :label "Name")
        (present-pure-object-slots object stream)))))
