(cl:in-package #:sicl-boot-inspector)

(defmethod clouseau:inspect-object-using-state
    ((object t)
     (state  inspected-impure-standard-generic-function)
     (style  (eql :collapsed))
     (stream t))
  (clim:with-drawing-options (stream :ink clim:+red+)
    (format stream
            "Impure standard generic function ~s"
            (funcall (sicl-genv:fdefinition 'sicl-clos:generic-function-name
                                            (sicl-boot::e4 *boot*))
                     object))))

(defmethod clouseau:inspect-object-using-state
    ((object t)
     (state  inspected-impure-standard-generic-function)
     (style  (eql :expanded-header))
     (stream t))
  (clim:with-drawing-options (stream :ink clim:+red+)
    (format stream
            "Impure standard generic function ~s"
            (funcall (sicl-genv:fdefinition 'sicl-clos:generic-function-name
                                            (sicl-boot::e4 *boot*))
                     object))))

(defmethod clouseau:inspect-object-using-state
    ((object t)
     (state  inspected-impure-standard-generic-function)
     (style  (eql :expanded-body))
     (stream t))
  (let ((e4 (sicl-boot::e4 *boot*)))
    (clouseau:with-preserved-cursor-x (stream)
      (clim:formatting-table (stream)
        (clouseau:format-place-row
         stream
         object
         'clouseau:pseudo-place
         (aref (slot-value object 'sicl-boot-phase-3::%rack) 0)
         :label "Stamp")
        (clouseau:format-place-row
         stream
         object
         'clouseau:pseudo-place
         (funcall (sicl-genv:fdefinition 'sicl-clos:generic-function-name e4)
                  object)
         :label "Name")
        (clouseau:format-place-row
         stream
         object
         'clouseau:pseudo-place
         (funcall (sicl-genv:fdefinition 'sicl-clos:generic-function-lambda-list e4)
                  object)
         :label "Lambda list")))))

