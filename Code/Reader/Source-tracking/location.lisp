(cl:in-package #:sicl-source-tracking-reader)

(defclass location ()
  ((%start-line :initarg :start-line :reader start-line)
   (%start-column :initarg :start-column :reader start-column)
   (%end-line :initarg :end-line :reader end-line)
   (%end-column :initarg :end-column :reader end-column)))
