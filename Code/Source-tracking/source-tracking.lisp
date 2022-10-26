(cl:in-package #:sicl-source-tracking)

;;; Our source location contains the vector of lines of the entire
;;; file, a line index, and a character index within the line.
(defclass source-position (cleavir-io:cloneable-mixin)
  ((%lines :initarg :lines :reader lines)
   (%line-index :initarg :line-index :reader line-index)
   (%character-index :initarg :character-index :reader character-index)))

(clonedijk:define-clone-information source-position
  (:lines lines)
  (:line-index line-index)
  (:character-index character-index))
