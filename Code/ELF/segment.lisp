(cl:in-package #:sicl-elf)

(defparameter *segment-type*
  '((0 . :unused-entry)
    (1 . :loadable-segment)
    (2 . :dynamic-link-tables)
    (3 . :program-interpreter-path-name)
    (4 . :note-sections)
    ;; 5 is reserved
    (6 . :program-header-table)))

(defparameter *segment-attribute*
  '((0x1 . :execute-permission)
    (0x2 . :write-permission)
    (0x4 . :read-permission)))

(defclass segment ()
  ((%data-encoding
    :initarg :data-encoding
    :reader data-encoding)
   (%segment-type
    :initarg :segment-type
    :reader segment-type)
   (%segment-attributes
    :initarg :segment-attributes
    :reader segment-attributes)
   (%virtual-address
    :initarg :virtual-address
    :reader virtual-address)
   (%size-in-memory
    :initarg :size-in-memory
    :reader size-in-memory)
   (%alignment
    :initarg :alignment
    :reader alignment)
   (%contents
    :initarg :contents
    :reader contents)))

(defvar *segment-offsets*)

(defun segment-offset (segment)
  (let ((result (gethash segment *segment-offsets*)))
    (assert (not (null result)))
    result))

(defun store-segment-header (segment pos)
  (let ((encoding (data-encoding segment)))
    (store-value (encode (segment-type segment) *segment-type*)
                 32 pos encoding)
    (let ((coded-attributes
            (loop for attribute in (segment-attributes segment)
                  sum (encode attribute *segment-attribute*))))
      (store-value coded-attributes 32 pos encoding))
    (store-value (segment-offset segment) 64 pos encoding)
    (store-value (virtual-address segment) 64 pos encoding)
    ;; Physical address is reserved.
    (store-value 0 64 pos encoding)
    ;; Size of segment in file.  Maybe this value needs to be rounded up?
    (store-value (length (contents segment)) 64 pos encoding)
    ;; Size of segment in memory.  Maybe this value needs to be rounded up?
    (store-value (length (contents segment)) 64 pos encoding)
    (store-value (alignment segment) 64 pos encoding)))

(defun store-segment-contents (segment pos)
  (replace (bytes pos) (contents segment) :start1 (index pos)))
