(cl:in-package #:sicl-elf)

(defun test3 ()
  (let* ((segment2
           (make-instance 'segment
             :data-encoding :little-endian
             :segment-type :loadable-segment
             :segment-attributes '(:read-permission :execute-permission)
             :virtual-address #x401000
             :alignment #x1000
             :contents
             (make-array #x25 :element-type '(unsigned-byte 8)
                              :initial-contents
              #(#xb8 #x01 #x00 #x00 #x00 #xbf #x01 #x00
                #x00 #x00 #x48 #xbe #x00 #x20 #x40 #x00
                #x00 #x00 #x00 #x00 #xba #x0d #x00 #x00
                #x00 #x0f #x05 #xb8 #x3c #x00 #x00 #x00
                #x48 #x31 #xff #x0f #x05))))
         (segment3
           (make-instance 'segment
             :data-encoding :little-endian
             :segment-type :loadable-segment
             :segment-attributes '(:read-permission :write-permission)
             :virtual-address #x402000
             :alignment #x1000
             :contents
             (make-array #xd :element-type '(unsigned-byte 8)
                             :initial-contents
              #(#x48 #x65 #x6c #x6c #x6f #x2c #x20 #x57
                #x6f #x72 #x6c #x64 #x0a))))
         (elf
           (make-instance 'elf
             :file-class :64-bit
             :data-encoding :little-endian
             :file-version :original-version
             :os/abi-identification :system-v
             :abi-version 0
             :file-type :executable
             :machine :amd-x86-64
             :entry-point-address #x401000
             :segments (list segment2 segment3))))
    (let ((*segment-offsets* (make-hash-table :test #'eq)))
      (values (store elf) elf))))
