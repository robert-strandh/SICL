(cl:in-package #:sicl-boot-phase3)

(setf sicl-boot-phase2:*make-instance-default*
      #'sicl-boot-phase2:make-instance-default)

(setf sicl-boot-phase2:*make-built-in-instance-default*
      #'sicl-boot-phase2:make-built-in-instance-default)

(setf sicl-boot-phase2:*shared-initialize-default*
      #'shared-initialize-default)

(setf sicl-boot-phase2:*find-class*
      #'sicl-boot-phase2:find-ersatz-class)
