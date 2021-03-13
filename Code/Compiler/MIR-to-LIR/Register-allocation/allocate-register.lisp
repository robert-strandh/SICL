(cl:in-package #:sicl-register-allocation)

;;; Find the first unassigned register.  If all registers are
;;; assigned, return NIL.
(defun find-unassigned-register (arrangement candidates)
  (loop for candidate in candidates
        unless (member candidate (attributions arrangement)
                       :test #'eq :key #'register)
          return candiate
        finally (return nil)))

(defun extract-locations (arrangement candidates)
  (loop for candidate in candidates
        for attribution = (find candidate (attributions arrangement)
                                :test #'eq :key #'register)
        collect (lexical-location attribution)))
