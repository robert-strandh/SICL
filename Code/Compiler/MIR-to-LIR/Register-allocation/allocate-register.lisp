(cl:in-package #:sicl-register-allocation)

;;; Find the first unassigned register.  If all registers are
;;; assigned, return NIL.
(defun find-unassigned-register (arrangement candidates)
  (loop for candidate in candidates
        unless (member candidate (attributions arrangement)
                       :test #'eq :key #'register)
          return candidate
        finally (return nil)))

(defun extract-locations (arrangement candidates)
  (loop for candidate in candidates
        for attribution = (find candidate (attributions arrangement)
                                :test #'eq :key #'register)
        collect (lexical-location attribution)))

(defun extract-pool-items (pool locations)
  (loop for location in locations
        collect (find location pool :test #'eq :key #'lexical-location)))

(defun allocate-register
    (predecessor instruction pool arrangement candidates)
  (let ((unassigned (find-unassigned-register arrangement candidates)))
    (if (not (null unassigned))
        unassigned
        (let* ((locations (extract-locations arrangement candidates))
               (pool-items (extract-pool-items pool locations))
               (sorted-pool-items (sort pool-items #'> :key #'distance))
               (location (lexical-location (first sorted-pool-items)))
               (attribution (find location (attributions arrangement)
                                  :test #'eq :key #'lexical-location))
               (register (register attribution)))
          (values register
                  (if (null (stack-slot attribution))
                      (spill predecessor instruction register)
                      predecessor))))))
