(defun nrev (v start end)
  (loop for i from start
        for j downfrom (1- end)
        repeat (floor (- end start) 2)
        do (rotatef (svref v i) (svref v j))))

(defun rotate (vector start middle end)
  (nrev vector start middle)
  (nrev vector middle end)
  (nrev vector start end))

;;; The vector has four zones.  The first zone contains elements that
;;; are in their final places.  The second zone, immediately following
;;; the first, contains a suffix of the initial first zone to be
;;; merged.  The third zone contains may contain a prefix of the
;;; initial first sequence to be merged.  The last zone contains what
;;; remains of the second sequence to be merged.

(defun my-merge (vector predicate start middle end)
  (let ((buffer (make-vector 10))
        (buffer-start 0)
        (buffer-end 0)
        (prefix-end 0)
        (zone-3-start middle)
        (zone-3-end middle)
        (zone-4-start middle))
    (tagbody
     start
       ;; The start state is when have never taken
       ;; anything from the second sequence.
       ;; Start by checking whether the first sequence
       ;; is empty
       (when (= prefix-end middle)
         (go end))
       (if (funcall predicate
                    (svref vector zone-4-start)
                    (svref vector prefix-end))
           (progn (let ((temp (svref vector zone-4-start)))
                    (setf (svref vector zone-4-start)
                          (svref vector prefix-end))
                    (setf (svref vector prefix-end) temp)
                    (incf zone-3-end)
                    (incf zone-4-start)
                    (incf prefix-end)
                    (go zone-3)))
           (progn (incf prefix-end)
                  (go start)))
     zone-3
       ;; In this state, we have things in zone 3 but nothing
       ;; in the buffer
       ;; Start by checking whether the second sequence
       ;; is empty.  If that is the case, and since the buffer
       ;; is empty, we need to rotate the contents of
       ;; zone 2 and zone 3.
       ;; But it is possible that zone 2 is empty, in which
       ;; case we are done.
       (when (= zone-4-start end)
         (unless (= prefix-end middle)
           ;; We need to rotate the elements.
           (rotate vector prefix-end middle end))
         (go end))
       (if (funcall predicate
                    (svref vector zone-4-start)
                    (svref vector zone-3-start))
           ;; We take from the beginning of zone 4, and
           ;; put the first element of zone 2 at the
           ;; end of zone 3.  We know there is room there.
           (progn (let ((temp (svref vector zone-4-start)))
                    (setf (svref vector zone-4-start)
                          (svref vector prefix-end))
                    (setf (svref vector prefix-end) temp)
                    (incf zone-3-end)
                    (incf zone-4-start)
                    (incf prefix-end)
                    (go zone-3)))
           ;; We need to take from zone 3.
           (cond ((= 1 (- zone-3-end zone-3-start))
                  ;; As a special case, when there is only
                  ;; one element in zone 3, we take advantage of the
                  ;; situation, and put the element from zone
                  ;; 2 right after zone 2.
                  (let ((temp (svref vector zone-3-start)))
                    (setf zone-3-start middle)
                    (setf zone-3-end (1+ middle))
                    (setf (svref vector zone-3-start)
                          (svref vector prefix-end))
                    (setf (svref vector prefix-end) temp)
                    (incf prefix-end))
                  (go zone-3))
                 ((< zone-3-end zone-4-start)
                  ;; There is room at the end of zone 3.
                  ;; That's where we put the first element of zone 2.
                  (setf (svref vector zone-3-end)
                        (svref vector prefix-end))
                  (incf zone-3-end)
                  (setf (svref vector prefix-end)
                        (svref vector zone-3-start))
                  (incf prefix-end)
                  (incf zone-3-start)
                  (go zone-3))
                 (t
                  ;; No room at the end of zone 3.  Use the buffer
                  (setf (svref buffer 0) (svref vector prefix-end))
                  (setf (svref vector prefix-end) (svref vector zone-3-start))
                  (incf prefix-end)
                  (incf zone-3-start)
                  (go zone-3-and-buffer))))
     buffer
       ;; In this state, we have things in the buffer, but
       ;; nothing in zone 3.
       ;; Start by checking whether the second sequence is
       ;; empty.
       (when (= zone-4-start end)
         (replace vector vector
                  :start1 (- end (- middle prefix-end))
                  :start2 prefix-end :end2 middle)
         (if (> buffer-end buffer-start)
             (replace vector buffer
                      :start1 prefix-end
                      :start2 buffer-start :end2 buffer-end)
             (progn (replace vector buffer
                             :start1 prefix-end
                             :start2 buffer-start :end2 (length buffer))
                    (replace vector buffer
                             :start1 (+ prefix-end (- (length buffer) buffer-start))
                             :start2 0 :end2 buffer-end)))
         (go end))
       (if (funcall predicate
                    (svref vector zone-4-start)
                    (svref buffer buffer-start))
           ;; We take an element from zone 4
           ;; We save the element zone 2 in the buffer if there is
           ;; room, otherwise in zone 3
           (if (or (and (= buffer-start 0) (= buffer-end (1- (length buffer))))
                   (= 1 (- buffer-start buffer-end)))
               ;; the buffer is full
               (progn (setf (svref vector zone-3-end)
                            (svref vector prefix-end))
                      (incf zone-3-end)
                      (setf (svref vector prefix-end)
                            (svref vector zone-4-start))
                      (incf zone-4-start)
                      (incf prefix-end)
                      (go buffer-and-zone-3))
               ;; The buffer is not full
               (progn (setf (svref buffer buffer-end)
                            (svref vector prefix-end))
                      (incf buffer-end)
                      (when (= buffer-end (length buffer))
                        (setf buffer-end 0))
                      (setf (svref vector prefix-end)
                            (svref vector zone-4-start))
                      (incf zone-4-start)
                      (incf prefix-end)
                      (go buffer)))
           ;; We take an element from the buffer, but
           ;; we also put another one in.
           (progn (setf (svref buffer buffer-end)
                        (svref vector prefix-end))
                  (incf buffer-end)
                  (when (= buffer-end (length buffer))
                    (setf buffer-end 0))
                  (setf (svref vector prefix-end)
                        (svref buffer buffer-start))
                  (incf buffer-start)
                  (when (= buffer-start (length buffer))
                    (setf buffer-start 0))
                  (incf prefix-end)
                  (go buffer)))
     zone-3-and-buffer
       ;; In this state, there are elemetns both in zone 3
       ;; and in the buffer.  zone-3 is first
       ;; Start by checking whether the second sequence is empty.
       (when (= zone-4-start end)
         ;; Sequence 2 is empty.
     buffer-and-zone-3
       ;; In this state, there are elemetns both in zone 3
       ;; and in the buffer.  buffer is first
     end)
