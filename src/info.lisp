(in-package :maxima-client)

(clim:define-application-frame maxima-info-frame ()
  ((nodes                 :initform (make-hash-table :test 'equal)
                          :accessor maxima-info-frame/nodes)
   (frame-ready           :initform nil
                          :accessor maxima-info-frame/frame-ready)
   (frame-ready-lock      :initform (bordeaux-threads:make-lock)
                          :reader maxima-info-frame/frame-ready-lock)
   (frame-ready-condition :initform (bordeaux-threads:make-condition-variable)
                          :reader maxima-info-frame/frame-ready-condition))
  (:panes (entry-list (clim:make-pane 'clim:list-pane
                                      :items nil
                                      :value-changed-callback 'selected-page-changed))
          (text-content :application
                        :display-function 'redraw-info-frame-content))
  (:layouts (default (clim:horizontally ()
                       (2/10 (clim:scrolling () entry-list))
                       (8/10 text-content)))))

(defun redraw-info-frame-content (frame stream)
  (declare (ignore stream))
  ;; By the time the redraw function is called, the info frame should
  ;; be ready to accept requests
  (bordeaux-threads:with-lock-held ((maxima-info-frame/frame-ready-lock frame))
    (unless (maxima-info-frame/frame-ready frame)
      (setf (maxima-info-frame/frame-ready frame) t)
      (bordeaux-threads:condition-notify (maxima-info-frame/frame-ready-condition frame)))))

(defun open-info-frame ()
  (let ((frame (clim:make-application-frame 'maxima-info-frame
                                            :width 800
                                            :height 800)))
    (bordeaux-threads:make-thread (lambda () (clim:run-frame-top-level frame)))
    ;; Wait until the frame is ready to accept requests before returning
    (bordeaux-threads:with-lock-held ((maxima-info-frame/frame-ready-lock frame))
      (loop
        until (maxima-info-frame/frame-ready frame)
        do (bordeaux-threads:condition-wait (maxima-info-frame/frame-ready-condition frame)
                                            (maxima-info-frame/frame-ready-lock frame))))
    frame))

(defun selected-page-changed (pane value)
  (let* ((frame (clim:pane-frame pane))
         (text-content (clim:find-pane-named frame 'text-content)))
    (display-info-page text-content value)))

(defun add-info-page (frame name)
  (let ((name-list (clim:find-pane-named frame 'entry-list))
        (content (with-output-to-string (s)
                   (let ((*standard-output* s))
                     (unless (cl-info::info-exact name)
                       (return-from add-info-page nil))))))
    (let ((prev-content (gethash name (maxima-info-frame/nodes frame))))
      (setf (gethash name (maxima-info-frame/nodes frame)) content)
      (unless prev-content
        (push name (clime:list-pane-items name-list)))
      (setf (clim:gadget-value name-list) name)
      (display-info-page (clim:find-pane-named frame 'text-content) name))))

(defun display-info-page (content-pane value)
  (let ((frame (clim:pane-frame content-pane)))
    (clim:window-clear content-pane)
    (let ((content (gethash value (maxima-info-frame/nodes frame))))
      (when content
        (clim:with-text-style (content-pane (clim:make-text-style :fix :roman 12))
          (format content-pane "~a" content))))))
