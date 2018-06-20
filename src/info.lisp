(in-package :maxima-client)

(clim:define-application-frame maxima-info-frame ()
  ((nodes :initform (make-hash-table :test 'equal)
          :accessor maxima-info-frame/nodes))
  (:panes (entry-list (clim:make-pane 'clim:list-pane
                                      :items nil
                                      :value-changed-callback 'selected-page-changed))
          (text-content :application))
  (:layouts (default (clim:horizontally ()
                       (clim:scrolling () entry-list)
                       text-content))))

(defun open-info-frame ()
  (let ((frame (clim:make-application-frame 'maxima-info-frame
                                            :width 600
                                            :height 800)))
    (bordeaux-threads:make-thread (lambda () (clim:run-frame-top-level frame)))
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
        (format content-pane "~a" content)))))
