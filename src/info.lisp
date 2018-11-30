(in-package :maxima-client.doc)

(defclass info-content-panel-old (clim:application-pane)
  ((text-content :initform ""
                 :accessor info-content-panel-old/content)))

(defun make-info-panel ()
  (clim:vertically ()
    (clim:horizontally ()
      (clim:make-pane 'clim:label-pane :label "Keyword:")
      (clim:make-pane 'clim:text-field-pane :name 'search-field
                                            :activate-callback 'search-updated
                                            :value-changed-callback 'search-field-value-change))
    (2/10 (clim:scrolling ()
            (clim:make-pane 'clim:list-pane
                            :items nil
                            :name 'entry-list
                            :name-key (lambda (v) (first (first (second v))))
                            :value-changed-callback 'entry-list-selection)))
    (8/10 (clim:scrolling ()
            (clim:make-pane 'info-content-panel-old :name 'info-content-old :display-function 'redraw-info-frame-content)))))

(defun redraw-info-frame-content (frame info-content-panel-old)
  (declare (ignore frame))
  (clim:with-text-style (info-content-panel-old (clim:make-text-style :fix :roman nil))
    (format info-content-panel-old "~a" (info-content-panel-old/content info-content-panel-old))))

(defun rearrange-matches-list (matches)
  (loop
    for (path entries) in matches
    append (loop
             for v in entries
             collect (list path (list v)))))

(defun add-info-page (name)
  (let ((matches (rearrange-matches-list (cl-info::exact-topic-match name))))
    (when matches
      (if (alexandria:sequence-of-length-p matches 1)
          (update-content-panel clim:*application-frame* (car matches))
          (error "More than one result")))))

(defun search-updated (pane)
  (let* ((entry-list (clim:find-pane-named (clim:pane-frame pane) 'entry-list))
         (value (clim:gadget-value pane)))
    (when (plusp (length value))
      (let ((matches (rearrange-matches-list (cl-info::inexact-topic-match value))))
        (setf (climb::list-pane-items entry-list :invoke-callback nil)
              matches)))))

(defun search-field-value-change (pane value)
  (declare (ignore value))
  (search-updated pane))

(defun update-content-panel (frame value)
  (let ((info-content-panel-old (clim:find-pane-named frame 'info-content-old))
        (result (with-output-to-string (*standard-output*)
                  (cl-info::display-items (list value)))))
    (setf (info-content-panel-old/content info-content-panel-old) result)
    (clim:redisplay-frame-pane frame info-content-panel-old)))

(defun entry-list-selection (pane value)
  (update-content-panel (clim:pane-frame pane) value))
