(in-package :maxima-client.doc)

(defclass info-content-panel (clim:application-pane)
  ((text-content :initform ""
                 :accessor info-content-panel/content)))

(defun make-info-panel ()
  (clim:vertically ()
    (clim:horizontally ()
      (clim:make-pane 'clim:label-pane :label "Keyword:")
      (clim:make-pane 'clim:text-field-pane :name 'search-field
                                            :activate-callback 'search-updated))
    (2/10 (clim:scrolling ()
            (clim:make-pane 'clim:list-pane
                            :items nil
                            :name 'entry-list
                            :name-key (lambda (v) (first (first (second v))))
                            :value-changed-callback 'entry-list-selection)))
    (8/10 (clim:scrolling ()
            (clim:make-pane 'info-content-panel :name 'text-content :display-function 'redraw-info-frame-content)))))

(defun redraw-info-frame-content (frame info-content-panel)
  (declare (ignore frame))
  (clim:with-text-style (info-content-panel (clim:make-text-style :fix :roman nil))
    (format info-content-panel "~a" (info-content-panel/content info-content-panel))))

(defun add-info-page (name)
  (log:info "adding info page for symbol: ~s" name))

(defun search-updated (pane)
  (let* ((entry-list (clim:find-pane-named (clim:pane-frame pane) 'entry-list))
         (value (clim:gadget-value pane))
         (matches (loop
                    for (path entries) in (cl-info::inexact-topic-match value)
                    append (loop
                             for v in entries
                             collect (list path (list v))))))
    (setf (climb::list-pane-items entry-list :invoke-callback nil)
          matches)))

(defun entry-list-selection (pane value)
  (let* ((frame (clim:pane-frame pane))
         (info-content-panel (clim:find-pane-named frame 'text-content))
         (result (with-output-to-string (*standard-output*)
                   (cl-info::display-items (list value)))))
    (setf (info-content-panel/content info-content-panel) result)
    (clim:redisplay-frame-pane frame info-content-panel)))
