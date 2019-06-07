(in-package :maxima-client)

(defclass watcher-pane (clim:vrack-pane)
  ((table-pane :initarg :table-pane
               :reader watcher-pane/table-pane)))

(defun make-watcher-pane ()
  (let* ((style (clim:make-text-style nil :bold nil))
         (table-pane (clim:make-pane 'clim:table-pane
                                     :contents (list (list (clim:labelling (:label "Variable" :text-style style))
                                                           (clim:labelling (:label "Value" :text-style style))))))
         (content (clim:vertically ()
                    table-pane)))
    (clim:make-pane 'watcher-pane :table-pane table-pane :contents (list content))))
