(in-package :maxima-client.workbench)

(clim:define-command-table workbench-commands)

(defclass workbench-pane (clim:vrack-pane)
  ((root-pane :initarg :root-pane
              :reader workbench-pane/root-pane)))

(defun make-workbench (root-pane)
  (clim:make-pane 'workbench-pane
                  :name 'workbench-pane
                  :root-pane root-pane
                  :contents (list root-pane)))

(defun add-top-pane (workbench-pane pane)
  (let ((root-pane (workbench-pane/root-pane workbench-pane)))
    (let ((adjuster (clim:make-pane 'clime:box-adjuster-gadget)))
      (clim:sheet-adopt-child workbench-pane adjuster)
      (clim:sheet-adopt-child workbench-pane pane)
      (clim:reorder-sheets workbench-pane (list root-pane adjuster pane))
      (clim:change-space-requirements workbench-pane)
      (clim:change-space-requirements root-pane))))
