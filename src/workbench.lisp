(in-package :maxima-client.workbench)

(clim:define-command-table workbench-commands)

(defclass workbench-pane (clim:hrack-pane)
  ((root-pane  :initarg :root-pane
               :reader workbench-pane/root-pane)
   (vrack-pane :initarg :vrack-pane
               :reader workbench-pane/vrack-pane)))

(defun make-workbench (root-pane)
  (let ((inner (clim:make-pane 'clim:vrack-pane
                               :name 'workbench-vrack
                               :contents (list root-pane))))
    (clim:make-pane 'workbench-pane
                    :name 'workbench-pane
                    :root-pane root-pane
                    :vrack-pane inner
                    :contents (list inner))))

(defun add-top-pane (workbench-pane pane)
  (let ((root-pane (workbench-pane/root-pane workbench-pane))
        (vrack-pane (workbench-pane/vrack-pane workbench-pane)))
    (let ((adjuster (clim:make-pane 'clime:box-adjuster-gadget)))
      (clim:sheet-adopt-child vrack-pane adjuster)
      (clim:sheet-adopt-child vrack-pane pane)
      (clim:reorder-sheets vrack-pane (list root-pane adjuster pane)))))

(defun add-right-pane (workbench-pane pane)
  (let ((vrack-pane (workbench-pane/vrack-pane workbench-pane))
        (adjuster (clim:make-pane 'clime:box-adjuster-gadget)))
    (clim:sheet-adopt-child workbench-pane adjuster)
    (clim:sheet-adopt-child workbench-pane pane)))
