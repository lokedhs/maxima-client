(in-package :maxima-client.workbench)

(clim:define-command-table workbench-commands)

(defclass workbench-pane (clim:hrack-pane)
  ((root-pane   :initarg :root-pane
                :reader workbench-pane/root-pane)
   (vrack-pane  :initarg :vrack-pane
                :reader workbench-pane/vrack-pane)
   (top-panel   :initform nil
                :accessor workbench-pane/top-panel)
   (right-panel :initform nil
                :accessor workbench-pane/right-panel))
  (:documentation "A workbench manages a set of subpanels that may be displayed on the sides of the root pane."))

(defun make-workbench (root-pane)
  (let ((inner (clim:make-pane 'clim:vrack-pane
                               :name 'workbench-vrack
                               :contents (list root-pane))))
    (clim:make-pane 'workbench-pane
                    :name 'workbench-pane
                    :root-pane root-pane
                    :vrack-pane inner
                    :contents (list inner))))

(defun add-top-pane (workbench-pane outer inner)
  (when (workbench-pane/top-panel workbench-pane)
    (error "Top panel already assigned"))
  (setf (workbench-pane/top-panel workbench-pane) (list outer inner)))

(defun show-top-pane (workbench-pane show-p)
  (when (workbench-pane/top-panel workbench-pane)
    (let* ((root-pane (workbench-pane/root-pane workbench-pane))
           (vrack-pane (workbench-pane/vrack-pane workbench-pane))
           (children (copy-seq (clim:sheet-children vrack-pane)))
           (length (length children)))
      (cond ((and show-p (= length 1))
             (let ((pane (first (workbench-pane/top-panel workbench-pane)))
                   (adjuster (clim:make-pane 'clime:box-adjuster-gadget)))
               (clim:sheet-adopt-child vrack-pane adjuster)
               (clim:sheet-adopt-child vrack-pane pane)
               (clim:reorder-sheets vrack-pane (list root-pane adjuster pane))))
            ((and (not show-p) (= length 3))
             (clim:sheet-disown-child vrack-pane (second children))
             (clim:sheet-disown-child vrack-pane (third children)))))
    (clim:redisplay-frame-panes (clim:pane-frame workbench-pane))))

(defun add-right-pane (workbench-pane outer inner)
  (when (workbench-pane/right-panel workbench-pane)
    (error "Right panel already assigned"))
  (setf (workbench-pane/right-panel workbench-pane) (list outer inner)))

(defun show-right-pane (workbench-pane show-p)
  (when (workbench-pane/right-panel workbench-pane)
    (let* ((children (copy-seq (clim:sheet-children workbench-pane)))
           (length (length children)))
      (cond ((and show-p (= length 1))
             (let ((pane (first (workbench-pane/right-panel workbench-pane)))
                   (adjuster (clim:make-pane 'clime:box-adjuster-gadget)))
               (clim:sheet-adopt-child workbench-pane adjuster)
               (clim:sheet-adopt-child workbench-pane pane)))
            ((and (not show-p) (= length 3))
             (clim:sheet-disown-child workbench-pane (first children))
             (clim:sheet-disown-child workbench-pane (second children)))))))

(defun pane-visible-p (workbench-pane pane)
  (declare (ignore workbench-pane))
  (clim:sheet-viewable-p pane))
