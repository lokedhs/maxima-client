(in-package :maxima-client.workbench)

(clim:define-command-table workbench-commands)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; workbench-pane
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass workbench-pane (clim:hrack-pane)
  ((right-panel :accessor workbench-pane/right-panel)
   (content :initform nil
            :accessor workbench-pane/content)))

(defmethod initialize-instance :after ((ws workbench-pane) &key root-pane panels)
  (let ((main (clim:make-pane 'workbench-main :root-pane root-pane)))
    (setf (workbench-pane/content ws) main)
    (clim:sheet-adopt-child ws main))
  (let ((right-panel (clim:make-pane 'workbench-side-panel :display-function 'display-side-panel :parent ws)))
    (setf (workbench-pane/right-panel ws) right-panel)
    (clim:sheet-adopt-child ws right-panel)
    (dolist (p panels)
      (add-drawer-panel ws p))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; workbench-main
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass workbench-main (clim:bboard-pane)
  ((root-pane :accessor workbench-main/root-pane)))

(defmethod initialize-instance :after ((panel workbench-main) &key root-pane)
  (when root-pane
    (setf (workbench-main/root-pane panel) root-pane)
    (clim:sheet-adopt-child panel root-pane)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; workbench-side-panel
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass workbench-side-panel (clim:application-pane)
  ((workbench-panel :initarg :parent
                    :reader workbench-side-panel/parent)
   (panels          :initform nil
                    :accessor workbench-side-panel/panels)))

(defmethod initialize-instance :after ((panel workbench-side-panel) &key)
  (clim:change-space-requirements panel :width 50 :min-width 50 :max-width 50))

(defun add-drawer-panel (ws panel)
  (let ((side-panel (workbench-pane/right-panel ws))
        (root-panel (workbench-pane/content ws))
        (p (clim:make-pane 'drawer-panel :panel panel)))
    (setf (workbench-side-panel/panels side-panel)
          (append (workbench-side-panel/panels side-panel)
                  (list p)))
    (clim:sheet-adopt-child root-panel p)
    (log:info "initially burying sheet: ~s" p)
    (clim:bury-sheet p)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; drawer-panel
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass drawer-panel (clim:spacing-pane)
  ())

(defmethod initialize-instance :after ((obj drawer-panel) &key panel)
  (clim:sheet-adopt-child obj panel))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; panel-button
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass panel-button ()
  ((title :initarg :title
          :reader panel-button/title)
   (panel :initarg :panel
          :reader panel-button/pane)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; display code
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(clim:define-presentation-method clim:present (obj (type panel-button) stream (view t) &key)
  (format stream "~a" (panel-button/title obj)))

(clim:define-presentation-to-command-translator select-drawer-panel
    (panel-button select-drawer-panel-command workbench-commands)
    (obj)
  (list (panel-button/pane obj)))

(defun display-side-panel (frame pane)
  (declare (ignore frame))
  (loop
    for panel in (workbench-side-panel/panels pane)
    for button = (make-instance 'panel-button :title "Foo" :panel panel)
    for first = t then nil
    unless first
      do (format pane "~%")
    do (clim:present button (clim:presentation-type-of button) :stream pane)))

(defmethod clim:note-sheet-region-changed :after ((sheet workbench-main))
  (alexandria:when-let ((root (workbench-main/root-pane sheet)))
    (fullscreen sheet root)))

(defmethod fullscreen (parent-pane pane)
  (multiple-value-bind (width height)
      (clim:rectangle-size (clim:sheet-region parent-pane))
    (clim:allocate-space pane width height)))

(defun make-workbench (&key name root-pane panels)
  (let ((ws (clim:make-pane 'workbench-pane :name name :root-pane root-pane :panels panels)))
    (fullscreen ws root-pane)
    ws))

(clim:define-command (select-drawer-panel-command :name "Select panel" :menu t :command-table workbench-commands)
    ((panel drawer-panel :prompt "Panel"))
  (log:info "Displaying sheet: ~s" panel))
