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
  (let ((main (clim:make-pane 'workbench-main :root-pane root-pane :parent ws)))
    (setf (workbench-pane/content ws) main)
    (clim:sheet-adopt-child ws main))
  (let ((right-panel (clim:make-pane 'workbench-side-panel :display-function 'display-side-panel :parent ws)))
    (setf (workbench-pane/right-panel ws) right-panel)
    (clim:sheet-adopt-child ws right-panel)
    (dolist (panel-descriptor panels)
      (destructuring-bind (panel &key title image select-fn close-fn)
          panel-descriptor
        (add-drawer-panel ws panel :title title :image image :select-fn select-fn :close-fn close-fn)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; workbench-main
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass workbench-main (clim:bboard-pane)
  ((root-pane :accessor workbench-main/root-pane)
   (workbench-panel :initarg :parent
                    :reader workbench-main/parent)))

(defmethod initialize-instance :after ((panel workbench-main) &key root-pane)
  (when root-pane
    (setf (workbench-main/root-pane panel) root-pane)
    (clim:sheet-adopt-child panel root-pane)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; workbench-side-panel
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *side-panel-icon-width* 32)
(defparameter *side-panel-margin* 5)

(defclass workbench-side-panel (clim:application-pane)
  ((workbench-panel :initarg :parent
                    :reader workbench-side-panel/parent)
   (panels          :initform nil
                    :accessor workbench-side-panel/panels)))

(defmethod initialize-instance :after ((panel workbench-side-panel) &key)
  (let ((width (+ *side-panel-icon-width* (* 2 *side-panel-margin*))))
    (clim:change-space-requirements panel :width width :min-width width :max-width width)))

(defun add-drawer-panel (ws panel &key title image select-fn close-fn)
  (let ((side-panel (workbench-pane/right-panel ws))
        (root-panel (workbench-pane/content ws))
        (p (clim:make-pane 'drawer-panel
                           :panel panel
                           :title title
                           :image image
                           :workbench ws
                           :select-fn select-fn
                           :close-fn close-fn)))
    (setf (workbench-side-panel/panels side-panel)
          (append (workbench-side-panel/panels side-panel)
                  (list p)))
    (clim:sheet-adopt-child root-panel p)
    (log:info "initially burying sheet: ~s" p)
    (clim:bury-sheet p)
    (setf (clim:sheet-enabled-p p) nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; drawer-panel
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass drawer-panel (clim:spacing-pane)
  ((title     :initarg :title
              :reader drawer-panel/title)
   (image     :initarg :image
              :reader drawer-panel/image)
   (workbench :initarg :workbench
              :reader drawer-panel/workbench)
   (select-fn :initarg :select-fn
              :initform nil
              :reader drawer-panel/select-fn)
   (close-fn  :initarg :close-fn
              :initform nil
              :reader drawer-panel/close-fn)))

(defmethod initialize-instance :after ((obj drawer-panel) &key panel)
  (clim:sheet-adopt-child obj panel))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; panel-button
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass panel-button ()
  ((title :initarg :title
          :reader panel-button/title)
   (image :initarg :image
          :reader panel-button/image)
   (panel :initarg :panel
          :reader panel-button/pane)))

(clim:define-presentation-method clim:present (obj (type panel-button) stream (view t) &key)
  (clim:draw-design stream (panel-button/image obj)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; display code
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun display-side-panel (frame pane)
  (declare (ignore frame))
  (loop
    for panel in (workbench-side-panel/panels pane)
    for button = (make-instance 'panel-button :title "Foo" :image (drawer-panel/image panel) :panel panel)
    for first = t then nil
    unless first
      do (format pane "~%")
    do (clim:with-room-for-graphics (pane :first-quadrant nil)
         (clim:surrounding-output-with-border (pane :padding *side-panel-margin* :ink clim:+transparent-ink+)
           (clim:present button (clim:presentation-type-of button) :stream pane)))))

(defmethod clim:note-sheet-region-changed :after ((sheet workbench-main))
  (alexandria:when-let ((root (workbench-main/root-pane sheet)))
    (fullscreen sheet root))
  (dolist (panel (workbench-side-panel/panels (workbench-pane/right-panel (workbench-main/parent sheet))))
    (update-side-panel-position panel)))

(defmethod fullscreen (parent-pane pane)
  (multiple-value-bind (width height)
      (clim:rectangle-size (clim:sheet-region parent-pane))
    (clim:allocate-space pane width height)))

(defun make-workbench (&key name root-pane panels)
  (let ((ws (clim:make-pane 'workbench-pane :name name :root-pane root-pane :panels panels)))
    (fullscreen ws root-pane)
    ws))

(defun update-side-panel-position (panel)
  (when (clim:sheet-enabled-p panel)
    (let* ((ws (drawer-panel/workbench panel))
           (ws-content (workbench-pane/content ws)))
      (multiple-value-bind (width height)
          (clim:rectangle-size (clim:sheet-region ws-content))
        (clim:move-sheet panel (/ width 2) 0)
        (clim:allocate-space panel (/ width 2) height)))))

(defun set-drawer-panel-active (panel)
  (let ((enable (not (clim:sheet-enabled-p panel))))
    (setf (clim:sheet-enabled-p panel) enable)
    (cond (enable
           (update-side-panel-position panel)
           (alexandria:when-let ((fn (drawer-panel/select-fn panel)))
             (funcall fn (first (clim:sheet-children panel)))))
          (t
           (alexandria:when-let ((fn (drawer-panel/close-fn panel)))
             (funcall fn (first (clim:sheet-children panel))))))))

(clim:define-presentation-to-command-translator select-drawer-panel
    (panel-button select-drawer-panel-command workbench-commands)
    (obj)
  (list (panel-button/pane obj)))

(clim:define-command (select-drawer-panel-command :name "Select panel" :menu t :command-table workbench-commands)
    ((panel drawer-panel :prompt "Panel"))
  (set-drawer-panel-active panel))
