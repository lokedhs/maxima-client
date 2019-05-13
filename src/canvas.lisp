(in-package :maxima-client.canvas)

(defclass canvas-view ()
  ())

(defvar +canvas-view+ (make-instance 'canvas-view))

(defclass canvas-pane (clim:application-pane)
  ())

(defun make-canvas-pane ()
  (clim:make-clim-stream-pane :type 'canvas-pane
                              :name 'canvas
                              :default-view +canvas-view+
                              :display-function 'repaint-canvas
                              :incremental-redisplay t))

(defun repaint-canvas (frame pane)
  (declare (ignore frame))
  (clim:draw-line* pane 10 10 100 50))

(maxima::defmfun maxima::$canvas_add_object (name)
  (unless (symbolp name)
    (maxima::merror "Name is not a symbol: ~M" name))
  (log:info "Adding sym: ~s" name))
