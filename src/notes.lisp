(in-package :maxima-client.notes)

(defclass notes-pane (clim:vrack-pane)
  ())

(defmethod initialize-instance :after ((obj notes-pane) &key)
  (let ((drei (clim:make-pane 'drei:drei-gadget-pane)))
    (clim:sheet-adopt-child obj drei)))
