(in-package :maxima-client.notes)

(clim:define-command-table notes-commands)

(defclass notes-pane (clim:vrack-pane)
  ((edit-pane :initarg :edit-pane
              :reader notes-pane/edit-pane)))

(defmethod initialize-instance :after ((obj notes-pane) &key)
  (let ((drei (clim:make-pane 'drei:drei-gadget-pane)))
    (setf (slot-value obj 'edit-pane) drei)
    (clim:sheet-adopt-child obj drei)))

(defun focus-notes-pane (pane)
  (let ((drei (notes-pane/edit-pane pane)))
    (setf (clim:port-keyboard-input-focus (clim:port drei)) drei)))

(defun insert-maxima-expr (notes expr)
  (drei:with-bound-drei-special-variables ((notes-pane/edit-pane notes))
    (let ((point (drei:point)))
      (drei-buffer:insert-object point expr))))

(clim:define-presentation-method clim:present (obj (type maxima-native-expr) stream (view drei:drei-view) &key)
  (let* ((expr (maxima-native-expr/expr obj))
         (output-record (maxima-client::make-expression-output-record stream expr)))
    (clim:with-room-for-graphics (stream)
      (clim:with-identity-transformation (stream)
        (clim:stream-add-output-record stream output-record)))))
