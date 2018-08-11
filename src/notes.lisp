(in-package :maxima-client.notes)

(clim:define-command-table notes-commands)

(drei-syntax:define-syntax-command-table notes-table
  :errorp nil)

(drei-syntax:define-syntax notes-syntax (drei-fundamental-syntax:fundamental-syntax)
  ()
  (:name "Maxima notes")
  (:pathname-types "org")
  (:command-table notes-table))

(defclass notes-pane (clim:vrack-pane)
  ((edit-pane :initarg :edit-pane
              :reader notes-pane/edit-pane)))

(defmacro with-notes-active ((notes) &body body)
  `(drei:with-bound-drei-special-variables ((notes-pane/edit-pane ,notes))
     ,@body))

(defmethod initialize-instance :after ((obj notes-pane) &key)
  (let ((drei (clim:make-pane 'drei:drei-gadget-pane)))
    (setf (slot-value obj 'edit-pane) drei)
    (clim:sheet-adopt-child obj drei)
    (drei:with-bound-drei-special-variables (drei)
      (drei-core:set-syntax (drei:current-view) "Maxima notes"))))

(defun focus-notes-pane (pane)
  (let ((drei (notes-pane/edit-pane pane)))
    (setf (clim:port-keyboard-input-focus (clim:port drei)) drei)))

(defun insert-maxima-expr (notes expr)
  (with-notes-active (notes)
    (let ((point (drei:point)))
      (drei-buffer:insert-object point expr))))

(clim:define-presentation-method clim:present (obj (type maxima-native-expr) stream (view drei:drei-view) &key)
  (let* ((expr (maxima-native-expr/expr obj))
         (output-record (maxima-client::make-expression-output-record stream expr)))
    (clim:with-room-for-graphics (stream)
      (clim:with-identity-transformation (stream)
        (clim:stream-add-output-record stream output-record)))))
