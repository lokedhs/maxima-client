(in-package :maxima-client)

(clim:define-command-table maxima-input-editor-table :inherit-from (drei::exclusive-input-editor-table))

(defclass history-mixin ()
  ((history      :initform (make-array 10 :initial-element nil :adjustable t :fill-pointer 0)
                 :reader history-mixin/history)
   (history-pos  :initform 0
                 :accessor history-mixin/history-pos)))

(defun add-maxima-input-editor-command (gestures function)
  (drei::set-key `(,(lambda (numeric-argument)
                      (funcall function drei::*drei-input-editing-stream*
                               (drei::stream-input-buffer drei::*drei-input-editing-stream*)
                               gestures
                               numeric-argument))
                   ,drei::*numeric-argument-marker*)
                 'maxima-input-editor-table
                 gestures))

(defun maxima-history-yank-next (stream input-buffer gesture numeric-argument)
  (declare (ignorable stream input-buffer gesture numeric-argument))
  (with-accessors ((history history-mixin/history)
                   (history-pos history-mixin/history-pos))
      clim:*application-frame*
    (when (< history-pos (length history))
      (incf history-pos)
      (let ((string (if (< history-pos (length history))
                        (aref history history-pos)
                        "")))
        (clim:presentation-replace-input stream string 'plain-text (clim:stream-default-view stream))))))

(defun maxima-history-yank-previous (stream input-buffer gesture numeric-argument)
  (declare (ignorable stream input-buffer gesture numeric-argument))
  (with-accessors ((history history-mixin/history)
                   (history-pos history-mixin/history-pos))
      clim:*application-frame*
    (when (plusp history-pos)
      (decf history-pos)
      (let ((string (aref history history-pos)))
        (clim:presentation-replace-input stream string 'plain-text (clim:stream-default-view stream))))))

(defun maxima-history-yank-this (stream input-buffer gesture numeric-argument)
  (declare (ignorable stream input-buffer gesture numeric-argument))
  (with-accessors ((history history-mixin/history)
                   (history-pos history-mixin/history-pos))
      clim:*application-frame*
    (let ((string (aref history history-pos)))
      (clim:presentation-replace-input stream string 'plain-text (clim:stream-default-view stream)))))

(add-maxima-input-editor-command '((#\n :meta)) 'maxima-history-yank-next)
(add-maxima-input-editor-command '((#\p :meta)) 'maxima-history-yank-previous)
(add-maxima-input-editor-command '((#\r :meta)) 'maxima-history-yank-this)

(defclass maxima-interactor-command-table (drei::drei-command-table)
  ())

(defmethod drei-syntax:additional-command-tables append ((drei drei:drei-area) (table maxima-interactor-command-table))
  '(maxima-input-editor-table))

(defclass maxima-interactor-editing-stream (clim:standard-input-editing-stream)
  ())

(defmethod climi::invoke-with-input-editing ((stream maxima-interactor-pane) continuation input-sensitizer initial-contents class)
  (call-next-method stream continuation input-sensitizer initial-contents 'maxima-interactor-editing-stream))

(defmethod initialize-instance :after ((obj maxima-interactor-editing-stream) &key)
  (let ((drei-area (drei:drei-instance obj)))
    (setf (slot-value drei-area 'drei::%command-table)
          (make-instance 'maxima-interactor-command-table :name 'drei-dispatching-table))))

(defun push-command-to-history (stream command)
  (let ((history (history-mixin/history stream)))
    (unless (and (plusp (length history))
                 (equal (aref history (1- (length history))) command))
      (vector-push-extend command history))
    (setf (history-mixin/history-pos stream) (length history))))
