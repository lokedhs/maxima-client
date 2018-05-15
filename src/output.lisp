(in-package :maxima-client)

(defclass maxima-output (trivial-gray-streams:fundamental-character-output-stream)
  ((stream :initform (make-string-output-stream)
           :reader maxima-output/stream)
   (update :initform nil
           :accessor maxima-output/update)))

(defmethod trivial-gray-streams:stream-write-char ((stream maxima-output) char)
  (setf (maxima-output/update stream) t)
  (write-char char (maxima-output/stream stream)))

(defun maxima-stream-updated-p (stream)
  (maxima-output/update stream))

(defun maxima-stream-text (stream)
  (get-output-stream-string (maxima-output/stream stream)))

(defclass maxima-error ()
  ((cmd     :initarg :cmd
            :reader maxima-error/cmd)
   (content :initarg :content
            :reader maxima-error/content)))

(clim:define-presentation-method clim:present (obj (type maxima-error) stream (view t) &key)
  (clim:with-drawing-options (stream :ink clim:+red+)
    (format stream "~a" (maxima-error/content obj))))
