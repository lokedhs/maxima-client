(in-package :maxima-client)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; output
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; input
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass maxima-input (trivial-gray-streams:fundamental-binary-input-stream)
  ())

(defmethod trivial-gray-streams:stream-read-char ((stream maxima-input))
  (log:info "trying to read from stream"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; io
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass maxima-io (maxima-output maxima-input)
  ((clim-stream :initarg :clim-stream
                :reader maxima-io/clim-stream)
   (buffer      :initform ""
                :accessor maxima-io/buffer)
   (pos         :initform 0
                :accessor maxima-io/pos)))

(defun maxima-io-buffer-nonempty (stream)
  (< (maxima-io/pos stream) (length (maxima-io/buffer stream))))

(defun maxima-io-input-plain-text (stream)
  (clim:accept 'plain-text :stream (maxima-io/clim-stream stream) :prompt nil))

(defun read-string-and-update-stream (stream)
  (let ((s (maxima-stream-text stream)))
    (when (plusp (length s))
      (format (maxima-io/clim-stream stream) "~a" s))
    (let* ((result (maxima-io-input-plain-text stream))
           (updated (format nil "~a~%" result)))
      (log:trace "got result: ~s" result)
      (setf (maxima-io/buffer stream) updated)
      (setf (maxima-io/pos stream) 0))))

(defmethod trivial-gray-streams:stream-read-char ((stream maxima-io))
  (unless (maxima-io-buffer-nonempty stream)
    (read-string-and-update-stream stream))
  (let ((pos (maxima-io/pos stream)))
    (incf (maxima-io/pos stream))
    (let ((result (aref (maxima-io/buffer stream) pos)))
      (log:trace "result from read-char: ~s" result)
      result)))

(defmethod trivial-gray-streams:stream-read-char-no-hang ((stream maxima-io))
  (trivial-gray-streams:stream-read-char stream))

(defmethod trivial-gray-streams:stream-peek-char ((stream maxima-io))
  (let ((ch (if (maxima-io-buffer-nonempty stream)
                (aref (maxima-io/buffer stream) (maxima-io/pos stream))
                nil)))
    (log:trace "peeking char, result: ~s" ch)
    ch))

(defmethod trivial-gray-streams:stream-read-line ((stream maxima-io))
  (let ((result (with-output-to-string (s)
                  (loop
                    for ch = (trivial-gray-streams:stream-read-char stream)
                    until (eql ch #\Newline)
                    do (princ ch s)))))
    (log:trace "result from stream-read-line: ~s" result)
    result))
