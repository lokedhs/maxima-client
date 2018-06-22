(in-package :maxima-client.markup)

#+sbcl
(defun split-word (string)
  (sb-unicode:words string))

#-sbcl
(defun split-word (string)
  (when (plusp (length string))
    (loop
      with start = 0
      with last-char-space-p = nil
      with result = nil
      for i from 0 below (length string)
      for ch = (aref string i)
      for space-p = (cl-unicode:has-property ch "White_Space")
      when (or space-p last-char-space-p)
        do (progn
             (when (< start i)
              (push (subseq string start i) result))
             (setq start i)
             (setq last-char-space-p space-p))
      finally (progn
                (push (subseq string start) result)
                (return (reverse result))))))

(defun present-text-with-wordwrap (stream text)
  (let ((words (split-word text)))
    (loop
      with pane-width = (- (clim:rectangle-width (clim:pane-viewport-region stream)) 10)
      for word in words
      unless (equal word #.(princ-to-string #\Newline))
        do (let ((x (clim:cursor-position (clim:stream-text-cursor stream))))
             (when (> (+ x (clim:stream-string-width stream word)) pane-width)
               (format stream "~%"))
             (format stream "~a" word)))))

(defun present-multiline-with-wordwrap (stream text)
  (loop
    with start = nil
    for i from 0 below (length text)
    if (eql (aref text i) #\Newline)
      do (progn
           (when (and start (> i start))
             (present-text-with-wordwrap stream (subseq text start i))
             (setq start nil))
           (format stream "~%"))
    else
      do (progn
           (when (null start)
             (setq start i)))
    finally (when start
              (present-text-with-wordwrap stream (subseq text start)))))

(defclass word-wrap-stream (clim:standard-encapsulating-stream clim:basic-pane)
  ((buf :type (array character (*))
        :initform (make-array 1024 :element-type 'character :adjustable t :fill-pointer 0)
        :accessor word-wrap-stream/buf)))

(defun %finalise-word-wrap-string (stream)
  (let ((buf (word-wrap-stream/buf stream)))
    (format *debug-io* "Wordwrapping: ~s~%" buf)
    (present-multiline-with-wordwrap (clim:encapsulating-stream-stream stream) buf)
    (setf (fill-pointer buf) 0)))

(defun %word-wrap-push-char (stream char)
  (vector-push-extend char (word-wrap-stream/buf stream)))

(defun %word-wrap-push-string (stream string start end)
  (let ((s (or start 0))
        (e (or end (length string))))
    (unless (<= s e)
      (error "Start (~a) is greater than end (~a)" start end))
    (loop
      for i from s below e
      do (%word-wrap-push-char stream (aref string i)))))

(defmethod trivial-gray-streams:stream-write-char ((stream word-wrap-stream) char)
  (%word-wrap-push-char stream char))

(defmethod trivial-gray-streams:stream-write-byte ((stream word-wrap-stream) char)
  (error "Can't write binary data to word-wrap streams"))

(defmethod trivial-gray-streams:stream-write-string ((stream word-wrap-stream) string &optional start end)
  (%word-wrap-push-string stream string start end))

(defmethod trivial-gray-streams:stream-write-sequence ((stream word-wrap-stream) string start end &key &allow-other-keys)
  (declare (ignore start end))
  (error "Can't write binary data to word-wrap streams"))

(defmethod trivial-gray-streams:stream-finish-output ((stream word-wrap-stream))
  (%finalise-word-wrap-string stream)
  (call-next-method))

(defmethod trivial-gray-streams:stream-terpri ((stream word-wrap-stream))
  (%word-wrap-push-char stream #\Newline))

(defmethod trivial-gray-streams:stream-line-column ((stream word-wrap-stream))
  (%finalise-word-wrap-string stream)
  (call-next-method))

(defmethod trivial-gray-streams:stream-fresh-line ((stream word-wrap-stream))
  (%finalise-word-wrap-string stream)
  (call-next-method))

(defmethod clim-internals::invoke-with-sheet-medium-bound (continuation medium (sheet word-wrap-stream))
  (%finalise-word-wrap-string sheet)
  (clim-internals::invoke-with-sheet-medium-bound continuation medium (clim:encapsulating-stream-stream sheet)))

(defmacro with-word-wrap ((stream) &body body)
  (let ((stream-sym (gensym))
        (wrapped-stream-sym (gensym)))
    `(let ((,stream-sym ,stream))
       (let ((,wrapped-stream-sym (make-instance 'word-wrap-stream :stream ,stream-sym)))
         (let ((,stream ,wrapped-stream-sym))
           (progn ,@body)
           (%finalise-word-wrap-string ,wrapped-stream-sym))))))
