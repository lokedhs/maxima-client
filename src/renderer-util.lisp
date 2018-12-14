(in-package :maxima-client)

(defvar *aligned-rendering-pos*)
(defvar *aligned-rendering-stream*)
(defvar *font-size*)

(clim:define-presentation-type plain-text ()
  :inherit-from 'string)

(clim:define-presentation-type maxima-native-expr
    ()
  :inherit-from t)

(clim:define-presentation-type maxima-native-symbol
    ()
  :inherit-from t)

(defun %aligned-render-and-move (stream pos fn)
  (let ((output-record (clim:with-output-to-output-record (stream)
                         (funcall fn))))
    (move-rec output-record pos 0)
    (clim:stream-add-output-record stream output-record)
    (dimension-bind (output-record :right right)
      right)))

(defmacro with-aligned-rendering ((stream) &body body)
  `(let ((*aligned-rendering-pos* 0)
         (*aligned-rendering-stream* ,stream))
     ,@body))

(defmacro render-aligned (() &body body)
  `(setf *aligned-rendering-pos* (%aligned-render-and-move *aligned-rendering-stream* *aligned-rendering-pos*
                                                           (lambda () ,@body))))

(defun aligned-spacing (spacing)
  (incf *aligned-rendering-pos* (* (char-width *aligned-rendering-stream*) spacing)))

(defun render-aligned-string (fmt &rest args)
  (render-aligned ()
    (clim:draw-text* *aligned-rendering-stream* (apply #'format nil fmt args) 0 0)))

(defun render-formatted (stream fmt &rest args)
  (with-aligned-rendering (stream)
    (apply #'render-aligned-string fmt args)))

(defun char-height (stream)
  (multiple-value-bind (width height)
      (clim:text-size stream "M")
    (declare (ignore width))
    height))

(defun char-width (stream)
  (multiple-value-bind (width height)
      (clim:text-size stream "M")
    (declare (ignore height))
    width))

(defun find-replacement-text-styles (stream string &key text-style)
  (clim:with-sheet-medium (medium stream)
    (clim-freetype::find-replacement-fonts (clim:port medium) (or text-style (clim:medium-text-style stream)) string)))

(defun render-formatted-with-replacement (stream fmt &rest args)
  (with-aligned-rendering (stream)
    (let ((blocks (find-replacement-text-styles stream (apply #'format nil fmt args)))
          (font-size (clim:text-style-size (clim:medium-text-style stream))))
      (log:trace "blocks: ~s" blocks)
      (loop
        for (string family style) in blocks
        if family
          do (clim:with-text-style (stream (clim:make-text-style family style font-size))
               (render-aligned-string "~a" string))
        else
          do (render-aligned-string "~a" string)))))
