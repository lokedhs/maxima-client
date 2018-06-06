(in-package :maxima-client)

(declaim (optimize (speed 0) (safety 3) (debug 3)))

(defclass highlighted-standard-presentation (clim:standard-presentation)
  ())

(defun find-rgba-format (display)
  (or (getf (xlib:display-plist display) 'rgba-format)
      (let* ((formats (xlib::render-query-picture-formats display))
             (format (find-if (lambda (v)
                                (and (= (byte-size (xlib:picture-format-red-byte v)) 8)
                                     (= (byte-size (xlib:picture-format-green-byte v)) 8)
                                     (= (byte-size (xlib:picture-format-blue-byte v)) 8)
                                     (= (byte-size (xlib:picture-format-alpha-byte v)) 8)))
                              formats)))
        (unless format
          (error "Can't find 8-bit RGBA format"))
        (setf (getf (xlib:display-plist display) 'rgba-format) format))))

(defun find-alpha-mask-format (display)
  (or (getf (xlib:display-plist display) 'alpha-mask-format)
      (let* ((formats (xlib::render-query-picture-formats display))
             (format (find-if (lambda (v)
                                (and (= (byte-size (xlib:picture-format-red-byte v)) 0)
                                     (= (byte-size (xlib:picture-format-green-byte v)) 0)
                                     (= (byte-size (xlib:picture-format-blue-byte v)) 0)
                                     (= (byte-size (xlib:picture-format-alpha-byte v)) 8)))
                              formats)))
        (unless format
          (error "Can't find 8-bit RGBA format"))
        (setf (getf (xlib:display-plist display) 'alpha-mask-format) format))))

(defun create-pen (drawable gc colour)
  (let ((cached-pen (getf (xlib:gcontext-plist gc) 'cached-pen)))
    (cond ((and cached-pen (equal (second cached-pen) colour))
           (first cached-pen))
          (t
           (when cached-pen
             (xlib:render-free-picture (first cached-pen)))
           (let* ((pixmap (xlib:create-pixmap :drawable (xlib:drawable-root drawable)
                                              :width 1
                                              :height 1
                                              :depth 32))
                  (picture (xlib:render-create-picture pixmap
                                                       :format (find-rgba-format (xlib::drawable-display drawable))
                                                       :repeat :on)))
             (xlib:render-fill-rectangle picture :src colour 0 0 1 1)
             (xlib:free-pixmap pixmap)
             (setf (getf (xlib:gcontext-plist gc) 'cached-pen) (list picture colour))
             picture)))))

(defun create-picture-from-drawable (drawable)
  (xlib:render-create-picture drawable
                              :format (xlib:find-window-picture-format (xlib:drawable-root drawable))
                              :poly-edge :smooth
                              :poly-mode :precise))

(defgeneric create-dest-picture (drawable)
  (:method ((drawable xlib:window))
    (or (getf (xlib:window-plist drawable) 'cached-picture)
        (setf (getf (xlib:window-plist drawable) 'cached-picture)
              (create-picture-from-drawable drawable))))
  (:method ((drawable xlib:pixmap))
    (or (getf (xlib:pixmap-plist drawable) 'cached-picture)
        (setf (getf (xlib:pixmap-plist drawable) 'cached-picture)
              (create-picture-from-drawable drawable)))))

(defun display-presentation-highlight-clx (stream record state)
  (dimension-bind (record :x x1 :y y1 :right x2 :bottom y2)
    (ecase state
      (:highlight
       (let* ((medium (clim:sheet-medium stream))
              (gc (clim-clx::medium-gcontext medium clim:+background-ink+))
              (mirror (climi::port-lookup-mirror (clim:port medium) (clim:medium-sheet medium)))
              (dest (create-dest-picture mirror))
              (src (create-pen mirror gc '(#xffff 0 0 #xa000))))
         (xlib:render-triangles dest :over src 0 0
                                (find-alpha-mask-format (xlib:gcontext-display gc))
                                (vector x1 y1 x2 y1 x2 y2
                                        x1 y1 x2 y2 x1 y2))))
      (:unhighlight
       (clim:repaint-sheet stream (clim:make-rectangle* (1- x1) (1- y1) (1+ x2) (1+ y2)))))))

(defmethod clim:highlight-output-record ((record highlighted-standard-presentation) stream state)
  (log:info "will highlight presentation: ~s with state ~s" record state)
  (let ((medium (clim:sheet-medium stream)))
    (typecase medium
      (clim-clx::clx-medium (display-presentation-highlight-clx stream record state))
      (t (call-next-method)))))
