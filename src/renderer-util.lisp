(in-package :maxima-client)

(defgeneric text-style-fallback-fonts (text-style)
  (:method ((text-style clim:standard-text-style))
    nil))

(defclass font-replacement-text-style (clim:standard-text-style)
  ((fallback-fonts :initarg :replacement
                   :reader font-replacement-text-style/replacement)))

(defmethod text-style-fallback-fonts ((text-style font-replacement-text-style))
  (font-replacement-text-style/replacement text-style))

(defmethod clim:merge-text-styles ((style1 font-replacement-text-style) (style2 clim:standard-text-style))
  (let ((merged-standard (call-next-method)))
    (make-font-replacement-text-style (clim:text-style-family merged-standard)
                                      (clim:text-style-face merged-standard)
                                      (clim:text-style-size merged-standard)
                                      (font-replacement-text-style/replacement style1))))

(defmethod clim:merge-text-styles ((style1 clim:standard-text-style) (style2 font-replacement-text-style))
  (let ((merged-standard (call-next-method)))
    (make-font-replacement-text-style (clim:text-style-family merged-standard)
                                      (clim:text-style-face merged-standard)
                                      (clim:text-style-size merged-standard)
                                      (font-replacement-text-style/replacement style2))))

(defun make-font-replacement-text-style (family face size replacement)
  (make-instance 'font-replacement-text-style
                 :text-family family
                 :text-face face
                 :text-size size
                 :replacement replacement))

(defun find-best-font (ch)
  (let* ((match (mcclim-fontconfig:match-font `((:charset . (:charset ,ch))) '(:family :style) :kind :match-font)))
    (log:trace "Match for ~s: ~s" ch match)
    (let ((family (cdr (assoc :family match)))
          (style (cdr (assoc :style match))))
      (cond ((and family style)
             (list family style))
            (t
             (log:warn "No font found for ~s" ch)
             '(nil nil))))))

(defun text-style-contains-p (stream ch &key text-style)
  (clim:with-sheet-medium (medium stream)
    (let* ((style (or text-style (clim:medium-text-style stream)))
           (font (clim-clx::text-style-to-x-font (clim:port medium) style))
           (charset (clim-freetype::freetype-font-face/charset (clim-freetype::freetype-font/face font))))
      (mcclim-fontconfig:charset-contains-p charset ch))))

(defun find-best-font-for-fallback (stream text-style ch)
  (loop
    for fallback in (text-style-fallback-fonts text-style)
    for fallback-family = (first fallback)
    for fallback-style = (second fallback)
    do (log:info "Checking fallback font: ~s" fallback)
    when (text-style-contains-p stream ch :text-style (clim:make-text-style fallback-family fallback-style 10))
      return fallback
    finally (return (find-best-font ch))))

(defun find-replacement-fonts (stream string)
  (clim:with-sheet-medium (medium stream)
    (let* ((port (clim:port medium))
           (default-text-style (clim:medium-text-style stream))
           (default-font (clim-clx::text-style-to-x-font port default-text-style))
           (default-charset (clim-freetype::freetype-font-face/charset (clim-freetype::freetype-font/face default-font)))
           (result nil)
           (current-string (make-string-output-stream))
           (current-text-style nil))
      (labels ((push-string ()
                 (let ((s (get-output-stream-string current-string)))
                   (when (plusp (length s))
                     (push (cons s current-text-style) result))))
               (collect-result (ch text-style)
                 (unless (equal current-text-style text-style)
                   (push-string)
                   (setq current-text-style text-style))
                 (write-char ch current-string)))
        (loop
          for ch across string
          do (log:trace "checking if ~s is in ~s: ~s" ch default-text-style (mcclim-fontconfig:charset-contains-p default-charset ch))
          do (collect-result ch (if (mcclim-fontconfig:charset-contains-p default-charset ch)
                                    '(nil nil)
                                    (find-best-font-for-fallback stream default-text-style ch))))
        (push-string)
        (reverse result)))))

(defun render-formatted-with-replacement (stream fmt &rest args)
  (with-aligned-rendering (stream)
    (let ((blocks (find-replacement-fonts stream (apply #'format nil fmt args)))
          (font-size (clim:text-style-size (clim:medium-text-style stream))))
      (log:trace "blocks: ~s" blocks)
      (loop
        for (string family style) in blocks
        if family
          do (clim:with-text-style (stream (clim:make-text-style family style font-size))
               (render-aligned-string "~a" string))
        else
          do (render-aligned-string "~a" string)))))
