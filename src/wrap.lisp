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

(defvar *word-wrap-x* nil)
(defvar *word-wrap-y* nil)
(defvar *word-wrap-line-content* nil)
(defvar *word-wrap-right-margin* nil)
(defvar *word-wrap-height* nil)

(defmacro with-adjusted-margin ((delta) &body body)
  (alexandria:once-only (delta)
    `(let ((*word-wrap-right-margin* (- *word-wrap-right-margin* ,delta)))
       ,@body)))

(defun font-height (stream)
  (multiple-value-bind (width ascent descent left right font-ascent font-descent)
      (clim-clx::font-text-extents (clim-clx::text-style-to-x-font (clim:port (clim:sheet-medium stream))
                                                                   (clim:pane-text-style stream))
                                   "M")
    (declare (ignore width ascent descent left right))
    (+ font-ascent font-descent)))

(defun draw-current-line (stream)
  (when (plusp (length *word-wrap-line-content*))
    (let ((height (max *word-wrap-height*
                       (reduce #'max *word-wrap-line-content*
                               :key (lambda (rec)
                                      (dimension-bind (rec :height height)
                                        height)))))
          (max-ascent (reduce #'max *word-wrap-line-content*
                              :key (lambda (rec)
                                     (dimension-bind (rec :y y)
                                       (- y))))))
      (loop
        for rec across *word-wrap-line-content*
        do #+nil (multiple-value-bind (x y)
                     (clim:output-record-position rec)
                   (declare (ignore y))
                   (setf (clim:output-record-position rec) (values x *word-wrap-y*))
                   (clim:stream-add-output-record stream rec))
           (dimension-bind (rec :x x :y y)
             ;; We need to adjust the vertical coordinate so that text of different sizes are aligned to the same
             ;; baseline. All text is drawn with the basline at y=0, and the ascent is a negative value.
             (set-rec-position rec x (+ *word-wrap-y* (max 0 (+ max-ascent y))))
             (clim:stream-add-output-record stream rec)))
      (incf *word-wrap-y* height))))

(defun draw-current-line-and-reset (stream)
  (draw-current-line stream)
  (setq *word-wrap-line-content* (make-array 0 :adjustable t :fill-pointer t))
  (setq *word-wrap-x* 0))

(defun add-vspacing (stream n)
  (draw-current-line-and-reset stream)
  (incf *word-wrap-y* n))

(defun draw-newline (stream)
  (let ((x *word-wrap-x*))
    (draw-current-line-and-reset stream)
    (when (zerop x)
      (add-vspacing stream (font-height stream)))))

(defmacro with-word-wrap ((stream &key right-margin height) &body body)
  (alexandria:once-only (stream right-margin height)
    `(let ((*word-wrap-x* 0)
           (*word-wrap-y* 0)
           (*word-wrap-right-margin* (or ,right-margin (clim:rectangle-width (clim:pane-viewport-region ,stream))))
           (*word-wrap-height* (or ,height (font-height stream)))
           (*word-wrap-line-content* (make-array 0 :adjustable t :fill-pointer t)))
       (prog1
           (progn ,@body)
         (draw-current-line ,stream)))))

(defun split-string-at-right-margin (stream parts right-margin)
  (loop
    with curr-string = ""
    with part = parts
    with width = 0
    while (not (endp part))
    while (let* ((next-string (format nil "~a~a" curr-string (car part)))
                 (w (clim:text-size stream next-string)))
            (cond ((<= w right-margin)
                   (setq curr-string next-string)
                   (setq part (cdr part))
                   (setq width w)
                   t)
                  (t
                   nil)))
    finally (return (values curr-string part width))))

(defun word-wrap-draw-one-line (stream parts)
  (let ((start *word-wrap-x*)
        (right-margin *word-wrap-right-margin*))
    (multiple-value-bind (initial more-parts width)
        (split-string-at-right-margin stream parts (- right-margin start))
      (let ((rec (clim:with-output-to-output-record (stream)
                   (clim:draw-text* stream initial start 0))))
        (vector-push-extend rec *word-wrap-line-content*))
      (cond (more-parts
             (draw-current-line-and-reset stream))
            (t
             (incf *word-wrap-x* width)))
      more-parts)))

(defun word-wrap-draw-record (stream rec)
  (let ((start *word-wrap-x*)
        (right-margin *word-wrap-right-margin*))
    (move-rec rec start 0)
    (dimension-bind (rec :width width)
      (cond ((<= (+ start width) right-margin)
             (incf *word-wrap-x* width))
            (t
             (draw-current-line-and-reset stream)))
      (vector-push-extend rec *word-wrap-line-content*))))

(defun word-wrap-draw-string (stream string)
  (let ((parts (split-word string)))
    (loop
      while parts
      do (setq parts (word-wrap-draw-one-line stream parts)))))

(defmacro with-word-wrap-record ((stream) &body body)
  (alexandria:once-only (stream)
    (alexandria:with-gensyms (rec)
      `(let ((,rec (clim:with-output-to-output-record (,stream)
                     ,@body)))
         (dimension-bind (,rec :x x :y y :width w :height h)
           (log:info "pos=(~s,~s) (~s,~s)" x y w h))
         (word-wrap-draw-record ,stream ,rec)))))
