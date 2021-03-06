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
(defvar *word-wrap-left-margin* nil)
(defvar *word-wrap-right-margin* nil)

(defmacro with-adjusted-margin ((left right) &body body)
  (alexandria:once-only (left right)
    `(let ((*word-wrap-left-margin* (+ *word-wrap-left-margin* ,left))
           (*word-wrap-right-margin* (+ *word-wrap-right-margin* ,right)))
       ,@body)))

(defun font-ascent (stream)
  (let ((style (clim:medium-text-style stream)))
    (climb:text-style-ascent style stream)))

(defun font-descent (stream)
  (let ((style (clim:medium-text-style stream)))
    (climb:text-style-descent style stream)))

(defun font-height (stream)
  (let ((style (clim:medium-text-style stream)))
    (+ (climb:text-style-ascent style stream)
       (climb:text-style-descent style stream))))

(defun font-char-width (stream)
  (let ((style (clim:medium-text-style stream)))
    (climb:text-style-character-width style stream #\m)))

(defun draw-current-line (stream)
  (when (plusp (length *word-wrap-line-content*))
    (let ((max-descent (max (font-descent stream)
                            (reduce #'max *word-wrap-line-content*
                                    :key (lambda (rec)
                                           (dimension-bind-new (stream rec :bottom bottom)
                                             bottom)))))
          (max-ascent (max (climb:text-style-ascent (clim:medium-text-style stream) stream)
                           (reduce #'max *word-wrap-line-content*
                                   :key (lambda (rec)
                                          (dimension-bind-new (stream rec :y y)
                                            (- y)))))))
      (loop
        for rec across *word-wrap-line-content*
        do #+nil (multiple-value-bind (x y)
                     (clim:output-record-position rec)
                   (declare (ignore y))
                   (setf (clim:output-record-position rec) (values x *word-wrap-y*))
                   (clim:stream-add-output-record stream rec))
           (dimension-bind-new (stream rec :x x :y y)
             ;; We need to adjust the vertical coordinate so that text of different sizes are aligned to the same
             ;; baseline. All text is drawn with the baseline at y=0, and the ascent is a negative value.
             (set-rec-position rec x (+ *word-wrap-y* max-ascent y))
             (clim:stream-add-output-record stream rec)))
      (incf *word-wrap-y* (+ max-ascent max-descent)))))

(defun draw-current-line-and-reset (stream)
  (draw-current-line stream)
  (setq *word-wrap-line-content* (make-array 0 :adjustable t :fill-pointer t))
  (setq *word-wrap-x* *word-wrap-left-margin*))

(defun add-vspacing (stream n)
  (draw-current-line-and-reset stream)
  (incf *word-wrap-y* n)
  (draw-current-line-and-reset stream))

(defun draw-newline (stream)
  (let ((x *word-wrap-x*))
    (draw-current-line-and-reset stream)
    (when (zerop x)
      (add-vspacing stream (font-height stream)))))

(defmacro with-word-wrap ((stream &key right-margin) &body body)
  (alexandria:once-only (stream right-margin)
    `(let ((*word-wrap-x* 0)
           (*word-wrap-y* 0)
           (*word-wrap-left-margin* 0)
           (*word-wrap-right-margin* (or ,right-margin (clim:rectangle-width (clim:pane-viewport-region ,stream))))
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
      (let ((rec (clim:with-identity-transformation (stream)
                   (clim:with-output-to-output-record (stream)
                     (clim:draw-text* stream initial start 0)))))
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
    (dimension-bind-new (stream rec :width width)
      (when (> (+ start width) right-margin)
        (draw-current-line-and-reset stream)
        (set-rec-position rec *word-wrap-left-margin* nil))
      (vector-push-extend rec *word-wrap-line-content*)
      (incf *word-wrap-x* width))))

(defun word-wrap-draw-string (stream string)
  (let ((parts (split-word string)))
    (loop
      while parts
      do (setq parts (word-wrap-draw-one-line stream parts)))))

(defmacro with-word-wrap-record ((stream) &body body)
  (alexandria:once-only (stream)
    (alexandria:with-gensyms (rec bottom)
      `(clim:with-identity-transformation (,stream)
         (let ((,rec (clim:with-output-to-output-record (,stream)
                       ,@body)))
           (dimension-bind-new (,stream ,rec :bottom ,bottom)
             (move-rec ,rec 0 (- ,bottom)))
           (word-wrap-draw-record ,stream ,rec))))))

(defun word-wrap-draw-presentation (stream obj &key presentation-type)
  (clim:with-identity-transformation (stream)
    (let ((rec (clim:with-output-to-output-record (stream)
                 (clim:stream-present stream obj (or presentation-type (clim:presentation-type-of obj))))))
      (word-wrap-draw-record stream rec))))

(defun word-wrap-add-absolute-spacing (position)
  (when (or (< position *word-wrap-left-margin*)
            (>= position *word-wrap-right-margin*))
    (error "position out of range: ~s. Should be between ~s and ~s" position *word-wrap-left-margin* *word-wrap-right-margin*))
  (setq *word-wrap-x* (max *word-wrap-x* position)))
