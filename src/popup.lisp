(in-package :maxima-client)

(defclass popup-selector-view (clim:textual-view)
  ())

(defvar +popup-selector-view+ (make-instance 'popup-selector-view))

(defclass popup-menu-element ()
  ((value  :initarg :value
           :reader popup-menu-element/value)
   (record :initarg :record
           :accessor popup-menu-element/record)))

(defun display-popup-menu-entry (stream value element-selected)
  (let ((rec (clim:with-output-to-output-record (stream)
               (clim:draw-text* stream (second value) 0 0 :ink clim:+black+))))
    (dimension-bind (rec :height h)
      (clim:draw-rectangle* stream 0 0 (clim:rectangle-width (clim:pane-viewport-region stream)) h
                            :ink (if element-selected
                                     (clim:make-rgb-color 0.7 1 0.7)
                                     clim:+white+)))
    (set-rec-position rec 0 0)
    (clim:stream-add-output-record stream rec)))

(defun make-menu-entry-vector (stream values)
  (let ((vector (make-array (length values))))
    (loop
      with y = 0
      for v in values
      for i from 0
      do (let* ((record (make-instance 'clim:standard-sequence-output-record))
                (inner-record (clim:with-output-to-output-record (stream 'clim:standard-sequence-output-record record)
                                (display-popup-menu-entry stream v (zerop i)))))
           (clim:add-output-record inner-record record)
           (setf (clim:output-record-position record) (values 0 y))
           (clim:stream-add-output-record stream record)
           (let ((element (make-instance 'popup-menu-element :value v :record record)))
             (setf (aref vector i) element))
           (dimension-bind (record :height height)
             (incf y height))))
    vector))

(defun redraw-popup-menu-entry (stream entry element-selected)
  (let* ((record (popup-menu-element/record entry)))
    (dimension-bind (record :y prev-y)
      (clim:clear-output-record record)
      (let ((inner-record (clim:with-output-to-output-record (stream 'clim:standard-sequence-output-record)
                            (display-popup-menu-entry stream (popup-menu-element/value entry) element-selected))))
        (setf (clim:output-record-position inner-record) (values 0 0))
        (clim:add-output-record inner-record record)
        (setf (clim:output-record-position record) (values 0 prev-y))
        (dimension-bind (record :x x :y y :right right :bottom bottom)
          (clim:repaint-sheet stream (clim:make-bounding-rectangle x y right bottom)))))))

(defun adjust-popup-dimensions (pane)
  (let ((top-level-pane (labels ((searching (pane)
				   (if (typep pane 'climi::top-level-sheet-pane)
				       pane
				       (searching (clim:sheet-parent pane)))))
			  (searching pane))))
    (clim:move-sheet top-level-pane 10 10)))

(defun ensure-output-record-visible (pane output-record)
  (dimension-bind ((clim:pane-viewport-region pane) :x viewport-x1 :y viewport-y1 :bottom viewport-y2 :height viewport-h)
    (dimension-bind (output-record :y rec-y1 :bottom rec-y2)
      (cond ((< rec-y1 viewport-y1)
             (clim:scroll-extent pane viewport-x1 rec-y1))
            ((> rec-y2 viewport-y2)
             (clim:scroll-extent pane viewport-x1 (max (- rec-y2 viewport-h) 0)))))))

(defun select-completion-match (values)
  (let* ((associated-frame clim:*application-frame*)
         (fm (clim:frame-manager associated-frame)))
    (clim:with-look-and-feel-realization (fm associated-frame)
      (let* ((menu-pane (clim:make-pane-1 fm associated-frame 'clim:clim-stream-pane))
             (menu-container (clim:scrolling (:scroll-bar :vertical) menu-pane))
             (frame (clim-internals::make-menu-frame (clim-internals::raising ()
                                                       (clim:labelling (:label "Completions" :name 'label :label-alignment :top)
                                                         menu-container))
                                                     :left nil
                                                     :top nil)))
        (clim:adopt-frame fm frame)
        (unwind-protect
             (progn
               (setf (clim:stream-end-of-line-action menu-pane) :allow)
               (setf (clim:stream-end-of-page-action menu-pane) :allow)
               ;; Draw menu
               (adjust-popup-dimensions menu-pane)
               (clim:enable-frame frame)
               (let ((entries (make-menu-entry-vector menu-pane values))
                     (selected-index 0))
                 (labels ((move (delta)
                            (let ((new-pos (max (min (+ selected-index delta) (1- (length entries))) 0)))
                              (when (/= new-pos selected-index)
                                (redraw-popup-menu-entry menu-pane (aref entries selected-index) nil)
                                (let ((entry (aref entries new-pos)))
                                  (redraw-popup-menu-entry menu-pane entry t)
                                  (ensure-output-record-visible menu-pane (popup-menu-element/record entry)))
                                (setq selected-index new-pos)))))
                   (loop
                     named control-loop
                     for gesture = (clim:with-input-context ('string :override nil)
                                       (object type)
                                       (clim:read-gesture :stream menu-pane)
                                     (string (log:info "got string: ~s" object)))
                     when (or (eql gesture #\Newline)
                              (eql gesture #\Tab))
                       do (return-from control-loop (first (popup-menu-element/value (aref entries selected-index))))
                     when (typep gesture 'clim:key-press-event)
                       do (if (gesture-modifier-p gesture :control)
                              (case (clim:keyboard-event-key-name gesture)
                                (:|p| (move -1))
                                (:|n| (move 1)))
                              (case (clim:keyboard-event-key-name gesture)
                                (:up (move -1))
                                (:down (move 1))
                                (:escape (return-from control-loop nil))))))))
          (clim:disown-frame fm frame))))))
