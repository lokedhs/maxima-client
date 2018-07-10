(in-package :maxima-client.gui-tools)

(defclass popup-selector-view (clim:textual-view)
  ())

(defvar +popup-selector-view+ (make-instance 'popup-selector-view))

(defclass popup-menu-element ()
  ((value  :initarg :value
           :reader popup-menu-element/value)
   (record :initarg :record
           :accessor popup-menu-element/record)))

(defgeneric render-element (value stream viewport-width)
  (:method ((value string) stream viewport-width)
    (clim:draw-text* stream value 0 0)))

(defgeneric get-element-filter-name (value)
  (:method ((value string))
    value))

(defun display-popup-menu-entry (stream value element-selected)
  (let* ((viewport-width (clim:rectangle-width (clim:pane-viewport-region stream)))
         (rec (clim:with-output-to-output-record (stream)
                (render-element value stream viewport-width))))
    (dimension-bind (rec :height h)
      (clim:draw-rectangle* stream 0 0 viewport-width h
                            :ink (if element-selected
                                     (clim:make-rgb-color 0.7 1 0.7)
                                     clim:+white+))
      (set-rec-position rec nil 0)
      (clim:stream-add-output-record stream rec))))

(defclass popup-menu-clickable-element ()
  ((value :initarg :value
          :reader popup-menu-clickable-element/value)))

(defun make-menu-entry-vector (stream values)
  ;(clim:clear-output-record (clim:stream-output-history stream))
  (clim:window-clear stream)
  (let ((vector (make-array (length values))))
    (loop
      with y = 0
      for v in values
      for i from 0
      do (clim:with-output-as-presentation (stream (make-instance 'popup-menu-clickable-element :value v)
                                                   'popup-menu-clickable-element)
           (let* ((record (make-instance 'clim:standard-sequence-output-record))
                  (inner-record (clim:with-output-to-output-record (stream 'clim:standard-sequence-output-record record)
                                  (display-popup-menu-entry stream v (zerop i)))))
             (clim:add-output-record inner-record record)
             (set-rec-position record nil y)
             (clim:stream-add-output-record stream record)
             (let ((element (make-instance 'popup-menu-element :value v :record record)))
               (setf (aref vector i) element))
             (dimension-bind (record :height height)
               (incf y height)))))
    (clim:queue-repaint stream (make-instance 'clim:window-repaint-event
                                              :region (clim:pane-viewport-region stream)
                                              :sheet stream))
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
    (clim:change-space-requirements top-level-pane :width 400 :height 400 :resize-frame t)
    (clim:move-sheet top-level-pane 10 50)))

(defun ensure-output-record-visible (pane output-record)
  (dimension-bind ((clim:pane-viewport-region pane) :x viewport-x1 :y viewport-y1 :bottom viewport-y2 :height viewport-h)
    (dimension-bind (output-record :y rec-y1 :bottom rec-y2)
      (cond ((< rec-y1 viewport-y1)
             (clim:scroll-extent pane viewport-x1 rec-y1))
            ((> rec-y2 viewport-y2)
             (clim:scroll-extent pane viewport-x1 (max (- rec-y2 viewport-h) 0)))))))

(defun menu-loop-inner (menu-pane entries)
  (let ((selected-index 0))
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
        for gesture = (clim:with-input-context ('popup-menu-clickable-element :override nil)
                          (object type)
                          (clim:read-gesture :stream menu-pane)
                        (popup-menu-clickable-element (return-from control-loop
                                                        (list :result (popup-menu-clickable-element/value object)))))
        if (characterp gesture)
          do (cond ((or (eql gesture #\Newline)
                        (eql gesture #\Tab))
                    (return-from control-loop
                      (list :result (popup-menu-element/value (aref entries selected-index)))))
                   ((eql gesture #\Backspace)
                    (return-from control-loop
                      (list :update-backspace nil)))
                   (t
                    (return-from control-loop
                      (list :update-filter gesture))))
        when (typep gesture 'clim:key-press-event)
          do (let ((event-name (clim:keyboard-event-key-name gesture)))
               (if (gesture-modifier-p gesture :control)
                   (case event-name
                     (:|p| (move -1))
                     (:|n| (move 1)))
                   (case event-name
                     (:up (move -1))
                     (:down (move 1))
                     (:next (log:info "Scroll down one page"))
                     (:prior (log:info "Scroll up one page"))
                     (:escape (return-from control-loop '(:result . nil))))))))))

(defun menu-loop (menu-pane values)
  (let ((filter-string "")
        (filtered-values values))
    (labels ((filter-by-prefix (prefix)
               (remove-if-not (lambda (value)
                                (alexandria:starts-with-subseq prefix (get-element-filter-name value)))
                              values)))
      (loop
        for entries = (make-menu-entry-vector menu-pane filtered-values)
        for result = (menu-loop-inner menu-pane entries)
        do (ecase (car result)
             (:result
              (return (cadr result)))
             (:update-filter
              (let ((ch (cadr result)))
                (let* ((updated-filter (format nil "~a~c" filter-string ch))
                       (result (filter-by-prefix updated-filter)))
                  (when result
                    (setq filter-string updated-filter)
                    (setq filtered-values result)))))
             (:update-backspace
              (when (plusp (length filter-string))
                (setq filter-string (subseq filter-string 0 (1- (length filter-string))))
                (setq filtered-values (filter-by-prefix filter-string)))))))))

(defun select-completion-match (values)
  "Display a popup allowing the user to select one of several elements."
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
               (adjust-popup-dimensions menu-pane)
               (clim:enable-frame frame)
               (menu-loop menu-pane values))
          (clim:disown-frame fm frame))))))
