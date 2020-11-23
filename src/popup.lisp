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

(defgeneric element-dimensions (value stream viewport-width)
  (:method ((value string) stream viewport-width)
    (let ((text-style (clim:medium-text-style stream)))
      (values (climb:text-style-ascent text-style stream)
              (climb:text-style-descent text-style stream)))))

(defgeneric get-element-filter-name (value)
  (:method ((value string))
    value))

(defun display-popup-menu-entry (stream value element-selected)
  (let ((viewport-width (clim:rectangle-width (clim:pane-viewport-region stream))))
    (multiple-value-bind (ascent descent)
        (element-dimensions value stream viewport-width)
      (let ((rec (clim:with-output-to-output-record (stream)
                   (render-element value stream viewport-width))))
        (clim:draw-rectangle* stream 0 0 viewport-width (+ ascent descent)
                              :ink (if element-selected
                                       (clim:make-rgb-color 0.7 1 0.7)
                                       clim:+white+))
        (move-rec rec 0 ascent)
        (clim:stream-add-output-record stream rec)))))

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

(defun max-x-y (frame)
  "Return the maximum X and Y coordinate values for a menu for
FRAME (essentially, the screen resolution with a slight padding.)"
  ;; FIXME? There may be a better way.
  (let* ((port (clim:port (clim:frame-manager frame)))
         (graft (clim:find-graft :port port)))
    (values (- (clim:graft-width graft) 50)
            (- (clim:graft-height graft) 50))))

(defun menu-size (menu frame)
  "Return two values, the height and width of MENU (adjusted for
maximum size according to FRAME)."
  (multiple-value-bind (max-width max-height)
      (max-x-y frame)
    (clim:with-bounding-rectangle* (x1 y1 x2 y2) menu
      (declare (ignore x1 y1))
      (values (min x2 max-width)
              (min y2 max-height)))))

(defun adjust-popup-dimensions (pane x-pos y-pos width height)
  (let ((top-level-pane (labels ((searching (pane)
				   (if (typep pane 'climi::top-level-sheet-pane)
				       pane
				       (searching (clim:sheet-parent pane)))))
			  (searching pane))))
    (clim:change-space-requirements top-level-pane :width width :height height :resize-frame t)
    (multiple-value-bind (frame-width frame-height)
        (menu-size top-level-pane clim:*application-frame*)
      (multiple-value-bind (screen-width screen-height)
          (max-x-y clim:*application-frame*)
        (clim:move-sheet top-level-pane
                         (max (min x-pos (- screen-width frame-width)) 0)
                         (max (min y-pos (- screen-height frame-height)) 0))))))

(defun ensure-output-record-visible (pane output-record)
  (dimension-bind ((clim:pane-viewport-region pane) :x viewport-x1 :y viewport-y1 :bottom viewport-y2 :height viewport-h)
    (dimension-bind (output-record :y rec-y1 :bottom rec-y2)
      (cond ((< rec-y1 viewport-y1)
             (clim:scroll-extent pane viewport-x1 rec-y1))
            ((> rec-y2 viewport-y2)
             (clim:scroll-extent pane viewport-x1 (max (- rec-y2 viewport-h) 0)))))))

(defun find-visible-elements (viewport-y1 viewport-y2 entries)
  (labels ((element-visible-p (entry)
             (dimension-bind ((popup-menu-element/record entry) :y rec-y1 :bottom rec-y2)
               (and (< viewport-y1 rec-y2)
                    (> viewport-y2 rec-y1)))))
    (loop
      with prev-visible-p = nil
      with first-visible-index = nil
      with last-visible-index = nil
      for i from 0
      for entry across entries
      for visible-p = (element-visible-p entry)
      if (and (not prev-visible-p) visible-p (not first-visible-index))
        do (setq first-visible-index i)
      if (and prev-visible-p (not visible-p) (not last-visible-index))
        do (setq last-visible-index i)
      do (setq prev-visible-p visible-p)
      until (and first-visible-index last-visible-index)
      finally (return (list first-visible-index (or last-visible-index (1- (length entries))))))))

(defun menu-loop-inner (menu-pane entries)
  (let ((selected-index 0))
    (labels ((update-selected (new-pos)
               (let ((updated-pos (max (min new-pos (1- (length entries))) 0)))
                 (when (/= updated-pos selected-index)
                   (redraw-popup-menu-entry menu-pane (aref entries selected-index) nil)
                   (let ((entry (aref entries updated-pos)))
                     (redraw-popup-menu-entry menu-pane entry t)
                     (ensure-output-record-visible menu-pane (popup-menu-element/record entry)))
                   (setq selected-index updated-pos))))
             (move (delta)
               (update-selected (+ selected-index delta)))
             (pagedown ()
               (when (plusp (length entries))
                 (dimension-bind ((clim:pane-viewport-region menu-pane) :y viewport-y1 :bottom viewport-y2)
                   (destructuring-bind (start end)
                       (find-visible-elements viewport-y1 (+ viewport-y1 (* 2 (- viewport-y2 viewport-y1))) entries)
                     (declare (ignore start))
                     (ensure-output-record-visible menu-pane (popup-menu-element/record (aref entries end)))
                     (update-selected end)))))
             (pageup ()
               (when (plusp (length entries))
                 (dimension-bind ((clim:pane-viewport-region menu-pane) :y viewport-y1 :bottom viewport-y2)
                   (destructuring-bind (start end)
                       (find-visible-elements (- viewport-y1 (- viewport-y2 viewport-y1)) viewport-y2 entries)
                     (declare (ignore end))
                     (ensure-output-record-visible menu-pane (popup-menu-element/record (aref entries start)))
                     (update-selected start))))))
      (loop
        named control-loop
        for gesture = (clim:with-input-context ('popup-menu-clickable-element :override nil)
                          (object type)
                          (clim:read-gesture :stream menu-pane)
                        (popup-menu-clickable-element (return-from control-loop
                                                        (list :result (popup-menu-clickable-element/value object)))))
        if (characterp gesture)
          do (cond ((or (eql gesture #\Return)
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
               (cond ((gesture-modifier-p gesture :control)
                      (case event-name
                        (:|p| (move -1))
                        (:|n| (move 1))
                        (:|v| (pagedown))))
                     ((gesture-modifier-p gesture :meta)
                      (case event-name
                        (:|v| (pageup))))
                     (t
                      (case event-name
                        (:up (move -1))
                        (:down (move 1))
                        (:next (pagedown))
                        (:prior (pageup))
                        (:escape (return-from control-loop '(:result . nil)))))))))))

(defun menu-loop (menu-pane load-fn initial-prefix)
  (let ((filter-string initial-prefix))
    (loop
      for entries = (make-menu-entry-vector menu-pane (funcall load-fn filter-string))
      for result = (menu-loop-inner menu-pane entries)
      do (ecase (car result)
           (:result
            (return (cadr result)))
           (:update-filter
            (let ((ch (cadr result)))
              (when result
                (setq filter-string (format nil "~a~c" filter-string ch)))))
           (:update-backspace
            (when (plusp (length filter-string))
              (setq filter-string (subseq filter-string 0 (1- (length filter-string))))))))))

(defun select-completion-match (load-fn &key x-pos y-pos (initial-prefix ""))
  "Display a popup allowing the user to select one of several elements.
LOAD-FN is called with a string prefix and is responsible for
returning a list of matches that match that prefix. This function can
also return :ERROR to indicate that there was a problem retrieving the
matches (this is different from NIL which indicates that no matches
were found)."
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
               (adjust-popup-dimensions menu-pane x-pos y-pos 400 400)
               (clim:enable-frame frame)
               (menu-loop menu-pane load-fn initial-prefix))
          (clim:disown-frame fm frame))))))
