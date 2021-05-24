(in-package :maxima-client.canvas)

(clim:define-command-table canvas-commands)

(defclass canvas-view ()
  ())

(defvar +canvas-view+ (make-instance 'canvas-view))

(defclass variables-list-view ()
  ())

(defvar +variables-list-view+ (make-instance 'variables-list-view))

#+nil
(clim:define-command (com-follow :name "Follow Link" :command-table foo)
    ((target 'string))
  (clim:notify-user clim:*application-frame*
                    (format nil "Going to ~A" target)))

#+nil
(defun display (frame pane)
  (clim:with-output-as-presentation (pane '(com-follow "target") '(clim:command :command-table foo))
    (write-string "goto target" pane)))

(defclass canvas-object-record (clim:standard-presentation)
  ())

(defclass canvas-object-record-inner (clim:standard-sequence-output-record)
  ())

(defclass updateable-value ()
  ((current    :initarg :current
               :initform 0
               :accessor updateable-value/current)
   (expression :initarg :expression
               :initform nil
               :accessor updateable-value/expression))
  (:documentation "A value that can be updated by a Maxima expression"))

(defun make-updateable-value (expression)
  (let ((initial (eval-maxima-expression-to-float expression)))
    (make-instance 'updateable-value
                   :current (or initial 0)
                   :expression expression)))

(defun update-value (value)
  (alexandria:when-let ((expr (updateable-value/expression value)))
    (alexandria:when-let ((new-value (eval-maxima-expression-to-float expr)))
      (setf (updateable-value/current value) new-value))))

(defclass param ()
  ((name          :initarg :name
                  :initform (alexandria:required-argument :name)
                  :type symbol
                  :reader param/name)
   (start-value   :initarg :start-value
                  :initform 0
                  :accessor param/start-value)
   (current-value :initarg :current-value
                  :initform 0
                  :accessor param/current-value)
   (expression    :initarg :expression
                  :initform nil
                  :accessor param/expression)))

(clim:define-presentation-type param-presentation ())

(defun eval-param (pane expr)
  (check-type pane canvas-pane)
  (let* ((params (canvas-pane/params pane))
         (wrapped `((maxima::$at)
                    ,expr
                    ((maxima::mlist)
                     ,@(append (loop
                                 for p in params
                                 collect `((maxima::mequal) ,(param/name p) ,(param/current-value p)))
                               `(((maxima::mequal) maxima::$t ,(canvas-pane/curr-time pane))
                                 ((maxima::mequal) maxima::$dt ,(canvas-pane/step-size pane))))))))
    (eval-maxima-expression-to-float wrapped)))

(defclass canvas-pane (clim:clim-stream-pane)
  ((objects         :initarg :objects
                    :initform nil
                    :accessor canvas-pane/objects
                    :documentation "A list of objects on the canvas")
   (params          :initarg :params
                    :initform nil
                    :accessor canvas-pane/params
                    :documentation "A list of dependent variables")
   (curr-time       :initform 0
                    :accessor canvas-pane/curr-time)
   (step-size       :initform 0.001
                    :accessor canvas-pane/step-size)
   (animation-state :initform :stopped
                    :accessor canvas-pane/animation-state)
   (scale           :initform 1
                    :accessor canvas-pane/scale)
   (x-centre        :initform 0
                    :accessor canvas-pane/x-centre)
   (y-centre        :initform 0
                    :accessor canvas-pane/y-centre)))

(defclass variables-list-pane (clim:clim-stream-pane)
  ())

(defgeneric draw-graphic-object-info (pane object))
(defgeneric draw-canvas-object (pane obj))
(defgeneric object-config (obj))

(defun redisplay-variables-list-pane ()
  (let ((pane (find-variable-list-pane)))
    (setf (clim:pane-needs-redisplay pane) t)
    (clim:redisplay-frame-pane (clim:pane-frame pane) pane)))

(defun repaint-variables-list (frame pane)
  (declare (ignore frame))
  (let ((canvas-pane (find-canvas-pane))
        (highlight-colour (clim:make-rgb-color 0.97 0.97 0.97)))
    (clim:with-text-style (pane (clim:make-text-style :sals-serif :bold :very-large))
      (format pane "State~%"))
    (clim:with-text-face (pane :bold)
      (format pane "Time: "))
    (format pane "~f~%" (canvas-pane/curr-time canvas-pane))
    (clim:with-text-face (pane :bold)
      (format pane "Step size: "))
    (format pane "~f~% ~%" (canvas-pane/step-size canvas-pane))
    (clim:with-text-style (pane (clim:make-text-style :sals-serif :bold :very-large))
      (format pane "Variables~%"))
    (clim:formatting-table (pane)
      (clim:formatting-row (pane)
        (clim:formatting-cell (pane)
          (clim:with-text-face (pane :bold)
            (format pane "Symbol")))
        (clim:formatting-cell (pane)
          (clim:with-text-face (pane :bold)
            (format pane "Start")))
        (clim:formatting-cell (pane)
          (clim:with-text-face (pane :bold)
            (format pane "Current")))
        (clim:formatting-cell (pane)
          (clim:with-text-face (pane :bold)
            (format pane "Expression")))
        (clim:formatting-cell (pane)
          (clim:with-text-face (pane :bold)
            (format pane "Options"))))
      (loop
        for param in (canvas-pane/params canvas-pane)
        for highlight = t then (not highlight)
        for symbol-expr = (make-instance 'maxima-native-expr :expr (param/name param))
        do (labels ((render-row ()
                      (clim:formatting-row (pane)
                        (clim:formatting-cell (pane)
                          (maxima-client:render-maxima-native-expr-toplevel pane symbol-expr))
                        (clim:formatting-cell (pane)
                          (clim:with-output-as-presentation (pane `(cmd-update-start-value ,param)
                                                                  '(clim:command :command-table canvas-commands))
                            (format pane "~f" (param/start-value param))))
                        (clim:formatting-cell (pane)
                          (clim:with-output-as-presentation (pane `(cmd-update-current-value ,param)
                                                                  '(clim:command :command-table canvas-commands))
                            (clim:with-text-size (pane :large)
                              (format pane "~:[unset~;~:*~f~]" (param/current-value param)))))
                        (clim:formatting-cell (pane)
                          (alexandria:when-let ((expr (param/expression param)))
                            (clim:with-room-for-graphics (pane :first-quadrant nil)
                              (maxima-client:render-maxima-native-expr-toplevel pane (make-instance 'maxima-native-expr :expr expr))))
                          (clim:with-output-as-presentation (pane `(cmd-update-expression ,param)
                                                                  '(clim:command :command-table canvas-commands))
                            (clim:with-text-size (pane :small) 
                              (format pane "edit"))))
                        (clim:formatting-cell (pane)
                          (clim:with-output-as-presentation (pane `(cmd-remove-variable ,param)
                                                                  '(clim:command :command-table canvas-commands))
                            (format pane "remove"))))))
             (if highlight
                 (clim:surrounding-output-with-border (pane :background highlight-colour
                                                            :line-thickness 0
                                                            :ink clim:+transparent-ink+)
                   (render-row))
                 (render-row)))))
    ;;
    (format pane "~%")
    (clim:with-text-style (pane (clim:make-text-style :sals-serif :bold :very-large))
      (format pane "Objects~%"))
    (clim:formatting-table (pane)
      (loop
        for obj in (canvas-pane/objects canvas-pane)
        do (clim:formatting-row (pane)
             (clim:formatting-cell (pane)
               (format pane "~a" (class-of obj)))
             (clim:formatting-cell (pane)
               (draw-graphic-object-info pane obj)))))))

(defun add-canvas-param (pane param)
  (check-type pane canvas-pane)
  (check-type param param)
  (setf (canvas-pane/params pane)
        (append (remove (param/name param) (canvas-pane/params pane))
                (list param))))

(defclass canvas-object ()
  ())

(defclass canvas-object-position-mixin ()
  ((x-position   :initarg :x
                 :initform (alexandria:required-argument :x)
                 :reader canvas-object/x)
   (y-position   :initarg :y
                 :initform (alexandria:required-argument :y)
                 :reader canvas-object/y)))

(defclass canvas-two-coord-mixin ()
  ((x1 :initarg :x1
       :initform (alexandria:required-argument :x1)
       :reader canvas-object/x1)
   (y1 :initarg :y1
       :initform (alexandria:required-argument :y1)
       :reader canvas-object/y1)
   (x2 :initarg :x2
       :initform (alexandria:required-argument :x2)
       :reader canvas-object/x2)
   (y2 :initarg :y2
       :initform (alexandria:required-argument :y2)
       :reader canvas-object/y2)))

(defclass canvas-circle (canvas-object canvas-object-position-mixin)
  ((size :initarg :size
         :initform 5
         :accessor canvas-circle/size)))

(defmethod initialize-instance ((obj canvas-circle) &key (saved-info :saved-info saved-info-p))
  (when saved-info-p
    (destructuring-bind (x y size)
        saved-info
      (setf (slot-value obj 'x-position) x)
      (setf (slot-value obj 'y-position) y)
      (setf (slot-value obj 'size) size)))
  (call-next-method))

(defmethod object-config ((obj canvas-circle))
  (list `(maxima::mlist)
        (canvas-object/x obj)
        (canvas-object/y obj)
        (canvas-circle/size obj)))

(defmethod draw-canvas-object (pane (obj canvas-circle))
  (let ((x (eval-param pane (canvas-object/x obj)))
        (y (eval-param pane (canvas-object/y obj)))
        (size (eval-param pane (canvas-circle/size obj))))
    (when (and (realp x)
               (realp y)
               (realp size))
      (clim:draw-circle* pane (scale-x pane x) (scale-y pane y) size :filled nil :ink clim:+black+))))

(defmethod draw-graphic-object-info (pane (obj canvas-circle))
  (format pane "Position: ~a,~a, size: ~a"
          (canvas-object/x obj)
          (canvas-object/y obj)
          (canvas-circle/size obj)))

(defun scale-x (pane x)
  (let ((scale (canvas-pane/scale pane)))
    (+ (* x scale) (compute-x-offset pane))))

(defun scale-y (pane y)
  (let ((scale (canvas-pane/scale pane)))
    (+ (* (- y) scale) (compute-y-offset pane))))

(defclass canvas-line (canvas-object canvas-two-coord-mixin)
  ())

(defmethod initialize-instance ((obj canvas-line) &key (saved-info :saved-info saved-info-p))
  (when saved-info-p
    (destructuring-bind (x1 y1 x2 y2)
        saved-info
      (setf (slot-value obj 'x1) x1)
      (setf (slot-value obj 'y1) y1)
      (setf (slot-value obj 'x2) x2)
      (setf (slot-value obj 'y2) y2)))
  (call-next-method))

(defmethod object-config ((obj canvas-line))
  (list '(maxima::mlist)
        (canvas-object/x1 obj)
        (canvas-object/y1 obj)
        (canvas-object/x2 obj)
        (canvas-object/y2 obj)))

(defmethod draw-canvas-object (pane (obj canvas-line))
  (let ((x1 (eval-param pane (canvas-object/x1 obj)))
        (y1 (eval-param pane (canvas-object/y1 obj)))
        (x2 (eval-param pane (canvas-object/x2 obj)))
        (y2 (eval-param pane (canvas-object/y2 obj))))
    (when (and (realp x1)
               (realp y1)
               (realp x2)
               (realp y2))
      (clim:draw-line* pane
                       (scale-x pane x1)
                       (scale-y pane y1)
                       (scale-x pane x2)
                       (scale-y pane y2)
                       :ink clim:+black+))))

(defmethod draw-graphic-object-info (pane (obj canvas-line))
  (format pane "(~s,~s) to (~s,~s)"
          (canvas-object/x1 obj)
          (canvas-object/y1 obj)
          (canvas-object/x2 obj)
          (canvas-object/y2 obj)))

(defclass canvas-box (canvas-object canvas-two-coord-mixin)
  ())

(defmethod initialize-instance ((obj canvas-box) &key (saved-info :saved-info saved-info-p))
  (when saved-info-p
    (destructuring-bind (x1 y1 x2 y2)
        saved-info
      (setf (slot-value obj 'x1) x1)
      (setf (slot-value obj 'y1) y1)
      (setf (slot-value obj 'x2) x2)
      (setf (slot-value obj 'y2) y2)))
  (call-next-method))

(defmethod object-config ((obj canvas-box))
  (list '(maxima::mlist)
        (canvas-object/x1 obj)
        (canvas-object/y1 obj)
        (canvas-object/x2 obj)
        (canvas-object/y2 obj)))

(defmethod draw-canvas-object (pane (obj canvas-box))
  (let ((x1 (eval-param pane (canvas-object/x1 obj)))
        (y1 (eval-param pane (canvas-object/y1 obj)))
        (x2 (eval-param pane (canvas-object/x2 obj)))
        (y2 (eval-param pane (canvas-object/y2 obj))))
    (when (and (realp x1)
               (realp y1)
               (realp x2)
               (realp y2))
      (clim:draw-rectangle* pane
                       (scale-x pane x1)
                       (scale-y pane y1)
                       (scale-x pane x2)
                       (scale-y pane y2)
                       :filled nil
                       :ink clim:+black+))))

(defmethod draw-graphic-object-info (pane (obj canvas-box))
  (format pane "(~s,~s) to (~s,~s)"
          (canvas-object/x1 obj)
          (canvas-object/y1 obj)
          (canvas-object/x2 obj)
          (canvas-object/y2 obj)))

(defun parse-colour-definition (name)
  (labels ((parse-value (s)
             (let ((result (parse-integer s :radix 16)))
               (/ result 256d0))))
   (multiple-value-bind (match strings)
       (cl-ppcre:scan-to-strings "^([0-9a-fA-F]{2})([0-9a-fA-F]{2})([0-9a-fA-F]{2})$" name)
     (if match
         (clim:make-rgb-color (parse-value (aref strings 0)) (parse-value (aref strings 1)) (parse-value (aref strings 2)))
         clim:+black+))))

(defun maxima-to-clim-colour (colour)
  (case colour
    (maxima::$red (clim:make-rgb-color 1 0 0))
    (maxima::$green (clim:make-rgb-color 0 1 0))
    (maxima::$blue (clim:make-rgb-color 0 0 1))
    (maxima::$magenta (clim:make-rgb-color 1 0 1))
    (maxima::$cyan (clim:make-rgb-color 0 1 1))
    (maxima::$yellow (clim:make-rgb-color 1 1 0))
    (maxima::$orange (clim:make-rgb-color 1 0.64 0))
    (maxima::$violet (clim:make-rgb-color 0.93 0.5 0.93))
    (maxima::$brown (clim:make-rgb-color 0.64 0.16 0.16))
    (maxima::$gray (clim:make-rgb-color 0.74 0.74 0.74))
    (maxima::$black (clim:make-rgb-color 0 0 0))
    (maxima::$white (clim:make-rgb-color 1 1 1))
    (t (clim:make-rgb-color 0 0 0))))

(defun convert-maxima-colour (name)
  (typecase name
    (symbol (maxima-to-clim-colour name))
    (string (parse-colour-definition name))
    (t clim:+black+)))

(defun canvas-button-callback (fn)
  (lambda (button)
    (let* ((frame (clim:pane-frame button))
           (pane (clim:find-pane-named frame 'canvas-pane)))
      (funcall fn pane))))

(defun reset-variables (pane)
  (setf (canvas-pane/curr-time pane) 0)
  (loop
    for param in (canvas-pane/params pane)
    do (setf (param/current-value param) (param/start-value param)))
  (redisplay-variables-list-pane))

(defun update-variables (pane)
  (let ((step-size (canvas-pane/step-size pane)))
    (incf (canvas-pane/curr-time pane) step-size)
    (loop
      for param in (canvas-pane/params pane)
      for expr = (param/expression param)
      when expr
        do (let ((new-value (eval-param pane expr)))
             (setf (param/current-value param) new-value)))))

(defun save-variables-list (pane)
  (cons '(maxima::mlist)
        (loop
          for param in (canvas-pane/params pane)
          collect (list '(maxima::mlist)
                        (param/name param)
                        (param/start-value param)
                        (param/current-value param)
                        (param/expression param)))))

(defun save-objects-list (pane)
  (cons '(maxima::mlist)
        (loop
          for obj in (canvas-pane/objects pane)
          collect (list '(maxima::mlist) (string (class-name (class-of obj))) (object-config obj)))))

(defun make-saved-canvas-state (pane)
  (list '(maxima::mlist)
        'maxima::$variables (save-variables-list pane)
        'maxima::$objects (save-objects-list pane)))

(defun clear-canvas-config (pane)
  (setf (canvas-pane/params pane) nil)
  (setf (canvas-pane/objects pane) nil))

(maxima::defmfun maxima::$canvas_save ()
  (let ((pane (find-canvas-pane)))
    (make-saved-canvas-state pane)))

(defun update-state (pane config)
  (flet ((config-value (name)
           (loop
             for (key value) on config by #'cddr
             when (eq key name)
               return value)))
    (let ((variables-config (config-value 'maxima::$variables))
          (objects-config (config-value 'maxima::$objects)))
      (clear-canvas-config pane)
      (when variables-config
        (loop
          for param-init in (maxima-list-to-list variables-config)
          do (destructuring-bind (name start-value current-value expr)
                 (maxima-list-to-list param-init)
               (let ((param (make-instance 'param
                                           :name name
                                           :start-value start-value
                                           :current-value current-value
                                           :expression expr)))
                 (add-canvas-param pane param)))))
      (when objects-config
        (loop
          with package = (find-package "MAXIMA-CLIENT.CANVAS")
          for conf in (maxima-list-to-list objects-config)
          do (destructuring-bind (class-name data)
                 (maxima-list-to-list conf)
               (let ((obj (make-instance (find-class (intern class-name package))
                                         :saved-info (maxima-list-to-list data))))
                 (push obj (canvas-pane/objects pane)))))))))

(maxima::defmfun maxima::$canvas_load (config)
  (let ((pane (find-canvas-pane))
        (config-list (maxima-list-to-list config)))
    (update-state pane config-list)))

(defun canvas-step (pane)
  (update-variables pane)
  (let ((rec-list nil))
    (labels ((recurse (rec)
               (if (typep rec 'canvas-object-record)
                   (push rec rec-list)
                   (loop
                     for child in (coerce (clim:output-record-children rec) 'list)
                     do (recurse child)))))
      (recurse (clim:stream-output-history pane))
      (let ((updated-region clim:+nowhere+))
        (loop
          for rec in rec-list
          ;;do (clim:delete-output-record rec (clim:output-record-parent rec))
          do (progn
               ;; We need to copy the rectangle here, since we'll change it later
               (clim:with-bounding-rectangle* (x1 y1 x2 y2) rec
                 (setq updated-region (clim:region-union updated-region (clim:make-rectangle* x1 y1 x2 y2))))
               (let ((children (coerce (clim:output-record-children rec) 'list)))
                 (loop
                   for child in children
                   do (clim:delete-output-record child rec)))
               (let ((new-rec (clim:with-output-to-output-record (pane 'canvas-object-record-inner)
                                (draw-canvas-object pane (clim:presentation-object rec)))))
                 (clim:add-output-record new-rec rec)
                 (setq updated-region (clim:region-union updated-region new-rec)))))
        (clim:repaint-sheet pane updated-region)))))

(defun canvas-step-one (pane)
  (canvas-step pane)
  (redisplay-variables-list-pane))

(defun schedule-next-frame (pane)
  (clime:schedule-event pane (make-instance 'clim:timer-event :sheet pane) 0.02))

(defun canvas-animate (pane)
  (with-accessors ((animation-state canvas-pane/animation-state)) pane
   (ecase animation-state
     (:stopped
      (setq animation-state :started)
      (update-animation-button-text pane)
      (schedule-next-frame pane))
     (:started
      (setq animation-state :stopping))
     (:stopping
      nil))))

(defmethod clim:handle-event ((pane canvas-pane) (event clim:timer-event))
  (canvas-step pane)
  (with-accessors ((animation-state canvas-pane/animation-state)) pane
    (ecase animation-state
      (:stopped
       nil)
      (:started
       (if (clim:sheet-grafted-p pane)
           (schedule-next-frame pane)
           (setq animation-state :stopped)))
      (:stopping
       (setq animation-state :stopped)
       (update-animation-button-text pane)
       (redisplay-variables-list-pane)))))

(defun find-canvas-pane (&key (error-p t))
  (maxima-client:find-pane-in-application-frame 'canvas-pane error-p))

(defun find-variable-list-pane (&key (error-p t))
  (maxima-client:find-pane-in-application-frame 'variables-list error-p))

(defun update-animation-button-text (pane)
  (let ((label (ecase (canvas-pane/animation-state pane)
                 ((:started :starting) "Stop Animation")
                 (:stopped "Start Animation"))))
    (let ((button (clim:find-pane-named (clim:pane-frame pane) 'animate-button)))
      (setf (clim:gadget-label button) label))))

(defun make-canvas-pane ()
  (multiple-value-bind (outer inner)
      (clim:make-clim-stream-pane :type 'canvas-pane
                                  :name 'canvas-pane
                                  :default-view +canvas-view+
                                  :display-function 'repaint-canvas
                                  :incremental-redisplay nil
                                  :display-time t
                                  :borders nil)
    (values (clim:horizontally ()
              (clim:vertically ()
                (:fill outer)
                (clim:horizontally ()
                  (clim:make-pane 'clim:push-button :label "Reset"
                                                    :activate-callback (canvas-button-callback #'reset-variables))
                  (clim:make-pane 'clim:push-button :label "Forward"
                                                    :activate-callback (canvas-button-callback #'canvas-step-one))
                  (clim:make-pane 'clim:push-button :name 'animate-button
                                                    :label "Start Animation"
                                                    :activate-callback (canvas-button-callback #'canvas-animate))))
              (clim:make-pane 'clime:box-adjuster-gadget)
              (clim:make-clim-stream-pane :type 'variables-list-pane
                                          :name 'variables-list
                                          :default-view +variables-list-view+
                                          :display-function 'repaint-variables-list
                                          :incremental-redisplay nil
                                          :display-time t
                                          :borders nil
                                          :text-margins '(:left (:absolute 2)
                                                          :right (:relative 2))
                                          :scroll-bars :both))
            inner)))

(defun compute-x-offset (pane)
  (let ((viewport (clim:pane-viewport-region pane)))
    (+ (canvas-pane/x-centre pane) (/ (clim:rectangle-width viewport) 2))))

(defun compute-y-offset (pane)
  (let ((viewport (clim:pane-viewport-region pane)))
    (+ (canvas-pane/y-centre pane) (/ (clim:rectangle-height viewport) 2))))

(defun draw-grid (pane)
  (let* ((scale (canvas-pane/scale pane))
         (viewport (clim:pane-viewport-region pane))
         (x-offset (compute-x-offset pane))
         (y-offset (compute-y-offset pane))
         (w (clim:rectangle-width viewport))
         (h (clim:rectangle-height viewport))
         (style (clim:make-text-style :sals-serif :normal :small))
         (char-height (multiple-value-bind (width height) (clim:text-size pane "M" :text-style style)
                        (declare (ignore width))
                        height)))
    (let ((spacing 100)
          (left (/ (- x-offset) scale))
          (right (/ (- w x-offset) scale))
          (top (/ y-offset scale))
          (bottom (/ (- y-offset h) scale)))
      (when (< right left)
        (rotatef left right))
      (when (< bottom top)
        (rotatef top bottom))
      (let* ((multiplier (expt 10
                               (round (log (/ (- right left)
                                              (/ w spacing))
                                           10))))
             (decimals (max (- (log multiplier 10)) 0)))
        (flet ((create-line-style (pos)
                 (append `(:ink ,clim:+black+)
                         (if (zerop pos)
                             nil
                             '(:line-thickness 1 :line-dashes (1 4) :line-cap-shape :square)))))
          (let ((start (* (ceiling (/ left multiplier)) multiplier))
                (end (* (floor (/ right multiplier)) multiplier)))
            (loop
              for pos from start to end by multiplier
              do (let ((x (scale-x pane pos))
                       (string (maxima-client::format-with-decimals pos decimals)))
                   (apply #'clim:draw-line* pane x 0 x h (create-line-style pos))
                   (clim:draw-text* pane string (+ x 5) (- h 5) :text-style style))))
          (let ((start (* (ceiling (/ top multiplier)) multiplier))
                (end (* (floor (/ bottom multiplier)) multiplier)))
            (loop
              for pos from start to end by multiplier
              do (let ((y (scale-y pane pos))
                       (string (maxima-client::format-with-decimals pos decimals)))
                   (apply #'clim:draw-line* pane 0 y w y (create-line-style pos))
                   (clim:draw-text* pane string 5 (+ y char-height 5) :text-style style)))))))))

(defun repaint-canvas (frame pane)
  (declare (ignore frame))
  (draw-grid pane)
  (loop
    for obj in (canvas-pane/objects pane)
    do (clim:with-output-as-presentation (pane obj (clim:presentation-type-of obj)
                                               :view (clim:stream-default-view pane)
                                               :allow-sensitive-inferiors t
                                               :single-box t
                                               :record-type 'canvas-object-record)
         (clim:with-new-output-record (pane 'canvas-object-record-inner)
           (draw-canvas-object pane obj)))))

(clim:define-presentation-to-command-translator update-param
    (param-presentation cmd-update-variable canvas-commands)
    (obj)
  (list obj))

#+nil
(clim:define-presentation-to-command-translator select-param-start-value
    (param-start-value-presentation cmd-update-start-value canvas-commands)
    (obj)
  (list obj))

(clim:define-command (cmd-canvas-add-circle :name "Add Circle" :menu t :command-table canvas-commands)
    ((size 'maxima-native-expr :prompt "Size")
     (x 'maxima-native-expr :prompt "X-Position")
     (y 'maxima-native-expr :prompt "Y-Position"))
  (let* ((pane (find-canvas-pane))
         (obj (make-instance 'canvas-circle
                             :x (maxima-native-expr/expr x)
                             :y (maxima-native-expr/expr y)
                             :size (maxima-native-expr/expr size))))
    (push obj (canvas-pane/objects pane))
    (redisplay-variables-list-pane)))

(clim:define-command (cmd-canvas-add-line :name "Add Line" :menu t :command-table canvas-commands)
    ((x1 'maxima-native-expr :prompt "Left")
     (y1 'maxima-native-expr :prompt "Top")
     (x2 'maxima-native-expr :prompt "Right")
     (y2 'maxima-native-expr :prompt "Bottom"))
  (let ((pane (find-canvas-pane))
        (obj (make-instance 'canvas-line
                            :x1 (maxima-native-expr/expr x1)
                            :y1 (maxima-native-expr/expr y1)
                            :x2 (maxima-native-expr/expr x2)
                            :y2 (maxima-native-expr/expr y2))))
    (push obj (canvas-pane/objects pane))
    (redisplay-variables-list-pane)))

(clim:define-command (cmd-canvas-add-box :name "Add Box" :menu t :command-table canvas-commands)
    ((x1 'maxima-native-expr :prompt "Left")
     (y1 'maxima-native-expr :prompt "Top")
     (x2 'maxima-native-expr :prompt "Right")
     (y2 'maxima-native-expr :prompt "Bottom"))
  (let ((pane (find-canvas-pane))
        (obj (make-instance 'canvas-box
                            :x1 (maxima-native-expr/expr x1)
                            :y1 (maxima-native-expr/expr y1)
                            :x2 (maxima-native-expr/expr x2)
                            :y2 (maxima-native-expr/expr y2))))
    (push obj (canvas-pane/objects pane))
    (redisplay-variables-list-pane)))

(clim:define-command (cmd-add-variable :name "Add Canvas Variable" :menu t :command-table canvas-commands)
    ((name 'maxima-native-symbol :prompt "Name")
     &key
     (initial-value 'number :prompt "Initial value")
     (expression 'maxima-native-expr :prompt "Updater expression"))
  (let* ((pane (find-canvas-pane))
         (initial (or initial-value 0))
         (param (make-instance 'param
                               :name name
                               :start-value initial
                               :current-value initial
                               :expression (if expression (maxima-native-expr/expr expression) nil))))
    (add-canvas-param pane param)
    (redisplay-variables-list-pane)))

(clim:define-command (cmd-update-expr :name "Update expression" :menu nil :command-table canvas-commands)
    ((obj 'param :prompt "Expression"))
  (let ((updated-value nil))
    (let ((stream (clim:frame-standard-input clim:*application-frame*)))
      (clim:accepting-values (stream)
        (setq updated-value (clim:accept 'maxima-native-expr
                                         :stream stream
                                         :prompt "Expression"
                                         :default (param/expression obj)))))
    (setf (param/expression obj) updated-value)
    (redisplay-variables-list-pane)))

(clim:define-command (cmd-update-step-size :name "Set step size" :menu nil :command-table canvas-commands)
    ((step-size 'number :prompt "Step size"))
  (cond ((< step-size 0)
         (format t "Step size must be positive"))
        (t
         (let ((pane (find-canvas-pane)))
           (setf (canvas-pane/step-size pane) step-size)
           (redisplay-variables-list-pane)))))

(clim:define-command (cmd-update-offset :name "Set offset" :menu nil :command-table canvas-commands)
    ((x-offset 'number :prompt "X-Offset")
     (y-offset 'number :prompt "Y-Offset"))
  (let ((pane (find-canvas-pane)))
    (setf (canvas-pane/x-centre pane) x-offset)
    (setf (canvas-pane/y-centre pane) y-offset)))

(clim:define-command (cmd-update-x-offset :name "Set X offset" :menu nil :command-table canvas-commands)
    ((x-offset 'number :prompt "X-Offset"))
  (let ((pane (find-canvas-pane)))
    (setf (canvas-pane/x-centre pane) x-offset)))

(clim:define-command (cmd-update-y-offset :name "Set Y offset" :menu nil :command-table canvas-commands)
    ((y-offset 'number :prompt "Y-Offset"))
  (let ((pane (find-canvas-pane)))
    (setf (canvas-pane/y-centre pane) y-offset)))

(clim:define-command (cmd-update-scale :name "Set scale" :menu nil :command-table canvas-commands)
    ((scale 'number :prompt "Scale"))
  (if (and (realp scale) (plusp scale))
      (let ((pane (find-canvas-pane)))
        (setf (canvas-pane/scale pane) scale))
      (format (find-interactor-pane) "Scale must be a positive real~%")))

(clim:define-command (cmd-update-start-value :name "Set Start Value" :menu nil :command-table canvas-commands)
    ((obj 'param :prompt "Param"))
  (let* ((pane (find-interactor-pane))
         (result (clim:accept 'number
                              :stream pane
                              :prompt "New value"
                              :default (param/start-value obj)
                              :insert-default t)))
    (setf (param/start-value obj) result)
    (setf (param/current-value obj) result)
    (redisplay-variables-list-pane)))

(clim:define-command (cmd-update-current-value :name "Set Curr Value" :menu nil :command-table canvas-commands)
    ((obj 'param :prompt "Param"))
  (let* ((pane (find-interactor-pane))
         (result (clim:accept 'number
                              :stream pane
                              :prompt "New value"
                              :default (param/current-value obj)
                              :insert-default t)))
    (setf (param/current-value obj) result)
    (redisplay-variables-list-pane)))

(clim:define-command (cmd-update-expression :name "Set Expression" :menu nil :command-table canvas-commands)
    ((obj 'param :prompt "Param"))
  (let* ((pane (find-interactor-pane))
         (result (clim:accept 'maxima-native-expr
                              :stream pane
                              :prompt "Expression"
                              :default (alexandria:if-let ((expression (param/expression obj)))
                                         (make-instance 'maxima-native-expr :expr expression)
                                         nil)
                              :insert-default t)))
    (setf (param/expression obj) (maxima-native-expr/expr result))
    (redisplay-variables-list-pane)))

(clim:define-command (cmd-remove-variable :name "Remove Animation Variable" :menu nil :command-table canvas-commands)
    ((obj 'param :prompt "Param"))
  (let ((pane (find-canvas-pane)))
    (setf (canvas-pane/params pane) (remove obj (canvas-pane/params pane)))
    (redisplay-variables-list-pane)))

(clim:define-command (cmd-clear-canvas-config :name "Clear Canvas Config" :menu nil :command-table canvas-commands)
    ()
  (let ((pane (find-canvas-pane)))
    (clear-canvas-config pane)
    (redisplay-variables-list-pane)))

(clim:define-command (cmd-load-canvas-config :name "Load Canvas Config" :menu nil :command-table canvas-commands)
    ((obj 'maxima-native-symbol :prompt "Variable"))
  (let ((pane (find-canvas-pane)))
    (update-state pane (maxima-list-to-list (eval-maxima-expression obj)))
    (redisplay-variables-list-pane)))

(clim:define-command (cmd-save-canvas-config :name "Save Canvas Config" :menu nil :command-table canvas-commands)
    ((obj 'maxima-native-symbol :prompt "Variable"))
  (let ((pane (find-canvas-pane)))
    (setf (symbol-value obj) (make-saved-canvas-state pane))))

(clim:make-command-table 'object-command-table
                         :errorp nil
                         :menu '(("Circle" :command cmd-canvas-add-circle)
                                 ("Line" :command cmd-canvas-add-line)
                                 ("Box" :command cmd-canvas-add-box)))

(clim:make-command-table 'maxima-canvas-command-table
                         :errorp nil
                         :menu '(("Toggle canvas" :command maxima-client::cmd-toggle-canvas)
                                 ("Change step size" :command cmd-update-step-size)
                                 ("Add dependent variable" :command cmd-add-variable)
                                 ("Add object" :menu object-command-table)))
