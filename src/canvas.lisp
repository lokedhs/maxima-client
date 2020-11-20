(in-package :maxima-client.canvas)

(clim:define-command-table canvas-commands)

(defclass canvas-view ()
  ())

(defvar +canvas-view+ (make-instance 'canvas-view))

(defclass canvas-object-record (clim:standard-presentation)
  (#+nil(canvas-object :initarg :canvas-object
                       :reader canvas-object-record/canvas-object)))

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

(defclass canvas-object ()
  ())

(defgeneric draw-canvas-object (pane obj))

(defclass canvas-object-position-mixin ()
  ((x-position   :initarg :x-position
                 :initform (make-instance 'updateable-value :current 50)
                 :type updateable-value
                 :reader canvas-object/x-position)
   (y-position   :initarg :y-position
                 :initform (make-instance 'updateable-value :current 50)
                 :type updateable-value
                 :reader canvas-object/y-position)))

(defgeneric recalculate-object-content (obj)
  (:method (obj)
    ;; Do nothing
    ))

(defmethod recalculate-object-content :after ((obj canvas-object-position-mixin))
  (update-value (canvas-object/x-position obj))
  (update-value (canvas-object/y-position obj)))

(defclass canvas-circle (canvas-object canvas-object-position-mixin)
  ((size :initarg :size
         :initform (make-instance 'updateable-value :current 5)
         :type updateable-value
         :accessor canvas-circle/size)))

(defmethod recalculate-object-content :after ((obj canvas-circle))
  (update-value (canvas-circle/size obj)))

(defmethod draw-canvas-object (pane (obj canvas-circle))
  (clim:draw-circle* pane
                     (updateable-value/current (canvas-object/x-position obj))
                     (updateable-value/current (canvas-object/y-position obj))
                     (updateable-value/current (canvas-circle/size obj))
                     :filled nil
                     :ink clim:+black+))

(defclass canvas-line (canvas-object)
  ((x1 :initarg :x1
       :initform (make-instance 'updateable-value)
       :type updateable-value
       :reader canvas-line/x1)
   (y1 :initarg :y1
       :initform (make-instance 'updateable-value)
       :type updateable-value
       :reader canvas-line/y1)
   (x2 :initarg :x2
       :initform (make-instance 'updateable-value)
       :type updateable-value
       :reader canvas-line/x2)
   (y2 :initarg :y2
       :initform (make-instance 'updateable-value)
       :type updateable-value
       :reader canvas-line/y2)))

(defmethod recalculate-object-content :after ((obj canvas-line))
  (update-value (canvas-line/x1 obj))
  (update-value (canvas-line/y1 obj))
  (update-value (canvas-line/x2 obj))
  (update-value (canvas-line/y2 obj)))

(defmethod draw-canvas-object (pane (obj canvas-line))
  (clim:draw-line* pane
                   (updateable-value/current (canvas-line/x1 obj))
                   (updateable-value/current (canvas-line/y1 obj))
                   (updateable-value/current (canvas-line/x2 obj))
                   (updateable-value/current (canvas-line/y2 obj))
                   :ink clim:+black+))

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

(defclass canvas-pane (clim:clim-stream-pane)
  ((objects :initarg :objects
            :initform nil
            :accessor canvas-pane/objects
            :documentation "A list of objects on the canvas")))

(defun canvas-button-callback (name fn)
  (lambda (button)
    (let* ((frame (clim:pane-frame button))
           (pane (clim:find-pane-named frame name)))
      (funcall fn pane))))

(defun recalculate-all-objects (pane)
  (mapc #'recalculate-object-content (canvas-pane/objects pane)))

(defvar *timevalue* 0)
(defvar *step-size* 0.01)
(defvar *animation-state* :stopped)

(defun canvas-step (pane)
  (incf *timevalue* *step-size*)
  (when (maxima::mget 'maxima::$step_function 'maxima::mexpr)
    (maxima::mfuncall 'maxima::$step_function *timevalue* *step-size*))
  (recalculate-all-objects pane)
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

(defun schedule-next-frame (pane)
  (clim-internals::schedule-timer-event pane 'animation-step 0.02))

(defun canvas-animate (pane)
  (ecase *animation-state*
    (:stopped
     (setq *animation-state* :started)
     (schedule-next-frame pane))
    (:started
     (setq *animation-state* :stopping))
    (:stopping
     nil)))

(defmethod clim:handle-event ((pane canvas-pane) (event clim:timer-event))
  (canvas-step pane)
  (ecase *animation-state*
    (:stopped
     nil)
    (:started
     (if (clim:sheet-grafted-p pane)
         (schedule-next-frame pane)
         (setq *animation-state* :stopped)))
    (:stopping
     (setq *animation-state* :stopped))))

(defun make-canvas-pane (name)
  (multiple-value-bind (outer inner)
      (clim:make-clim-stream-pane :type 'canvas-pane
                                  :name name
                                  :default-view +canvas-view+
                                  :display-function 'repaint-canvas
                                  :incremental-redisplay nil
                                  :display-time t
                                  :borders t)
    (values (clim:vertically ()
              (:fill outer)
              (clim:horizontally ()
                (clim:make-pane 'clim:push-button :label "Reset")
                (clim:make-pane 'clim:push-button :label "Forward"
                                                  :activate-callback (canvas-button-callback name #'canvas-step))
                (clim:make-pane 'clim:push-button :label "Animate"
                                                  :activate-callback (canvas-button-callback name #'canvas-animate))))
            inner)))

(defun repaint-canvas (frame pane)
  (declare (ignore frame))
  (recalculate-all-objects pane)
  (loop
    for obj in (canvas-pane/objects pane)
    do (clim:with-output-as-presentation (pane obj (clim:presentation-type-of obj)
                                               :view (clim:stream-default-view pane)
                                               :allow-sensitive-inferiors t
                                               :single-box t
                                               :record-type 'canvas-object-record)
         (clim:with-new-output-record (pane 'canvas-object-record-inner)
           (draw-canvas-object pane obj)))))

(clim:define-command (cmd-canvas-add-circle :name "Add Circle" :menu t :command-table canvas-commands)
    ((size 'maxima-native-expr :prompt "Size")
     (x 'maxima-native-expr :prompt "X-Position")
     (y 'maxima-native-expr :prompt "Y-Position"))
  (let* ((pane (maxima-client::find-canvas-pane))
         (obj (make-instance 'canvas-circle
                             :x-position (make-updateable-value (maxima-native-expr/expr x))
                             :y-position (make-updateable-value (maxima-native-expr/expr y))
                             :size (make-updateable-value (maxima-native-expr/expr size)))))
    (push obj (canvas-pane/objects pane))))

(clim:define-command (cmd-canvas-add-line :name "Add Line" :menu t :command-table canvas-commands)
    ((x1 'maxima-native-expr :prompt "Left")
     (y1 'maxima-native-expr :prompt "Top")
     (x2 'maxima-native-expr :prompt "Right")
     (y2 'maxima-native-expr :prompt "Bottom"))
  (let* ((pane (maxima-client::find-canvas-pane))
         (obj (make-instance 'canvas-line
                             :x1 (make-updateable-value (maxima-native-expr/expr x1))
                             :y1 (make-updateable-value (maxima-native-expr/expr y1))
                             :x2 (make-updateable-value (maxima-native-expr/expr x2))
                             :y2 (make-updateable-value (maxima-native-expr/expr y2)))))
    (push obj (canvas-pane/objects pane))))
