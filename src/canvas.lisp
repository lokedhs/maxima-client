(in-package :maxima-client.canvas)

(clim:define-command-table canvas-commands)

(defclass canvas-view ()
  ())

(defvar +canvas-view+ (make-instance 'canvas-view))

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

(defun eval-param (pane expr)
  (check-type pane canvas-pane)
  (let ((wrapped (alexandria:if-let ((params (canvas-pane/params pane)))
                   `((maxima::$at)
                     ,expr
                     ((maxima::mlist)
                      ,@(loop
                          for p in params
                          collect `((maxima::mequal) (param/name p) (param/current-value p)))))
                   expr)))
    (maxima::meval wrapped)))

(defclass canvas-pane (clim:clim-stream-pane)
  ((objects :initarg :objects
            :initform nil
            :accessor canvas-pane/objects
            :documentation "A list of objects on the canvas")
   (params  :initarg :params
            :initform nil
            :accessor canvas-pane/params
            :documentation "A list of dependent variables")))

(defclass variables-list-pane (clim:clim-stream-pane)
  ())

(defun repaint-variables-list (frame pane)
  (declare (ignore frame))
  (break)
  (let ((canvas-pane (maxima-client::find-canvas-pane)))
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
            (format pane "Current"))))
      (loop
        for param in (canvas-pane/params canvas-pane)
        for symbol-expr = (make-instance 'maxima-native-expr :expr (param/name param))
        do (clim:formatting-row (pane)
             (clim:formatting-cell (pane)
               (maxima-client:render-maxima-native-expr-toplevel pane symbol-expr))
             (clim:formatting-cell (pane)
               (format pane "~f" (param/start-value param)))
             (clim:formatting-cell (pane)
               (format pane "~f" (param/current-value param))))))))

(defun add-canvas-param (pane param)
  (check-type pane canvas-pane)
  (check-type param param)
  (setf (canvas-pane/params pane)
        (append (remove (param/name param) (canvas-pane/params pane))
                (list param))))

(defclass canvas-object ()
  ())

(defgeneric draw-canvas-object (pane obj))

(defclass canvas-object-position-mixin ()
  ((x-position   :initarg :x
                 :initform (alexandria:required-argument :x)
                 :reader canvas-object/x)
   (y-position   :initarg :y
                 :initform (alexandria:required-argument :y)
                 :reader canvas-object/y)))

(defclass canvas-circle (canvas-object canvas-object-position-mixin)
  ((size :initarg :size
         :initform (make-instance 'updateable-value :current 5)
         :type updateable-value
         :accessor canvas-circle/size)))

(defmethod draw-canvas-object (pane (obj canvas-circle))
  (clim:draw-circle* pane
                     (eval-param pane (canvas-object/x obj))
                     (eval-param pane (canvas-object/y obj))
                     (eval-param pane (canvas-circle/size obj))
                     :filled nil
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

(defun canvas-button-callback (name fn)
  (lambda (button)
    (let* ((frame (clim:pane-frame button))
           (pane (clim:find-pane-named frame name)))
      (funcall fn pane))))

(defvar *animation-state* :stopped)

(defun canvas-step (pane)
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
    (values (clim:horizontally ()
              (clim:vertically ()
                (:fill outer)
                (clim:horizontally ()
                  (clim:make-pane 'clim:push-button :label "Reset")
                  (clim:make-pane 'clim:push-button :label "Forward"
                                                    :activate-callback (canvas-button-callback name #'canvas-step))
                  (clim:make-pane 'clim:push-button :label "Animate"
                                                    :activate-callback (canvas-button-callback name #'canvas-animate))))
              (clim:make-pane 'clime:box-adjuster-gadget)
              (clim:make-clim-stream-pane :type 'variables-list-pane
                                          :name 'variables-list
                                          :display-function 'repaint-variables-list
                                          :incremental-redisplay nil
                                          :display-time t
                                          :borders t))
            inner)))

(defun repaint-canvas (frame pane)
  (declare (ignore frame))
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
                             :x (maxima-native-expr/expr x)
                             :y (maxima-native-expr/expr y)
                             :size (make-updateable-value (maxima-native-expr/expr size)))))
    (push obj (canvas-pane/objects pane))))

(clim:define-command (cmd-add-variable :name "Add animation variable" :menu t :command-table canvas-commands)
    ((name 'maxima-native-symbol :prompt "Name")
     &key
     (initial-value 'number :prompt "Initial value")
     (expression 'maxima-native-expr :prompt "Updater expression"))
  (let* ((pane (maxima-client::find-canvas-pane))
         (initial (or initial-value 0))
         (param (make-instance 'param
                               :name name
                               :start-value initial
                               :current-value initial
                               :expression (if expression (maxima-native-expr/expr expression) initial))))
    (add-canvas-param pane param)))
