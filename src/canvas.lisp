(in-package :maxima-client.canvas)

(clim:define-command-table canvas-commands)

(defclass canvas-view ()
  ())

(defvar +canvas-view+ (make-instance 'canvas-view))

(defclass variables-list-view ()
  ())

(defvar +variables-list-view+ (make-instance 'variables-list-view))

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

(clim:define-presentation-type param-start-value-presentation ())
(clim:define-presentation-type param-presentation ())

(defun eval-param (pane expr)
  (check-type pane canvas-pane)
  (let ((wrapped (alexandria:if-let ((params (canvas-pane/params pane)))
                   `((maxima::$at)
                     ,expr
                     ((maxima::mlist)
                      ,@(loop
                          for p in params
                          collect `((maxima::mequal) ,(param/name p) ,(param/current-value p)))))
                   expr)))
    (maxima::meval wrapped)))

(defclass canvas-pane (clim:clim-stream-pane)
  ((objects   :initarg :objects
              :initform nil
              :accessor canvas-pane/objects
              :documentation "A list of objects on the canvas")
   (params    :initarg :params
              :initform nil
              :accessor canvas-pane/params
              :documentation "A list of dependent variables")
   (curr-time :initform 0
              :accessor canvas-pane/curr-time)
   (step-size :initform 0.00001
              :accessor canvas-pane/step-size)))

(defclass variables-list-pane (clim:clim-stream-pane)
  ())

(defgeneric draw-graphic-object-info (pane object))

(defun redisplay-variables-list-pane ()
  (let ((pane (find-variable-list-pane)))
    (setf (clim:pane-needs-redisplay pane) t)
    (clim:redisplay-frame-pane (clim:pane-frame pane) pane)))

(defun repaint-variables-list (frame pane)
  (declare (ignore frame))
  (let ((canvas-pane (find-canvas-pane)))
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
        for symbol-expr = (make-instance 'maxima-native-expr :expr (param/name param))
        do (clim:formatting-row (pane)
             (clim:formatting-cell (pane)
               (maxima-client:render-maxima-native-expr-toplevel pane symbol-expr))
             (clim:formatting-cell (pane)
               (clim:with-output-as-presentation (pane param 'param-start-value-presentation)
                 (format pane "~f" (param/start-value param))))
             (clim:formatting-cell (pane)
               (alexandria:when-let ((v (param/current-value param)))
                 (clim:with-text-size (pane :large)
                   (format pane "~f" v))))
             (clim:formatting-cell (pane)
               (alexandria:when-let ((expr (param/expression param)))
                 (maxima-client:render-maxima-native-expr-toplevel pane (make-instance 'maxima-native-expr :expr expr))))
             (clim:formatting-cell (pane)
               (clim:with-output-as-presentation (pane param 'param-presentation)
                 (format pane "Edit"))))))
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
                (list param)))
  (redisplay-variables-list-pane))

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
         :initform 5
         :accessor canvas-circle/size)))

(defmethod draw-canvas-object (pane (obj canvas-circle))
  (let ((x (eval-param pane (canvas-object/x obj)))
        (y (eval-param pane (canvas-object/y obj)))
        (size (eval-param pane (canvas-circle/size obj))))
    (when (and (realp x)
               (realp y)
               (realp size))
      (clim:draw-circle* pane x y size :filled nil :ink clim:+black+))))

(defmethod draw-graphic-object-info (pane (obj canvas-circle))
  (format pane "Position: ~a,~a, size: ~a"
          (canvas-object/x obj)
          (canvas-object/y obj)
          (canvas-circle/size obj)))

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

(defun find-canvas-pane (&key (error-p t))
  (maxima-client:find-pane-in-application-frame 'canvas-pane error-p))

(defun find-variable-list-pane (&key (error-p t))
  (maxima-client:find-pane-in-application-frame 'variables-list error-p))

(defun make-canvas-pane ()
  (multiple-value-bind (outer inner)
      (clim:make-clim-stream-pane :type 'canvas-pane
                                  :name 'canvas-pane
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
                                                    :activate-callback (canvas-button-callback 'canvas-pane #'canvas-step))
                  (clim:make-pane 'clim:push-button :label "Animate"
                                                    :activate-callback (canvas-button-callback 'canvas-pane #'canvas-animate))))
              (clim:make-pane 'clime:box-adjuster-gadget)
              (clim:make-clim-stream-pane :type 'variables-list-pane
                                          :name 'variables-list
                                          :default-view +variables-list-view+
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

(clim:define-command (cmd-add-variable :name "Add animation variable" :menu t :command-table canvas-commands)
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
    (add-canvas-param pane param)))

#+nil
(clim:define-command (cmd-update-start-value :name "Update start value" :menu nil :command-table canvas-commands)
    ((obj 'param :prompt "Variable"))
  (let ((updated-value nil))
    (let ((stream (clim:frame-standard-input clim:*application-frame*)))
      (clim:accepting-values (stream)
        (setq updated-value (clim:accept 'number :stream stream :prompt "Updated value" :default (param/start-value obj)))))
    (setf (param/start-value obj) updated-value)
    (redisplay-variables-list-pane)))

#+nil
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

(clim:define-command (cmd-update-variable :name "Update variable" :menu nil :command-table canvas-commands)
    ((obj 'param :prompt "Expression"))
  (let ((updated-start-value nil)
        (updated-current-value nil)
        (updated-expr nil))
    (let ((stream (clim:frame-standard-input clim:*application-frame*)))
      (clim:accepting-values (stream)
        (format stream "~%")
        (setq updated-start-value (clim:accept 'number
                                               :stream stream
                                               :prompt "Initial value"
                                               :default (param/start-value obj)))
        (format stream "~%")
        (setq updated-current-value (clim:accept 'number
                                                 :stream stream
                                                 :prompt "Current value"
                                                 :default (param/current-value obj)))
        (format stream "~%")
        (setq updated-expr (clim:accept 'maxima-native-expr
                                         :stream stream
                                         :prompt "Expression"
                                         :default (param/expression obj)))))
    (setf (param/start-value obj) updated-start-value)
    (setf (param/current-value obj) updated-current-value)
    (setf (param/expression obj) (if updated-expr (maxima-native-expr/expr updated-expr) nil))
    (redisplay-variables-list-pane)))

(clim:make-command-table 'maxima-canvas-command-table
                         :errorp nil
                         :menu '(("Toggle canvas" :command maxima-client::cmd-toggle-canvas)
                                 ("Add dependent variable" :command cmd-add-variable)))
