(in-package :maxima-client.canvas)

(defclass canvas-view ()
  ())

(defvar +canvas-view+ (make-instance 'canvas-view))

(defclass canvas-object ()
  ((name         :initarg :name
                 :reader canvas-object/name
                 :documentation "The name of the object. Should be a symbol in the MAXIMA package.")
   (x-position   :initarg :x-position
                 :initform 0
                 :accessor canvas-object/x-position)
   (y-position   :initarg :y-position
                 :initform 0
                 :accessor canvas-object/y-position)
   (x-expression :initarg :x-expression
                 :initform nil
                 :accessor canvas-object/x-expression)
   (y-expression :initarg :y-expression
                 :initform nil
                 :accessor canvas-object/y-expression)))

(defmethod initialize-instance :after ((obj canvas-object) &key)
  (let* ((name (canvas-object/name obj))
         (s (symbol-name name))
         (n (if (and (plusp (length s))
                     (eql (aref s 0) #\$))
                (maxima::print-invert-case name)
                (error "Name is not a maxima symbol"))))
    (macrolet ((make-coordinate-sym (coord-name prefix)
                 `(let ((expr (slot-value obj ',coord-name)))
                    (unless expr
                      (setf (slot-value obj ',coord-name)
                            (maxima::implode (coerce (format nil "$~a_~a" ,prefix (subseq n 1)) 'list)))))))
      (make-coordinate-sym x-expression "x")
      (make-coordinate-sym y-expression "y"))))

(defgeneric draw-canvas-object (pane obj))

(defmethod draw-canvas-object (pane (obj canvas-object))
  (clim:draw-circle* pane
                     (canvas-object/x-position obj)
                     (canvas-object/x-position obj)
                     5
                     :filled nil
                     :ink clim:+black+))

(defclass canvas-pane (clim:application-pane)
  ((objects :initform nil
            :accessor canvas-pane/objects
            :documentation "List of objects that are displayed on the canvas")))

(defun canvas-button-callback (name fn)
  (lambda (button)
    (let* ((frame (clim:pane-frame button))
           (pane (clim:find-pane-named frame name)))
      (funcall fn pane))))

(defun canvas-step (pane)
  (log:info "step = ~s" pane)
  (loop
    for obj in (canvas-pane/objects pane)
    do (let ((expr (canvas-object/x-expression obj)))
         ;; TODO: evaluate the expr here, or maybe remove this whole thing and replace with a better api
         )))

(defun add-object (pane name)
  (with-accessors ((objects canvas-pane/objects)) pane
    (let ((updated-objects (remove name objects :key #'canvas-object/name :test #'eq)))
      (setq objects (cons (make-instance 'canvas-object :name name) updated-objects))
      (clim:redisplay-frame-pane (clim:pane-frame pane) pane))))

(defun make-canvas-pane (name)
  (clim:vertically ()
    (clim:make-clim-stream-pane :type 'canvas-pane
                                :name name
                                :default-view +canvas-view+
                                :display-function 'repaint-canvas
                                :incremental-redisplay t)
    (clim:horizontally ()
      (clim:make-pane 'clim:push-button :label "Reset")
      (clim:make-pane 'clim:push-button :label "Forward" :activate-callback (canvas-button-callback name #'canvas-step)))))

(defun repaint-canvas (frame pane)
  (declare (ignore frame))
  (clim:with-translation (pane 100 100)
    (loop
      for obj in (canvas-pane/objects pane)
      do (draw-canvas-object pane obj))))

(maxima::defmfun maxima::$canvas_add_object (name)
  (unless (symbolp name)
    (maxima::merror "Name is not a symbol: ~M" name))
  (log:info "Adding sym: ~s" name)
  (add-object (maxima-client::find-canvas-pane) name))

(maxima::defmfun maxima::$canvas_step ()
  (canvas-step (maxima-client::find-canvas-pane)))
