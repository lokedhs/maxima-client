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

#+nil
(defmethod clim:bounding-rectangle ((region canvas-object-record))
  (break)
  (clim:make-bounding-rectangle 0 0 1000 1000))

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
  (#+nil (name         :initarg :name
                 :reader canvas-object/name
                 :documentation "The name of the object. Should be a symbol in the MAXIMA package.")
   (x-position   :initarg :x-position
                 :initform (make-instance 'updateable-value :current 50)
                 :type updateable-value
                 :accessor canvas-object/x-position)
   (y-position   :initarg :y-position
                 :initform (make-instance 'updateable-value :current 50)
                 :type updateable-value
                 :accessor canvas-object/y-position)))

(defgeneric recalculate-object-content (obj))

(defmethod recalculate-object-content ((obj canvas-object))
  (update-value (canvas-object/x-position obj))
  (update-value (canvas-object/y-position obj)))

#+nil
(defgeneric canvas-object-bounds (obj))

(defclass canvas-circle (canvas-object)
  ((size :initarg :size
         :initform (make-instance 'updateable-value :current 5)
         :type updateable-value
         :accessor canvas-circle/size)))

#+nil
(defmethod canvas-object-bounds ((obj canvas-circle))
  (let ((size (updateable-value/current (canvas-circle/size obj)))
        (x (updateable-value/current (canvas-object/x-position obj)))
        (y (updateable-value/current (canvas-object/y-position obj))))
    (list (- x size) (- y size) (+ x size) (+ y size))))

#+nil
(defmethod initialize-instance :after ((obj canvas-object-record) &key)
  (setf (clim:rectangle-edges* obj) (apply #'values (canvas-object-bounds (clim:presentation-object obj)))))

(defmethod recalculate-object-content :after ((obj canvas-circle))
  (update-value (canvas-circle/size obj)))

#+nil
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

(defmethod draw-canvas-object (pane (obj canvas-circle))
  #+nil
  (clim:with-output-as-presentation (pane obj 'canvas-circle
                                          :view (clim:stream-default-view pane)
                                          :allow-sensitive-inferiors t))
  (clim:draw-circle* pane
                     (updateable-value/current (canvas-object/x-position obj))
                     (updateable-value/current (canvas-object/y-position obj))
                     (updateable-value/current (canvas-circle/size obj))
                     :filled nil
                     :ink clim:+black+))

#+nil
(clim:define-presentation-method clim:present (obj (type canvas-circle) stream (view canvas-view) &key)
  (clim:draw-circle* stream
                     (updateable-value/current (canvas-object/x-position obj))
                     (updateable-value/current (canvas-object/y-position obj))
                     (updateable-value/current (canvas-circle/size obj))
                     :filled nil
                     :ink clim:+black+))

#+nil
(defmethod clim:replay-output-record ((rec canvas-object-record) stream &optional region x-offset y-offset)
  (declare (ignore region x-offset y-offset))
  (draw-canvas-object stream (clim:presentation-object rec)))

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

#+nil
(defun draw-canvas-object (pane obj)
  (unless (and (listp obj)
               (eq (caar obj) 'maxima::mlist))
    (error "Canvas object is not a list"))
  (let ((x nil)
        (y nil)
        (colour 'maxima::$black)
        (size 5)
        (filled nil))
    (loop
      for (key val) on (cdr obj) by #'cddr
      do (case key
           (maxima::$x (setq x (eval-maxima-expression-to-float val)))
           (maxima::$y (setq y (eval-maxima-expression-to-float val)))
           (maxima::$colour (setq colour (eval-maxima-expression val)))
           (maxima::$size (setq size (eval-maxima-expression-to-float val 5)))
           (maxima::$filled (setq filled (if (eval-maxima-expression val) t nil))))
      finally (return (values x y colour)))
    (when (and (realp x) (realp y))
      (clim:draw-circle* pane x y (max size 1) :filled filled :ink (convert-maxima-colour colour)))))

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

(defun canvas-step (pane)
  (recalculate-all-objects pane)
  #+nil(clim:repaint-sheet pane clim:+everywhere+)
  #+nil(clim:redisplay-frame-pane (clim:pane-frame pane) pane)
  #+nil(clim:replay-output-record (clim:stream-output-history pane) pane)
  #+nil(loop
         for obj in (canvas-pane/objects pane)
         do (clim:redisplay-output-record))
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
               (setq updated-region (clim:region-union updated-region rec))
               ;; This output record should have only a single child
               ;; which is of type canvas-object-record-inner
               (let ((children (clim:output-record-children rec)))
                 (assert (alexandria:sequence-of-length-p children 1))
                 (let ((child (aref children 0)))
                   (assert (typep child 'canvas-object-record-inner))
                   (clim:delete-output-record child rec)
                   (let ((new-rec (clim:with-output-to-output-record (pane 'canvas-object-record-inner)
                                    (draw-canvas-object pane (clim:presentation-object rec)))))
                     (clim:add-output-record new-rec rec)
                     (setq updated-region (clim:region-union updated-region new-rec)))))))
        (clim:repaint-sheet pane updated-region)
        ;; TODO: Copied from REPAINT-CANVAS
        #+nil
        (loop
          for obj in (canvas-pane/objects pane)
          do (clim:with-output-as-presentation (pane obj (clim:presentation-type-of obj)
                                                     :view (clim:stream-default-view pane)
                                                     :allow-sensitive-inferiors t
                                                     :single-box t
                                                     :record-type 'canvas-object-record)
               (draw-canvas-object pane obj)))))))

#+nil
(defun add-object (pane name)
  (with-accessors ((objects canvas-pane/objects)) pane
    (let ((updated-objects (remove name objects :key #'canvas-object/name :test #'eq)))
      (setq objects (cons (make-instance 'canvas-object :name name) updated-objects))
      (clim:redisplay-frame-pane (clim:pane-frame pane) pane))))

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
                                                  :activate-callback (canvas-button-callback name #'canvas-step))))
            inner)))

#+nil
(defun maxima-array-p (ary)
  (or (maxima::mget ary 'maxima::hashar)
      (maxima::mget ary 'maxima::array)
      (arrayp ary)
      (hash-table-p ary)
      (eq (maxima::marray-type ary) 'maxima::$functional)))

(defun repaint-canvas (frame pane)
  (declare (ignore frame))
  #+nil
  (let ((content-ex (canvas-pane/content pane)))
    (when (maxima-array-p content-ex)
      (clim:with-translation (pane 100 100)
        (let ((content (maxima::$listarray content-ex)))
          (unless (and (listp content)
                       (eq (caar content) 'maxima::mlist))
            (error "Array content is not a maxima list"))
          (loop
            for obj in (cdr content)
            do (draw-canvas-object pane obj))))))
  #+nil
  (loop
    for obj in (canvas-pane/objects pane)
    collect (let ((rec (clim:with-output-to-output-record (pane)
                         (clim:stream-present pane obj (clim:presentation-type-of obj)))))
              (clim:stream-add-output-record pane rec)
              rec))
  #+nil
  (loop
    for obj in (canvas-pane/objects pane)
    do (clim:with-output-as-presentation (pane obj (clim:presentation-type-of obj)
                                               :view (clim:stream-default-view pane)
                                               :allow-sensitive-inferiors t
                                               :single-box t
                                               :record-type 'canvas-object-record)))
  (loop
    ;;with parent = (clim:stream-output-history pane)
    for obj in (canvas-pane/objects pane)
    do (clim:with-output-as-presentation (pane obj (clim:presentation-type-of obj)
                                               :view (clim:stream-default-view pane)
                                               :allow-sensitive-inferiors t
                                               :single-box t
                                               :record-type 'canvas-object-record)
         (let ((rec (clim:with-output-to-output-record (pane 'canvas-object-record-inner)
                      (draw-canvas-object pane obj))))
           (clim:draw-line* pane 0 0 100 50)
           (clim:stream-add-output-record pane rec)))
    #+nil (clim:add-output-record (make-instance 'canvas-object-record
                                                 :object obj
                                                 :type (clim:presentation-type-of obj)
                                                 :view (clim:stream-default-view pane)
                                                 ;;:parent parent
                                                 )
                                  parent)
    #+nil
     (clim:stream-present pane obj (clim:presentation-type-of obj)
                          :record-type canvas-object-record))
  #+nil
  (loop
    for obj in (canvas-pane/objects pane)
    do ()))

#+nill
(maxima::defmfun maxima::$canvas_add_object (name)
  (unless (symbolp name)
    (maxima::merror "Name is not a symbol: ~M" name))
  (log:info "Adding sym: ~s" name)
  (add-object (maxima-client::find-canvas-pane) name))

#+nil
(maxima::defmfun maxima::$canvas_step ()
  (canvas-step (maxima-client::find-canvas-pane)))

(clim:define-command (cmd-canvas-add-circle :name "Add Circle" :menu t :command-table canvas-commands)
    ((size 'maxima-native-expr :prompt "Size")
     (x 'maxima-native-expr :prompt "X-Position")
     (y 'maxima-native-expr :prompt "Y-Position"))
  (let ((pane (maxima-client::find-canvas-pane)))
    #+nil
    (push (make-instance 'canvas-circle
                         :x-position (make-updateable-value (maxima-native-expr/expr x))
                         :y-position (make-updateable-value (maxima-native-expr/expr y))
                         :size (make-updateable-value (maxima-native-expr/expr size)))
          (canvas-pane/objects pane))
    (let ((obj (make-instance 'canvas-circle
                              :x-position (make-updateable-value (maxima-native-expr/expr x))
                              :y-position (make-updateable-value (maxima-native-expr/expr y))
                              :size (make-updateable-value (maxima-native-expr/expr size)))))
      #+nil (progn
              (clim:present obj (clim:presentation-type-of obj) :stream pane)
              (push obj (canvas-pane/objects pane)))
      (push obj (canvas-pane/objects pane))
      #+nil
      (clim:add-output-record (make-instance 'canvas-object-record :canvas-object obj)))))
