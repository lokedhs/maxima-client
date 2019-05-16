(in-package :maxima-client.canvas)

(defclass canvas-view ()
  ())

(defvar +canvas-view+ (make-instance 'canvas-view))

#+nil
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

#+nil
(defgeneric draw-canvas-object (pane obj))

#+nil
(defmethod draw-canvas-object (pane (obj canvas-object))
  (clim:draw-circle* pane
                     (canvas-object/x-position obj)
                     (canvas-object/x-position obj)
                     5
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

(defclass canvas-pane (clim:application-pane)
  ((content :initarg :content
                     :reader canvas-pane/content
                     :documentation "A maxima variable holding the canvas data")))

(defun canvas-button-callback (name fn)
  (lambda (button)
    (let* ((frame (clim:pane-frame button))
           (pane (clim:find-pane-named frame name)))
      (funcall fn pane))))

(defun canvas-step (pane)
  (log:info "step = ~s" pane))

(defun add-object (pane name)
  (declare (ignore pane name))
  #+nil
  (with-accessors ((objects canvas-pane/objects)) pane
    (let ((updated-objects (remove name objects :key #'canvas-object/name :test #'eq)))
      (setq objects (cons (make-instance 'canvas-object :name name) updated-objects))
      (clim:redisplay-frame-pane (clim:pane-frame pane) pane))))

(defun make-canvas-pane (name)
  (clim:vertically ()
    (clim:make-clim-stream-pane :type 'canvas-pane
                                :content 'maxima::$canvas
                                :name name
                                :default-view +canvas-view+
                                :display-function 'repaint-canvas
                                :incremental-redisplay nil
                                :display-time t)
    (clim:horizontally ()
      (clim:make-pane 'clim:push-button :label "Reset")
      (clim:make-pane 'clim:push-button :label "Forward" :activate-callback (canvas-button-callback name #'canvas-step)))))

(defun maxima-array-p (ary)
  (or (maxima::mget ary 'maxima::hashar)
      (maxima::mget ary 'maxima::array)
      (arrayp ary)
      (hash-table-p ary)
      (eq (maxima::marray-type ary) 'maxima::$functional)))

(defun repaint-canvas (frame pane)
  (declare (ignore frame))
  (let ((content-ex (canvas-pane/content pane)))
    (when (maxima-array-p content-ex)
      (clim:with-translation (pane 100 100)
        (let ((content (maxima::$listarray content-ex)))
          (unless (and (listp content)
                       (eq (caar content) 'maxima::mlist))
            (error "Array content is not a maxima list"))
          (loop
            for obj in (cdr content)
            do (draw-canvas-object pane obj)))))))

(maxima::defmfun maxima::$canvas_add_object (name)
  (unless (symbolp name)
    (maxima::merror "Name is not a symbol: ~M" name))
  (log:info "Adding sym: ~s" name)
  (add-object (maxima-client::find-canvas-pane) name))

(maxima::defmfun maxima::$canvas_step ()
  (canvas-step (maxima-client::find-canvas-pane)))
