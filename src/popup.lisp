(in-package :maxima-client)

(defclass popup-selector-view (clim:textual-view)
  ())

(defvar +popup-selector-view+ (make-instance 'popup-selector-view))

(clim:define-application-frame popup-selector ()
  ((values   :initarg :values
             :reader popup-selector/values)
   (selected :initform 0
             :accessor popup-selector/selected)
   (returned :initform nil
             :accessor popup-selector/returned))
  (:panes (content :application
                   :default-view +popup-selector-view+
                   :display-function 'display-popup-content
                   :scroll-bars :vertical
                   :incremental-redisplay t))
  (:layouts (default (clim:vertically ()
                       content))))

(defmethod clim-extensions:find-frame-type ((frame popup-selector))
  :dialog)

(defgeneric draw-popup-value (stream value selected))

(defmethod draw-popup-value (stream (value string) selected)
  (clim:draw-text* stream value 0 0 :ink (if selected clim:+green+ clim:+black+)))

(defun display-popup-content (frame stream)
  (clim:updating-output (stream)
    (loop
      with sel = (popup-selector/selected frame)
      for value in (popup-selector/values frame)
      for i from 0
      for element-selected = (eql i sel)
      do (clim:updating-output (stream :unique-id i
                                       :cache-value (list i element-selected)
                                       :cache-test #'equal)
           (clim:with-room-for-graphics (stream :first-quadrant nil)
             (draw-popup-value stream value element-selected))))))

(define-popup-selector-command (selector-close :name "Close" :keystroke (:escape))
    ()
  (setf (popup-selector/returned clim:*application-frame*) nil)
  (clim:frame-exit clim:*application-frame*))

(defun adjust-selected-value (frame delta)
  (with-accessors ((sel popup-selector/selected)
                   (values popup-selector/values))
      frame
    (setf sel (min (max (+ sel delta) 0) (1- (length values))))
    (clim:redisplay-frame-pane frame (clim:find-pane-named frame 'content))))

(define-popup-selector-command (selector-select :name "Select" :keystroke (:newline))
    ()
  (let ((frame clim:*application-frame*))
    (setf (popup-selector/returned frame)
          (nth (popup-selector/selected frame) (popup-selector/values frame)))
    (clim:frame-exit frame)))

(define-popup-selector-command (selector-next-value :name "Next value" :keystroke (:down))
    ()
  (adjust-selected-value clim:*application-frame* 1))

(define-popup-selector-command (selector-previous-value :name "Previous value" :keystroke (:up))
    ()
  (adjust-selected-value clim:*application-frame* -1))

(defun test-popup-selector ()
  (let ((frame (clim:make-application-frame 'popup-selector
                                            :values '("Foo" "Bar" "Test value" "Another row" "Test test")
                                            :width 200
                                            :height 200
                                            :x 100
                                            :y 400)))
    (clim:run-frame-top-level frame)
    (popup-selector/returned frame)))
