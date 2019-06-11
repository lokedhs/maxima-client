(in-package :maxima-client)

(clim:define-command-table watcher-commands)

(defclass watcher-pane-view ()
  ())

(defvar +watcher-pane-view+ (make-instance 'watcher-pane-view))

(defclass watcher-pane (clim:clim-stream-pane)
  ((variable-list :initform nil
                  :accessor watcher-pane/vars)))

(defun redraw-watcher-pane (frame stream)
  (declare (ignore frame))
  (clim:formatting-table (stream :x-spacing 5 :y-spacing 10)
    (clim:formatting-row (stream)
      (clim:formatting-cell (stream)
        (clim:with-text-face (stream :bold)
          (format stream "Variable")))
      (clim:formatting-cell (stream)
        (format stream " "))
      (clim:formatting-cell (stream :min-width 200)
        (clim:with-text-face (stream :bold)
          (format stream "Value")))
      (clim:formatting-cell (stream)
        (format stream " ")))
    (loop
      for var in (watcher-pane/vars stream)
      for highlight = t then (not highlight)
      when (boundp var)
        do (let ((symbol-expr (make-instance 'maxima-native-expr :expr var))
                 (value-expr (make-instance 'maxima-native-expr :expr (symbol-value var)))
                 (highlight-colour (clim:make-rgb-color 0.9 0.9 0.9)))
             (labels ((render-row ()
                        (clim:formatting-row (stream)
                          (clim:formatting-cell (stream :align-y :center)
                            (render-maxima-native-expr-toplevel stream symbol-expr))
                          (clim:formatting-cell (stream :align-y :center)
                            (format stream " "))
                          (clim:formatting-cell (stream :align-y :center)
                            (render-maxima-native-expr-toplevel stream value-expr))
                          (clim:formatting-cell (stream)
                            (format stream " ")))))
               (if highlight
                   (clim:surrounding-output-with-border (stream :background highlight-colour
                                                                :line-thickness 0
                                                                :ink clim:+transparent-ink+)
                     (render-row))
                   (render-row)))))))

(defun make-watcher-pane ()
  (clim:make-clim-stream-pane :type 'watcher-pane
                              :name 'watcher-pane-root
                              :default-view +watcher-pane-view+
                              :display-function 'redraw-watcher-pane
                              :incremental-redisplay nil
                              :display-time t
                              :borders t
                              :text-margins '(:left (:absolute 2)
                                              :right (:relative 2))))

(clim:define-command (cmd-watch-variable :name "Watch Variable" :menu t :command-table watcher-commands)
    ((varname maxima-native-symbol :prompt "Variable"))
  (let ((watcher-pane (find-or-create-watcher-pane)))
    (pushnew varname (watcher-pane/vars watcher-pane))))
