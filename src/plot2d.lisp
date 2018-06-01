(in-package :maxima-client)

(defun maxima-eval-to-number (expr)
  (let ((v (maxima::meval expr)))
    (unless (numberp v)
      (error "~s does not evaluate to a number" expr))
    v))

(defclass standard-plot ()
  ((caller-fun           :initarg :caller-fun)
   (caller-range         :initarg :caller-range)
   (caller-extra-options :initarg :caller-extra-options)
   (data                 :initarg :data
                         :initform nil
                         :accessor standard-plot/data)
   (options              :initarg :options
                         :initform nil
                         :accessor standard-plot/options)
   (max-x                :initarg :max-x
                         :initform nil
                         :accessor standard-plot/max-x)
   (min-x                :initarg :min-x
                         :initform nil
                         :accessor standard-plot/min-x)
   (max-y                :initarg :max-y
                         :initform nil
                         :accessor standard-plot/max-y)
   (min-y                :initarg :min-y
                         :initform nil
                         :accessor standard-plot/min-y)))

(defun clim-plot2d-backend (caller-fun caller-range caller-extra-options points-lists options)
  (log:info "clim plotter: n: ~s, options: ~s" (length points-lists) options)
  (let ((plot (make-instance 'standard-plot
                             :caller-fun caller-fun
                             :caller-range caller-range
                             :caller-extra-options caller-extra-options
                             :data points-lists
                             :options options)))
    (present-to-stream plot *current-stream*)))

(clim:define-command (command-test :name "Test command" :menu t :command-table maxima-commands)
    ((value 'string :prompt "Value")
     &key
     (type 'string :prompt "Type")
     (size 'string :prompt "Size"))
  (log:info "value=~s, type=~s, size=~s" value type size))

(clim:define-command (plot2d-with-range :name "Plot 2D" :menu t :command-table maxima-commands)
    ((expression 'maxima-native-expr :prompt "Expression")
     (variable 'maxima-native-expr :prompt "Variable")
     (lower-bound 'maxima-native-expr :prompt "Lower bound")
     (upper-bound 'maxima-native-expr :prompt "Upper bound"))
  (let ((plot2d-expression `((maxima::$plot2d) ,(maxima-native-expr/expr expression)
                             ((maxima::mlist)
                              ,(maxima-native-expr/expr variable)
                              ,(maxima-native-expr-as-float lower-bound)
                              ,(maxima-native-expr-as-float upper-bound)))))
    (maxima-eval (make-instance 'maxima-native-expr :expr plot2d-expression))))

(clim:define-command (zoom-in :name "Zoom by" :menu t :command-table maxima-commands)
    ((plot standard-plot :prompt "Plot")
     &key
     (amount integer :prompt "Amount"))
  (log:info "Zooming ~s by ~s" plot amount))

(clim:define-command (plot2d-demo :name "Demo plot" :menu t :command-table maxima-commands)
    ()
  (maxima-eval (string-to-native-expr "plot2d(sin(x),[x,0,%pi*2])")))

#|
  Typical options:

(:YLABEL "1/x" :X (0.0 10.0) :XLABEL "x" :YBOUNDS
             (-1.7555597020139802e305 1.7555597020139802e305) :PLOT_FORMAT
             MAXIMA::$GNUPLOT_PIPES :GRID (30 30) :RUN_VIEWER T :AXES T :NTICKS
             29 :ADAPT_DEPTH 5 :COLOR
             (MAXIMA::$BLUE MAXIMA::$RED MAXIMA::$GREEN MAXIMA::$MAGENTA
              MAXIMA::$BLACK MAXIMA::$CYAN)
             :POINT_TYPE
             (MAXIMA::$BULLET MAXIMA::$BOX MAXIMA::$TRIANGLE MAXIMA::$PLUS
              MAXIMA::$TIMES MAXIMA::$ASTERISK)
             :PALETTE
             (((MAXIMA::MLIST) MAXIMA::$GRADIENT MAXIMA::$GREEN MAXIMA::$CYAN
               MAXIMA::$BLUE MAXIMA::$VIOLET)
              ((MAXIMA::MLIST) MAXIMA::$GRADIENT MAXIMA::$MAGENTA
               MAXIMA::$VIOLET MAXIMA::$BLUE MAXIMA::$CYAN MAXIMA::$GREEN
               MAXIMA::$YELLOW MAXIMA::$ORANGE MAXIMA::$RED MAXIMA::$BROWN
               MAXIMA::$BLACK))
             :GNUPLOT_PREAMBLE "" :GNUPLOT_TERM MAXIMA::$DEFAULT)
|#

(defun find-dimensions-for-datasets (data)
  (let ((dimensions (loop
                      for dataset in data
                      collect (loop
                                with max-y = nil
                                with min-y = nil
                                with max-x = nil
                                with min-x = nil
                                for points on dataset by #'cddr
                                for x = (car points)
                                for y = (cadr points)
                                when (numberp y)
                                  do (progn
                                       (setf max-y (if max-y (max max-y y) y))
                                       (setf min-y (if min-y (min min-y y) y))
                                       (setf max-x (if max-x (max max-x x) x))
                                       (setf min-x (if min-x (min min-x x) x)))
                                finally (return (list max-y min-y max-x min-x))))))
    (values (reduce #'max (remove nil (mapcar #'first dimensions)))
            (reduce #'min (remove nil (mapcar #'second dimensions)))
            (reduce #'max (remove nil (mapcar #'third dimensions)))
            (reduce #'min (remove nil (mapcar #'fourth dimensions))))))

(defun draw-centered-text (stream text x y)
  (multiple-value-bind (width)
      (clim:text-size stream text)
    (clim:draw-text* stream text (- x (/ width 2)) y)))

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
    ((t) (clim:make-rgb-color 0 0 0))))

(defun nicenum (x round)
  (let* ((exp (floor (log x 10)))
         (f (/ x (expt 10 exp)))
         (nf (if round
                 (cond ((< f 1.5) 1)
                       ((< f 3) 2)
                       ((< f 7) 5)
                       (t 10))
                 (cond ((<= f 1) 1)
                       ((<= f 2) 2)
                       ((<= f 5) 5)
                       (t 10)))))
    (* nf (expt 10 exp))))

(defun draw-tick-marks (min max num-steps draw-fn)
  (when (< (- max min) 10d-5)
    (return-from draw-tick-marks))
  (let* ((range (nicenum (- max min) nil))
         (d (nicenum (/ range (1- num-steps)) t))
         (graph-min (* (floor (/ min d)) d))
         (graph-max (* (ceiling (/ max d)) d))
         (num-decimals (max (- (floor (log d 10))) 0)))
    (log:info "graph-min=~s graph-max=~s d=~s min=~s max=~s" graph-min graph-max d min max)
    (loop
      ;; The tick marks starts at graph-min, but this value may be less than min
      for x from (+ graph-min (* d (ceiling (/ (- min graph-min) d)))) by d
      while (and
             (<= x max)
             (<= x (+ graph-max (* d 0.5))))
      do (funcall draw-fn x num-decimals))))

(defun format-with-decimals (n num-decimals)
  (if (zerop num-decimals)
      (format nil "~d" n)
      (format nil "~,vf" num-decimals n)))

(clim:define-presentation-method clim:present (obj (type standard-plot) stream (view maxima-renderer-view) &key)
  (let* ((w 500)
         (h 300)
         (left-margin 50)
         (tick-height 5)
         (data (standard-plot/data obj))
         (options (standard-plot/options obj))
         (x-label (getf options :xlabel))
         (y-label (getf options :ylabel))
         (colours (getf options :color))
         (char-height (char-height stream)))
    (multiple-value-bind (max-y-dimension min-y-dimension max-x-dimension min-x-dimension)
        (find-dimensions-for-datasets data)
      (let ((max-y (or (standard-plot/max-y obj) max-y-dimension))
            (min-y (or (standard-plot/min-y obj) min-y-dimension))
            (max-x (or (standard-plot/max-x obj) max-x-dimension))
            (min-x (or (standard-plot/min-x obj) min-x-dimension)))
        (let ((max-y-bound max-y)
              (min-y-bound min-y)
              (max-x-bound max-x)
              (min-x-bound min-x))
          (labels ((x-to-pos (x)
                     (+ left-margin
                        (* (/ (- x min-x-bound)
                              (- max-x-bound min-x-bound))
                           w)))
                   (y-to-pos (y)
                     (- h (* (/ (- y min-y-bound)
                                (- max-y-bound min-y-bound))
                             h))))
            (clim:with-room-for-graphics (stream)
              (clim:with-identity-transformation (stream)
                (clim:draw-rectangle* stream left-margin 0 (+ left-margin w) h :filled nil)
                ;; If the zero-axis is visible, draw it
                (when (< min-y-bound 0 max-y-bound)
                  (clim:draw-line* stream left-margin (y-to-pos 0) (+ left-margin w) (y-to-pos 0)
                                   :line-thickness 1 :line-dashes '(1 2) :line-cap-shape :square))
                (when (< min-x-bound 0 max-x-bound)
                  (clim:draw-line* stream (x-to-pos 0) 0 (x-to-pos 0) h
                                   :line-thickness 1 :line-dashes '(1 2) :line-cap-shape :square))
                ;; Draw X-axis tick marks
                (draw-tick-marks min-x-bound max-x-bound 8
                                 (lambda (x num-decimals)
                                   (let ((string (format-with-decimals x num-decimals)))
                                     (multiple-value-bind (width)
                                         (clim:text-size stream string)
                                       (let ((pos (x-to-pos x)))
                                         (clim:draw-line* stream pos h pos (- h tick-height))
                                         (clim:draw-text* stream
                                                          string
                                                          (- pos (/ width 2))
                                                          (+ h (* char-height 2))))))))
                ;; Draw X-label
                (draw-centered-text stream x-label (+ (/ w 2) left-margin) (+ h (* char-height 4)))
                ;; Draw Y-axis ticks
                (let ((max-width 0))
                  (draw-tick-marks min-y-bound max-y-bound 8
                                   (lambda (y num-decimals)
                                     (let ((string (format-with-decimals y num-decimals)))
                                       (multiple-value-bind (width)
                                           (clim:text-size stream string)
                                         (let ((yp (y-to-pos y)))
                                           (clim:draw-line* stream left-margin yp (+ left-margin 5) yp)
                                           (clim:draw-text* stream
                                                            string
                                                            (- left-margin width 5)
                                                            (+ yp (/ char-height 2)))
                                           (setq max-width (max max-width width)))))))
                  ;; Draw y-label, if it exists
                  (when y-label
                    (multiple-value-bind (width)
                        (clim:text-size stream y-label)
                      (let ((pos (- left-margin 30)))
                        (clim:draw-text* stream y-label (- pos (/ width 2)) (/ h 2)
                                         :transformation (clim:make-rotation-transformation* (- (/ pi 2))
                                                                                             pos
                                                                                             (/ h 2)))))))
                ;; Draw the datasets
                (loop
                  for i from 0
                  for dataset in data
                  for colour = (maxima-to-clim-colour (nth (mod i (length colours)) colours))
                  do (let ((curr nil))
                       (labels ((draw-list ()
                                  (when (and curr (cdr curr))
                                    (clim:draw-design stream (clim:make-polyline* (reverse curr))
                                                      :ink colour))
                                  (setq curr nil))
                                (push-points (x y)
                                  (push (round (x-to-pos x)) curr)
                                  (push (round (y-to-pos y)) curr)))
                         (loop
                           with prev-outside = nil
                           for (x y) on dataset by #'cddr
                           for outside = (if (and (numberp y) (or (< y min-y-bound) (> y max-y-bound)))
                                             (list x y)
                                             nil)
                           if (or (and outside prev-outside) (eq x 'maxima::moveto))
                             do (draw-list)
                           else
                             do (let ((new-y (clamp y min-y-bound max-y-bound)))
                                  (when prev-outside
                                    (destructuring-bind (prev-x prev-y)
                                        prev-outside
                                      (push-points prev-x prev-y)))
                                  (push-points x new-y))
                           do (setq prev-outside outside)
                           finally (draw-list)))))
                ;; Zoom controls
                (clim:with-output-as-gadget (stream :x (- (+ w left-margin) 70) :y (+ h 25))
                  (clim:make-pane 'clim:push-button-pane
                                  :label "+"
                                  :width 30
                                  :activate-callback (lambda (pane &rest args)
                                                       (declare (ignore pane args))
                                                       (log:info "zoom"))))
                (clim:with-output-as-gadget (stream :x (- (+ w left-margin) 30) :y (+ h 25))
                  (clim:make-pane 'clim:push-button-pane
                                  :label "-"
                                  :width 30
                                  :activate-callback (lambda (pane &rest args)
                                                       (declare (ignore pane args))
                                                       (log:info "zoom out"))))))))))))
