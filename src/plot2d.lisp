(in-package :maxima-client)

(defun maxima-eval-to-number (expr)
  (let ((v (maxima::meval expr)))
    (unless (numberp v)
      (error "~s does not evaluate to a number" expr))
    v))

(defclass standard-plot ()
  ((fun         :initarg :fun
                :reader standard-plot/fun)
   (vairable    :initarg :variable
                :reader standard-plot/variable)
   (lower-bound :initarg :lower-bound
                :reader standard-plot/lower-bound)
   (upper-bound :initarg :upper-bound
                :reader standard-plot/upper-bound)
   (options     :initform nil
                :accessor standard-plot/options)
   (data        :initarg :data
                :initform nil
                :accessor standard-plot/data)))

(defun recompute-plot (plot)
  (let ((fun (standard-plot/fun plot))
        (variable (standard-plot/variable plot))
        (lower-bound (standard-plot/lower-bound plot))
        (upper-bound (standard-plot/upper-bound plot)))

    (let ((options (copy-tree maxima::*plot-options*))
          extra-options
          (range `((maxima::mlist) ,variable ,lower-bound ,upper-bound)))

      ;; From maxima: A single parametric or discrete plot is placed inside a maxima list
      (when (and (consp fun)
                 (or (eq (second fun) 'maxima::$parametric) (eq (second fun) 'maxima::$discrete)))
        (setq fun `((mlist) ,fun)))
      ;; If at this point fun is not a maxima list, it is then a single function
      (unless (maxima::$listp fun) (setq fun `((maxima::mlist) ,fun)))
      ;; 2- Get names for the two axis and values for xmin and xmax if needed.
      (let ((no-range-required t)
            small huge)
        (setq small (- (/ most-positive-double-float 1024)))
        (setq huge (/ most-positive-double-float 1024))
        ;; FIXME: maxima really loves its plists
        (setf (getf options :ybounds) (list small huge))
        ;; Loop over all functions that should be plotted
        (dolist (subfun (rest fun))
          (if (not (maxima::$listp subfun))
              (setq no-range-required nil))) 
        (unless no-range-required
          (setq range (maxima::check-range range))
          (setf (getf options :xlabel) (maxima::ensure-string (second range)))
          (setf (getf options :x) (cddr range)))
        (when no-range-required
          ;; Make the default ranges on X nd Y large so parametric plots
          ;; don't get prematurely clipped. Don't use most-positive-flonum
          ;; because draw2d will overflow.
          (setf (getf options :xbounds) (list small huge))
          (when range
            ;; second argument was really a plot option, not a range
            (setq extra-options (cons range extra-options))))
        ;;
        ;; When only one function is being plotted:
        ;; If a simple function use, its name for the vertical axis.
        ;; If parametric, give the axes the names of the two parameters.
        ;; If discrete points, name the axes x and y.
        (when (= (length fun) 2)
          (let ((v (second fun)) label)
            (cond ((atom v) 
                   (setq label (coerce (maxima::mstring v) 'string))
                   (if (< (length label) 80)
                       (setf (getf options :ylabel) label)))
                  ((eq (second v) 'maxima::$parametric)
                   (setq label (coerce (maxima::mstring (third v)) 'string))
                   (if (< (length label) 80)
                       (setf (getf options :xlabel) label))
                   (setq label (coerce (maxima::mstring (fourth v)) 'string))
                   (if (< (length label) 80)
                       (setf (getf options :ylabel) label)))
                  ((eq (second v) 'maxima::$discrete)
                   (setf (getf options :xlabel) "x")
                   (setf (getf options :ylabel) "y"))
                  (t
                   (setq label (coerce (maxima::mstring v) 'string))
                   (if (< (length label) 80)
                       (setf (getf options :ylabel) label))))))
        ;;
        ;; Parse the given options into the options list
        (setq options (maxima::plot-options-parser extra-options options))
        (when (getf options :y) (setf (getf options :ybounds) (getf options :y)))
        ;;
        ;; Remove axes labels when no box is used in gnuplot
        (when (and (member :box options) (not (getf options :box))
	           (not (eq (getf options :plot_format) 'maxima::$xmaxima)))
          (remf options :xlabel)
          (remf options :ylabel))
        ;;
        ;; Check x bounds
        (let ((xmin (first (getf options :x))) (xmax (second (getf options :x))))
          (when (and (getf options :logx) xmin xmax)
            (if (> xmax 0)
                (when (<= xmin 0)
                  (let ((revised-xmin (/ xmax 1000)))
                    (maxima::mtell (intl:gettext "plot2d: lower bound must be positive when 'logx' in effect.~%plot2d: assuming lower bound = ~M instead of ~M") revised-xmin xmin)
                    (setf (getf options :x) (list revised-xmin xmax))
                    (setq range `((mlist) ,(second range) ,revised-xmin ,xmax))))
                (maxima::merror (intl:gettext "plot2d: upper bound must be positive when 'logx' in effect; found: ~M") xmax))))
        ;;
        ;; Check y bounds
        (let ((ymin (first (getf options :y)))
              (ymax (second (getf options :y))))
          (when (and (getf options :logy) ymin ymax)
            (if (> ymax 0)
                (when (<= ymin 0)
                  (let ((revised-ymin (/ ymax 1000)))
                    (maxima::mtell (intl:gettext "plot2d: lower bound must be positive when 'logy' in effect.~%plot2d: assuming lower bound = ~M instead of ~M") revised-ymin ymin)
                    (setf (getf options :y) (list revised-ymin ymax))))
                (maxima::merror (intl:gettext "plot2d: upper bound must be positive when 'logy' in effect; found: ~M") ymax))))
        ;;
        ;; FIXME: use of a global variable here
        (setq maxima::*plot-realpart* (getf options :plot_realpart))
        ;;
        ;; Compute points to plot for each element of FUN.
        ;; If no plottable points are found, return immediately from $PLOT2D
        (let ((points-lists (mapcar #'(lambda (f) (cdr (maxima::draw2d f range options))) (cdr fun))))
          (when (every #'null points-lists)
            (maxima::mtell (intl:gettext "plot2d: nothing to plot.~%"))
            (return-from recompute-plot))
          ;;
          (setf (standard-plot/options plot) options)
          (setf (standard-plot/data plot) points-lists))))))

(defun plot2d-impl (fun variable lower-bound upper-bound)
  (let ((p (make-instance 'standard-plot
                          :fun fun
                          :variable variable
                          :lower-bound lower-bound
                          :upper-bound upper-bound)))
    (recompute-plot p)
    p))

(clim:define-command (command-test :name "Test command" :menu t :command-table maxima-commands)
    ((value 'string :prompt "Value")
     &key
     (type 'string :prompt "Type")
     (size 'string :prompt "Size"))
  (log:info "value=~s, type=~s, size=~s" value type size))

(clim:define-command (plot2d-with-range :name "Plot 2D" :menu t :command-table maxima-commands)
    ((expression 'maxima-expression :prompt "Expression")
     (variable 'maxima-expression :prompt "Variable")
     (lower-bound 'maxima-expression :prompt "Lower bound")
     (upper-bound 'maxima-expression :prompt "Upper bound"))
  (let ((start (maxima::meval* lower-bound))
        (end (maxima::meval* upper-bound)))
    (declare (ignore start end))
    (let ((plot (plot2d-impl (maxima::meval* expression) variable lower-bound upper-bound)))
      (present-to-stream plot *standard-output*))))

(clim:define-command (plot2d-demo :name "Demo plot" :menu t :command-table maxima-commands)
    ()
  (plot2d-with-range (string-to-maxima-expr "sin(x)") (string-to-maxima-expr "x")
                     (string-to-maxima-expr "0") (string-to-maxima-expr "%pi*2")))

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
                                for points on dataset by #'cddr
                                for y = (cadr points)
                                when (numberp y)
                                  do (progn
                                       (setf max-y (if max-y (max max-y y) y))
                                       (setf min-y (if min-y (min min-y y) y)))
                                finally (return (list max-y min-y))))))
    (values (reduce #'max (remove nil (mapcar #'car dimensions)))
            (reduce #'min (remove nil (mapcar #'cadr dimensions))))))

(defun draw-centered-text (stream text left right y)
  (multiple-value-bind (width)
      (clim:text-size stream text)
    (clim:draw-text* stream text (+ (/ (+ (- right left) width) 2) left) y)))

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

(clim:define-presentation-method clim:present (obj (type standard-plot) stream (view t) &key)
  (let* ((w 500)
         (h 300)
         (left-margin 40)
         (data (standard-plot/data obj))
         (options (standard-plot/options obj))
         (x-label (getf options :xlabel))
         (y-label (getf options :ylabel))
         (colours (getf options :color)))
    (multiple-value-bind (max-y min-y)
        (find-dimensions-for-datasets data)
      (destructuring-bind (min-x-bound max-x-bound)
          (getf options :x)
        (clim:with-room-for-graphics (stream)
          (clim:with-identity-transformation (stream)
            (clim:draw-rectangle* stream left-margin 0 (+ left-margin w) h :filled nil)
            (loop
              for i from 0
              for dataset in data
              for colour = (maxima-to-clim-colour (nth (mod i (length colours)) colours))
              do (let ((curr nil))
                   (labels ((draw-list ()
                              (when curr
                                (clim:draw-design stream (clim:make-polyline* (reverse curr))
                                                  :ink colour))))
                     (loop
                       for (x y) on dataset by #'cddr
                       if (eq x 'maxima::moveto)
                         do (progn
                              (draw-list)
                              (setq curr nil))
                       else
                         do (progn
                              (push (+ left-margin (* (/ (- x min-x-bound) (- max-x-bound min-x-bound)) w)) curr)
                              (push (- h (* (/ (- y min-y) (- max-y min-y)) h)) curr))
                       finally (draw-list)))))
            ;; Draw X-label
            (draw-centered-text stream x-label left-margin (+ left-margin w) (+ h 16))
            ;; Draw y-label, if it exists
            ;; The Y-label is disabled because text transformations are still a mess
            #+nil
            (when y-label
              (multiple-value-bind (width)
                  (clim:text-size stream y-label)
                (let ((baseline-to-edge 10))
                  (clim:draw-text* stream y-label (- left-margin baseline-to-edge (/ width 2)) (/ h 2)
                                   :transformation (clim:make-rotation-transformation* (- (/ pi 2))
                                                                                       (- left-margin baseline-to-edge)
                                                                                       (/ h 2))))))))))))
