(in-package :maxima)

#+nil
(maxima-client.common::wrap-function check-option-format (option)
  (if (eq (cadr option) '$clim)
      '$clim
      ;; ELSE: Call into the old implementation
      (funcall *OLD-FN-CHECK-OPTION-FORMAT* option)))

#+nil
(maxima-client.common::wrap-function $plot2d (fun &optional range &rest extra-options)
  (apply #'cplot2d fun range extra-options))

#+nil
(defun cplot2d (fun &optional range &rest extra-options)
  (let ((orig-fun fun)
        (orig-range range)
        (orig-extra-options extra-options)
        ($display2d nil) (*plot-realpart* *plot-realpart*)
        (options (copy-tree *plot-options*)) (i 0)
        output-file gnuplot-term gnuplot-out-file file points-lists)

    ;; 1- Put fun in its most general form: a maxima list with several objects
    ;; that can be expressions (simple functions) and maxima lists (parametric
    ;; functions or discrete sets of points).

    ;; A single parametric or discrete plot is placed inside a maxima list
    (setf (getf options :type) "plot2d")
    (when (and (consp fun)
               (or (eq (second fun) '$parametric) (eq (second fun) '$discrete)))
      (setq fun `((mlist) ,fun)))

    ;; If at this point fun is not a maxima list, it is then a single function
    (unless ($listp fun ) (setq fun `((mlist) ,fun)))

    ;; 2- Get names for the two axis and values for xmin and xmax if needed.

    ;; If any of the objects in the fun list is a simple function,
    ;; the range option is mandatory and will provide the name of
    ;; the horizontal axis and the values of xmin and xmax.
    (let ((no-range-required t) small huge)
      #-clisp (setq small (- (/ most-positive-flonum 1024)))
      #+clisp (setq small (- (/ most-positive-double-float 1024.0)))
      #-clisp (setq huge (/ most-positive-flonum 1024))
      #+clisp (setq huge (/ most-positive-double-float 1024.0))
      (setf (getf options :ybounds) (list small huge))
      (dolist (subfun (rest fun))
        (if (not ($listp subfun))
            (setq no-range-required nil))) 
      (unless no-range-required
        (setq range (check-range range))
        (setf (getf options :xlabel) (ensure-string (second range)))
        (setf (getf options :x) (cddr range)))
      (when no-range-required
        ;; Make the default ranges on X nd Y large so parametric plots
        ;; don't get prematurely clipped. Don't use most-positive-flonum
        ;; because draw2d will overflow.
        (setf (getf options :xbounds) (list small huge))
        (when range
          ;; second argument was really a plot option, not a range
          (setq extra-options (cons range extra-options)))))

    ;; When only one function is being plotted:
    ;; If a simple function use, its name for the vertical axis.
    ;; If parametric, give the axes the names of the two parameters.
    ;; If discrete points, name the axes x and y.
    (when (= (length fun) 2)
      (let ((v (second fun)) label)
        (cond ((atom v) 
               (setq label (coerce (mstring v) 'string))
               (if (< (length label) 80)
                   (setf (getf options :ylabel) label)))
              ((eq (second v) '$parametric)
               (setq label (coerce (mstring (third v)) 'string))
               (if (< (length label) 80)
                   (setf (getf options :xlabel) label))
               (setq label (coerce (mstring (fourth v)) 'string))
               (if (< (length label) 80)
                   (setf (getf options :ylabel) label)))
              ((eq (second v) '$discrete)
               (setf (getf options :xlabel) "x")
               (setf (getf options :ylabel) "y"))
              (t
               (setq label (coerce (mstring v) 'string))
               (if (< (length label) 80)
                   (setf (getf options :ylabel) label))))))

    ;; Parse the given options into the options list
    (setq options (plot-options-parser extra-options options))
    (when (getf options :y) (setf (getf options :ybounds) (getf options :y)))

    ;; Remove axes labels when no box is used in gnuplot
    (when (and (member :box options) (not (getf options :box))
	       (not (eq (getf options :plot_format) '$xmaxima)))
      (remf options :xlabel)
      (remf options :ylabel))


    (let ((xmin (first (getf options :x))) (xmax (second (getf options :x))))
      (when
          (and (getf options :logx) xmin xmax)
        (if (> xmax 0)
            (when (<= xmin 0)
              (let ((revised-xmin (/ xmax 1000)))
                (mtell (intl:gettext "plot2d: lower bound must be positive when 'logx' in effect.~%plot2d: assuming lower bound = ~M instead of ~M") revised-xmin xmin)
                (setf (getf options :x) (list revised-xmin xmax))
                (setq range `((mlist) ,(second range) ,revised-xmin ,xmax))))
            (merror (intl:gettext "plot2d: upper bound must be positive when 'logx' in effect; found: ~M") xmax))))

    (let ((ymin (first (getf options :y)))
          (ymax (second (getf options :y))))
      (when (and (getf options :logy) ymin ymax)
        (if (> ymax 0)
            (when (<= ymin 0)
              (let ((revised-ymin (/ ymax 1000)))
                (mtell (intl:gettext "plot2d: lower bound must be positive when 'logy' in effect.~%plot2d: assuming lower bound = ~M instead of ~M") revised-ymin ymin)
                (setf (getf options :y) (list revised-ymin ymax))))
            (merror (intl:gettext "plot2d: upper bound must be positive when 'logy' in effect; found: ~M") ymax))))

    (setq *plot-realpart* (getf options :plot_realpart))

    ;; Compute points to plot for each element of FUN.
    ;; If no plottable points are found, return immediately from $PLOT2D.

    (setq points-lists
          (mapcar #'(lambda (f) (cdr (draw2d f range options))) (cdr fun)))
    (when (= (count-if #'(lambda (x) x) points-lists) 0)
      (mtell (intl:gettext "plot2d: nothing to plot.~%"))
      (return-from cplot2d))

    (setq gnuplot-term (getf options :gnuplot_term))
    (setf gnuplot-out-file (getf options :gnuplot_out_file))
    (if (and (find (getf options :plot_format) '($gnuplot_pipes $gnuplot))
             (eq gnuplot-term '$default) gnuplot-out-file)
        (setq file (plot-file-path gnuplot-out-file))
        (setq file
              (plot-file-path
               (format nil "maxout~d.~(~a~)"
		       (getpid)
                       (ensure-string (getf options :plot_format))))))

    ;; old function $plot2dopen incorporated here
    (case (getf options :plot_format)
      ($clim
       (multiple-value-bind (result return-p)
           (maxima-client::clim-plot2d-backend orig-fun orig-range orig-extra-options points-lists options)
         (when return-p
           (return-from cplot2d result))))
      ($xmaxima
       (when (getf options :ps_file)
         (setq output-file (list (getf options :ps_file))))
       (show-open-plot
        (with-output-to-string
            (st)
          (xmaxima-print-header st options)
          (let ((legend (getf options :legend))
                (colors (getf options :color))
                (styles (getf options :style)) style plot-name)
            (unless (listp legend) (setq legend (list legend)))
            (unless (listp colors) (setq colors (list colors)))
            (unless (listp styles) (setq styles (list styles)))
            (loop for f in (cdr fun) for points-list in points-lists do
              (when points-list
                (if styles
                    (progn
                      (setq style (nth (mod i (length styles)) styles))
                      (setq style (if ($listp style) (cdr style) `(,style))))
                    (setq style nil))
                (incf i)
                (if (member :legend options)
                    ;; a legend has been given in the options
                    (setq plot-name
                          (if (first legend)
                              (ensure-string
                               (nth (mod (- i 1) (length legend)) legend))
                              nil)) ; no legend if option [legend,false]
                    (if (= 2 (length fun))
                        (progn
                          (setq plot-name nil) ;no legend for single function
                          (format st " {nolegend 1}"))
                        (setq plot-name
                              (let ((string ""))
                                (cond
                                  ((atom f) 
                                   (setq
                                    string (coerce (mstring f) 'string)))
                                  ((eq (second f) '$parametric)
                                   (setq
                                    string 
                                    (concatenate 
                                     'string
                                     (coerce (mstring (third f)) 'string)
                                     ", " (coerce (mstring (fourth f)) 'string))))
                                  ((eq (second f) '$discrete)
                                   (setq string
                                         (format nil "discrete~a" i)))
                                  (t
                                   (setq string
                                         (coerce (mstring f) 'string))))
                                (cond ((< (length string) 80) string)
                                      (t (format nil "fun~a" i)))))))
                (when plot-name 
                  (format st " {label ~s}" plot-name))
                (format st " ~a~%" (xmaxima-curve-style style colors i))
                (format st " {xversusy~%")
                (let ((lis points-list))
                  (loop
                    while lis
                    do
                       (loop while (and lis (not (eq (car lis) 'moveto)))
                             collecting (car lis) into xx
                             collecting (cadr lis) into yy
                             do (setq lis (cddr lis))
                             finally
                                ;; only output if at least two points for line
                                (when (cdr xx)
                                  (tcl-output-list st xx)
                                  (tcl-output-list st yy)))
                       ;; remove the moveto
                       (setq lis (cddr lis))))
                (format st "}"))))
          (format st "} "))
        file)
       (cons '(mlist) (cons file output-file)))
      (t
       (with-open-file (st file :direction :output :if-exists :supersede)
         (case (getf options :plot_format)
           ($gnuplot
            (setq output-file (gnuplot-print-header st options))
            (format st "plot")
            (when (getf options :x)
              (format st " [~{~g~^ : ~}]" (getf options :x)))
            (when (getf options :y)
              (unless (getf options :x)
                (format st " []")) 
              (format st " [~{~g~^ : ~}]" (getf options :y))))
           ($gnuplot_pipes
            (check-gnuplot-process)
            ($gnuplot_reset)
            (setq output-file (gnuplot-print-header *gnuplot-stream* options))
            (setq *gnuplot-command* (format nil "plot"))
            (when (getf options :x)
              (setq
               *gnuplot-command*
               ($sconcat
                *gnuplot-command* 
                (format nil " [~{~g~^ : ~}]" (getf options :x)))))
            (when (getf options :y) 
              (unless (getf options :x)
                (setq *gnuplot-command*
                      ($sconcat *gnuplot-command* (format nil " []"))))
              (setq
               *gnuplot-command*
               ($sconcat
                *gnuplot-command* 
                (format nil " [~{~g~^ : ~}]"  (getf options :y)))))))
         (let ((legend (getf options :legend))
               (colors (getf options :color))
               (types (getf options :point_type))
               (styles (getf options :style))
               style plot-name)
           (unless (listp legend) (setq legend (list legend)))
           (unless (listp colors) (setq colors (list colors)))
           (unless (listp styles) (setq styles (list styles)))
           (loop for v in (cdr fun) for points-list in points-lists do
             (when points-list
               (case (getf options :plot_format)
                 ($gnuplot_pipes
                  (if (> i 0)
                      (setq *gnuplot-command*
                            ($sconcat *gnuplot-command* ", ")))
                  (setq *gnuplot-command*
                        ($sconcat *gnuplot-command* 
                                  (format nil "~s index ~a " file i)))))
               (if styles
                   (setq style (nth (mod i (length styles)) styles))
                   (setq style nil))
               (when ($listp style) (setq style (cdr style)))
               (incf i)
               (if (member :legend options)
                   ;; a legend has been defined in the options
                   (setq plot-name
                         (if (first legend)
                             (ensure-string
                              (nth (mod (- i 1) (length legend)) legend))
                             nil)) ; no legend if option [legend,false]
                   (if (= 2 (length fun))
                       (setq plot-name nil) ; no legend if just one function
                       (setq plot-name
                             (let ((string ""))
                               (cond ((atom v) 
                                      (setq string
                                            (coerce (mstring v) 'string)))
                                     ((eq (second v) '$parametric)
                                      (setq
                                       string 
                                       (concatenate
                                        'string
                                        (coerce (mstring (third v)) 'string)
                                        ", "
                                        (coerce (mstring (fourth v)) 'string))))
                                     ((eq (second v) '$discrete)
                                      (setq
                                       string (format nil "discrete~a" i)))
                                     (t
                                      (setq string 
                                            (coerce (mstring v) 'string))))
                               (cond ((< (length string) 80) string)
                                     (t (format nil "fun~a" i)))))))
               (case (getf options :plot_format)
                 ($gnuplot
                  (when (> i 1) (format st ","))
                  (format st " '-'")
                  (if plot-name
                      (format st " title ~s" plot-name)
                      (format st " notitle"))
                  (format st " ~a"
                          (gnuplot-curve-style style colors types i)))
                 ($gnuplot_pipes
                  (setq *gnuplot-command*
                        ($sconcat
                         *gnuplot-command*
                         (if plot-name 
                             (format
                              nil " title ~s ~a" plot-name 
                              (gnuplot-curve-style style colors types i))
                             (format
                              nil " notitle ~a"
                              (gnuplot-curve-style style colors types i)
                              )))))))))
         (case (getf options :plot_format)
           ($gnuplot
            (format st "~%"))
           ($gnuplot_pipes
            (format st "~%")))
         (setq i 0)
         (loop for v in (cdr fun) for points-list in points-lists do
           (when points-list
             (incf i)
             (case (getf options :plot_format)
               ($gnuplot
                (if (> i 1)
                    (format st "e~%")))
               ($gnuplot_pipes
                (if (> i 1)
                    (format st "~%~%")))
               ($mgnuplot
                (format st "~%~%# \"Fun~a\"~%" i))
               )
             (let (in-discontinuity points)
               (loop for (v w) on points-list by #'cddr
                     do
                        (cond ((eq v 'moveto)
                               (cond 
                                 ((find (getf options :plot_format) '($gnuplot_pipes $gnuplot))
                                  ;; A blank line means a discontinuity
                                  (if (null in-discontinuity)
                                      (progn
                                        (format st "~%")
                                        (setq in-discontinuity t))))
                                 ((equal (getf options :plot_format) '$mgnuplot)
                                  ;; A blank line means a discontinuity
                                  (format st "~%"))
                                 (t
                                  (format st "move "))))
                              (t (format st "~g ~g ~%" v w)
                                 (setq points t)
                                 (setq in-discontinuity nil))))
               (if (and (null points)
                        (first (getf options :x))
                        (first (getf options :y)))
                   (format
                    st "~g ~g ~%"
                    (first (getf options :x))
                    (first (getf options :y))))))))))
    
    (case (getf options :plot_format)
      ($gnuplot 
       (gnuplot-process options file output-file))
      ($gnuplot_pipes
       (send-gnuplot-command *gnuplot-command*))
      ($mgnuplot 
       ($system (concatenate 'string *maxima-plotdir* "/" $mgnuplot_command) 
                (format nil " -plot2d ~s -title ~s" file "Fun1"))))
    (cons '(mlist) (cons file output-file))))
