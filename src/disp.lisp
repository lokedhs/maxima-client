(in-package :maxima-client)

(defun mlabel-expression-p (expr)
  (and (listp expr)
       (alexandria:sequence-of-length-p expr 3)
       (eq (caar expr) 'maxima::mlabel)))

(defun render-mlabel-content (stream tag obj)
  (clim:with-room-for-graphics (stream :first-quadrant nil)
    (clim:surrounding-output-with-border (stream :padding 10 :ink clim:+transparent-ink+)
      (present-to-stream (make-instance 'labelled-expression
                                        :tag tag
                                        :expr obj)
                         stream))))

(define-render-function (render-mlabel maxima::mlabel stream args :inhibit-presentation t)
  (destructuring-bind (tag expr)
      args
    (let ((maxima-expr (make-instance 'maxima-native-expr :expr expr)))
      (cond (tag
             (render-mlabel-content stream tag maxima-expr))
            ((stringp expr)
             ;; Labelled strings are treated differently, as they should not be displayed with quotes
             (clim:with-room-for-graphics (stream :first-quadrant nil)
               (with-sans-serif-font (stream)
                 (clim:draw-text* stream expr 0 0))))
            (t
             (present-to-stream maxima-expr stream))))))

(define-render-function (render-mtext maxima::mtext stream args :inhibit-presentation t)
  (with-aligned-rendering (stream)
    (dolist (element args)
      (cond ((and (listp element)
                  (eq (caar element) 'maxima::text-string))
             (with-sans-serif-font (stream)
               (render-aligned-string "~a" (coerce (cdr element) 'string))))
            (t
             (render-aligned () (render-maxima-expression stream element)))))))

(wrap-function maxima::displa (expr)
  (log:info "DISPLA: ~s" expr)
  (let ((stream (maxima-io/clim-stream *standard-output*)))
    (format stream "~&")
    (clim:with-room-for-graphics (stream :first-quadrant nil)
      (let ((*font-size* maxima::$font_size))
        (let ((rec (make-expression-output-record stream expr)))
          (clim:stream-add-output-record stream rec))))
    (setf (maxima-output/inhibit-next-terpri-p *standard-output*) t)))

(wrap-function maxima::mread-synerr (fmt &rest args)
  (flet ((column ()
           (let ((n (get 'maxima::*parse-window* 'maxima::length))
	         ch some)
	     (loop for i from (1- n) downto (- n 20)
	     	   while (setq ch (nth i maxima::*parse-window*))
		   do (cond ((char= ch #\newline)
			     (return-from column some))
			    (t (push ch some))))
	     some)))
    (error 'maxima-expr-parse-error :src "source not available"
                                    :message (apply #'format nil fmt args)
                                    :pos (- (length (column)) 2)))
  #+nil
  (clim:with-drawing-options (stream :ink clim:+red+)
    (format stream "~%~a" string)))
