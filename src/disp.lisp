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

(define-render-function (render-mlabel maxima::mlabel stream args)
  (destructuring-bind (tag expr)
      args
    (let ((maxima-expr (make-instance 'maxima-native-expr :expr expr)))
      (cond (tag
             (render-mlabel-content stream tag maxima-expr))
            ((stringp expr)
             ;; Labelled strings are treated differently, as they should not be displayed with quotes
             (clim:with-room-for-graphics (stream :first-quadrant nil)
               (clim:with-text-family (stream :sans-serif)
                 (clim:draw-text* stream expr 0 0))))
            (t
             (present-to-stream maxima-expr stream))))))

(wrap-function maxima::displa (expr)
  (log:info "displa = ~s" expr)
  (let ((stream (maxima-io/clim-stream *standard-output*)))
    (format stream "~&")
    (present-to-stream (make-instance 'maxima-native-expr :expr expr) stream))
  #+nil
  (let ((stream (maxima-io/clim-stream *standard-output*)))
    (format stream "~&")
    (cond ((not (mlabel-expression-p expr))
           (present-to-stream (make-instance 'maxima-native-expr :expr expr) stream))
          ((second expr)
           (let ((obj (make-instance 'maxima-native-expr :expr (third expr))))
             (render-mlabel stream (second expr) obj)))
          ((stringp (third expr))
           (format stream "~a" (third expr)))
          (t
           (present-to-stream (make-instance 'maxima-native-expr :expr (third expr)) stream)))))

(wrap-function maxima::mread-synerr (fmt &rest args)
  (let ((stream (maxima-io/clim-stream *standard-output*))
        (string (apply #'format nil fmt args)))
    (clim:with-drawing-options (stream :ink clim:+red+)
      (format stream "~a" string))))
