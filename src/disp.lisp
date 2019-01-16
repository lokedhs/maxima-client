(in-package :maxima-client)

(defun mlabel-expression-p (expr)
  (and (listp expr)
       (alexandria:sequence-of-length-p expr 3)
       (eq (caar expr) 'maxima::mlabel)))

(wrap-function maxima::displa (expr)
  (log:info "displa = ~s" expr)
  (let ((stream (maxima-io/clim-stream *standard-output*)))
    (format stream "~&")
    (if (mlabel-expression-p expr)
        (if (second expr)
            (present-to-stream (make-instance 'labelled-expression
                                              :tag (second expr)
                                              :expr (make-instance 'maxima-native-expr :expr (third expr)))
                               stream)
            ;; ELSE: No label
            (progn
              ;; Are all mlabel entries without a label just plain strings?
              (assert (stringp (third expr)))
              (format stream "~a" (third expr))))
        ;; ELSE: Plain expression
        (present-to-stream (make-instance 'maxima-native-expr :expr expr) stream))))

(wrap-function maxima::mread-synerr (fmt &rest args)
  (let ((stream (maxima-io/clim-stream *standard-output*))
        (string (apply #'format nil fmt args)))
    (clim:with-drawing-options (stream :ink clim:+red+)
      (format stream "~a" string))))
