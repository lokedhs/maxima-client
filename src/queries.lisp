(in-package :maxima-client)

(defun name-from-sign (sign)
  (ecase sign
    (maxima::$pos "positive")
    (maxima::$neg "negative")
    (maxima::$zero "zero")
    (maxima::$nonzero "nonzero")))

(wrap-function maxima::ensure-sign (expr &optional domain squared)
  (block ensure-sign-wrapped-function
    (let ((stream *current-stream*)
          (new-sign (maxima::match-sign maxima::sign domain expr squared)))
      (when new-sign
        (return-from ensure-sign-wrapped-function new-sign))

      (let* ((buttons (case domain
                        (maxima::$znz '(:zero :nonzero))
                        (maxima::$pz '(:positive :zero))
                        (maxima::$nz '(:negative :zero))
                        (maxima::$pn '(:positive :negative))
                        (t '(:positive :negative :zero))))
             (val (car buttons)))
        (clim:accepting-values (stream)
          (format stream "~%Need category for expression: ~a~%~%" (maxima-expr-as-string expr))
          (setq val (clim:accept `(clim:completion ,buttons)
                                 :stream stream
                                 :view climi::+pop-up-menu-view+
                                 :prompt "Select category"
                                 :default val)))
        (setq maxima::sign (ecase val
                             (:positive 'maxima::$pos)
                             (:negative 'maxima::$neg)
                             (:zero 'maxima::$zero)
                             (:nonzero 'maxima::$nonzero)))
        (let ((prev-sign maxima::sign)
              (v (maxima::match-sign maxima::sign domain expr squared)))
          (unless v
            (error "Unable to parse result from ACCEPT"))
          (clim:with-room-for-graphics (stream :first-quadrant nil)
            (let* ((style (clim:medium-text-style stream))
                   (rec (make-rendered-output-record (stream)
                          (clim:with-drawing-options (stream :ink (clim:make-rgb-color 0 0.45 0))
                            (with-aligned-rendering (stream)
                              (render-aligned () (render-maxima-expression stream expr))
                              (aligned-spacing 5)
                              (clim:with-text-style (stream style)
                                (render-aligned-string "assumed to be ~a" (name-from-sign prev-sign))))))))
              (clim:stream-add-output-record stream rec)))
          v)))))
