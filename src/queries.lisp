(in-package :maxima-client)

(defvar *use-clim-retrieve* nil)
(defvar *current-stream* nil)

(defmacro wrap-function (name args &body body)
  (let ((new-fn-name (intern (concatenate 'string "CLIM-" (symbol-name name))))
        (wrapped-fn-name (intern (concatenate 'string "WRAPPED-" (symbol-name name))))
        (old-fn-ptr (intern (concatenate 'string "*OLD-FN-" (symbol-name name) "*"))))
    `(progn
       (defvar ,old-fn-ptr nil)
       (defun ,new-fn-name ,args ,@body)
       (defun ,wrapped-fn-name ,args
         (if *use-clim-retrieve*
             (,new-fn-name ,@(lambda-fiddle:extract-lambda-vars args))
             (funcall ,old-fn-ptr ,@(lambda-fiddle:extract-lambda-vars args))))
       (unless ,old-fn-ptr
         (setq ,old-fn-ptr (symbol-function ',name))
         (setf (symbol-function ',name) #',wrapped-fn-name)))))

#+nil
(wrap-function maxima::retrieve (msg flag)
  (declare (special maxima::behaviour))
  (log:info "in clim-retrieve: MSG: ~s FLAG: ~s, BEHAVIOUR: ~s" msg flag (if (boundp 'maxima::behaviour)
                                                                             maxima::behaviour
                                                                             :unbound))
  (funcall *old-fn-retrieve* msg flag))

(defun name-from-sign (sign)
  (ecase sign
    (maxima::$pos "positive")
    (maxima::$neg "negative")
    (maxima::$zero "zero")))

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
                                 :prompt "Select category:"
                                 :default val)))
        (setq maxima::sign (ecase val
                             (:positive 'maxima::$pos)
                             (:negative 'maxima::$neg)
                             (:zero 'maxima::$zero)))
        (let ((prev-sign maxima::sign)
              (v (maxima::match-sign maxima::sign domain expr squared)))
          (unless v
            (error "Unable to parse result from ACCEPT"))
          (clim:with-room-for-graphics (stream :first-quadrant nil)
            (let* ((style (clim:pane-text-style stream))
                   (rec (make-rendered-output-record (stream)
                          (clim:with-drawing-options (stream :ink (clim:make-rgb-color 0 0.45 0))
                            (with-aligned-rendering (stream)
                              (render-aligned () (render-maxima-expression stream expr))
                              (aligned-spacing 5)
                              (clim:with-text-style (stream style)
                                (render-aligned-string "assumed to be ~a" (name-from-sign prev-sign))))))))
              (clim:stream-add-output-record stream rec)))
          v)))))
