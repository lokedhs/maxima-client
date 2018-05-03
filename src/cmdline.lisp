(in-package :maxima-client)

(clim:define-command-table maxima-commands)

(defclass maxima-interactor-view (clim:textual-view)
  ())

(defparameter +listener-view+ (make-instance 'maxima-interactor-view))

(defclass maxima-interactor-pane (clim:interactor-pane)
  ())

(clim:define-application-frame maxima-main-frame ()
  ()
  (:panes (text-content (clim:make-clim-stream-pane :type 'maxima-interactor-pane
                                                    :name 'maxima-interactor
                                                    :default-view +listener-view+)))
  (:top-level (clim:default-frame-top-level :prompt 'print-listener-prompt))
  (:command-table (maxima-main-frame :inherit-from (maxima-commands)))
  (:layouts (default (clim:vertically ()
                       text-content))))

(defmethod clim:read-frame-command ((frame maxima-main-frame) &key (stream *standard-input*))
  (multiple-value-bind (object type)
      (let ((clim:*command-dispatchers* '(#\;)))
        (clim:with-text-style (stream (clim:make-text-style :fix :roman :normal))
          (clim:accept 'string :stream stream :prompt nil :default "" :default-type 'string)))
    (log:info "Got input: object=~s, type=~s" object type)
    (let ((expression (string-trim " " object)))
      (unless (equal expression "")
        `(maxima-eval ,expression)))))

(defmethod clim:stream-present :around ((stream maxima-interactor-pane) object type
                                   &rest args
                                   &key (single-box nil single-box-p) &allow-other-keys)
  (declare (ignore single-box single-box-p))
  (apply #'call-next-method stream object type :single-box t args))

(defun print-listener-prompt (stream frame)
  (declare (ignore frame))
  (format stream "maxima> "))

(defun maxima-client ()
  (let ((frame (clim:make-application-frame 'maxima-main-frame)))
    (clim:run-frame-top-level frame)))

(clim:define-command (maxima-eval :name "Run command" :menu t :command-table maxima-commands)
    ((cmd 'string :prompt "command"))
  (log:info "eval command. form=~s" cmd)
  (let* ((form (with-input-from-string (s (format nil "~a;~%" cmd))
                 (maxima::dbm-read s nil nil))))
    (assert (and (listp form)
                 (= (length form) 3)
                 (equal (first form) '(maxima::displayinput))
                 (null (second form))))
    #-nil
    (let* ((result (maxima::toplevel-macsyma-eval (third form)))
           (output-record (make-expression-output-record *standard-output* result)))
      (clim:with-room-for-graphics (*standard-output*)
        (clim:surrounding-output-with-border (*standard-output* :padding 10 :ink clim:+transparent-ink+)
          (clim:stream-add-output-record *standard-output* output-record))))
    #+nil
    (let* ((result (maxima::toplevel-macsyma-eval (third form)))
           (expr (make-instance 'maxima-native-expr :expr result)))
      (write expr :stream *standard-output*))))
