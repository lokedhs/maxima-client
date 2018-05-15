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

(clim:define-presentation-type maxima-expression ()
  :inherit-from t)

(clim:define-presentation-method clim:accept ((type maxima-expression) stream (view clim:textual-view) &key)
  (let ((s (clim:accept 'string :stream stream :view view :prompt nil :history 'string)))
    (let ((trimmed (string-trim " " s)))
      (if (equal trimmed "")
          nil
          (string-to-maxima-expr s)))))

(clim:define-presentation-method clim:present (obj (type maxima-expression) stream (view maxima-interactor-view) &key)
  (let ((output-record (make-expression-output-record stream obj)))
    (clim:with-room-for-graphics (stream)
      (clim:stream-add-output-record stream output-record))))

(clim:define-presentation-type maxima-expression-or-command
    (&key (command-table (clim:frame-command-table clim:*application-frame*)))
  :inherit-from t)

(clim:define-presentation-method clim:accept ((type maxima-expression-or-command) stream
				                                                  (view clim:textual-view)
				                                                  &key)
  (let ((command-ptype `(clim:command :command-table ,command-table)))
    (clim:with-input-context (`(or ,command-ptype form))
        (object type event options)
        (let ((initial-char (clim:read-gesture :stream stream :peek-p t)))
	  (if (member initial-char clim:*command-dispatchers*)
	      (progn
		(clim:read-gesture :stream stream)
		(clim:accept command-ptype :stream stream :view view :prompt nil :history 'clim:command))
	      (clim:accept 'maxima-expression :stream stream :view view :prompt nil :history 'string)))
      (t
       (funcall (cdar clim:*input-context*) object type event options)))))

(clim:define-presentation-translator maxima-native-expr-to-expr (maxima-native-expr maxima-expression maxima-commands)
    (object)
  (maxima-native-expr/expr object))

(defmethod clim:read-frame-command ((frame maxima-main-frame) &key (stream *standard-input*))
  (multiple-value-bind (object type)
      (let ((clim:*command-dispatchers* '(#\; #\:)))
        (clim:with-text-style (stream (clim:make-text-style :fix :roman :normal))
          (clim:accept 'maxima-expression-or-command :stream stream :prompt nil :default "" :default-type 'string)))
    (log:info "Got input: object=~s, type=~s" object type)
    (cond
      ((eq type 'maxima-expression)
       (when object
         `(maxima-eval ,object)))
      ((and (listp type) (eq (car type) 'clim:command))
       object))))

(defmethod clim:stream-present :around ((stream maxima-interactor-pane) object type
                                   &rest args
                                   &key (single-box nil single-box-p) &allow-other-keys)
  (declare (ignore single-box single-box-p))
  (apply #'call-next-method stream object type :single-box t args))

(defun print-listener-prompt (stream frame)
  (declare (ignore frame))
  (format stream "maxima> "))

(defun maxima-client ()
  (let ((frame (clim:make-application-frame 'maxima-main-frame
                                            :width 900
                                            :height 600)))
    (clim:run-frame-top-level frame)))

(defun string-to-maxima-expr (string)
  (with-input-from-string (s (format nil "~a;~%" string))
    (let ((form (maxima::dbm-read s nil nil)))
      (assert (and (listp form)
                   (= (length form) 3)
                   (equal (first form) '(maxima::displayinput))
                   (null (second form))))
      (third form))))

(clim:define-command (maxima-eval :name "Eval expression" :menu t :command-table maxima-commands)
    ((cmd 'maxima-expression :prompt "expression"))
  (let ((maxima-stream (make-instance 'maxima-output)))
    (let ((eval-ret (catch 'maxima::macsyma-quit
                      (let ((result (let ((*standard-output* maxima-stream))
                                      (maxima::meval* cmd))))
                        (log:info "Result: ~s" result)
                        (let ((obj (make-instance 'maxima-native-expr :expr result)))
                          (clim:with-room-for-graphics (*standard-output* :first-quadrant nil)
                            (clim:surrounding-output-with-border (*standard-output* :padding 10 :ink clim:+transparent-ink+)
                              (present-to-stream obj *standard-output*))))))))
      (when (eq eval-ret 'maxima::maxima-error)
        (present-to-stream (make-instance 'maxima-error
                                          :cmd cmd
                                          :content (maxima-stream-text maxima-stream))
                           *standard-output*)))))
