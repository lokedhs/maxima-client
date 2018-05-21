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

(defclass labelled-expression ()
  ((tag :initarg :tag
          :reader labelled-expression/tag)
   (expr  :initarg :expr
          :reader labelled-expression/expr)))

(clim:define-presentation-method clim:present (obj (type labelled-expression) stream (view t) &key)
  (let* ((name (format-sym-name (labelled-expression/tag obj)))
         (s (if (and (plusp (length name))
                     (eql (aref name 0) #\$))
                (subseq name 1)
                name)))
    (clim:formatting-table (stream)
      (clim:formatting-row (stream)
        (clim:formatting-cell (stream :align-y :center)
          (format stream "(~a)" s))
        (clim:formatting-cell (stream :align-y :center)
          (present-to-stream (labelled-expression/expr obj) stream))))))

(clim:define-presentation-type maxima-expression ()
  :inherit-from t)

(clim:define-presentation-type plain-text ()
  :inherit-from 'string)

(defun read-plain-text (stream
                        &key
                          (input-wait-handler clim:*input-wait-handler*)
		          (pointer-button-press-handler clim:*pointer-button-press-handler*)
		          click-only)
  (declare (ignore click-only))
  (let ((result (make-array 1 :adjustable t :fill-pointer 0 :element-type 'character)))
    (loop
      for first-char = t then nil
      for gesture = (clim:read-gesture :stream stream
		                       :input-wait-handler input-wait-handler
		                       :pointer-button-press-handler
		                       pointer-button-press-handler)
      do (log:info "got gesture: ~s" gesture)
      do (cond ((or (null gesture)
		    (clim:activation-gesture-p gesture)
		    (typep gesture 'clim:pointer-button-event)
		    (clim:delimiter-gesture-p gesture))
		(loop-finish))
	       ((characterp gesture)
		(vector-push-extend gesture result))
	       (t nil))
      finally (progn
		(when gesture
		  (clim:unread-gesture gesture :stream stream))
		(return (subseq result 0))))))

(clim:define-presentation-method clim:accept ((type plain-text) stream (view clim:textual-view)
                                                                &key (default nil defaultp)
                                                                (default-type type))
  (let ((result (read-plain-text stream)))
    (cond ((and (zerop (length result)) defaultp)
           (values default default-type))
          (t (values result type)))))

(clim:define-presentation-method clim:accept ((type maxima-expression) stream (view clim:textual-view) &key)
  (let ((s (clim:accept 'plain-text :stream stream :view view :prompt nil :history 'string)))
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
	      (clim:accept 'maxima-expression :stream stream :view view :prompt nil :history 'maxima-expression)))
      (t
       (funcall (cdar clim:*input-context*) object type event options)))))

(clim:define-presentation-translator maxima-native-expr-to-expr (maxima-native-expr maxima-expression maxima-commands)
    (object)
  (maxima-native-expr/expr object))

(defmethod clim:read-frame-command ((frame maxima-main-frame) &key (stream *standard-input*))
  (multiple-value-bind (object type)
      (let ((clim:*command-dispatchers* '(#\:)))
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
  ;; It would be more logical to put this in the ACCEPT method, but by
  ;; then the prompt has already been printed, so we'll update it here
  ;; instead.
  (when (or (not (maxima::checklabel maxima::$inchar))
	    (not (maxima::checklabel maxima::$outchar)))
    (incf maxima::$linenum))
  (format stream "~a " (maxima::main-prompt)))

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
  (let ((c-tag (maxima::makelabel maxima::$inchar)))
    (setf (symbol-value c-tag) cmd)
    (let* ((maxima-stream (make-instance 'maxima-io :clim-stream *standard-output*))
           (eval-ret (catch 'maxima::macsyma-quit
                       (let ((result (let ((*standard-output* maxima-stream)
                                           (*standard-input* maxima-stream))
                                       (maxima::meval* cmd))))
                         (log:info "Result: ~s" result)
                         (let ((d-tag (maxima::makelabel maxima::$outchar)))
                           (setf (symbol-value d-tag) result)
                           (let ((obj (make-instance 'maxima-native-expr :expr result)))
                             (clim:with-room-for-graphics (*standard-output* :first-quadrant nil)
                               (clim:surrounding-output-with-border (*standard-output* :padding 10 :ink clim:+transparent-ink+)
                                 (present-to-stream (make-instance 'labelled-expression
                                                                   :tag d-tag
                                                                   :expr obj)
                                                    *standard-output*)))))))))
      (let ((content (maxima-stream-text maxima-stream)))
        (cond ((eq eval-ret 'maxima::maxima-error)
               (present-to-stream (make-instance 'maxima-error
                                                 :cmd cmd
                                                 :content content)
                                  *standard-output*))
              (t
               (when (plusp (length content))
                 (log:info "Output from command: ~s" content))))))))

(clim:define-command (maxima-quit :name "Quit" :menu t :command-table maxima-commands)
    ()
  (clim:frame-exit clim:*application-frame*))
