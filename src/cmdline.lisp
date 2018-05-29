(in-package :maxima-client)

(clim:define-command-table maxima-commands)

(defclass maxima-interactor-pane (clim:interactor-pane)
  ())

(clim:define-application-frame maxima-main-frame ()
  ()
  (:panes (text-content (clim:make-clim-stream-pane :type 'maxima-interactor-pane
                                                    :name 'maxima-interactor
                                                    :default-view +listener-view+)))
  (:menu-bar maxima-menubar-command-table)
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
        (clim:formatting-cell (stream :align-y :center :min-width 75)
          (format stream "(~a)" s))
        (clim:formatting-cell (stream :align-y :center)
          (present-to-stream (labelled-expression/expr obj) stream))))))

(clim:define-presentation-type plain-text ()
  :inherit-from 'string)

(clim:define-presentation-type maxima-empty-input ())

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
		                       :pointer-button-press-handler pointer-button-press-handler)
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
                                                                &key
                                                                (default nil defaultp)
                                                                (default-type type))
  (let ((result (read-plain-text stream)))
    (log:info "Got string from reading plain-text: ~s" result)
    (cond ((and (equal result "") defaultp)
           (values default default-type))
          (t (values result type)))))

(clim:define-presentation-method clim:accept ((type maxima-native-expr) stream (view clim:textual-view)
                                                                        &key
                                                                        (default nil defaultp)
                                                                        (default-type type))
  (let ((s (read-plain-text stream)))
    (log:info "Got string from reading native expr: ~s" s)
    (let ((trimmed (string-trim " " s)))
      (if (equal trimmed "")
          (if defaultp
              (values default default-type)
              nil)
          (string-to-native-expr trimmed)))))

#+nil
(clim:define-presentation-method clim:present (obj (type maxima-expression) stream (view maxima-interactor-view) &key)
  (let ((output-record (make-expression-output-record stream obj)))
    (clim:with-room-for-graphics (stream)
      (clim:stream-add-output-record stream output-record))))

(clim:define-presentation-type maxima-expression-or-command
    (&key (command-table (clim:frame-command-table clim:*application-frame*)))
  :inherit-from t)

;;; As an alternative to the below method, the it is possible to create a presentation method
;;; specialised on string-stream that creates a plain-text representation of an expression
#+nil
(clim:define-presentation-method clim:present (obj (type maxima-native-expr) (stream string-stream) (view t) &key)
  (let ((expr (maxima-native-expr/expr obj)))
    (format stream "~a" (maxima-expr-as-string expr))))

#+nil
(defmethod drei::presentation-replace-input ((stream drei::drei-input-editing-mixin) object
                                             (type (eql 'maxima-native-expr)) view
                                             &rest args
                                             &key
                                               (buffer-start (drei::input-position stream))
                                               rescan
                                               query-identifier
                                               (for-context-type type))
  (declare (ignore query-identifier rescan for-context-type buffer-start))
  (climi::with-keywords-removed (args (:type :view :query-identifier :for-context-type))
    (apply #'clim:replace-input stream (maxima-native-expr/src object) args)))

(clim:define-presentation-method clim:accept ((type maxima-expression-or-command)
                                              stream
				              (view clim:textual-view)
				              &key)
  (let ((command-ptype `(clim:command :command-table ,command-table)))
    (clim:with-input-context (`(or ,command-ptype maxima-native-expr))
        (object type event options)
        (let ((initial-char (clim:read-gesture :stream stream :peek-p t)))
	  (if (member initial-char clim:*command-dispatchers*)
	      (progn
		(clim:read-gesture :stream stream)
		(clim:accept command-ptype :stream stream :view view :prompt nil :history 'clim:command))
	      (clim:accept 'maxima-native-expr :stream stream :view view :prompt nil :history 'maxima-native-expr)))
      (t
       (funcall (cdar clim:*input-context*) object type event options)))))

(clim:define-presentation-translator maxima-to-plain-text (maxima-native-expr plain-text maxima-commands)
    (object)
  (log:info "Converting to text: ~s" object)
  (maxima-native-expr/src object))

(clim:define-presentation-translator plain-text-to-maxima (plain-text maxima-native-expr maxima-commands)
    (object)
  (log:info "Converting to maxima expr: ~s" object)
  (string-to-native-expr object))

(defmethod clim:read-frame-command ((frame maxima-main-frame) &key (stream *standard-input*))
  (handler-case
      (multiple-value-bind (object type)
          (let ((clim:*command-dispatchers* '(#\:)))
            (clim:with-text-style (stream (clim:make-text-style :fix :roman :normal))
              (clim:accept 'maxima-expression-or-command :stream stream :prompt nil :default nil :default-type 'maxima-empty-input)))
        (log:info "Got input: object=~s, type=~s" object type)
        (cond
          ((null object)
           nil)
          ((eq type 'maxima-native-expr)
           `(maxima-eval ,object))
          ((and (listp type) (eq (car type) 'clim:command))
           object)))
    (maxima-native-error (condition)
      (render-error-message stream (format nil "~a" condition))
      nil)))

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
  ;; This variable is needed sometimes inside Maxima, but it's not set by default
  (unless (boundp 'maxima::*maxima-tempdir*)
    (setq maxima::*maxima-tempdir* #p"/tmp/"))
  ;; Set up default plot options
  (setf (getf maxima::*plot-options* :plot_format) 'maxima::$clim)
  ;;
  (let ((frame (clim:make-application-frame 'maxima-main-frame
                                            :width 900
                                            :height 600)))
    (clim:run-frame-top-level frame)))

(clim:define-command (maxima-eval :name "Eval expression" :menu t :command-table maxima-commands)
    ((cmd 'maxima-native-expr :prompt "expression"))
  (let ((c-tag (maxima::makelabel maxima::$inchar)))
    (setf (symbol-value c-tag) cmd)
    (let* ((maxima-stream (make-instance 'maxima-io :clim-stream *standard-output*))
           (eval-ret (catch 'maxima::macsyma-quit
                       (let ((result (let ((*use-clim-retrieve* t)
                                           (*current-stream* *standard-output*)
                                           (*standard-output* maxima-stream)
                                           (*standard-input* maxima-stream))
                                       (maxima::meval* (maxima-native-expr/expr cmd)))))
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

(clim:make-command-table 'maxima-menubar-command-table
                         :errorp nil
                         :menu '(("File" :menu maxima-file-command-table)
                                 ("Plot" :menu maxima-plot-command-table)))

(clim:make-command-table 'maxima-file-command-table
                         :errorp nil
                         :menu '(("Quit" :command maxima-quit)))

(clim:make-command-table 'maxima-plot-command-table
                         :errorp nil
                         :menu '(("Discrete" :command plot2d-with-range)
                                 ("Plot examle" :command plot2d-demo)))
