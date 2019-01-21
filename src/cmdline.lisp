(in-package :maxima-client)

(defvar maxima::$font_size 14)

(clim:define-command-table expression-commands)

(clim:define-command-table maxima-commands
  :inherit-from (maxima-client.markup:text-commands expression-commands maxima-client.workbench:workbench-commands))

#+nil
(defmethod clim:additional-command-tables append ((drei drei:drei-pane) (command-table maxima-table))
  '())

(defvar maxima::$submit_on_return t
  "If true, pressing return will terminate input, and shift-return is
needed in order to add a newline. If false, a command needs to be
terminated by ;.")

(defclass maxima-interactor-pane (clim:interactor-pane)
  ())

(defclass maxima-pointer-documentation-view (maxima-renderer-view
                                             clim:pointer-documentation-view)
  ())

(defparameter +maxima-pointer-documentation-view+ (make-instance 'maxima-pointer-documentation-view))

(defclass maxima-input-expression ()
  ((expr :initarg :expr
         :reader maxima-input-expression/expr)
   (src  :initarg :src
         :reader maxima-input-expression/src)))

(defun ensure-expression-finished (string)
  (let ((is-completed (cl-ppcre:scan "[;$] *$" string)))
    (format nil "~a~a~%" string (if is-completed "" ";"))))

(defun string-to-input-expression (string)
  (log:info "parsing: ~s" (ensure-expression-finished string))
  (with-input-from-string (in (format nil "~a~%" (ensure-expression-finished string)))
    (let ((result (maxima::dbm-read in)))
      (log:info "parse result = ~s" result)
      (make-instance 'maxima-input-expression :expr result :src string))))

(clim:define-application-frame maxima-main-frame ()

  ((info-app :initform nil
             :accessor maxima-main-frame/info-app))
  (:panes (text-content (clim:make-clim-stream-pane :type 'maxima-interactor-pane
                                                    :name 'maxima-interactor
                                                    :default-view +listener-view+
                                                    :display-function 'display-cmdline-content
                                                    :incremental-redisplay t))
          #+nil (notes (clim:make-pane 'maxima-client.notes:notes-pane))
          #+nil (info-panel (maxima-client.doc:make-info-panel))
          (doc :pointer-documentation :default-view +maxima-pointer-documentation-view+))
  (:menu-bar maxima-menubar-command-table)
  (:top-level (clim:default-frame-top-level :prompt 'print-listener-prompt))
  (:command-table (maxima-main-frame :inherit-from (maxima-commands)))
  (:layouts (default (clim:vertically ()
                       #+nil (maxima-client.workbench:make-workbench :name 'workbench :root-pane text-content
                                                               :panels `((,notes :title "Notes"
                                                                                 :image ,(load-image "outline-notes-32.png")
                                                                                 :select-fn ,#'maxima-client.notes:focus-notes-pane)
                                                                         (,info-panel :title "Info"
                                                                                      :image ,(load-image "outline-info-32.png"))))
                       text-content
                       doc))))

(defun display-cmdline-content (frame stream)
  (declare (ignore frame))
  (clim:with-room-for-graphics (stream :first-quadrant nil)
    (maxima-client.markup:display-markup
     stream
     `((:heading "Maxima " ,(maxima::maxima-version1))
       (:p "Maxima project web site: " (:link "http://maxima.sourceforge.net/")
           (:newline) "CLIM interface code at " (:link "https://github.com/lokedhs/maxima-client"))
       (:p (:key (:meta "p")) ", " (:key (:meta "n")) " iterates through the command history."
           (:newline) (:key (:meta "s")) " inserts special symbols."
           (:newline) (:key ("TAB")) " provides command completion for Maxima symbols."
           (:newline) "The : character is used as a prefix to access CLIM commands.")
       (:p (:code ":Quit") " to quit the application"
           (:newline) (:code ":Lisp") " evaluates Lisp forms"))))
  (multiple-value-bind (x y)
      (clim:stream-cursor-position stream)
    (setf (clim:stream-cursor-position stream) (values x (+ y (char-height stream))))))

(defgeneric presentation-pointer-motion (presentation x y)
  (:method (presentation x y)
    nil))

(defmethod clim-internals::frame-input-context-track-pointer ((frame maxima-main-frame)
                                                              input-context
                                                              stream
                                                              event)
  (let* ((x (clim-internals::device-event-x event))
         (y (clim-internals::device-event-y event))
         (presentation (find-presentation-at-pos stream x y)))
    (when presentation
      (presentation-pointer-motion presentation x y)))
  (call-next-method))

(defclass labelled-expression ()
  ((tag :initarg :tag
          :reader labelled-expression/tag)
   (expr  :initarg :expr
          :reader labelled-expression/expr)))

(clim:define-presentation-method clim:present (obj (type labelled-expression) stream (view t) &key)
  (let* ((name (format-sym-name (labelled-expression/tag obj))))
    (clim:formatting-table (stream)
      (clim:formatting-row (stream)
        (clim:formatting-cell (stream :align-y :center :min-width 75)
          (clim:with-text-family (stream :sans-serif)
            (format stream "(~a)" name)))
        (clim:formatting-cell (stream :align-y :center)
          (present-to-stream (labelled-expression/expr obj) stream))))))

(defclass maxima-input-error ()
  ((command :initarg :command
            :reader maxima-input-error/command)
   (message :initarg :message
            :reader maxima-input-error/message)))

(clim:define-presentation-type maxima-empty-input ())

(clim:define-presentation-type maxima-lisp-package-form
    ()
  :inherit-from 'clim:form)

(clim:define-presentation-method clim:present (obj (type plain-text) stream (view clim:textual-view) &key)
  (format stream "~a" obj))

(clim:define-presentation-method clim:present (obj (type plain-text) (stream string-stream) (view t) &key)
  (format stream "~a" obj))

(clim:define-presentation-method clim:present (obj (type maxima-native-expr) stream
                                                   (view maxima-pointer-documentation-view) &key)
  (format stream "~a" (maxima-native-expr/src obj)))

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

(clim:define-presentation-method clim:accept ((type plain-text)
                                              stream (view clim:textual-view)
                                              &key
                                              (default nil defaultp)
                                              (default-type type))
  (let ((result (read-plain-text stream)))
    (log:trace "Got string from reading plain-text: ~s" result)
    (cond ((and (equal result "") defaultp)
           (values default default-type))
          (t (values result type)))))

(defmethod clim:presentation-replace-input ((stream drei:drei-input-editing-mixin) (obj maxima-native-expr) type view
                                            &key (buffer-start nil buffer-start-p) (rescan nil rescan-p)
                                              query-identifier
                                              for-context-type)
  #+nil (declare (ignore query-identifier for-context-type))
  (log:trace "Replacing input for ~s, bs=~s, bs-p=~s, rescan=~s, rsp=~s, qi=~s fct=~s"
             obj buffer-start buffer-start-p rescan rescan-p query-identifier for-context-type)
  (apply #'clim:presentation-replace-input stream (maxima-native-expr/src obj) 'plain-text view
         (append (if buffer-start-p (list :buffer-start buffer-start) nil)
                 (if rescan-p (list :rescan rescan) nil))))

(defmethod clim:presentation-replace-input ((stream drei:drei-input-editing-mixin) (obj maxima-input-error) type view
                                            &key (buffer-start nil buffer-start-p) (rescan nil rescan-p)
                                              query-identifier
                                              for-context-type)
  #+nil (declare (ignore query-identifier for-context-type))
  (log:trace "Replacing input for ~s, bs=~s, bs-p=~s, rescan=~s, rsp=~s, qi=~s fct=~s"
             obj buffer-start buffer-start-p rescan rescan-p query-identifier for-context-type)
  (apply #'clim:presentation-replace-input stream (maxima-input-error/command obj) 'plain-text view
         (append (if buffer-start-p (list :buffer-start buffer-start) nil)
                 (if rescan-p (list :rescan rescan) nil))))

(defmethod clim:presentation-replace-input ((stream drei:drei-input-editing-mixin) (obj maxima-input-expression) type view
                                            &key (buffer-start nil buffer-start-p) (rescan nil rescan-p)
                                              query-identifier
                                              for-context-type)
  #+nil (declare (ignore query-identifier for-context-type))
  (log:trace "Replacing input for ~s, bs=~s, bs-p=~s, rescan=~s, rsp=~s, qi=~s fct=~s"
             obj buffer-start buffer-start-p rescan rescan-p query-identifier for-context-type)
  (apply #'clim:presentation-replace-input stream (maxima-input-expression/src obj) 'plain-text view
         (append (if buffer-start-p (list :buffer-start buffer-start) nil)
                 (if rescan-p (list :rescan rescan) nil))))

(defun cleanup-input (string)
  "Removes final ; in the string, if it exists"
  (loop
    for i from (1- (length string)) downto 0
    for ch = (aref string i)
    when (eql ch #\;)
      return (subseq string 0 i)
    while (eql ch #\Space)
    finally (return string)))

(defmethod drei:handle-drei-condition (drei (condition maxima-expr-parse-error))
  (drei::with-minibuffer-stream (minibuffer)
    (format minibuffer "~a" condition))
  ;; TODO: Move point to the error position
  (let ((view (clim:view drei)))
    (when (drei:point-mark-view-p view)
      (setf (drei-buffer:offset (drei:point view))
            (maxima-expr-parse-error/pos condition)))))

(defun submit-command-p (command)
  (let ((trimmed (string-trim " " command)))
    (or (alexandria:ends-with-subseq ";" trimmed)
        (alexandria:ends-with-subseq "$" trimmed))))

(clim:define-presentation-method clim:accept ((type maxima-input-expression)
                                              (stream drei:drei-input-editing-mixin)
                                              (view clim:textual-view)
                                              &key)
  (drei:with-drei-options ((drei:drei-instance stream)
                           :syntax "Maxima"
                           :keep-syntax t)
    (let ((clim:*completion-gestures* nil)
          (clim:*possibilities-gestures* nil))
      (clim:with-delimiter-gestures (nil :override t)
        (loop
          named control-loop
          with drei = (drei:drei-instance stream)
          ;;with syntax = (drei:syntax (clim:view drei))
          ;; The input context permits the user to mouse-select displayed
          ;; Lisp objects and put them into the input buffer as literal
          ;; objects.
          for gesture = (labels ((insert-into-editor (object)
                                   (drei:performing-drei-operations (drei :with-undo t
                                                                          :redisplay t)
                                     (clim:presentation-replace-input
                                      stream object type (clim:view drei)
                                      :buffer-start (clim:stream-insertion-pointer stream)
                                      :allow-other-keys t
                                      :accept-result nil
                                      :rescan t))
                                   (clim:rescan-if-necessary stream)
                                   nil))
                          (clim:with-input-context ('(or maxima-native-expr maxima-input-error) :override nil)
                              (object type)
                              (clim:read-gesture :stream stream)
                            (maxima-native-expr
                             (insert-into-editor object))
                            (maxima-input-error
                             (insert-into-editor object))))

          for current-command = (let* ((buffer (drei:buffer (clim:view drei)))
                                       (start (drei::input-position stream))
                                       (end (drei::size buffer)))
                                  (drei::buffer-substring buffer start end))

          ;; True if `gesture' was freshly read from the user, and not
          ;; just retrieved from the buffer during a rescan.
          for freshly-inserted = (and (plusp (clim:stream-scan-pointer stream))
                                      (not (equal (drei::buffer-object
                                                   (drei:buffer (clim:view drei))
                                                   (1- (clim:stream-scan-pointer stream)))
                                                  gesture)))
          ;;for form = (drei-lisp-syntax::form-after syntax (drei::input-position stream))
          ;; We do not stop until the input is complete and an activation
          ;; gesture has just been provided. The freshness check is so
          ;; #\Newline characters in the input will not cause premature
          ;; activation.
          until (and (clim:activation-gesture-p gesture)
                     (and freshly-inserted
                          (or (and maxima::$submit_on_return
                                   (let ((gesture-event (climi::last-gesture (clim::encapsulating-stream-stream stream))))
                                     (and (typep gesture-event 'clim:keyboard-event)
                                          (not (gesture-modifier-p gesture-event :shift)))))
                              (and (not maxima::$submit_on_return)
                                   (submit-command-p current-command)))))
          ;; We only want to process the gesture if it is fresh,
          ;; because if it isn't, it has already been processed at
          ;; some point in the past.
          when (and (clim:activation-gesture-p gesture)
                    freshly-inserted)
            do (progn
                 (clim:with-activation-gestures (nil :override t)
                   (clim:stream-process-gesture stream gesture nil)))
          finally
             (progn
               (clim:unread-gesture gesture :stream stream)
               (let ((string (cleanup-input current-command)))
                 (handler-case
                     (let* ((object (string-to-input-expression string)
                                    #+nil
                                    (handler-case
                                        (string-to-native-expr (cleanup-input current-command))
                                      (maxima-expr-parse-error (condition)
                                        ;; Move point to the problematic form
                                        ;; and signal a rescan.
                                        (setf (drei::activation-gesture stream) nil)
                                        (drei:handle-drei-condition drei condition)
                                        (drei:display-drei drei :redisplay-minibuffer t)
                                        (clim:immediate-rescan stream))))
                            (ptype (clim:presentation-type-of object)))
                       (return-from control-loop
                         (values object
                                 (if (clim:presentation-subtypep ptype 'maxima-input-expression)
                                     ptype 'maxima-input-expression-expr))))
                   (maxima-native-error (condition)
                     (return-from control-loop
                       (values (make-instance 'maxima-input-error
                                              :command string
                                              :message (maxima-expr-parse-error/message condition))
                               'maxima-input-error)))))))))))

(clim:define-presentation-method clim:accept ((type maxima-native-expr)
                                              (stream drei:drei-input-editing-mixin)
                                              (view clim:textual-view)
                                              &key)
  (let ((result (clim:accept 'maxima-input-expression :stream stream :view view :prompt nil)))
    (log:info "RES = ~s" result)
    (when (typep result 'maxima-input-expression)
      (log:info "expr = ~s, src = ~s"
                (maxima-input-expression/expr result)
                (maxima-input-expression/src result)))
    (make-instance 'maxima-native-expr
                   :expr (third (maxima-input-expression/expr result))
                   :src (maxima-input-expression/src result))))

(clim:define-presentation-method clim:accept ((type maxima-lisp-package-form)
                                              stream
                                              (view clim:textual-view)
                                              &key)
  (with-maxima-package
    (clim:accept 'clim:form :stream stream :view view :prompt nil)))

(clim:define-presentation-type maxima-expression-or-command
    (&key (command-table (clim:frame-command-table clim:*application-frame*)))
  :inherit-from t)

(clim:define-presentation-method clim:accept ((type maxima-expression-or-command)
                                              stream
				              (view clim:textual-view)
				              &key)
  (let ((command-ptype `(clim:command :command-table ,command-table)))
    (clim:with-input-context (command-ptype)
        (object type event options)
        ;; There used to be only a single WITH-INPUT-CONTEXT here,
        ;; using a type in the form (OR command maxima-native-expr),
        ;; but that resulted in the wrong behaviour when an expression
        ;; was clicked.
        ;;
        ;; What happened was that any presentation to command
        ;; translators got higher priority than the regular handler in
        ;; the input context. Thus, that handler was never called. By
        ;; using two separate input contexts causes the handler to be
        ;; called first.
        ;;
        ;; Arguably this should be the default behaviour, and if that
        ;; is fixed, this hack should no longer be needed.
        (clim:with-input-context ('maxima-native-expr :override nil)
            (inner-object inner-type inner-event inner-options)
            (let ((initial-char (clim:read-gesture :stream stream :peek-p t)))
	      (if (member initial-char clim:*command-dispatchers*)
	          (progn
		    (clim:read-gesture :stream stream)
                    (clim:accept command-ptype :stream stream :view view :prompt nil :history 'clim:command))
	          (progn
                    (clim:accept 'maxima-input-expression :stream stream :view view :prompt nil
                                                          :history 'maxima-expression-or-command :replace-input t))))
          (maxima-native-expr
           (log:trace "a native expr was found: obj=~s type=~s ev=~s options=~s"
                      inner-object inner-type inner-event inner-options)
           (clim:accept 'maxima-native-expr :stream stream :view view :prompt nil
                                            :history 'maxima-expression-or-command :replace-input t
                                            :default inner-object :insert-default t)))
      (t
       (log:trace "other command type: obj=~s type=~s ev=~s options=~s" object type event options)
       (funcall (cdar clim:*input-context*) object type event options)))))

(clim:define-presentation-method clim:accept ((type maxima-native-symbol)
                                              stream
                                              (view clim:textual-view)
                                              &key prompt default)
  (let* ((result (clim:accept 'plain-text :stream stream :prompt prompt :default default))
         (expr (string-to-maxima-expr result)))
    (unless (symbolp expr)
      (error "Input was not a symbol"))
    (values expr 'maxima-native-symbol)))

(clim:define-presentation-translator maxima-to-plain-text (maxima-native-expr plain-text maxima-commands)
    (object)
  (log:trace "Converting to text: ~s" object)
  (maxima-native-expr/src object))

(clim:define-presentation-translator plain-text-to-maxima (plain-text maxima-native-expr maxima-commands)
    (object)
  (log:trace "Converting to maxima expr: ~s" object)
  (string-to-native-expr object))

(clim:define-presentation-translator maxima-to-lisp-expression (maxima-native-expr clim:expression maxima-commands)
    (object)
  (log:trace "Converting to lisp expression: ~s" object)
  (maxima-native-expr/expr object))

(clim:define-presentation-translator symbol-to-plain-text (maxima-native-symbol plain-text maxima-commands)
    (object)
  (log:trace "Converting to string: ~s" object)
  (format-sym-name object))

(wrap-function maxima::dbm-read (&optional (stream *standard-input*) (eof-error-p t)
		                           (eof-value nil) repeat-if-newline)
  (typecase stream
    (maxima-io
     (let ((pane (maxima-io/clim-stream stream))
           (mprompt maxima::*mread-prompt*)
           (maxima::*mread-prompt* ""))
       (declare (ignore mprompt))
       (multiple-value-bind (object type)
           (let ((clim:*command-dispatchers* '(#\:)))
             (clim:with-text-style (pane (clim:make-text-style :fix :roman :normal))
               (clim:accept 'maxima-expression-or-command :stream pane :prompt nil
                                                          :default nil :default-type 'maxima-empty-input
                                                          :history 'maxima-expression-or-command
                                                          :replace-input t)))
         (log:trace "Got input: object=~s, type=~s" object type)
         (cond
           ((null object)
            nil)
           ((eq type 'maxima-input-expression)
            (maxima-input-expression/expr object))
           ((eq type 'maxima-input-error)
            (let ((size (clim:text-style-size (clim:medium-text-style pane))))
              (clim:with-drawing-options (pane :ink clim:+red+ :text-style (clim:make-text-style :fix :roman size))
                (format pane "~a" (maxima-input-error/message object))))
            nil)
           ((and (listp type) (eq (car type) 'clim:command))
            (clim:execute-frame-command clim:*application-frame* object)
            nil)))))
    (t
     (funcall *old-fn-dbm-read* stream eof-error-p eof-value repeat-if-newline))))

(defmethod clim:read-frame-command ((frame maxima-main-frame) &key (stream *standard-input*))
  (let* ((maxima-stream (make-instance 'maxima-io :clim-stream stream))
         (*current-stream* stream)
         (*standard-output* maxima-stream)
         (*standard-input* maxima-stream)
         (*use-clim-retrieve* t))
    (unwind-protect
         (maxima::continue :stream maxima-stream :one-shot t)
      (log:trace "After continue"))))

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
  (let ((fonts-location (or *font-directory*
                            (merge-pathnames #p"fonts/tex/" (asdf:component-pathname (asdf:find-system :maxima-client))))))
    (mcclim-fontconfig:app-font-add-dir fonts-location))
  (with-maxima-package
    (maxima::initialize-runtime-globals))
  (setq *debugger-hook* nil)
  ;; Set up default plot options
  ;(setf (getf maxima::*plot-options* :plot_format) 'maxima::$clim)
  ;;
  (let ((frame (clim:make-application-frame 'maxima-main-frame
                                            :width 900
                                            :height 600)))
    (clim:run-frame-top-level frame)))

(defvar *catch-errors* t)

(clim:define-command (maxima-quit :name "Quit" :menu t :command-table maxima-commands)
    ()
  (clim:frame-exit clim:*application-frame*))

(clim:define-command (maxima-eval-lisp-expression :name "Lisp" :menu "Eval Lisp form" :command-table maxima-commands)
    ((form maxima-lisp-package-form :prompt "Form"))
  (let ((stream (find-interactor-pane))
        (result (with-maxima-package
                  (maxima::eval form))))
    (format t "~&")
    (clim:with-output-as-presentation (stream result (clim:presentation-type-of result) :single-box t)
      (clim:present result 'clim:expression :stream stream))))

(defun maxima-eval-lisp-expr (expr)
  (maxima-eval (make-instance 'maxima-native-expr :expr expr)))

(clim:define-command (info-command :name "Info" :menu "Info" :command-table maxima-commands)
    ((name '(or maxima-client.doc-new:maxima-function-name  maxima-native-symbol) :prompt "Name"))
  (log:info "type: ~s" name)
  (let ((string (etypecase name
                  (string name)
                  (symbol (format-sym-name name)))))
    (maxima-client.doc-new:display-function-help string)))

(clim:define-command (font-size-command :name "Set font size" :menu t :command-table maxima-commands)
    ((size integer :prompt "points"))
  (setq maxima::$font_size size)
  (format t "~%Maths font size changed to ~a~%~%" size))

(clim:define-command (enable-submit-on-return-command :name "Enable submit on return" :menu t :command-table maxima-commands)
    ()
  (setq maxima::$submit_on_return t)
  (format t "~%Statements are terminated by typing RETURN.~%~%"))

(clim:define-command (disable-submit-on-return-command :name "Enable submit on return" :menu t :command-table maxima-commands)
    ()
  (setq maxima::$submit_on_return nil)
  (format t "~%Statements must be terminated by a semicolon.~%~%"))

(clim:define-command (show-documentation-frame-command :name "Show Documentation Frame" :menu t :command-table maxima-commands)
    ()
  (maxima-client.doc-new:open-documentation-frame '(:file "maxima-client")))

(clim:define-command (cmd-show-maxima-manual :name "Maxima Documentation" :menu t :command-table maxima-commands)
    ()
  (maxima-client.doc-new:open-documentation-frame `(:file ,(pathname-name maxima-client.doc-new:*maxima-toplvel-filename*))))

(clim:define-presentation-to-command-translator select-maxima-expression-maxima-command
    (maxima-native-expr copy-expression-as-maxima-command expression-commands
                        :echo nil :documentation "Copy expression as Maxima command")
    (obj)
  (list obj))

(clim:define-presentation-to-command-translator select-maxima-expression-latex
    (maxima-native-expr copy-expression-as-latex expression-commands
                        :echo nil :documentation "Copy expression as LaTeX")
    (obj)
  (list obj))

(clim:define-presentation-to-command-translator select-maxima-expression-notes
    (maxima-native-expr copy-maxima-expr-to-notes-command maxima-commands
                        :echo nil :documentation "Add expression to current notes")
    (obj)
  (list obj))

#+nil
(clim:define-command (copy-expression-as-maxima-command :name "Copy expression as text" :menu t :command-table expression-commands)
    ((expr maxima-native-expr :prompt "Expression"))
  (maxima-client.clipboard:bind-clipboard (find-interactor-pane) (maxima-native-expr/src expr)))

#+nil
(clim:define-command (copy-expression-as-latex :name "Copy expression as LaTeX" :menu t :command-table expression-commands)
    ((expr maxima-native-expr :prompt "Expression"))
  (maxima-client.clipboard:bind-clipboard (find-interactor-pane) (maxima-expr-to-latex (maxima-native-expr/expr expr))))

(clim:define-command (copy-maxima-expr-to-notes-command :name "Copy expression to notes" :menu t :command-table maxima-commands)
    ((expr maxima-native-expr :prompt "Expression"))
  (let ((notes-panel (clim:find-pane-named clim:*application-frame* 'notes)))
    (maxima-client.notes:insert-maxima-expr notes-panel expr)))

(clim:make-command-table 'maxima-menubar-command-table
                         :errorp nil
                         :menu '(("File" :menu maxima-file-command-table)
                                 ("Algebra" :menu maxima-algebra-command-table)
                                 ("Matrix" :menu maxima-matrix-command-table)
                                 ("Plot" :menu maxima-plot-command-table)
                                 ("Lisp" :menu maxima-lisp-command-table)
                                 ("Display" :menu maxima-interaction-command-table)
                                 ("Help" :menu maxima-help-command-table)))

(clim:make-command-table 'maxima-file-command-table
                         :errorp nil
                         :menu '(("Quit" :command maxima-quit)))

(clim:make-command-table 'maxima-plot-command-table
                         :errorp nil
                         :menu '(("Discrete" :command plot2d-with-range)
                                 ("Parametric" :command plot2d-with-range)
                                 ("Plot example" :command plot2d-demo)))

(clim:make-command-table 'maxima-lisp-command-table
                         :errorp nil
                         :menu '(("Eval Lisp Form" :command maxima-eval-lisp-expression)))

(clim:make-command-table 'font-size-command-table
                         :errorp nil
                         :menu '(("14" :command (font-size-command 14))
                                 ("18" :command (font-size-command 18))
                                 ("20" :command (font-size-command 20))
                                 ("24" :command (font-size-command 24))))

(clim:make-command-table 'submit-options-command-table
                         :errorp nil
                         :menu '(("Submit on return" :command enable-submit-on-return-command)
                                 ("Require closing symbol" :command disable-submit-on-return-command)))

(clim:make-command-table 'maxima-interaction-command-table
                         :errorp nil
                         :menu '(("Font size" :menu font-size-command-table)
                                 ("Submit style" :menu submit-options-command-table)))

(clim:make-command-table 'maxima-help-command-table
                         :errorp nil
                         :menu '(("Maxima-Client Introduction" :command (show-documentation-frame-command))
                                 ("Maxima Documentation" :command (cmd-show-maxima-manual))
                                 ("Symbol Help" :command info-command)))
