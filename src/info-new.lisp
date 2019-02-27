(in-package :maxima-client.doc-new)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (clim:define-presentation-type node ()
    :description "Node in the documentation")
  (clim:define-presentation-type category ()
    :description "A list of references to a category")
  (clim:define-presentation-type maxima-function-name ()
    :description "The name of a maxima function in the documentation"))

(defparameter *maxima-toplvel-filename* #p"include-maxima.lisp")

(clim:define-command-table info-commands
  :inherit-from (maxima-client.markup:text-commands))

(defclass info-content-panel-view (maxima-client::maxima-renderer-view maxima-client.markup:markup-text-view)
  ())

(defvar *doc-frame-lock* (bordeaux-threads:make-lock "doc-frame-lock"))
(defvar *doc-frame* nil)

(defvar *index-symbols* nil)
(defvar *index-nodes* nil)
(defvar *index-categories* nil)

(defparameter +info-content-panel-view+ (make-instance 'info-content-panel-view))

(clim:define-presentation-method clim:accept ((type maxima-function-name) stream view &key)
  (clim:with-input-context ('maxima-client.markup:maxima-function-reference :override nil)
      (object type)
      (values (clim:completing-from-suggestions (stream :partial-completers '(#\Space))
                (dolist (e *index-symbols*)
                  (clim:suggest (car e) (car e))))
              'maxima-function-name)
    (maxima-client.markup:maxima-function-reference
     (let ((function-name (maxima-client.markup:named-reference/destination object)))
       (clim:replace-input stream function-name)))))

(clim:define-presentation-method clim:present (obj (type category) stream view &key)
  (format stream "Category ~a~%~%" (first obj))
  (clim:with-room-for-graphics (stream)
    (clim:with-identity-transformation (stream)
      (maxima-client.markup:with-word-wrap (stream)
        (loop
          for (definition-type type name) in (second obj)
          do (progn
               (maxima-client.markup:add-vspacing stream 8)
               (clim:with-text-face (stream :bold)
                 (maxima-client.markup:word-wrap-draw-string stream type))
               (maxima-client.markup:word-wrap-draw-string stream " ")
               (let ((p (make-instance 'maxima-client.markup:maxima-function-reference :destination name)))
                 (maxima-client.markup:word-wrap-draw-presentation stream p))
               (maxima-client.markup:draw-current-line-and-reset stream)))))))

(defclass info-content-panel (clim:application-pane)
  ())

(clim:define-application-frame documentation-frame ()
  ((content       :initform nil
                  :accessor documentation-frame/content)
   (prev-commands :initform nil
                  :accessor documentation-frame/prev-commands)
   (next-commands :initform nil
                  :accessor documentation-frame/next-commands)
   (curr-command  :initform nil
                  :accessor documentation-frame/curr-command))
  (:panes (info-content (clim:make-pane 'info-content-panel
                                        :display-function 'display-text-content
                                        :default-view +info-content-panel-view+
                                        :end-of-line-action :wrap*))
          (interaction-pane :interactor)
          (prev-button (clim:make-pane 'clim:push-button-pane
                                       :label "Back"
                                       :activate-callback 'prev-button-pressed))
          (next-button (clim:make-pane 'clim:push-button-pane
                                       :label "Forward"
                                       :activate-callback 'next-button-pressed)))
  (:layouts (default (clim:vertically ()
                       (clim:horizontally ()
                         prev-button
                         next-button)
                       (4/5 (clim:scrolling ()
                              info-content))
                       (1/5 interaction-pane))))
  (:menu-bar info-menubar-command-table)
  (:command-table (documentation-frame :inherit-from (info-commands))))

(define-condition content-load-error (error)
  ((type    :initarg :type
            :initform (error "~s is a required initarg for ~s" :type 'content-load-error)
            :reader content-load-error/command)
   (name    :initarg :name
            :initform (error "~s is a required initarg for ~s" :name 'content-load-error)
            :reader content-load-error/name)
   (message :initarg :message
            :initform (error "~s is a required initarg for ~s" :message 'content-load-error)
            :reader content-load-error/message))
  (:report (lambda (condition stream)
             (format stream "Error loading ~a: ~a"
                     (content-load-error/name condition)
                     (content-load-error/message condition)))))

(defun process-documentation-request (frame command &key inhibit-history redisplay)
  (when (and (not inhibit-history)
             (documentation-frame/curr-command frame))
    (push (documentation-frame/curr-command frame) (documentation-frame/prev-commands frame)))
  (setf (documentation-frame/curr-command frame) command)
  (destructuring-bind (type name)
      command
    (let ((content (ecase type
                     (:function (cons 'maxima-client.markup:markup (load-function name)))
                     (:node (cons 'maxima-client.markup:markup (load-node name)))
                     (:file (cons 'maxima-client.markup:markup (load-doc-file name)))
                     (:category (cons 'category (load-category name))))))
      (setf (documentation-frame/content frame) content)
      (when redisplay
        (let ((info-content-panel (clim:find-pane-named clim:*application-frame* 'info-content)))
          (clim:redisplay-frame-pane frame info-content-panel))))))

(defun display-function-help (name)
  (open-documentation-frame (list :function name)))

(defun display-documentation-frame (&optional command)
  (let ((frame (clim:make-application-frame 'documentation-frame
                                            :width 900
                                            :height 800)))
    (load-index)
    (when command
      (process-documentation-request frame command))
    (clim:run-frame-top-level frame)))

(defun open-documentation-frame (command)
  (bordeaux-threads:with-lock-held (*doc-frame-lock*)
    (if (null *doc-frame*)
        (let ((frame (clim:make-application-frame 'documentation-frame
                                                  :width 900
                                                  :height 800)))
          (setq *doc-frame* frame)
          (handler-case
              (process-documentation-request frame command)
            (content-load-error (condition)
              (log:error "Error loading content: ~a" condition)))
          (load-index)
          (bordeaux-threads:make-thread (lambda ()
                                          (clim:run-frame-top-level frame))))
        ;; ELSE: Frame was already created, just run the command
        (when command
          (call-in-event-handler *doc-frame*
                                 (lambda ()
                                   (process-documentation-request *doc-frame* command :redisplay t)))))))

(defmethod clim:frame-exit ((frame documentation-frame))
  (bordeaux-threads:with-lock-held (*doc-frame-lock*)
    (setq *doc-frame* nil))
  (call-next-method))

(defun display-text-content (frame panel)
  (let ((content (documentation-frame/content frame)))
    (when content
      (clim:stream-present panel (cdr content) (car content)))))

(defvar *doc-file-cache* nil)

(defmacro with-doc-file-cache (&body body)
  `(let ((*doc-file-cache* (make-hash-table :test 'equal)))
     ,@body))

(defun load-doc-file (name)
  (labels ((load-file ()
             (let* ((info-root-path (find-info-root-path))
                    (file (merge-pathnames (format nil "docs/~a.lisp" name) info-root-path))
                    (content (with-open-file (in file :external-format :utf-8 :if-does-not-exist nil)
                               (unless in
                                 (error 'content-load-error :type :file :name name :message "file not found"))
                               (read in))))
               content))
           (load-from-cache ()
             (alexandria:ensure-gethash name *doc-file-cache* (load-file))))
    (if *doc-file-cache*
        (load-from-cache)
        (with-doc-file-cache
          (load-from-cache)))))

(defun load-index ()
  (when (or (null *index-symbols*)
            (null *index-nodes*)
            (null *index-categories*))
    (let* ((info-root-path (find-info-root-path))
           (file (merge-pathnames #p"docs/index.lisp" info-root-path))
           (content (with-open-file (in file :external-format :utf-8)
                      (read in))))
      (setq *index-symbols* (cdr (assoc :symbols content)))
      (setq *index-nodes* (cdr (assoc :nodes content)))
      (setq *index-categories* (cdr (assoc :categories content))))))

(defun load-node (name)
  (load-index)
  (let ((entry (find name *index-nodes* :test #'equal :key (lambda (v)
                                                             ;; Each entry consists of a node descriptor
                                                             ;; Each node descriptor consists of 4 elements
                                                             ;; The first element of each node descriptor is its name
                                                             (first (first v))))))
    (unless entry
      (error 'content-load-error :type :node :name name :message "node not found"))
    (let ((file-content (load-doc-file (second entry))))
      ;; Find the content from the given node, until the next node
      (let ((start (member-if (lambda (v)
                                (and (listp v)
                                     (eq (car v) :node)
                                     (equal (second v) name)))
                              file-content)))
        ;; Copy everything up until the next node
        (cons (car start)
              (loop
                for v in (cdr start)
                until (and (listp v)
                           (eq (car v) :node))
                collect v))))))

(defun find-index-entry-function (name)
  (let ((entry (find name *index-symbols* :key #'car :test #'equal)))
    (unless entry
      (error 'content-load-error :type :function :name name :message "function not found"))
    entry))

(defun load-function (name)
  (load-index)
  (let ((entry (let* ((definition (find-index-entry-function name))
                      (v (cdr definition)))
                 (if (eq (car v) :ref)
                     (find-index-entry-function (cadr v))
                     definition))))
    (with-doc-file-cache
      (loop
        for (type file description) in (cdr entry)
        collect (let ((file-content (load-doc-file file)))
                  (let ((found (find-if (lambda (definition)
                                          (and (eq (car definition) type)
                                               (equal (second (second definition)) (car entry))))
                                        file-content)))
                    (unless found
                      (error "symbol found in index but not in file: ~s type: ~s" (car entry) type))
                    (list :paragraph found)))))))

(defun load-category (name)
  (load-index)
  (let ((entry (find name *index-categories* :key #'car :test #'equal)))
    (unless entry
      (error 'content-load-error :type :cateogry :name name :message "category not found"))
    (list name (cdr entry))))

(define-documentation-frame-command (cmd-add-code :name "code")
    ()
  (setf (documentation-frame/content clim:*application-frame*)
        '(maxima-client.markup:markup . ((:table ()
                                          (:item "foo" (:p "content"))
                                          (:item "bar" (:p "other content")))))))

(defun find-interaction-pane ()
  (clim:find-pane-named clim:*application-frame* 'interaction-pane))

(defun process-doc-command-and-redisplay (frame type name)
  (let ((info-content-panel (clim:find-pane-named clim:*application-frame* 'info-content)))
    (handler-case
        (progn
          (process-documentation-request frame (list type name) :redisplay t)
          (clim:redisplay-frame-pane clim:*application-frame* info-content-panel))
      (content-load-error (condition)
        (format (find-interaction-pane) "~a~%" condition)))))

(defun prev-button-pressed (pane)
  (declare (ignore pane))
  (clim:execute-frame-command clim:*application-frame* '(cmd-prev-screen)))

(define-documentation-frame-command (cmd-prev-screen :name "History Back")
    ()
  (let ((frame clim:*application-frame*))
    (cond ((documentation-frame/prev-commands frame)
           (let* ((command (pop (documentation-frame/prev-commands frame)))
                  (curr (documentation-frame/curr-command frame)))
             (when curr
               (push curr (documentation-frame/next-commands frame)))
             (process-documentation-request frame command :inhibit-history t :redisplay t)))
          (t
           (format (find-interaction-pane) "~&History is empty~%")))))

(defun next-button-pressed (pane)
  (declare (ignore pane))
  (clim:execute-frame-command clim:*application-frame* '(cmd-next-screen)))

(define-documentation-frame-command (cmd-next-screen :name "History Forward")
    ()
  (let ((frame clim:*application-frame*))
    (cond ((documentation-frame/next-commands frame)
           (let* ((command (pop (documentation-frame/next-commands frame)))
                  (curr (documentation-frame/curr-command frame)))
             (when curr
               (push curr (documentation-frame/prev-commands frame)))
             (process-documentation-request frame command :inhibit-history t :redisplay t)))
          (t
           (format (find-interaction-pane) "~&No next screen in history~%")))))

(define-documentation-frame-command (cmd-info-close :name "Close")
    ()
  (clim:frame-exit clim:*application-frame*))

(define-documentation-frame-command (cmd-open-help-node :name "Node")
    ((name '(or string maxima-client.markup:node-reference) :prompt "Node"))
  (let* ((node-name (etypecase name
                               (string name)
                               (maxima-client.markup:node-reference (maxima-client.markup:named-reference/destination name)))))
    (process-doc-command-and-redisplay clim:*application-frame* :node node-name)))

(clim:define-command (cmd-open-help-function :name "Function" :menu t :command-table info-commands)
    ((name 'maxima-function-name :prompt "Name"))
  (process-doc-command-and-redisplay clim:*application-frame* :function name))

(define-documentation-frame-command (cmd-doc-introduction :name "Intro")
    ()
  (process-doc-command-and-redisplay clim:*application-frame* :file "maxima-client"))

(define-documentation-frame-command (cmd-open-file :name "file")
    ((file 'string :prompt "Filename"))
  (process-doc-command-and-redisplay clim:*application-frame* :file file))

(define-documentation-frame-command (cmd-doc-maxima-manual :name "Manual")
    ()
  (process-doc-command-and-redisplay clim:*application-frame* :file (pathname-name *maxima-toplvel-filename*)))

(clim:define-presentation-translator text-to-maxima-function-reference (string maxima-client.markup:maxima-function-reference info-commands)
    (object)
  (make-instance 'maxima-client.markup:named-reference :destination object))

(clim:define-presentation-to-command-translator select-maxima-function
    (maxima-client.markup:maxima-function-reference cmd-open-help-function info-commands)
    (obj)
  (list (maxima-client.markup:named-reference/destination obj)))

(clim:define-presentation-to-command-translator select-node
    (maxima-client.markup:node-reference cmd-open-help-node info-commands)
    (obj)
  (list obj))

(clim:define-command (show-category-command :name "Category" :menu t :command-table info-commands)
    ((category '(or string maxima-client.markup:category-reference) :prompt "Category"))
  (let ((name (etypecase category
                (string category)
                (maxima-client.markup:category-reference (maxima-client.markup:named-reference/destination category)))))
    (process-doc-command-and-redisplay clim:*application-frame* :category name)))

(clim:define-presentation-to-command-translator select-category
    (maxima-client.markup:category-reference show-category-command info-commands)
    (obj)
  (list obj))

(clim:make-command-table 'info-menubar-command-table
                         :errorp nil
                         :menu '(("File" :menu info-file-command-table)
                                 ("Navigation" :menu info-nav-command-table)
                                 ("Documentation" :menu info-doc-command-table)))

(clim:make-command-table 'info-file-command-table
                         :errorp nil
                         :menu '(("Close" :command cmd-info-close)))

(clim:make-command-table 'info-nav-command-table
                         :errorp nil
                         :menu '(("History Back" :command cmd-prev-screen)
                                 ("History Forward" :command cmd-next-screen)))

(clim:make-command-table 'info-doc-command-table
                         :errorp nil
                         :menu '(("Maxima-Client Introduction" :command cmd-doc-introduction)
                                 ("Maxima Documentation" :command cmd-doc-maxima-manual)
                                 ("Describe Function or Variable" :command cmd-open-help-function)))
