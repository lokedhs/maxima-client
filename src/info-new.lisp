(in-package :maxima-client.doc-new)

(defclass info-content-panel-view (maxima-client::maxima-renderer-view)
  ())

(defparameter +info-content-panel-view+ (make-instance 'info-content-panel-view))

(defclass info-content-panel (clim:application-pane)
  ((content :initform nil
            :accessor info-content-panel/content)))

(defun display-text-content (frame panel)
  (declare (ignore frame))
  (let ((content (info-content-panel/content panel)))
    (when content
      (maxima-client.markup:display-markup panel content))))

(clim:define-application-frame documentation-frame ()
  ()
  (:panes (info-content (clim:make-pane 'info-content-panel
                                        :display-function 'display-text-content
                                        :default-view +info-content-panel-view+))
          (interaction-pane :interactor))
  (:layouts (default (clim:vertically ()
                       (1/2 (clim:scrolling ()
                              info-content))
                       (1/2 interaction-pane)))))

(defun load-doc-file (name)
  (let* ((info-root-path (asdf:system-relative-pathname (asdf:find-system :maxima-client) #p"infoparser/docs/"))
         (file (merge-pathnames (concatenate 'string name ".lisp") info-root-path))
         (content (with-open-file (in file :external-format :utf-8)
                    (read in))))
    content))

(defun display-documentation-frame ()
  (let ((frame (clim:make-application-frame 'documentation-frame
                                            :width 900
                                            :height 800)))
    (clim:run-frame-top-level frame)))

(define-documentation-frame-command (arrays-command :name "add")
    ()
  (let ((info-content-panel (clim:find-pane-named clim:*application-frame* 'info-content)))
    (setf (info-content-panel/content info-content-panel) (load-doc-file "DataTypes"))))
