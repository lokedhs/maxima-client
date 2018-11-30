(in-package :maxima-client.doc-new)

(defclass info-content-panel (clim:application-pane)
  ((content :initform nil
            :accessor info-content-panel/content)))

(defun display-text-content (frame panel)
  (declare (ignore frame))
  (log:info "typep = ~s/~s, type = ~s"
            (typep panel 'info-content-panel)
            (typep panel 'clim:mirrored-sheet-mixin)
            (type-of panel))
  (let ((content (info-content-panel/content panel)))
    (when content
      (maxima-client.markup:display-markup panel content))))

(clim:define-application-frame documentation-frame ()
  ()
  (:panes (info-content (clim:make-pane 'info-content-panel
                                        :display-function 'display-text-content))
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

(defun find-node (name content)
  (labels ((node-name-from-node (node)
             (let ((node-info (cadr node)))
               (car node-info))))
    (loop
      for v in content
      when (and (eq (car v) :node)
                (equal (node-name-from-node v) name))
        return v)))

(defun display-documentation-frame ()
  (let ((frame (clim:make-application-frame 'documentation-frame)))
    (clim:run-frame-top-level frame)))

(define-documentation-frame-command (arrays-command :name "add")
    ()
  (let ((info-content-panel (clim:find-pane-named clim:*application-frame* 'info-content)))
    (setf (info-content-panel/content info-content-panel) (find-node "Arrays" (load-doc-file "Arrays")))))
