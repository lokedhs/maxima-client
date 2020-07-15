(in-package :maxima-client.markup)

(clim:define-command-table text-commands)

(defclass markup-text-view ()
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; text-link
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass text-link ()
  ())

(defgeneric text-link/description (obj))

(defmacro with-link-drawing-style (&body body)
  "Set up drawing options such that the content is drawn in the style of a link"
  `(clim:with-drawing-options (stream :ink clim:+blue+)
     ,@body))

(clim:define-presentation-method clim:present (obj (type text-link) stream (view markup-text-view) &key)
  (with-link-drawing-style
    (clim:draw-text* stream (text-link/description obj) 0 0)))

(clim:define-presentation-method clim:present (obj (type text-link) stream (view clim:textual-view) &key)
  (format stream "~a" (text-link/description obj)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; text-link-url
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass text-link-url (text-link)
  ((src         :initarg :src
                :reader text-link-url/src)
   (description :initarg :description
                :initform nil)))

(defmethod text-link/description ((obj text-link-url))
  (or (slot-value obj 'description)
      (text-link-url/src obj)))

(clim:define-presentation-translator text-to-text-link-url (string text-link-url text-commands)
    (object)
  (make-instance 'text-link-url :src object))

(clim:define-command (open-url :name "Open URL" :menu t :command-table text-commands)
    ((url 'text-link-url :prompt "URL"))
  (let ((s (text-link-url/src url))
        (stream (find-interactor-pane)))
    (format stream "Opening URL: ~a" s)
    (bordeaux-threads:make-thread (lambda ()
                                    (uiop/run-program:run-program (list "xdg-open" s))))))

(clim:define-presentation-to-command-translator select-url
    (text-link-url open-url text-commands)
    (obj)
  (list obj))

(defun make-text-link-from-markup (args)
  (cond ((alexandria:sequence-of-length-p args 1)
         (make-instance 'text-link-url :src (first args)))
        ((alexandria:sequence-of-length-p args 2)
         (make-instance 'text-link-url :src (first args) :description (second args)))
        (t
         (error "Illegally formatted text-link entry: ~s" args))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Named reference
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass named-reference (text-link)
  ((name        :initarg :name
                :initform nil
                :reader named-reference/name)
   (destination :initarg :destination
                :initform (error "~s required when creating ~s" :destination 'named-reference)
                :reader named-reference/destination)))

(defmethod text-link/description ((obj named-reference))
  (or (named-reference/name obj)
      (named-reference/destination obj)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; maxima-function-reference
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass maxima-function-reference (named-reference)
  ())

(defmethod print-object ((obj maxima-function-reference) stream)
  (print-unreadable-safely (name destination) obj stream
    (format stream "DESTINATION ~s NAME ~s" destination name)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; maxima-function-reference
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass node-reference (named-reference)
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Category reference
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass category-reference (named-reference)
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Markup rendering
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun display-markup-list (stream content &key skip-first)
  (log:trace "displaying markup list: ~s" content)
  (loop
    for v in content
    for first = t then nil
    do (display-markup-int stream (if (and skip-first first (paragraph-item-p v)) (cdr v) v))))

(defun keyword->key-label (key)
  (ecase key
    (:control "Control")
    (:shift "Shift")
    (:meta "Meta")
    (:super "Super")
    (:hyper "Hyper")))

(defun call-with-key-string (stream fn)
  (let ((rec (clim:with-output-to-output-record (stream)
               (funcall fn stream))))
    (clim:stream-add-output-record stream rec)
    (let ((x-spacing 2))
      (dimension-bind-new (stream rec :x x :y y :right right :bottom bottom)
        (clim:draw-rectangle* stream
                              (- x x-spacing)
                              (min (- (font-ascent stream)) y)
                              (+ right x-spacing)
                              (max (font-descent stream) bottom)
                              :filled nil )))))

(defmacro with-key-string ((stream) &body body)
  (check-type stream symbol)
  `(call-with-key-string ,stream (lambda (,stream) ,@body)))

(defun call-with-spanned-box (stream fn)
  (let ((margin 50)
        (padding 5))
    (let ((rec (clim:with-output-to-output-record (stream)
                 (with-adjusted-margin (0 (+ padding margin))
                   (funcall fn stream)))))
      (move-rec rec (+ padding margin) (+ padding margin))
      (dimension-bind-new (stream rec :height height)
        (let ((x1 margin)
              (y1 margin)
              (x2 (- *word-wrap-right-margin* margin))
              (y2 (+ (* padding 2) margin height)))
          (clim:draw-rectangle* stream x1 y1 x2 y2
                                :ink (clim:make-rgb-color 0.8 1 1)
                                :filled t)
          (clim:draw-rectangle* stream x1 y1 x2 y2
                                :ink clim:+black+
                                :filled nil)
          (clim:stream-add-output-record stream rec))))))

(defmacro with-spanned-box ((stream) &body body)
  (check-type stream symbol)
  `(call-with-spanned-box ,stream (lambda (,stream) ,@body)))

(defun render-key-command (stream key-seq)
  (let ((rec (clim:with-output-to-output-record (stream)
               (clim:with-text-style (stream (clim:make-text-style :fix :roman nil))
                 (loop
                   for group in key-seq
                   do (with-key-string (stream)
                        (let ((string (etypecase group
                                        (string group)
                                        (list (with-output-to-string (s)
                                                (loop
                                                  for key in group
                                                  for first = t then nil
                                                  unless first
                                                    do (format s "-")
                                                  do (etypecase key
                                                       (keyword (format s "~a" (keyword->key-label key)))
                                                       (string (format s "~a" key)))))))))
                          (clim:draw-text* stream string 0 0))))))))
    (word-wrap-draw-record stream rec)))

(defun paragraph-item-p (item)
  (and (listp item)
       (or (eq (car item) :p)
           (eq (car item) :paragraph))))

(defun render-catbox (stream categories)
  (draw-current-line-and-reset stream)
  (add-vspacing stream 5)
  (with-word-wrap-record (stream)
    (clim:surrounding-output-with-border (stream :ink clim:+black+)
      (loop
        with pos = 0
        for name in categories
        do (let ((rec (clim:with-output-to-output-record (stream)
                        (present-to-stream (make-instance 'category-reference :destination name) stream))))
             (move-rec rec pos 0)
             (dimension-bind (rec :width width)
               (incf pos (+ width 15)))
             (clim:stream-add-output-record stream rec)))))
  (draw-current-line-and-reset stream)
  (add-vspacing stream 10))

(defmacro with-indent ((stream indent) &body body)
  (alexandria:once-only (stream indent)
    `(with-adjusted-margin (,indent (- ,indent))
       (draw-current-line-and-reset ,stream)
       ,@body
       (draw-current-line-and-reset ,stream))))

(defun render-example (stream content)
  (let ((res (assoc :demo-result content)))
    (if (null res)
        (render-preformatted stream (cdr (assoc :example-info content)))
        ;; ELSE: We have results, use nice rendering
        (let ((results (cdr res)))
          (if (eq (car results) :error)
              ;; Check for error output, this shouldn't be needed
              (render-preformatted stream (cdr (assoc :example-info content)))
              ;; ELSE: No error
              (let ((code (cdr (assoc :demo-source content))))
                (draw-current-line-and-reset stream)
                (add-vspacing stream (font-height stream))
                (clim:with-identity-transformation (stream)
                  (with-word-wrap-record (stream)
                    (with-spanned-box (stream)
                      (loop
                        for code-line in code
                        for res in results
                        for i from 1
                        do (progn
                             (clim:with-text-face (stream :bold)
                               (format stream "~&(%i~a) " i))
                             (clim:with-text-family (stream :fix)
                               (format stream "~a~%" code-line))
                             (when (eq (car res) :result)
                               (clim:formatting-table (stream)
                                 (clim:formatting-row (stream)
                                   (clim:formatting-cell (stream :align-y :center :min-width 75)
                                     (format stream "(%o~a)" i))
                                   (clim:formatting-cell (stream :align-y :center)
                                     (clim:with-identity-transformation (stream)
                                       (present-to-stream (make-instance 'maxima-native-expr :expr (cdr res)) stream))))))))))))))))
  (draw-current-line-and-reset stream)
  (add-vspacing stream (font-height stream)))

(defun render-definition (stream type name primary-args secondary-args content)
  (draw-current-line-and-reset stream)
  (word-wrap-draw-string stream (format nil "~a: " type))
  (clim:with-text-face (stream :bold)
    (word-wrap-draw-string stream name))
  (when primary-args
    (display-markup-int stream primary-args))
  (draw-current-line-and-reset stream)
  (dolist (args-descriptor secondary-args)
    (destructuring-bind (name args)
        args-descriptor
      (with-indent (stream 10)
        (word-wrap-draw-string stream (format nil "~a " name))
        (display-markup-int stream args))
      (draw-current-line-and-reset stream)))
  (with-indent (stream 30)
    (display-markup-int stream content)))

(defun render-deffn (stream descriptor content)
  (destructuring-bind (type name &key arglist definitions anchors)
      descriptor
    (declare (ignore anchors))
    (render-definition stream type name arglist definitions content)))

(defun render-defvr (stream descriptor content)
  (destructuring-bind (type name &key arglist definitions anchors)
      descriptor
    (declare (ignore anchors))
    (render-definition stream type name arglist definitions content)))

(defun render-section (stream content)
  (draw-current-line-and-reset stream)
  (add-vspacing stream (/ (font-height stream) 2))
  (clim:with-text-style (stream (clim:make-text-style :sans-serif :bold :large))
    (display-markup-int stream content)))

(defun render-subsection (stream content)
  (draw-current-line-and-reset stream)
  (add-vspacing stream (/ (font-height stream) 2))
  (clim:with-text-style (stream (clim:make-text-style :sans-serif :bold :large))
    (display-markup-int stream content)))

(defun render-preformatted (stream content)
  (draw-current-line-and-reset stream)
  (add-vspacing stream (font-height stream))
  (with-word-wrap-record (stream)
    (clim:with-identity-transformation (stream)
      (with-spanned-box (stream)
        (clim:with-text-family (stream :fix)
          (loop
            for line in content
            do (format stream "~a~%" line))))))
  (draw-current-line-and-reset stream)
  (add-vspacing stream (font-height stream)))

(defun render-verbatim (stream content)
  (draw-current-line-and-reset stream)
  (add-vspacing stream (font-height stream))
  (with-word-wrap-record (stream)
    (clim:with-text-family (stream :fix)
      (loop
        for line in content
        do (format stream "~a~%" line))))
  (draw-current-line-and-reset stream)
  (add-vspacing stream (font-height stream)))

(defun render-itemize (stream args content)
  ;; For now, just render itemised blocks as a sequence of indented
  ;; paragraphs. This seems to be in line with what the HTML renderer
  ;; does.
  (let ((bullet-p (member :bullet args)))
    (with-indent (stream 50)
      (dolist (item content)
        (add-vspacing stream 18)
        (when bullet-p
          (word-wrap-draw-string stream (format nil "~c " #\BULLET)))
        (display-markup-list stream item :skip-first t)))))

(defun render-enumerate (stream content)
  (with-indent (stream 50)
    (loop
      for item in content
      for i from 1
      do (progn
           (add-vspacing stream 18)
           (word-wrap-draw-string stream (format nil "~a. " i))
           (display-markup-list stream item :skip-first t)))))

(defun render-table (stream args content)
  (let ((fix-p (member :code args)))
    (dolist (entry content)
      (draw-current-line-and-reset stream)
      (add-vspacing stream 18)
      (when (eq (first entry) :item)
        (let ((label (second entry))
              (item (cddr entry)))
          (with-indent (stream 10)
            (if fix-p
                (clim:with-text-family (stream :fix)
                  (word-wrap-draw-string stream label))
                (word-wrap-draw-string stream label))
            (draw-current-line-and-reset stream))
          (add-vspacing stream 10)
          (with-indent (stream 50)
            (display-markup-list stream item :skip-first t)
            (draw-current-line-and-reset stream)))))))

(defun render-picture (stream file)
  (draw-current-line-and-reset stream)
  (let* ((p (merge-pathnames (format nil "~a.png" file) (find-info-root-path)))
         (image (clim:make-pattern-from-bitmap-file p)))
    (if image
        (with-word-wrap-record (stream)
          (clim:draw-pattern* stream image 0 0)
          (clim:draw-rectangle* stream 0 0 (clim:pattern-width image) (clim:pattern-height image)
                                :filled nil :ink clim:+black+))
        ;; ELSE: Image couldn't be loaded
        (log:warn "Could not load image file: ~s" p))))

(defun render-node (stream content)
  (draw-current-line-and-reset stream)
  (let ((title (first content))
        (next (second content))
        (prev (third content))
        (up (fourth content)))
    (word-wrap-draw-string stream (format nil "Title: ~a" title))
    (draw-current-line-and-reset stream)
    (when next
      (word-wrap-draw-string stream "Next: ")
      (word-wrap-draw-presentation stream (make-instance 'node-reference :destination next))
      (when prev
        (word-wrap-draw-string stream " Previous: ")
        (word-wrap-draw-presentation stream (make-instance 'node-reference :destination prev))
        (when up
          (word-wrap-draw-string stream " Up: ")
          (word-wrap-draw-presentation stream (make-instance 'node-reference :destination up))))
      (draw-current-line-and-reset stream))))

(defun render-menu (stream content)
  (draw-current-line-and-reset stream)
  (add-vspacing stream 10)
  (loop
    for e in content
    for tag = (car e)
    do (ecase tag
         (:menu-text
          (display-markup-list stream (cdr e))
          (draw-current-line-and-reset stream))
         (:menu-entry
          (word-wrap-draw-presentation stream (make-instance 'node-reference :destination (second e)))
          (alexandria:when-let ((description (third e)))
            (word-wrap-add-absolute-spacing (* (font-char-width stream) 10))
            (word-wrap-draw-string stream description))
          (draw-current-line-and-reset stream)))))

(defun make-mref-link (link &optional name)
  (make-instance 'maxima-function-reference :name name :destination link))

(defun display-possibly-tagged-list (stream content)
  (labels ((display (v)
             (display-markup-list stream v)))
    (cond ((null content)
           (return-from display-possibly-tagged-list nil))
          ((keywordp (car content))
           (ecase (car content)
             (:heading (draw-current-line-and-reset stream) (clim:with-text-style (stream (clim:make-text-style :sans-serif :bold :large)) (display (cdr content))))
             (:bold (clim:with-text-face (stream :bold) (display (cdr content))))
             (:italic (clim:with-text-face (stream :italic) (display (cdr content))))
             ((:code :math :file) (clim:with-text-family (stream :fix) (display (cdr content))))
             (:pre (render-preformatted stream (cdr content)))
             (:verbatim (render-verbatim stream (cdr content)))
             ((:link :url) (word-wrap-draw-presentation stream (make-text-link-from-markup (cdr content))))
             (:key (render-key-command stream (cdr content)))
             ((:p :paragraph) (draw-current-line-and-reset stream) (add-vspacing stream 18) (display (cdr content)))
             (:newline (draw-newline stream))
             (:section (render-section stream (cdr content)))
             (:subsection (render-subsection stream (cdr content)))
             ((:var) (clim:with-text-family (stream :fix) (display (cdr content))))
             ((:mxref :xref) (display (list (third content)))) ;; Format: (:mxref "link" "text")
             ((:mref :fname) (word-wrap-draw-presentation stream (make-mref-link (second content))))
             (:catbox (render-catbox stream (cdr content)))
             (:demo-code (render-example stream (cdr content)))
             (:deffn (render-deffn stream (second content) (cddr content)))
             (:defvr (render-defvr stream (second content) (cddr content)))
             (:anchor nil)
             (:node (render-node stream (cdr content)))
             (:menu (render-menu stream (cdr content)))
             (:footnote nil)
             (:itemize (render-itemize stream (second content) (cddr content)))
             (:enumerate (render-enumerate stream (cddr content)))
             (:table (render-table stream (second content) (cddr content)))
             (:ref (display (cdr content)))
             (:image (render-picture stream (second content)))
             (:chapter nil)))
          ((listp content)
           (display-markup-list stream content)))))

(defun stringify-tagged-list (stream content)
  (let ((tag (car content)))
    (check-type tag keyword)
    (let ((args (cdr content)))
      (case (car content)
        ((:bold :italic :code :pre :verbatim :section :subsection :p :paragraph :var)
         (mapc (lambda (element)
                 (stringify-markup-int stream element))
               args))
        ((:deffn)
         (let ((declaration (first args))
               (body (second args)))
           (format stream " ~a " (second declaration))
           (stringify-markup-int stream body)))))))

(defun stringify-markup-int (stream content)
  (etypecase content
    (string (write-string content stream))
    (list (stringify-tagged-list stream content))))

(defun stringify-markup (content)
  (with-output-to-string (out)
    (stringify-markup-int out content)))

(defun display-markup-int (stream content)
  (etypecase content
    (string (word-wrap-draw-string stream content))
    (list (display-possibly-tagged-list stream content))))

(defun display-markup (stream content)
  (maxima-client.markup:with-word-wrap (stream)
    (display-markup-int stream content)))

(clim:define-presentation-type markup ())

(clim:define-presentation-method clim:present (obj (type markup) stream view &key)
  (clim:with-room-for-graphics (stream :first-quadrant nil)
    (maxima-client.markup:display-markup stream obj)))
