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

(clim:define-presentation-method clim:present (obj (type text-link) stream (view markup-text-view) &key)
  (clim:with-drawing-options (stream :ink clim:+blue+)
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
;;; maxima-function-reference
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass maxima-function-reference (text-link)
  ((name :initarg :name
         :reader maxima-function-reference/name)))

(defmethod text-link/description ((obj maxima-function-reference))
  (maxima-function-reference/name obj))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; maxima-function-reference
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass node-reference (text-link)
  ((name :initarg :name
         :reader node-reference/name)))

(defmethod text-link/description ((obj node-reference))
  (node-reference/name obj))

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
      (dimension-bind (rec :x x :y y :right right :bottom bottom)
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
                 (with-adjusted-margin ((+ padding margin))
                   (funcall fn stream)))))
      (move-rec rec (+ padding margin) (+ padding margin))
      (dimension-bind (rec :height height)
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
                        (let ((string (with-output-to-string (s)
                                        (loop
                                          for key in group
                                          for first = t then nil
                                          unless first
                                            do (format s "-")
                                          do (etypecase key
                                               (keyword (format s "~a" (keyword->key-label key)))
                                               (string (format s "~a" key)))))))
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
                        (present-to-stream (make-instance 'node-reference :name name) stream))))
             (move-rec rec pos 0)
             (dimension-bind (rec :width width)
               (incf pos (+ width 15)))
             (clim:stream-add-output-record stream rec)))))
  (draw-current-line-and-reset stream)
  (add-vspacing stream 10))

(defmacro with-indent ((stream indent) &body body)
  (alexandria:once-only (stream indent)
    `(clim:with-translation (,stream ,indent 0)
       (with-adjusted-margin (,indent)
         ,@body))))

(defun render-example (stream code results)
  (draw-current-line-and-reset stream)
  (add-vspacing stream (font-height stream))
  (if (eq (car results) :error)
      (log:info "Not rendering error output: ~s" results)
      (with-word-wrap-record (stream)
        (clim:with-identity-transformation (stream)
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
                             (present-to-stream (make-instance 'maxima-native-expr :expr (cdr res)) stream))))))))))))
  (draw-current-line-and-reset stream)
  (add-vspacing stream (font-height stream)))

(defun render-definition (stream type name args content)
  (draw-current-line-and-reset stream)
  (word-wrap-draw-string stream (format nil "~a: " type))
  (clim:with-text-face (stream :bold)
    (word-wrap-draw-string stream name))
  (when args
    (display-markup-int stream args))
  (draw-current-line-and-reset stream)
  (with-indent (stream 30)
    (display-markup-int stream content)))

(defun render-deffn (stream descriptor content)
  (destructuring-bind (type name args)
      descriptor
    (render-definition stream type name args content)))

(defun render-defvr (stream descriptor content)
  (destructuring-bind (type name)
      descriptor
    (render-definition stream type name nil content)))

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

(defun render-itemize (stream args content)
  ;; For now, just render itemised blocks as a sequence of indented
  ;; paragraphs. This seems to be in line with what the HTML renderer
  ;; does.
  (let ((bullet-p (member :bullet args)))
    (with-indent (stream 50)
      (dolist (item content)
        (draw-current-line-and-reset stream)
        (add-vspacing stream 18)
        (when bullet-p
          (word-wrap-draw-string stream (format nil "~c " #\BULLET)))
        (display-markup-list stream item :skip-first t)))))

(defun draw-presentation (stream obj)
  (let ((rec (clim:with-output-to-output-record (stream)
               (present-to-stream obj stream))))
    (word-wrap-draw-record stream rec)))

(defun render-picture (stream file)
  (let ((parsed-name (alexandria:if-let ((pos (position #\, file)))
                       (subseq file 0 pos)
                       file)))
    (draw-current-line-and-reset stream)
    (let* ((p (merge-pathnames (format nil "~a.png" parsed-name) (find-info-root-path)))
           (image (clim:make-pattern-from-bitmap-file p)))
      (if image
          (with-word-wrap-record (stream)
            (clim:draw-pattern* stream image 0 0)
            (clim:draw-rectangle* stream 0 0 (clim:pattern-width image) (clim:pattern-height image)
                                  :filled nil :ink clim:+black+))
          ;; ELSE: Image couldn't be loaded
          (log:warn "Could not load image file: ~s" p)))))

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
      (draw-presentation stream (make-instance 'node-reference :name next))
      (when prev
        (word-wrap-draw-string stream " Previous: ")
        (draw-presentation stream (make-instance 'node-reference :name prev))
        (when up
          (word-wrap-draw-string stream " Up: ")
          (draw-presentation stream (make-instance 'node-reference :name up))))
      (draw-current-line-and-reset stream))))

(defun render-menu (stream content)
  (declare (ignore stream content))
  nil)

(defun make-mref-link (name)
  (make-instance 'maxima-function-reference :name name))

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
             ((:code :math) (clim:with-text-family (stream :fix) (display (cdr content))))
             (:pre (render-preformatted stream (cdr content)))
             ((:link :url) (draw-presentation stream (make-text-link-from-markup (cdr content))))
             (:key (render-key-command stream (cdr content)))
             ((:p :paragraph) (draw-current-line-and-reset stream) (add-vspacing stream 18) (display (cdr content)))
             (:newline (draw-newline stream))
             (:section (render-section stream (cdr content)))
             (:subsection (render-subsection stream (cdr content)))
             ((:var) (clim:with-text-family (stream :fix) (display (cdr content))))
             ((:mrefdot :mxrefdot :mrefcomma :xref) (display (cdr content)))
             ((:mref :fname) (draw-presentation stream (make-mref-link (second content))))
             (:catbox (render-catbox stream (cdr content)))
             (:demo-code (render-example stream (cdr (assoc :demo-source (cdr content))) (cdr (assoc :demo-result (cdr content)))))
             (:deffn (render-deffn stream (second content) (cddr content)))
             (:defvr (render-defvr stream (second content) (cddr content)))
             (:anchor nil)
             (:node (render-node stream (cdr content)))
             (:menu (render-menu stream (cdr content)))
             (:footnote nil)
             (:figure nil)
             (:itemize (render-itemize stream (second content) (cddr content)))
             (:ref (display (cdr content)))
             (:image (render-picture stream (second content)))))
          ((listp content)
           (display-markup-list stream content)))))

(defun display-markup-int (stream content)
  (etypecase content
    (string (word-wrap-draw-string stream content))
    (list (display-possibly-tagged-list stream content))))

(defun display-markup (stream content)
  (maxima-client.markup:with-word-wrap (stream)
    (display-markup-int stream content)))
