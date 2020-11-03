(in-package :maxima-client)

(defclass completion-popup-element ()
  ((name :type string
         :initarg :name
         :reader completion-popup-element/name)
   (args :type (or string null t)
         :initarg :args
         :reader completion-popup-element/args)))

(drei-syntax:define-syntax-command-table maxima-table
  :errorp nil)

(drei::define-syntax maxima-syntax (drei-fundamental-syntax:fundamental-syntax)
  ()
  (:name "Maxima")
  (:pathname-types "mac")
  (:command-table maxima-table))

(defun find-maxima-completions (prefix &key (verbs t) (nouns t))
  (cond ((not (and verbs nouns))
         (error "At least one of :VERBS or :NOUNS must be provided"))
        ((equal prefix "")
         ;; Don't allow matching all symbols
         nil)
        (t
         (let ((syms (loop
                       with package = (find-package "MAXIMA")
                       for sym being each symbol in package
                       for sym-string = (symbol-name sym)
                       when (or (and verbs (alexandria:starts-with-subseq "$" sym-string))
                                (and nouns (alexandria:starts-with-subseq "%" sym-string)))
                         append (let ((sym-name-fixed (format-sym-name sym)))
                                  (if (alexandria:starts-with-subseq prefix sym-name-fixed)
                                      (list (make-instance 'completion-popup-element
                                                           :name sym-name-fixed
                                                           :args (function-signature sym))))))))
           #+nil(remove-duplicates (sort syms #'string<) :test #'equal)
           (sort syms #'string< :key #'completion-popup-element/name)))))

(defun symbol-constituent-p (ch)
  (let ((code (char-code ch)))
    (or (<= (char-code #\a) code (char-code #\z))
        (<= (char-code #\A) code (char-code #\Z))
        (<= (char-code #\0) code (char-code #\9))
        (eql ch #\_))))

(defun insert-completed-symbol (mark string)
  (loop
    until (drei::beginning-of-buffer-p mark)
    for ch = (drei::object-before mark)
    while (symbol-constituent-p ch)
    do (drei-buffer:backward-object mark))
  (loop
    until (drei::end-of-buffer-p mark)
    for ch = (drei::object-after mark)
    while (symbol-constituent-p ch)
    do (drei-editing::forward-delete-object mark))
  (drei-buffer:insert-sequence mark string))

(defmethod maxima-client.gui-tools:render-element ((value completion-popup-element) stream viewport-width)
  (clim:draw-text* stream (completion-popup-element/name value) 2 0 :ink clim:+black+)
  (alexandria:when-let ((args (completion-popup-element/args value)))
    (clim:with-drawing-options (stream :ink (clim:make-rgb-color 0.6 0.6 0.6))
      (multiple-value-bind (width)
          (clim:text-size stream args)
        (clim:draw-text* stream args (- viewport-width width 2) 0)))))

(defmethod maxima-client.gui-tools:element-dimensions ((value completion-popup-element) stream viewport-width)
  (let ((text-style (clim:medium-text-style stream)))
    (values (climb:text-style-ascent text-style stream)
            (climb:text-style-descent text-style stream))))

(defmethod maxima-client.gui-tools:get-element-filter-name ((value completion-popup-element))
  (completion-popup-element/name value))

(defun get-screen-position-of-pane (drei)
  (dimension-bind (drei :x x :y y)
    (multiple-value-bind (px py)
        (clim:transform-position (clim:sheet-delta-transformation (drei:editor-pane drei) nil) x y)
      (values px py))))

(clim:define-command (complete-maxima-function :name "Complete function" :command-table maxima-table)
    ()
  "Complete the name of the function at the curstor position."
  (let* ((point (drei:point))
         (offset (drei::offset point))
         (string (drei-buffer:buffer-substring (drei:buffer point) 0 offset)))
    (multiple-value-bind (match strings)
        (cl-ppcre:scan-to-strings "([a-zA-Z_][a-zA-Z0-9_]*)$" string)
      (when match
        (let ((matches (find-maxima-completions (aref strings 0))))
          (cond ((null matches)
                 (esa:display-message "No completions"))
                ((alexandria:sequence-of-length-p matches 1)
                 (insert-completed-symbol point (completion-popup-element/name (car matches))))
                (t
                 (multiple-value-bind (editor-x editor-y)
                     (get-screen-position-of-pane (drei:drei-instance))
                   (let ((result (maxima-client.gui-tools:select-completion-match
                                  (lambda (prefix)
                                    (let ((matches (find-maxima-completions prefix)))
                                      matches))
                                  :x-pos editor-x
                                  :y-pos editor-y
                                  :initial-prefix (aref strings 0))))
                     (when result
                       (insert-completed-symbol point (completion-popup-element/name result)))))))))))
  nil)

(clim:define-command (maxima-newline :name "Insert a newline into the commandline" :command-table maxima-table)
    ()
  "Insert newline into the commandline"
  (let ((point (drei:point)))
    (drei-buffer:insert-sequence point (string #\Newline) #+nil#(:maxima-submit))))

(drei::set-key 'complete-maxima-function
               'maxima-table
               '((#\Tab)))

(drei::set-key 'select-char
               'maxima-table
               '((#\s :meta)))

(drei::set-key 'maxima-newline
               'maxima-table
               '((:return :shift)))
