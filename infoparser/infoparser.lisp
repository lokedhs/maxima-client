(in-package :infoparser)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun %process-single-line-command (string clauses)
    (alexandria:with-gensyms (match parts)
      (let ((clause (car clauses)))
        (if (eq (car clause) t)
            `(progn ,@(cdr clause))
            `(multiple-value-bind (,match ,parts)
                 (cl-ppcre:scan-to-strings ,(caar clause) ,string)
               (declare (ignorable ,parts))
               (if ,match
                   ,(if (cadar clause)
                        `(let ((,(cadar clause) ,parts))
                           ,@(cdr clause))
                        `(progn ,@(cdr clause)))
                   ,@(if (cdr clauses)
                         (list (%process-single-line-command string (cdr clauses)))
                         nil))))))))

(defun resolve-destination-dir ()
  (asdf:system-relative-pathname (asdf:find-system :maxima-client) #p"infoparser/docs/"))

(defmacro process-single-line-command (string &body clauses)
  (alexandria:once-only (string)
    (%process-single-line-command string clauses)))

(defun %skip-block (stream end-tag)
  (loop
    for s = (read-line stream)
    until (cl-ppcre:scan end-tag s)
    collect s))

(defmacro skip-block (stream end-tag &environment env)
  (alexandria:once-only (stream)
    `(%skip-block ,stream
                  ,(if (constantp end-tag env)
                       `(load-time-value (cl-ppcre:create-scanner ,end-tag))
                       `(cl-ppcre:create-scanner ,end-tag)))))

(defun %markup-from-regexp (regexp string callback &optional plain-string-markup-fn)
  (flet ((markup-string (s)
           (if plain-string-markup-fn
               (funcall plain-string-markup-fn s)
               (list s))))
    (loop
      with length = (length string)
      with start = 0
      while (< start length)
      append (multiple-value-bind (match-start match-end reg-starts reg-ends)
                 (cl-ppcre:scan regexp string :start start)
               (if match-start
                   ;; Some highlighted text was found
                   (let* ((highlight (funcall callback reg-starts reg-ends))
                          (old-start start))
                     (setq start match-end)
                     (if (> match-start old-start)
                         ;; There is some unmatched text before the match
                         (append (markup-string (subseq string old-start match-start))
                                 (list highlight))
                         ;; ELSE: The match is at the beginning of the string
                         (list highlight)))
                   ;; ELSE: No match, copy the last part of the text and finish the loop
                   (progn
                     (let ((old-start start))
                       (setq start length)
                       (markup-string (subseq string old-start)))))))))

(defmacro markup-from-regexp (regexp string callback &optional plain-string-markup-fn &environment env)
  `(%markup-from-regexp ,(if (constantp regexp env) `(load-time-value (cl-ppcre:create-scanner ,regexp)) regexp)
                        ,string ,callback ,@(if plain-string-markup-fn (list plain-string-markup-fn))))

(defun %iterate-regexps (regexp string callback)
  (loop
    with length = (length string)
    with start = 0
    while (< start length)
    do (multiple-value-bind (match-start match-end reg-starts reg-ends)
           (cl-ppcre:scan regexp string :start start)
         (cond (match-start
                (setq start match-end)
                (let ((vec (make-array (length reg-starts) :element-type 'string :initial-element "")))
                  (loop
                    for start across reg-starts
                    for end across reg-ends
                    for i from 0
                    do (setf (aref vec i) (subseq string start end)))
                  (funcall callback vec)))
               (t
                (setq start length))))))

(defmacro iterate-regexps ((sym regexp string) &body body &environment env)
  `(%iterate-regexps ,(if (constantp regexp env) `(load-time-value (cl-ppcre:create-scanner ,regexp)) regexp)
                     ,string
                     (lambda (,sym) ,@body)))

(defun process-menu (stream)
  (cons :menu
        ;; Only process lines matching * title ::
        (loop
          for s = (read-line stream)
          until (cl-ppcre:scan "^@end menu *$" s)
          append (multiple-value-bind (match strings)
                     (cl-ppcre:scan-to-strings "^ *\\* *([^ ]+(?: +[^ ]+)*) *:: *$" s)
                   (if match
                       (list (aref strings 0))
                       ;; ELSE: No match
                       nil)))))

(defparameter *simple-tags*
  '(("mref" :mref)
    ("emph" :italic)
    ("i" :italic)
    ("strong" :bold)
    ("b" :bold)
    ("anchor" :anchor)
    ("xref" :xref)
    ("math" :math)
    ("ref" :ref)
    ("mxref" :mxref)
    ("file" :file)
    ("footnote" :footnote)
    ("kbd" :kbd)
    ("key" :key)
    ("image" :image)
    ("dfn" :dfn)
    ("url" :url)
    ("uref" :url)
    ("figure" :figure)
    ("verb" :verb)
    ("dotless" :dotless)
    ("email" :email)
    ("pxref" :pxref)
    ("var" :var)))

(defun process-markup-tag (name args)
  (labels ((ensure-string-arg (s)
             (unless (and (listp s)
                          (= (length s) 1)
                          (stringp (car s)))
               (error "Single argument expected"))
             (car s)))
    (let ((tag (find name *simple-tags* :key #'car :test #'equal)))
      (cond (tag
             (list (list (cadr tag) args)))
            (t
             (string-case:string-case (name)
               ("code" (list (cons :code args)))
               ("mrefcomma" `((:mref ,(ensure-string-arg args)) ","))
               ("mrefdot" `((:mref ,(ensure-string-arg args)) "."))
               ("mrefparen" `((:mref ,(ensure-string-arg args)) ")"))
               ("mxrefcomma" `((:mxref ,(ensure-string-arg args)) ","))
               ("mxrefdot" `((:mxref ,(ensure-string-arg args)) "."))
               ("mxrefparen" `((:mxref ,(ensure-string-arg args)) ")"))
               ("w" nil)
               ("dots" '("…"))))))))

(defun compress-strings (list)
  (collectors:with-collector (coll)
    (loop
      with curr-string = nil
      for e in list
      if (stringp e)
        do (setq curr-string (if curr-string (concatenate 'string curr-string e) e))
      else
        do (progn
             (when curr-string
               (coll curr-string)
               (setq curr-string nil))
             (coll e))
      finally (when curr-string
                (coll curr-string)))
    (coll)))

(defun no-recurse-tag-p (name)
  (member name '("anchor") :test #'equal))

(defun no-latex-tag-p (name)
  (member name '("anchor" "code") :test #'equal))

(defun parse-paragraph-content (s start recursive-p no-recurse no-latex)
  (collectors:with-collector (coll)
    (let ((len (length s))
          (beg start)
          (i start))
      (labels ((collect-str ()
                 (when (> i beg)
                   (coll (subseq s beg i)))))
        (loop
          while (< i len)
          do (let ((ch (aref s i)))
               (cond ((eql ch #\})
                      (if recursive-p
                          (progn
                            (collect-str)
                            (return (values (compress-strings (coll)) (1+ i))))
                          (error "Unexpected end-of-arglist character")))
                     ((eql ch #\@)
                      (cond ((and (< i (1- len))
                                  (eql (aref s (1+ i)) #\@))
                             ;; This is an escaped @-sign
                             (collect-str)
                             (setq beg (1+ i))
                             (incf i 2))
                            (no-recurse
                             (incf i))
                            (t
                             (let ((pos (position #\{ s :start (1+ i))))
                               (unless pos
                                 (error "No arglist found"))
                               (collect-str)
                               (let ((name (subseq s (1+ i) pos)))
                                 (multiple-value-bind (result next-pos)
                                     (parse-paragraph-content s (1+ pos) t (no-recurse-tag-p name) (no-latex-tag-p name))
                                   (let ((processed (process-markup-tag name result)))
                                     (dolist (val processed)
                                       (coll val)))
                                   (setq beg next-pos)
                                   (setq i next-pos)))))))
                     ((and (not no-latex)
                           (eql ch #\$)
                           (< i (1- len))
                           (eql (aref s (1+ i)) #\$))
                      ;; A LaTeX block should be appended as :CODE
                      (collect-str)
                      (let ((pos (search "$$" s :start2 (+ i 2))))
                        (unless pos
                          (error "No end of LaTeX section block found"))
                        (coll (list :code (format nil "$$~a$$"  (subseq s (+ i 2) pos))))
                        (setq i (+ pos 2))
                        (setq beg i)))
                     ((and (not no-latex)
                           (eql ch #\$))
                      (collect-str)
                      (let ((pos (position #\$ s :start (1+ i))))
                        (unless pos
                          (error "No end of inline LaTeX maths found"))
                        (coll (list :code (format nil "$~a$" (subseq s (1+ i) pos))))
                        (setq i (1+ pos))
                        (setq beg i)))
                     (t
                      ;; ELSE: Check next character
                      (incf i))))
          finally (progn
                    (when recursive-p
                      (error "End of string while scanning for end-of-arglist character"))
                    (collect-str)
                    (return (values (compress-strings (coll)) len))))))))

(defun parse-paragraph (s)
  (cons :paragraph (parse-paragraph-content s 0 nil nil nil)
        #+nil
        (markup-from-regexp "@([a-z]+){([^}]+)}" s
                            (lambda (reg-starts reg-ends)
                              (let ((cmd (subseq s (aref reg-starts 0) (aref reg-ends 0)))
                                    (content (subseq s (aref reg-starts 1) (aref reg-ends 1))))
                                (list (string-case:string-case (cmd)
                                        ("code" :code)
                                        ("var" :var)
                                        ("mref" :mref)
                                        ("emph" :italic)
                                        ("i" :italic)
                                        ("strong" :bold)
                                        ("b" :bold)
                                        ("anchor" :anchor)
                                        ("xref" :xref)
                                        ("fname" :fname)
                                        ("math" :math)
                                        ("mrefcomma" :mrefcomma)
                                        ("mrefdot" :mrefdot)
                                        ("ref" :ref)
                                        ("mxrefcomma" :mxrefcomma) ;; 2-arg
                                        ("mxref" :mxref) ;; 2-arg
                                        ("mxrefdot" :mxrefdot) ;; 2-arg
                                        ("file" :file)
                                        ("mrefparen" :mrefparen)
                                        ("footnote" :footnote)
                                        ("kbd" :kbd)
                                        ("key" :key) ;; Same as kbd?
                                        ("image" :image) ;; filename,size
                                        ("dfn" :dfn)
                                        ("url" :url)
                                        ("uref" :url)
                                        ("figure" :figure)
                                        ("verb" :verb)
                                        ("dotless" :dotless) ;; Only used to create a dotless i
                                        ("email" :email)
                                        ("pxref" :pxref)
                                        (t (error "Unknown cmd: ~s" cmd)))
                                      content)))
                            (lambda (s)
                              (list (cl-ppcre:regex-replace-all "@dots{}" s "…"))))))

(defun process-opencatbox (stream)
  (cons :catbox
        (loop
          for s = (read-line stream)
          until (cl-ppcre:scan "^@closecatbox *$" s)
          append (collectors:with-collector (coll)
                   (iterate-regexps (category "@category{([^}]+)}" s)
                     (coll (aref category 0)))
                   (coll)))))

(defun parse-node (args)
  (labels ((nil-or-trim (s)
             (if s
                 (let ((trimmed (string-trim " " s)))
                   (if (zerop (length trimmed)) nil trimmed))
                 nil)))
    (let* ((category-names (split-sequence:split-sequence #\, args))
           (name (string-trim " " (nth 0 category-names)))
           (next (nil-or-trim (nth 1 category-names)))
           (prev (nil-or-trim (nth 2 category-names)))
           (up (nil-or-trim (nth 3 category-names))))
      (list name next prev up))))

(alexandria:define-constant +newline-str+ (format nil "~c" #\Newline) :test #'equal)

(defun process-demo-code (stream extended)
  (multiple-value-bind (code input)
      (collectors:with-collectors (coll input-coll)
        (loop
          with curr = ""
          for s = (read-line stream)
          until (if extended
                    (cl-ppcre:scan "^@c ===endx===" s)
                    (cl-ppcre:scan "^@c ===end===" s))
          unless (zerop (length s))
            do (progn
                 (process-single-line-command s
                   (("@c +input:(.*[^ ]) *$" strings)
                    (input-coll (aref strings 0)))
                   (("@c +(.*[^ ]) *$" strings)
                    (setq curr (format nil "~a~a~a" curr (if (zerop (length curr)) "" +newline-str+) (aref strings 0))))
                   (("^[^@]")
                    (setq curr (format nil "~a~c~a" curr #\Newline s)))
                   (t
                    (error "Demo code block does not have the expected format: ~s" s)))
                 (when (cl-ppcre:scan "(?:;|\\$) *$" curr)
                   (coll curr)
                   (setq curr ""))))
        (values (coll) (input-coll)))
    ;; After the example code, the following example group needs to be skipped
    (loop
      for s = (read-line stream)
      until (cl-ppcre:scan "^@example *$" s)
      unless (cl-ppcre:scan "^ *$" s)
        do (error "No example block following demo code"))
    (let ((example (skip-block stream "^@end example$")))
      `(:demo-code (:demo-source . ,code)
                   ,@(if input `((:input . ,input)) nil)
                   (:example-info . ,example)))))

(defun parse-arglist (args-string)
  (let ((parsed-arglist (with-input-from-string (s args-string)
                          (parse-stream s nil))))
    (unless (and (alexandria:sequence-of-length-p parsed-arglist 1)
                 (eq (caar parsed-arglist) :paragraph))
      (error "Unexpected arglist format: ~s" args-string))
    (cdar parsed-arglist)))

(defun parse-deffn (stream type name args)
  (let ((parsed-arglist (parse-arglist args)))
    (list* :deffn type name parsed-arglist nil
           (parse-stream stream (cl-ppcre:create-scanner "^@end +deffn")))))

(defun parse-multiple-deffn (stream type name args)
  (let ((definitions (loop
                       with completed-p = nil
                       for line = (read-line stream)
                       collect (multiple-value-bind (match strings)
                                   (cl-ppcre:scan-to-strings "^@fname{([^ }]+)} +(.*[^ ]) *$" line)
                                 (unless match
                                   (error "Unexpected format for @fname row: ~s" line))
                                 (let ((fname-name (aref strings 0))
                                       (fname-args (aref strings 1)))
                                   (if (cl-ppcre:scan "@$" fname-args)
                                       ;; If the line ends with a @, then trim off the last character
                                       (setq fname-args (string-trim " " (subseq fname-args 0 (1- (length fname-args)))))
                                       ;; ELSE: We're done
                                       (setq completed-p t))
                                   (let ((parsed-arglist (parse-arglist fname-args)))
                                     (list fname-name parsed-arglist))))
                       until completed-p)))
    (list* :deffn type name (if args (parse-arglist args) nil) definitions
           (parse-stream stream (cl-ppcre:create-scanner "^@end +deffn")))))

(defun parse-defvr (stream type name)
  (list* :defvr (list type name)
         (parse-stream stream (cl-ppcre:create-scanner "^@end +defvr"))))

(defun parse-itemize (stream args)
  ;; Any content before the first @item will be dropped
  ;; We should probably check for anchors here
  (let ((bullet-p (cl-ppcre:scan "@bullet" args))
        (all-content (skip-block stream "^@end itemize")))
    (collectors:with-collector (items-collector)
      (let ((current-item nil))
        (labels ((parse-item ()
                   (when current-item
                     (let ((string (make-multiline-string (reverse current-item))))
                       (with-input-from-string (s string)
                         (items-collector (parse-stream s nil))))
                     (setq current-item nil))))
          (loop
            with before-first-item = t
            for line in all-content
            do (multiple-value-bind (match strings)
                   (cl-ppcre:scan-to-strings "^@item(?: +(.*[^ ]))? *$" line)
                 (if match
                     (progn
                       (cond (before-first-item
                              (setq before-first-item nil))
                             (t
                              (parse-item)))
                       (setq current-item (let ((row (aref strings 0)))
                                            (if row (list row) nil))))
                     ;; ELSE: Plain line
                     (push line current-item)))
            finally (parse-item))))
      (list* :itemize (if bullet-p '(:bullet) nil) (items-collector)))))

(defun parse-ifhtml (stream)
  (let ((content (skip-block stream "^@end ifhtml")))
    (loop
      for row in content
      append (multiple-value-bind (match strings)
                 (cl-ppcre:scan-to-strings "@image{([^}]+)}" row)
               (if match
                   (list (list :image (aref strings 0)))
                   nil)))))

(defun parse-stream (stream end-tag)
  (collectors:with-collector (info-collector)
    (let ((current-paragraph (make-array 0 :element-type 'character :adjustable t :fill-pointer t))
          (terminating-string nil))
      (labels ((collect-paragraph ()
                 (when (plusp (length current-paragraph))
                   (info-collector (parse-paragraph current-paragraph))
                   (setq current-paragraph (make-array 0 :element-type 'character :adjustable t :fill-pointer t)))))
        (loop
          for s = (read-line stream nil nil)
          do (setq terminating-string s)
          until (or (null s)
                    (and end-tag
                         (cl-ppcre:scan end-tag s)))
          do (cond ((cl-ppcre:scan "^@[a-z]+(?: |$)" s)
                    (process-single-line-command s
                      (("^@c ===beg(x?)===" args)
                       (collect-paragraph)
                       (info-collector (process-demo-code stream (not (equal (aref args 0) "")))))
                      (("^@menu *$") (collect-paragraph) (info-collector (process-menu stream)))
                      (("^@opencatbox *$") (collect-paragraph) (info-collector (process-opencatbox stream)))

                      (("^@node +(.*)$" args)
                       (collect-paragraph)
                       (info-collector (cons :node (parse-node (aref args 0)))))

                      (("@section +(.*[^ ]) *$" name)
                       (collect-paragraph)
                       (info-collector (list :section (aref name 0))))

                      (("@subsection +(.*[^ ]) *$" name)
                       (collect-paragraph)
                       (info-collector (list :subsection (aref name 0))))

                      (("^@c ") nil)
                      (("^@ifhtml *$") (dolist (image (parse-ifhtml stream))
                                         (info-collector image)))
                      (("^@iftex *$") (skip-block stream "^@end iftex"))

                      (("^@deffn *{([^}]+)} +([^ ]+)(?: *(.*[^ ]))? *@ *$" args)
                       (collect-paragraph)
                       (info-collector (parse-multiple-deffn stream (aref args 0) (aref args 1) (aref args 2))))

                      (("^@deffn *{([^}]+)} +([^ ]+) +(.*[^ @]) *$" args)
                       (collect-paragraph)
                       (info-collector (parse-deffn stream (aref args 0) (aref args 1) (aref args 2))))

                      (("^@defvr +{([^}]+)} +(.*[^ ]) *$" args)
                       (collect-paragraph)
                       (info-collector (parse-defvr stream (aref args 0) (aref args 1))))

                      (("^@example *$")
                       (collect-paragraph)
                       (info-collector (cons :pre (skip-block stream "^@end example"))))

                      (("^@itemize(?: +(.*))?$" args)
                       (collect-paragraph)
                       (info-collector (parse-itemize stream (aref args 0))))))
                   ((cl-ppcre:scan "^ *$" s)
                    (collect-paragraph))
                   (t
                    ;; This is normal content, so we'll collect it into the output string
                    (when (plusp (length current-paragraph))
                      (vector-push-extend #\Space current-paragraph))
                    (loop for ch across s do (vector-push-extend ch current-paragraph)))))
        (collect-paragraph)
        (values (info-collector) terminating-string)))))

(defun parse-file (file)
  (log:trace "Parsing file: ~s" file)
  (with-open-file (stream file :direction :input :external-format :utf-8)
    (parse-stream stream nil)))

(defun evaluate-one-demo-src-line (src)
  (when (cl-ppcre:scan "^ *:" src)
    (return-from evaluate-one-demo-src-line nil))
  (when (or (not (maxima::checklabel maxima::$inchar))
	    (not (maxima::checklabel maxima::$outchar)))
    (incf maxima::$linenum))
  (let* ((expr (string-to-maxima-expr src))
         (c-tag (maxima::makelabel maxima::$inchar))
         (out (make-string-output-stream)))
    (setf (symbol-value c-tag) expr)
    (let ((eval-ret (catch 'maxima::macsyma-quit
                      (catch 'eval-expr-error
                        (let ((result (let ((*use-clim-retrieve* nil)
                                            (*standard-output* out))
                                        (handler-bind ((error (lambda (condition)
                                                                (throw 'eval-expr-error (cons :lisp-error condition)))))
                                          (eval-maxima-expression expr)))))
                          (let ((d-tag (maxima::makelabel maxima::$outchar)))
                            (setq maxima::$% result)
                            (setf (symbol-value d-tag) result)
                            (cons :result result)))))))
      (unless (and (consp eval-ret)
                   (eq (car eval-ret) :result))
        (error "Error when evaluating command: ~s: ~s" src eval-ret))
      (cdr eval-ret))))

(defun evaluate-demo-src (src standard-input-content)
  (uiop:with-temporary-file (:stream input)
    (with-standard-io-syntax
      (let ((*print-circle* t))
        (print (list src standard-input-content) input)))
    (close input)
    (uiop:with-temporary-file (:stream output)
      (uiop:with-temporary-file (:stream error-out)
        (handler-case
            (let* ((dir (asdf:system-relative-pathname (asdf:find-system :maxima-client) #p"infoparser/"))
                   (exec-file (merge-pathnames "maxima-parser.bin" dir))
                   (exec-name (namestring exec-file)))
              (uiop:run-program (list exec-name "--dynamic-space-size" "3000")
                                :input (pathname input)
                                :output (pathname output)
                                :error-output (pathname error-out))
              (close output)
              (with-open-file (s (pathname output) :external-format :utf-8)
                (read s)))
          (uiop:subprocess-error (condition)
            (close error-out)
            (format t "Error when calling external program: ~a~%Error output:~%~a"
                    condition
                    (uiop:read-file-string (pathname error-out)))
            (list :error "Error when calling external program"))
          (error (condition)
            (format t "Error while evaluating Maxima expression: ~a" condition)
            (list :error "Error evaluating expression")))))))

(defun resolve-example-code (info-content)
  (labels ((parse-branch (nodes)
             (loop
               for v in nodes
               if (listp v)
                 collect (if (eq (car v) :demo-code)
                             (let ((src (cdr (assoc :demo-source (cdr v))))
                                   (input (cdr (assoc :input (cdr v))))
                                   (example-info (cdr (assoc :example-info (cdr v)))))
                               `(:demo-code (:demo-source . ,src)
                                            (:example-info . ,example-info)
                                            (:demo-result . ,(evaluate-demo-src src input))))
                             (parse-branch v))
               else
                 collect v)))
    (parse-branch info-content)))

(defun parse-and-write-file (file destination-directory &key skip-example)
  (log:info "Parsing file: ~s" file)
  (let* ((content (parse-file file))
         (processed (if skip-example
                        content
                        (resolve-example-code content))))
    (with-open-file (s (make-pathname :name (pathname-name file)
                                      :type "lisp"
                                      :defaults destination-directory)
                       :direction :output
                       :if-exists :supersede
                       :external-format :utf-8)
      (with-standard-io-syntax
        (let ((*print-circle* t)
              (*print-pretty* t))
          (print processed s))))))

(defun parse-doc-directory (info-directory destination-directory &key skip-example)
  "Parse all texinfo files in the given directory and output
corresponding lisp files to the output directory."
  (let ((dir (merge-pathnames "*.texi" info-directory)))
    (dolist (file (directory dir))
      (parse-and-write-file file destination-directory :skip-example skip-example))))

(defun make-multiline-string (lines)
  (with-output-to-string (s)
    (loop
      for line in lines
      do (format s "~a~%" line))))

(defun scan-categories (definition-type type name hash content)
  (loop
    for v in content
    when (eq (car v) :catbox)
      do (loop for category-name in (cdr v)
               do (let ((refs (gethash category-name hash)))
                    (setf (gethash category-name hash)
                          (cons (list definition-type type name) refs))))))

(defun extract-functions (content functions-hash nodes-hash categories file)
  (loop
    for v in content
    when (and (listp v)
              (or (eq (car v) :deffn)
                  (eq (car v) :defvr)))
      do (let* ((args (second v))
                (type (first args))
                (fn (second args))
                (prev (gethash fn functions-hash)))
           (setf (gethash fn functions-hash) (cons (list (car v) file type)
                                                   prev))
           (scan-categories (car v) type fn categories (cddr v)))
    when (and (listp v)
              (eq (car v) :node))
      do (let ((name (second v)))
           (setf (gethash name nodes-hash) (list (cdr v) file)))))

(defun generate-index ()
  (let ((destination-dir (resolve-destination-dir))
        (index-filename "index")
        (functions (make-hash-table :test 'equal))
        (nodes (make-hash-table :test 'equal))
        (categories (make-hash-table :test 'equal)))
    (loop
      for file in (directory (merge-pathnames #p"*.lisp" destination-dir))
      for name = (pathname-name file)
      unless (equal name index-filename)
        do (let  ((content (with-open-file (s file :external-format :utf-8)
                             (with-standard-io-syntax
                               (read s)))))
             (extract-functions content functions nodes categories name)))
    (let* ((content-list (loop
                           for key being each hash-key in functions using (hash-value value)
                           collect (cons key value)))
           (symbols-sorted (sort content-list (lambda (o1 o2)
                                                (string< (string-downcase o1) (string-downcase o2)))
                                 :key #'car))
           (nodes-sorted (sort (loop for v being each hash-value in nodes collect v)
                               (lambda (o1 o2)
                                 (string< (string-downcase o1) (string-downcase o2)))
                               :key #'caar))
           (categories-sorted (sort (collectors:with-collector (categories-collector)
                                      (maphash (lambda (key value)
                                                 (categories-collector (cons key value)))
                                               categories)
                                      (categories-collector))
                                    #'string<
                                    :key #'car)))
      (with-open-file (s (merge-pathnames (format nil "~a.lisp" index-filename) destination-dir)
                         :direction :output
                         :if-exists :supersede
                         :external-format :utf-8)
        (with-standard-io-syntax
          (let ((*print-pretty* t))
            (print `((:symbols . ,symbols-sorted)
                     (:nodes . ,nodes-sorted)
                     (:categories . ,categories-sorted))
                   s))))
      nil)))

(defun resolve-example-code-external ()
  "This function is called from the standalone maxima expression evaluator."
  (with-maxima-package
    (maxima::initialize-runtime-globals))
  (setq *debugger-hook* nil)
  (let ((src-data (with-standard-io-syntax
                    (read))))
    (with-input-from-string (in (make-multiline-string (second src-data)))
      (let ((*standard-input* in))
        (let ((result (loop
                        for v in (first src-data)
                        collect (let ((res (evaluate-one-demo-src-line v)))
                                  (if (cl-ppcre:scan "\\$ *$" v)
                                      ;; If the command ended with a $, don't save the result
                                      (cons :no-result nil)
                                      ;; ELSE: Normal command, the result needs to be saved
                                      (cons :result res))))))
          (with-standard-io-syntax
            (let ((*print-circle* t))
              (print result))))))))

(defun copy-file-to-dir (file dir)
  (let ((destination-name (merge-pathnames (format nil "~a.~a" (pathname-name file) (pathname-type file)) dir)))
    (uiop:copy-file file destination-name)))

(defun convert-pdf-to-png (file destination-name)
  (uiop:run-program (list "gs" "-sDEVICE=png16m" (format nil "-sOutputFile=~a" (namestring destination-name))
                          "-dNOPAUSE" "-dBATCH" "-dQUIET" "-r85" (namestring file))))

(defun convert-figures ()
  (let ((destination-dir (asdf:system-relative-pathname (asdf:find-system :maxima-client) #p"infoparser/figures/")))
    (ensure-directories-exist destination-dir)
    (dolist (file (directory (merge-pathnames #p"*.*" (asdf:system-relative-pathname (asdf:find-system :maxima) "../doc/info/figures/"))))
      (multiple-value-bind (match strings)
          (cl-ppcre:scan-to-strings "\\.(pdf|png)" (namestring file))
        (when match
          (log:trace "Converting file: ~s" file)
          (let ((destination-name (merge-pathnames (format nil "~a.png" (pathname-name file))
                                                   destination-dir)))
            (string-case:string-case ((aref strings 0))
              ("png" (copy-file-to-dir file destination-dir))
              ("pdf" (convert-pdf-to-png file destination-name)))))))))

(defun generate-doc-directory (&key skip-example skip-figures)
  "Generate lisp files for all the texinfo files in the maxima distribution."
  (let ((destination-directory (resolve-destination-dir)))
    (ensure-directories-exist destination-directory)
    (parse-doc-directory (asdf:system-relative-pathname (asdf:find-system :maxima) "../doc/info/")
                         destination-directory
                         :skip-example skip-example)
    (parse-doc-directory (asdf:system-relative-pathname (asdf:find-system :maxima-client) "info/")
                         destination-directory
                         :skip-example skip-example)
    (generate-index)
    (unless skip-figures
      (convert-figures))))

(defun generate-one-file (name)
  (let ((file (merge-pathnames (format nil "~a.texi" name)
                               (asdf:system-relative-pathname (asdf:find-system :maxima) "../doc/info/"))))
    (parse-and-write-file file (resolve-destination-dir))))
